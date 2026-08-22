"""
Bastion server helpers — request caching and rate limiting for the
paper-light HTTP tier.

The bastion is an HTTP gateway that turns curl-friendly ``POST``s into
swarm jobs. Two guardrails keep it cheap and friendly to the public
network:

* :class:`ResultCache` — an in-memory, TTL-bounded LRU keyed by the
  *exact* request (base64 ``document_blob`` + timeout). Identical
  requests short-circuit execution; semantic dedupe is deliberately out
  of scope.
* :class:`TokenBucketLimiter` — a per-client-IP token bucket.
* :class:`ConcurrencyLimiter` — a global cap on simultaneous jobs.

Nothing here touches Podman or iroh, so it is unit-testable in isolation.
"""
from __future__ import annotations

import asyncio
import hashlib
import time
from typing import Any


class ResultCache:
    """
    A TTL-bounded LRU of completed run responses.

    ``max_entries`` bounds memory; ``ttl_seconds`` bounds staleness. The
    cache is single-event-loop by convention (the FastAPI app owns one
    loop), so no locking is needed.
    """

    def __init__(self, max_entries: int = 512, ttl_seconds: float = 300.0) -> None:
        self.max_entries = max_entries
        self.ttl_seconds = ttl_seconds
        self._entries: dict[str, tuple[float, dict]] = {}  # key -> (expiry, value)
        self._lru: dict[str, float] = {}                  # key -> last access

    @staticmethod
    def _key(document_blob: str, timeout: int) -> str:
        digest = hashlib.sha256()
        digest.update(document_blob.encode("utf-8"))
        digest.update(b"\x00")
        digest.update(str(int(timeout)).encode("ascii"))
        return digest.hexdigest()

    def _prune_expired(self, now: float) -> None:
        expired = [k for k, (expiry, _v) in self._entries.items() if expiry <= now]
        for key in expired:
            self._entries.pop(key, None)
            self._lru.pop(key, None)

    def _evict_lru(self) -> None:
        while len(self._entries) > self.max_entries and self._lru:
            lru_key = min(self._lru, key=self._lru.get)
            self._entries.pop(lru_key, None)
            self._lru.pop(lru_key, None)

    def get(self, document_blob: str, timeout: int) -> dict | None:
        """Return the cached response for an exact request, or ``None``."""
        key = self._key(document_blob, timeout)
        now = time.monotonic()
        entry = self._entries.get(key)
        if entry is None:
            return None
        expiry, value = entry
        if expiry <= now:
            self._entries.pop(key, None)
            self._lru.pop(key, None)
            return None
        self._lru[key] = now
        return value

    def put(self, document_blob: str, timeout: int, value: dict) -> None:
        """Store a response for an exact request."""
        key = self._key(document_blob, timeout)
        now = time.monotonic()
        self._prune_expired(now)
        self._entries[key] = (now + self.ttl_seconds, dict(value))
        self._lru[key] = now
        self._evict_lru()

    def clear(self) -> None:
        self._entries.clear()
        self._lru.clear()

    def __len__(self) -> int:
        return len(self._entries)


class TokenBucketLimiter:
    """
    A per-client-IP token bucket.

    ``rate`` is the sustained refill rate (tokens/second) and ``burst`` is
    the bucket capacity. Buckets that have fully refilled are periodically
    dropped so a flood of distinct IPs cannot grow the table unboundedly.
    """

    def __init__(
        self,
        rate: float = 1.0,
        burst: int = 60,
        prune_interval: float = 60.0,
    ) -> None:
        self.rate = rate
        self.burst = burst
        self._buckets: dict[str, tuple[float, float]] = {}  # ip -> (tokens, last)
        self._last_prune = time.monotonic()
        self._prune_interval = prune_interval

    def _refill(self, ip: str, now: float) -> float:
        tokens, last = self._buckets.get(ip, (float(self.burst), now))
        tokens = min(float(self.burst), tokens + (now - last) * self.rate)
        self._buckets[ip] = (tokens, now)
        return tokens

    def allow(self, ip: str, now: float | None = None) -> bool:
        """Whether ``ip`` may make one request right now."""
        now = time.monotonic() if now is None else now
        if now - self._last_prune >= self._prune_interval:
            self._prune(now)
        tokens = self._refill(ip, now)
        if tokens < 1.0:
            return False
        self._buckets[ip] = (tokens - 1.0, now)
        return True

    def _prune(self, now: float) -> None:
        """Drop buckets that have sat fully refilled (idle clients)."""
        stale = [
            ip
            for ip, (tokens, last) in self._buckets.items()
            if tokens >= float(self.burst) and now - last >= self._prune_interval
        ]
        for ip in stale:
            self._buckets.pop(ip, None)
        self._last_prune = now

    def __len__(self) -> int:
        return len(self._buckets)


class ConcurrencyLimiter:
    """
    A global cap on simultaneous jobs.

    ``acquire`` fails fast when the cap is reached (the caller returns
    503 rather than queueing unbounded work); ``release`` always runs in
    the caller's ``finally`` block.
    """

    def __init__(self, limit: int | None = 8) -> None:
        self.limit = limit
        self._active = 0
        self._lock = asyncio.Lock()

    async def acquire(self) -> bool:
        async with self._lock:
            if self.limit is not None and self._active >= self.limit:
                return False
            self._active += 1
            return True

    async def release(self) -> None:
        async with self._lock:
            if self._active > 0:
                self._active -= 1

    @property
    def active(self) -> int:
        return self._active


def client_ip(request) -> str:
    """
    The effective client IP, honoring the proxy header the bastion sits
    behind (Railway/Caddy set ``X-Forwarded-For``).
    """
    forwarded = request.headers.get("x-forwarded-for")
    if forwarded:
        return forwarded.split(",")[0].strip()
    client = getattr(request, "client", None)
    return getattr(client, "host", "unknown") if client else "unknown"


__all__ = [
    "ConcurrencyLimiter",
    "ResultCache",
    "TokenBucketLimiter",
    "client_ip",
]
