"""
Tests for the bastion server (paper-light HTTP gateway).

Pure tests — no iroh, no Podman, no network. Covers the exact-match result
cache, per-IP + concurrency rate limiting, the orchestration-only executor,
and the Gateway's ``compute=False`` wiring.

Run:  python test_bastion.py
"""
from __future__ import annotations

import asyncio
import base64
import time

from ephemeral_net.jobs import JobErrorEvent, JobRequest
from ephemeral_self_host.bastion import (
    ConcurrencyLimiter,
    ResultCache,
    TokenBucketLimiter,
    client_ip,
)
from ephemeral_self_host.gateway import Gateway, OrchestrationOnlyExecutor


def _blob(seed: str) -> str:
    return base64.b64encode(seed.encode("utf-8")).decode("ascii")


def test_result_cache_exact_match():
    cache = ResultCache(max_entries=16, ttl_seconds=60)
    blob = _blob("```python\nprint(1)\n```")
    assert cache.get(blob, 300) is None
    cache.put(blob, 300, {"exit_code": 0, "stdout": "1\n"})
    assert cache.get(blob, 300)["stdout"] == "1\n"
    # The timeout is part of the identity: a different timeout misses.
    assert cache.get(blob, 60) is None
    # A different document (even a semantically identical one) misses.
    assert cache.get(_blob("```python\nprint( 1 )\n```"), 300) is None
    print("PASS: result cache keys on exact document_blob + timeout")


def test_result_cache_lru_and_expiry():
    cache = ResultCache(max_entries=2, ttl_seconds=0.05)
    for i in range(3):
        cache.put(_blob(f"doc{i}"), 300, {"i": i})
    # LRU: doc0 evicted when doc2 was inserted.
    assert cache.get(_blob("doc0"), 300) is None
    assert cache.get(_blob("doc1"), 300)["i"] == 1
    assert cache.get(_blob("doc2"), 300)["i"] == 2
    # TTL: everything expires.
    time.sleep(0.06)
    assert cache.get(_blob("doc1"), 300) is None
    assert cache.get(_blob("doc2"), 300) is None
    print("PASS: result cache evicts LRU and expires by TTL")


def test_token_bucket_limiter():
    limiter = TokenBucketLimiter(rate=0.0, burst=2)  # no refill
    assert limiter.allow("1.2.3.4") is True
    assert limiter.allow("1.2.3.4") is True
    assert limiter.allow("1.2.3.4") is False
    # Buckets are per-IP, so another client still has its own allowance.
    assert limiter.allow("5.6.7.8") is True
    print("PASS: token bucket limits per client IP with a burst")


def test_concurrency_limiter():
    async def run():
        limiter = ConcurrencyLimiter(limit=2)
        assert await limiter.acquire() is True
        assert await limiter.acquire() is True
        assert await limiter.acquire() is False  # cap reached
        await limiter.release()
        assert await limiter.acquire() is True
        await limiter.release()
        await limiter.release()
        assert limiter.active == 0
        # limit=None means uncapped.
        uncapped = ConcurrencyLimiter(limit=None)
        assert await uncapped.acquire() is True
        assert await uncapped.acquire() is True

    asyncio.run(run())
    print("PASS: concurrency limiter caps simultaneous jobs and releases cleanly")


def test_client_ip_proxy_header():
    class _Client:
        host = "10.0.0.1"

    class _Request:
        def __init__(self, headers):
            self.headers = headers
            self.client = _Client()

    forwarded = _Request({"x-forwarded-for": "1.2.3.4, 10.0.0.1"})
    assert client_ip(forwarded) == "1.2.3.4"
    direct = _Request({})
    assert client_ip(direct) == "10.0.0.1"
    print("PASS: client_ip honors X-Forwarded-For then the socket address")


def test_orchestration_only_executor():
    class _Core:
        def prepare(self, request):
            return "sanitized-md", ["docker.io/library/alpine:latest"]

    executor = OrchestrationOnlyExecutor(_Core())
    assert executor.is_warm("anything") is False

    async def run():
        req = JobRequest(job_id="j1", document_blob=_blob("```bash\necho hi\n```"))
        events = [e async for e in executor(req)]
        assert len(events) == 1
        assert isinstance(events[0], JobErrorEvent)
        assert "orchestration-only" in events[0].message

    asyncio.run(run())
    print("PASS: orchestration-only executor rejects local runs with a clear error")


class _FakeBastionNode:
    """Node-like object recording lifecycle calls for the Gateway."""

    def __init__(self):
        self.executor = None
        self.started = False
        self.closed = False
        self.bootstrapped = []

    async def start(self):
        self.started = True

    async def close(self):
        self.closed = True

    async def bootstrap(self, seeds):
        self.bootstrapped = list(seeds)

    async def bootstrap_nodes(self, nodes):
        self.bootstrapped = list(nodes)

    async def bootstrap_from_list(self):
        self.bootstrapped = ["list"]

    def node_id(self):
        return "fake-bastion"

    def warm_images(self):
        return []


def test_gateway_orchestration_only_wiring():
    async def run():
        fake = _FakeBastionNode()
        gw = Gateway(compute=False, node_factory=lambda **kw: fake)
        await gw.start()
        assert fake.started
        from ephemeral_net.fanout import FanoutExecutor
        from ephemeral_net.offload import OffloadingExecutor
        from ephemeral_net.sandbox import CoreJobExecutor

        # Same fan-out + offloading chain, but the local runner is wrapped
        # in the orchestration-only executor instead of running Podman.
        assert isinstance(fake.executor, FanoutExecutor)
        assert isinstance(fake.executor.local, OffloadingExecutor)
        assert isinstance(fake.executor.local.local, OrchestrationOnlyExecutor)
        assert isinstance(fake.executor.local.local.core, CoreJobExecutor)
        await gw.close()
        assert fake.closed

    asyncio.run(run())
    print("PASS: compute=False wires the orchestration-only local runner")


def main():
    test_result_cache_exact_match()
    test_result_cache_lru_and_expiry()
    test_token_bucket_limiter()
    test_concurrency_limiter()
    test_client_ip_proxy_header()
    test_orchestration_only_executor()
    test_gateway_orchestration_only_wiring()
    print("\n=== ALL BASTION TESTS PASSED ===")


if __name__ == "__main__":
    main()
