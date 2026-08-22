"""
Seed-mediated discovery.

Ephemeral uses iroh's Endpoint layer directly (the Python bindings do
not expose gossip), so discovery works the way the spikes validated:
nodes dial configured seed nodes, every ``hello`` handshake carries the
sender's dial-back ticket plus the peer entries it already knows, and
each side merges what it learns. Peers discovered this way can
subsequently be dialed directly by ticket.

Neighborhoods (from the roadmap) become a routing/partition field on top
of this table in a later phase; a client falls back to the default iroh
distributed peergroup when its neighborhood has no reachable members.
"""
from __future__ import annotations

import time
from dataclasses import dataclass
from typing import Iterable, Iterator


@dataclass
class PeerInfo:
    """A peer known to this node."""

    node_id: str
    ticket: str | None = None      # EndpointTicket to dial this peer (fallback)
    relay: str | None = None       # the peer's relay URL — dial by id + relay
    images: set[str] | None = None  # warm container images (for routing)
    active_jobs: int = 0           # jobs the peer is currently running
    max_jobs: int | None = None    # peer's concurrency ceiling (None = unknown)
    url: str | None = None         # peer's public HTTP(S) endpoint (bastions)
    last_seen: float = 0.0         # time.monotonic() of last contact


# How long a peer entry survives without being seen (directly or via
# gossip) before it is presumed dead and pruned. The maintenance loop
# re-dials the live swarm every ~60 s, so live nodes refresh constantly
# and stale ones age out within a few minutes of going quiet. Without
# this the table only ever grows: every hello re-shares every entry, so
# a node that died long ago keeps circulating through gossip forever.
PEER_TTL_SECONDS = 600.0


class PeerTable:
    """Thread-safe-by-convention table of known peers (single event loop)."""

    def __init__(self) -> None:
        self._peers: dict[str, PeerInfo] = {}

    def prune(self, ttl: float = PEER_TTL_SECONDS) -> int:
        """Drop peers not seen (directly or via gossip) within ``ttl`` seconds.

        Returns the number of entries evicted. Called on every merge and
        before every snapshot so stale entries both age out locally and
        stop propagating to other nodes through hello gossip.
        """
        now = time.monotonic()
        dead = [
            nid
            for nid, info in self._peers.items()
            if now - (info.last_seen or now) > ttl
        ]
        for nid in dead:
            del self._peers[nid]
        return len(dead)

    def merge(self, infos: Iterable[PeerInfo]) -> int:
        """
        Merge peer info, keeping the newest ticket/last_seen per node.
        Returns the number of newly discovered nodes.
        """
        new_count = 0
        now = time.monotonic()
        for info in infos:
            if not info.node_id:
                continue
            existing = self._peers.get(info.node_id)
            if existing is None:
                self._peers[info.node_id] = PeerInfo(
                    node_id=info.node_id,
                    ticket=info.ticket,
                    relay=info.relay,
                    images=set(info.images) if info.images else None,
                    active_jobs=info.active_jobs,
                    max_jobs=info.max_jobs,
                    url=info.url,
                    last_seen=info.last_seen or now,
                )
                new_count += 1
            else:
                if info.ticket:
                    existing.ticket = info.ticket
                if info.relay:
                    existing.relay = info.relay
                if info.images:
                    existing.images = set(info.images)
                if info.url:
                    existing.url = info.url
                existing.active_jobs = info.active_jobs
                existing.max_jobs = info.max_jobs
                existing.last_seen = info.last_seen or now
        # Evict stale entries after the merge so a dead peer arriving via
        # gossip can't linger until the next merge call.
        self.prune()
        return new_count

    def snapshot(self) -> list[dict]:
        """Peer entries for embedding in a ``hello`` frame."""
        self.prune()
        return [
            {
                "node_id": info.node_id,
                "ticket": info.ticket,
                "relay": info.relay,
                "images": sorted(info.images or []),
                "active_jobs": info.active_jobs,
                "max_jobs": info.max_jobs,
                "url": info.url,
            }
            for info in self._peers.values()
        ]

    def known_peer_ids(self) -> list[str]:
        return list(self._peers.keys())

    def ticket_for(self, node_id: str) -> str | None:
        info = self._peers.get(node_id)
        return info.ticket if info else None

    def relay_for(self, node_id: str) -> str | None:
        info = self._peers.get(node_id)
        return info.relay if info else None

    def info_for(self, node_id: str) -> PeerInfo | None:
        return self._peers.get(node_id)

    def __iter__(self) -> Iterator[PeerInfo]:
        return iter(self._peers.values())

    def __len__(self) -> int:
        return len(self._peers)
