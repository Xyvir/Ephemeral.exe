"""
Seed-mediated discovery.

Ephemeral uses iroh's Endpoint layer directly (the Python bindings do
not expose gossip), so discovery works the way the spikes validated:
nodes dial configured seed nodes, every ``hello`` handshake carries the
sender's dial-back ticket plus the peer entries it already knows, and
each side merges what it learns. Peers discovered this way can
subsequently be dialed directly by ticket.

Room codes (from the roadmap) become a routing/partition field on top of
this table in a later phase.
"""
from __future__ import annotations

import time
from dataclasses import dataclass
from typing import Iterable, Iterator


@dataclass
class PeerInfo:
    """A peer known to this node."""

    node_id: str
    ticket: str | None = None      # EndpointTicket to dial this peer
    last_seen: float = 0.0         # time.monotonic() of last contact


class PeerTable:
    """Thread-safe-by-convention table of known peers (single event loop)."""

    def __init__(self) -> None:
        self._peers: dict[str, PeerInfo] = {}

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
                    last_seen=info.last_seen or now,
                )
                new_count += 1
            else:
                if info.ticket:
                    existing.ticket = info.ticket
                existing.last_seen = info.last_seen or now
        return new_count

    def snapshot(self) -> list[dict]:
        """Peer entries for embedding in a ``hello`` frame."""
        return [
            {"node_id": info.node_id, "ticket": info.ticket}
            for info in self._peers.values()
        ]

    def known_peer_ids(self) -> list[str]:
        return list(self._peers.keys())

    def ticket_for(self, node_id: str) -> str | None:
        info = self._peers.get(node_id)
        return info.ticket if info else None

    def info_for(self, node_id: str) -> PeerInfo | None:
        return self._peers.get(node_id)

    def __iter__(self) -> Iterator[PeerInfo]:
        return iter(self._peers.values())

    def __len__(self) -> int:
        return len(self._peers)
