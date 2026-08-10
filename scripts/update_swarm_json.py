"""
Refresh ``docs/swarm.json`` with the currently-live swarm.

Thin/first-time joiners (the wasm SPA, paper-thin REST clients, other
nodes) can fetch this file instead of running a node themselves. A
scheduled GitHub Action (``.github/workflows/swarm-bootstrap.yml``) runs
this every six hours: it joins the swarm as a throwaway client, dials the
compiled-in seed nodes *plus* whatever the previous list knew (so the
list survives a dead compiled seed), and commits the refreshed list.

File shape:

    {
      "updated": "2026-08-10T12:00:00Z",
      "relay": "https://use1-1.relay.n0.iroh.link.",
      "nodes": [
        {"node_id": "…", "relay": "…", "ticket": "…", "images": ["…"]},
        ...
      ]
    }

``node_id`` + ``relay`` are the stable, iroh-native dial target; ``ticket``
is kept as a fallback for clients that still dial by EndpointTicket.

Usage:
    python scripts/update_swarm_json.py [--out docs/swarm.json]
"""
from __future__ import annotations

import argparse
import asyncio
import json
import sys
import time
from pathlib import Path

# ``python scripts/update_swarm_json.py`` puts ``scripts/`` on sys.path,
# not the repo root — add the root so ``ephemeral_net`` imports.
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from ephemeral_net.node import Node
from ephemeral_net.swarm import DEFAULT_RELAY, DEFAULT_SWARM_NODES

DEFAULT_OUT = Path(__file__).resolve().parent.parent / "docs" / "swarm.json"


def _existing_targets(out_path: Path) -> list[tuple[str, str | None, str | None]]:
    """``(node_id, relay, ticket)`` entries the previous list knew about."""
    try:
        data = json.loads(out_path.read_text(encoding="utf-8"))
    except Exception:
        return []
    nodes: list[tuple[str, str | None, str | None]] = []
    for entry in data.get("nodes") or []:
        node_id = entry.get("node_id")
        if node_id:
            nodes.append((node_id, entry.get("relay"), entry.get("ticket")))
    return nodes


async def discover(out_path: Path) -> dict:
    """Join the swarm, dial known nodes, and return the refreshed list."""
    node = Node(relay="n0")
    await node.start()
    try:
        # Compiled seed nodes (by id + relay), plus whatever the previous
        # list knew — the list keeps regenerating even if the compiled
        # seed goes offline, as long as any member stays reachable.
        targets: dict[str, tuple[str | None, str | None]] = {}
        for node_id, relay in DEFAULT_SWARM_NODES:
            targets[node_id] = (relay, None)
        for node_id, relay, ticket in _existing_targets(out_path):
            targets.setdefault(node_id, (relay, ticket))

        for node_id, (relay, ticket) in targets.items():
            try:
                if relay:
                    await asyncio.wait_for(node.dial_node(node_id, relay), timeout=20)
                elif ticket:
                    await asyncio.wait_for(node.dial(ticket), timeout=20)
            except Exception as e:  # unreachable — the list keeps it for next time
                print(f"  - {node_id[:12]}... unreachable: {e}", flush=True)

        my_id = node.node_id()
        # Newly-seen nodes (seed + whatever its hello carried) ∪ previous
        # list, deduped. Keeping un-reachable entries lets the next run try
        # them again and keeps thin clients pointed at recovering nodes.
        nodes: dict[str, dict] = {}
        for info in node.table:
            if info.node_id == my_id:
                continue
            nodes[info.node_id] = {
                "node_id": info.node_id,
                "relay": info.relay,
                "ticket": info.ticket,
                "images": sorted(info.images or []),
            }
        for node_id, relay, ticket in _existing_targets(out_path):
            nodes.setdefault(
                node_id,
                {"node_id": node_id, "relay": relay, "ticket": ticket, "images": []},
            )

        return {
            "updated": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
            "relay": DEFAULT_RELAY,
            "nodes": sorted(nodes.values(), key=lambda n: n["node_id"]),
        }
    finally:
        await node.close()


async def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--out", type=Path, default=DEFAULT_OUT)
    args = parser.parse_args()

    result = await discover(args.out)
    args.out.parent.mkdir(parents=True, exist_ok=True)
    tmp = args.out.with_name(args.out.name + ".tmp")
    tmp.write_text(json.dumps(result, indent=2) + "\n", encoding="utf-8")
    tmp.replace(args.out)
    print(
        f"swarm.json updated: {len(result['nodes'])} node(s) -> {args.out}",
        flush=True,
    )


if __name__ == "__main__":
    asyncio.run(main())
