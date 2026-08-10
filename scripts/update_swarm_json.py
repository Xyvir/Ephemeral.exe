"""
Refresh ``docs/swarm.json`` with the currently-live swarm.

Thin/first-time joiners (the wasm SPA, paper-thin REST clients, other
nodes) can fetch this file instead of running a node themselves. A
scheduled GitHub Action (``.github/workflows/swarm-bootstrap.yml``) runs
this every six hours: it joins the swarm as a throwaway client, dials a
single genesis anchor *plus* whatever the previous list knew (so the
list survives a dead genesis and regenerates from its own members), and
commits the refreshed list.

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
import os
import sys
import time
import urllib.request
from pathlib import Path

# ``python scripts/update_swarm_json.py`` puts ``scripts/`` on sys.path,
# not the repo root — add the root so ``ephemeral_net`` imports.
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from ephemeral_net.node import Node
from ephemeral_net.swarm import DEFAULT_RELAY

DEFAULT_OUT = Path(__file__).resolve().parent.parent / "docs" / "swarm.json"

# The genesis anchor — the ONLY hard-coded node in the whole system, and it
# lives here in the refresh script, never in the shipped binaries. It exists
# solely to bootstrap the very first, empty list: once the list has any live
# member, it regenerates from its own members and the genesis node can go
# offline forever. Override with the SWARM_GENESIS repo variable / env var
# (comma-separated node_id@relay) or the --genesis flag.
GENESIS_DEFAULT: list[tuple[str, str]] = [
    ("154e7308b6af310df575c7c90bc8fe86146cfef036ac098662768a4f3c411ec5", DEFAULT_RELAY),
]


def parse_genesis(value: str | None) -> list[tuple[str, str]]:
    """Parse a ``node_id@relay`` list; None/empty uses :data:`GENESIS_DEFAULT`."""
    if not value or not value.strip():
        return list(GENESIS_DEFAULT)
    nodes: list[tuple[str, str]] = []
    for raw in value.split(","):
        raw = raw.strip()
        if not raw:
            continue
        if "@" in raw:
            node_id, relay = raw.split("@", 1)
            nodes.append((node_id.strip(), relay.strip() or DEFAULT_RELAY))
        else:
            nodes.append((raw, DEFAULT_RELAY))
    return nodes


def pick_anchor(result: dict, genesis: list[tuple[str, str]]) -> tuple[str, str] | None:
    """
    The node the DNS TXT record should point at.

    First contact must reach a LIVE node, so prefer a member reached
    THIS run (fastest RTT first), falling back to the genesis anchor,
    then the first listed entry. ``None`` when the list is empty.
    """
    nodes = result.get("nodes") or []
    reached = [n for n in nodes if n.get("rtt_ms") is not None]
    if reached:
        n = reached[0]
        return n["node_id"], n.get("relay") or DEFAULT_RELAY
    if genesis:
        return genesis[0]
    if nodes:
        n = nodes[0]
        return n["node_id"], n.get("relay") or DEFAULT_RELAY
    return None


def update_dns_txt(
    anchor: tuple[str, str], token: str | None, hostname: str | None
) -> bool:
    """
    Point the swarm DNS TXT record at ``anchor`` (node_id, relay).

    The scheduled refresh keeps a TXT record (``iroh1:<node_id>;<relay>``)
    in sync with the list so thin/first-time joiners have an independent,
    tiered path to the swarm when GitHub itself is unreachable (see
    ``ephemeral_net.swarm.fetch_swarm_anchor_dns``). Uses the Cloudflare
    API — ``EPHEMERAL_DNS_TOKEN`` (secret), ``EPHEMERAL_DNS_TXT`` (the
    record hostname), optional ``EPHEMERAL_DNS_ZONE`` (the DNS zone name;
    auto-detected by longest suffix when unset). Returns True when the
    record was written or already current.
    """
    if not token or not hostname:
        return False
    zone = (os.environ.get("EPHEMERAL_DNS_ZONE") or "").strip()
    headers = {
        "Authorization": f"Bearer {token}",
        "Content-Type": "application/json",
    }

    def _api(method: str, url: str, body: dict | None = None) -> dict:
        data = json.dumps(body).encode("utf-8") if body is not None else None
        req = urllib.request.Request(url, data=data, headers=headers, method=method)
        with urllib.request.urlopen(req, timeout=15) as res:
            return json.loads(res.read().decode("utf-8"))

    try:
        if zone:
            zones = _api(
                "GET",
                f"https://api.cloudflare.com/client/v4/zones?name={zone}",
            ).get("result") or []
        else:
            zones = _api("GET", "https://api.cloudflare.com/client/v4/zones").get(
                "result"
            ) or []
            zones = [
                z
                for z in zones
                if hostname == z.get("name")
                or hostname.endswith("." + (z.get("name") or ""))
            ]
        if not zones:
            print(f"  DNS: no zone found for {hostname}", flush=True)
            return False
        zid = max(zones, key=lambda z: len(z.get("name") or ""))["id"]
        found = _api(
            "GET",
            f"https://api.cloudflare.com/client/v4/zones/{zid}"
            f"/dns_records?name={hostname}&type=TXT",
        ).get("result") or []
        content = f"iroh1:{anchor[0]};{anchor[1]}"
        body = {"type": "TXT", "name": hostname, "content": content, "ttl": 120}
        if found:
            if found[0].get("content") == content:
                print(f"  DNS: TXT {hostname} already current", flush=True)
                return True
            _api(
                "PUT",
                f"https://api.cloudflare.com/client/v4/zones/{zid}"
                f"/dns_records/{found[0]['id']}",
                body,
            )
        else:
            _api(
                "POST",
                f"https://api.cloudflare.com/client/v4/zones/{zid}/dns_records",
                body,
            )
        print(f"  DNS: TXT {hostname} -> {content}", flush=True)
        return True
    except Exception as e:
        print(f"  DNS: update failed: {e}", flush=True)
        return False


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


async def discover(out_path: Path, max_nodes: int, genesis: list[tuple[str, str]]) -> dict:
    """Join the swarm, dial known nodes, and return the refreshed list."""
    node = Node(relay="n0")
    await node.start()
    try:
        # The genesis anchor (first-ever list only), plus whatever the
        # previous list knew — the list keeps regenerating from its own
        # members, so a dead genesis node doesn't matter after the first run.
        targets: dict[str, tuple[str | None, str | None]] = {}
        for node_id, relay in genesis:
            targets[node_id] = (relay, None)
        for node_id, relay, ticket in _existing_targets(out_path):
            targets.setdefault(node_id, (relay, ticket))

        reached: dict[str, float] = {}  # node_id -> hello RTT (this run)
        for node_id, (relay, ticket) in targets.items():
            try:
                if relay:
                    peer = await asyncio.wait_for(node.dial_node(node_id, relay), timeout=20)
                elif ticket:
                    peer = await asyncio.wait_for(node.dial(ticket), timeout=20)
                else:
                    continue
                reached[node_id] = peer.rtt if peer.rtt is not None else 0.0
            except Exception as e:  # unreachable — the list keeps it for next time
                print(f"  - {node_id[:12]}... unreachable: {e}", flush=True)

        my_id = node.node_id()
        # Everything we know about: hello-learned nodes (seed + its peers)
        # ∪ previous list, deduped. Keeping un-reachable entries lets the
        # next run retry them and keeps thin clients pointed at recovering
        # nodes — but they rank last and only fill space under the cap.
        infos: dict[str, dict] = {}
        for info in node.table:
            if info.node_id == my_id:
                continue
            infos[info.node_id] = {
                "node_id": info.node_id,
                "relay": info.relay,
                "ticket": info.ticket,
                "images": sorted(info.images or []),
            }
        for node_id, relay, ticket in _existing_targets(out_path):
            infos.setdefault(
                node_id,
                {"node_id": node_id, "relay": relay, "ticket": ticket, "images": []},
            )

        # Rank: genesis anchor first, then nodes reached this run (fastest
        # RTT first), then hello-learned peers, then stale previous
        # entries — capped so the address list stays small and fresh.
        seed_ids = [nid for nid, _ in genesis]
        known = set(node.table.known_peer_ids())

        def rank_key(nid: str) -> tuple[int, float, str]:
            if nid in seed_ids:
                return (0, 0.0, nid)
            if nid in reached:
                return (1, reached[nid], nid)
            if nid in known:
                return (2, 0.0, nid)
            return (3, 0.0, nid)

        ordered = sorted(infos.values(), key=lambda n: rank_key(n["node_id"]))
        for n in ordered:
            rtt = reached.get(n["node_id"])
            if rtt is not None:
                n["rtt_ms"] = round(rtt * 1000)

        return {
            "updated": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
            "relay": DEFAULT_RELAY,
            "max_nodes": max_nodes,
            "nodes": ordered[:max_nodes],
        }
    finally:
        await node.close()


async def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--out", type=Path, default=DEFAULT_OUT)
    parser.add_argument(
        "--max-nodes",
        type=int,
        default=50,
        help="cap on the address list (default: 50, fastest-first)",
    )
    parser.add_argument(
        "--genesis",
        default=None,
        help="genesis node_id@relay list (default: SWARM_GENESIS env or GENESIS_DEFAULT)",
    )
    args = parser.parse_args()

    genesis = parse_genesis(args.genesis or os.environ.get("SWARM_GENESIS"))
    result = await discover(args.out, max_nodes=args.max_nodes, genesis=genesis)
    args.out.parent.mkdir(parents=True, exist_ok=True)
    tmp = args.out.with_name(args.out.name + ".tmp")
    tmp.write_text(json.dumps(result, indent=2) + "\n", encoding="utf-8")
    tmp.replace(args.out)
    print(
        f"swarm.json updated: {len(result['nodes'])} node(s) -> {args.out}",
        flush=True,
    )

    # Keep the DNS TXT anchor in sync (optional): thin/first-time joiners
    # get an independent, tiered path to the swarm when GitHub is down.
    # Configure via repo settings: EPHEMERAL_DNS_TXT (variable) + the
    # EPHEMERAL_DNS_TOKEN secret (Cloudflare API token with DNS edit).
    dns_txt = (os.environ.get("EPHEMERAL_DNS_TXT") or "").strip()
    dns_token = (os.environ.get("EPHEMERAL_DNS_TOKEN") or "").strip()
    if dns_txt and dns_token:
        anchor = pick_anchor(result, genesis)
        if anchor:
            update_dns_txt(anchor, dns_token, dns_txt)
        else:
            print("  DNS: swarm list empty — skipping TXT update", flush=True)


if __name__ == "__main__":
    asyncio.run(main())
