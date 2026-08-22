"""
Refresh ``docs/swarm.json`` with the currently-live swarm.

Thin/first-time joiners (the wasm SPA, paper-thin REST clients, other
nodes) can fetch this file instead of running a node themselves. A
scheduled GitHub Action (``.github/workflows/swarm-bootstrap.yml``) runs
this every six hours: it joins the swarm as a throwaway client, dials
whatever the previous list knew, and commits the refreshed list. The list
is self-sustaining — as long as one member is alive between refresh
cycles it regenerates from its own members, and the pinned genesis anchor
(operator config via the ``SWARM_GENESIS`` repo variable) is only
consulted when the previous list is empty (first run / ``--reset``) or
every member was unreachable.

Liveness probe: a successful dial + hello handshake proves a node speaks
the ephemeral wire protocol, but not that it is a live compute node. So
every node that answers is additionally sent a real job — a tiny Python
script that prints a fresh per-node nonce — and is only recorded as
verified when it executes the payload and echoes the nonce back.Nodes that cannot be reached keep their entry for a few runs (they may be
temporarily offline), then age out; nodes that are reachable but never
run the probe job (a bot that merely answers hello, a broken executor)
are evicted after a few failed probes. The genesis anchor is exempt from
eviction only while it is the active bootstrap source for that run (first
run / ``--reset`` / all-previous-dead fallback); otherwise it is an
ordinary member and ages out like any other node. See
``ephemeral_net.probe`` for the bookkeeping.

File shape:

    {
      "updated": "2026-08-10T12:00:00Z",
      "relay": "https://use1-1.relay.n0.iroh.link.",
      "nodes": [
        {"node_id": "…", "relay": "…", "ticket": "…", "images": ["…"],
         "probe": "ok", "probe_at": "…", "probe_detail": "…",
         "probe_fails": 0, "misses": 0},
        ...
      ],
      "bastions": [
        {"node_id": "…", "relay": "…", "ticket": "…", "images": ["…"],
         "url": "https://…",
         "probe": "ok", "probe_at": "…", "probe_detail": "…",
         "probe_ms": 123, "probe_fails": 0, "misses": 0},
        ...
      ]
    }

``node_id`` + ``relay`` are the stable, iroh-native dial target; ``ticket``
is kept as a fallback for clients that still dial by EndpointTicket. The
``probe*``/``misses`` fields are diagnostic, written by this script, and
ignored by all consumers.

Usage:
    python scripts/update_swarm_json.py [--out docs/swarm.json]
        [--no-probe] [--probe-timeout 180] [--probe-concurrency 8]
        [--reset]

``--reset`` forgets the entire previous list and regenerates a fresh
census from the genesis anchor alone (and whatever it reveals via
hello). Use it when the list has gone stale and you want a clean
regeneration instead of the incremental merge.
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
from ephemeral_net.probe import (
    DEFAULT_PROBE_TIMEOUT,
    PROBE_MAX_FAILS,
    UNREACHABLE_MAX_MISSES,
    mark_probe,
    run_probe,
    should_evict,
)
from ephemeral_net.swarm import DEFAULT_RELAY

DEFAULT_OUT = Path(__file__).resolve().parent.parent / "docs" / "swarm.json"

# The genesis anchor — the ONLY hard-coded node in the whole system, and it
# lives here in the refresh script, never in the shipped binaries. It exists
# solely to bootstrap the very first, empty list: once the list has any live
# member, it regenerates from its own members and the genesis node can go
# offline forever. Override with the SWARM_GENESIS repo variable / env var
# (comma-separated node_id@relay) or the --genesis flag.
#
# The anchor is the always-on Railway bastion (paper-light HTTP gateway). Its
# identity is pinned via the EPHEMERAL_SECRET env var on the Railway service,
# so this (node_id, relay) pair is stable across redeploys. Note the relay is
# the EU one the bastion actually advertises, not DEFAULT_RELAY.
GENESIS_DEFAULT: list[tuple[str, str]] = [
    ("ed7106bced606bede735b4c9b215052855f9747e8cb56629759ae672ce29b9c8", "https://euc1-1.relay.n0.iroh.link./"),
]

# How long each dial attempt may take (matches Node._dial_timeout).
DIAL_TIMEOUT = 20.0


def genesis_anchor_required(
    *,
    reset: bool,
    has_prev: bool,
    prev_reached: int,
) -> bool:
    """Whether the pinned genesis anchor must be consulted this run.

    The previous list is the primary census source: as long as one member
    is alive between refresh cycles the swarm regenerates from its own
    members and the genesis anchor is never contacted. The anchor is only
    dialed when the previous list is empty (first run / ``--reset``) or
    every previous-list member was unreachable.
    """
    return reset or not has_prev or prev_reached == 0


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


# DNS TXT strings are capped at 255 chars. Two compact entries fit in a
# single 255-char string (~108 chars each with the default n0 relay) — no
# multi-string splitting needed. Two is enough for first contact: dialing
# ANY live entry reveals the whole swarm via hello, and the record is
# re-ranked/re-written every 6 h. The entry-count cap is belt-and-
# suspenders on top of the char cap (a long self-hosted relay could
# otherwise push two entries past 255).
DNS_MIRROR_MAX_ENTRIES = 2
DNS_MIRROR_MAX_CHARS = 255


def build_dns_mirror(nodes: list[dict]) -> str:
    """
    Compact ``iroh1:<node_id>;<relay>`` mirror of the top ranked nodes.

    Mirrors the top of the same list ``docs/swarm.json`` carries, minus
    tickets (they're ~200+ chars each and arrive via the hello handshake
    anyway; current code dials by node id + relay). Capped to two entries
    in one 255-char TXT string.
    """
    parts: list[str] = []
    total = 0
    for n in nodes:
        node_id = n.get("node_id")
        relay = n.get("relay") or DEFAULT_RELAY
        if not node_id:
            continue
        entry = f"iroh1:{node_id};{relay}"
        sep = 1 if parts else 0
        if len(parts) >= DNS_MIRROR_MAX_ENTRIES or total + sep + len(entry) > DNS_MIRROR_MAX_CHARS:
            break
        parts.append(entry)
        total += sep + len(entry)
    return ",".join(parts)


def update_dns_txt(
    nodes: list[dict], token: str | None, hostname: str | None
) -> bool:
    """
    Mirror the live swarm list into a DNS TXT record.

    The scheduled refresh keeps a TXT record in sync with ``docs/swarm.json``
    so thin/first-time joiners have an independent, tiered path to the
    swarm when GitHub itself is unreachable (see
    ``ephemeral_net.swarm.fetch_swarm_list_dns``). Uses the Cloudflare
    API — ``EPHEMERAL_DNS_TOKEN`` (secret), ``EPHEMERAL_DNS_TXT`` (the
    record hostname), optional ``EPHEMERAL_DNS_ZONE`` (the DNS zone name;
    auto-detected by longest suffix when unset). Returns True when the
    record was written or already current.
    """
    if not token or not hostname:
        return False
    content = build_dns_mirror(nodes)
    if not content:
        print("  DNS: no nodes to mirror — skipping TXT update", flush=True)
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
        body = {
            "type": "TXT",
            "name": hostname,
            "content": content,  # single string, <= 255 chars by construction
            "ttl": 120,
        }
        if found:
            if found[0].get("content") == body["content"]:
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
        print(
            f"  DNS: TXT {hostname} -> {len(content.split(','))} node(s) mirrored",
            flush=True,
        )
        return True
    except Exception as e:
        print(f"  DNS: update failed: {e}", flush=True)
        return False


def build_status_payload(nodes: list[dict]) -> dict:
    """
    Shields.io endpoint-badge payload for the README's live-node counter.

    A README is static markdown (no JavaScript), so the "live" count is
    rendered by a shields.io badge that fetches this small file
    server-side on every view: ``img.shields.io/endpoint?url=…``. The
    count is probe-verified nodes only (``probe == "ok"``) — the nodes
    that actually executed a job this census — not merely listed entries
    (stale entries are kept for a few runs to retry).
    """
    verified = sum(1 for n in nodes if n.get("probe") == "ok")
    return {
        "schemaVersion": 1,
        "label": "live nodes",
        "message": str(verified),
        "color": "brightgreen" if verified else "red",
        "cacheSeconds": 3600,
    }


def _existing_nodes(out_path: Path) -> dict[str, dict]:
    """
    Previous list entries keyed by node id.

    Carries the staleness bookkeeping (``probe_fails`` / ``misses``)
    forward so counters survive across runs. Missing/malformed file
    returns {}.
    """
    try:
        data = json.loads(out_path.read_text(encoding="utf-8"))
    except Exception:
        return {}
    nodes: dict[str, dict] = {}
    for entry in data.get("nodes") or []:
        node_id = entry.get("node_id")
        if not node_id:
            continue
        nodes[node_id] = {
            "node_id": node_id,
            "relay": entry.get("relay"),
            "ticket": entry.get("ticket"),
            "images": entry.get("images") or [],
            "probe_fails": entry.get("probe_fails") or 0,
            "misses": entry.get("misses") or 0,
            "seen_alive": bool(entry.get("seen_alive")),
        }
    return nodes


def _existing_bastions(out_path: Path) -> dict[str, dict]:
    """
    Previous bastion entries keyed by node id.

    Carries the staleness bookkeeping (``probe_fails`` / ``misses``) and
    the advertised public URL forward so bastions age out and re-rank
    across runs. Missing/malformed file returns {}.
    """
    try:
        data = json.loads(out_path.read_text(encoding="utf-8"))
    except Exception:
        return {}
    bastions: dict[str, dict] = {}
    for entry in data.get("bastions") or []:
        node_id = entry.get("node_id")
        if not node_id or not entry.get("url"):
            continue
        bastions[node_id] = {
            "node_id": node_id,
            "relay": entry.get("relay"),
            "ticket": entry.get("ticket"),
            "images": entry.get("images") or [],
            "url": entry.get("url"),
            "probe_fails": entry.get("probe_fails") or 0,
            "misses": entry.get("misses") or 0,
            "seen_alive": bool(entry.get("seen_alive")),
        }
    return bastions


def http_health_check(url: str, timeout: float = 10.0) -> dict:
    """
    GET ``{url}/health`` and report reachability + latency.

    Returns ``{"ok", "reachable", "detail", "ms"}``. ``reachable``
    distinguishes "answered but unhealthy" (HTTP non-2xx) from a transport
    failure, so bookkeeping can record the right status.
    """
    started = time.monotonic()
    endpoint = url.rstrip("/") + "/health"
    try:
        req = urllib.request.Request(
            endpoint, headers={"Accept": "application/json"}
        )
        with urllib.request.urlopen(req, timeout=timeout) as res:
            status = getattr(res, "status", getattr(res, "code", 200))
            ms = round((time.monotonic() - started) * 1000)
            ok = 200 <= status < 300
            return {"ok": ok, "reachable": True, "detail": f"HTTP {status}", "ms": ms}
    except Exception as e:
        return {
            "ok": False,
            "reachable": False,
            "detail": f"{type(e).__name__}",
            "ms": round((time.monotonic() - started) * 1000),
        }


async def discover(
    out_path: Path,
    max_nodes: int,
    genesis: list[tuple[str, str]],
    *,
    probe: bool = True,
    probe_timeout: float = DEFAULT_PROBE_TIMEOUT,
    probe_concurrency: int = 8,
    reset: bool = False,
) -> dict:
    """
    Join the swarm, dial every candidate, probe the reachable ones, and
    return the refreshed list.

    Candidates are whatever the previous list knew, plus any peers learned
    via hello — every entry that ends up in the list is dialed this run,
    and (with ``probe``) every node that answers is sent a real job and
    must echo a fresh nonce to be listed as verified. The pinned genesis
    anchor is only consulted when the previous list is empty (first run /
    ``--reset``) or every member was unreachable. With ``reset`` the
    previous list is forgotten entirely and the run starts from the
    genesis anchor alone (a fresh census).
    """
    node = Node(relay="n0")
    await node.start()
    try:
        prev = {} if reset else _existing_nodes(out_path)
        prev_bastions = {} if reset else _existing_bastions(out_path)
        if reset:
            print(
                "reset: previous list forgotten — regenerating from the "
                "genesis anchor",
                flush=True,
            )

        # Candidate strategy — self-sustaining by design:
        #   * The previous list is the primary candidate set. As long as
        #     one member is alive between refresh cycles, the swarm
        #     regenerates from its own members and the pinned genesis
        #     anchor is never contacted.
        #   * The genesis anchor is consulted only when the previous list
        #     is empty (first run / --reset) or every member was
        #     unreachable, and while it is the active bootstrap source it
        #     is exempt from eviction for that run.
        targets: dict[str, tuple[str | None, str | None]] = {
            entry["node_id"]: (entry.get("relay"), entry.get("ticket"))
            for entry in list(prev.values()) + list(prev_bastions.values())
        }

        sem = asyncio.Semaphore(probe_concurrency)
        reached: dict[str, float] = {}  # node_id -> hello RTT (this run)
        peers: dict[str, object] = {}   # node_id -> live PeerConnection

        async def _dial_one(node_id: str, relay: str | None, ticket: str | None) -> None:
            """Dial one node (by id+relay, then ticket) and register the peer."""
            async with sem:
                try:
                    if relay:
                        peer = await asyncio.wait_for(
                            node.dial_node(node_id, relay), timeout=DIAL_TIMEOUT
                        )
                    elif ticket:
                        peer = await asyncio.wait_for(node.dial(ticket), timeout=DIAL_TIMEOUT)
                    else:
                        return
                except Exception as e:  # unreachable — the list keeps it for a few runs
                    # Show the exception type: a bare TimeoutError stringifies
                    # to "" which tells nobody anything.
                    print(
                        f"  - {node_id[:12]}... unreachable: {type(e).__name__}: {e}",
                        flush=True,
                    )
                    return
                peers[node_id] = peer
                reached[node_id] = peer.rtt if peer.rtt is not None else 0.0

        # Phase 1 — dial the previous list's members only. The list renews
        # itself as long as any one of them is alive.
        await asyncio.gather(
            *(_dial_one(nid, relay, ticket) for nid, (relay, ticket) in targets.items())
        )

        # Phase 2 — genesis fallback (first run / reset / all-prev-dead).
        if genesis_anchor_required(
            reset=reset,
            has_prev=bool(prev),
            prev_reached=sum(1 for nid in prev if nid in reached),
        ):
            seed_ids = {nid for nid, _ in genesis}
            await asyncio.gather(
                *(_dial_one(nid, relay, None) for nid, relay in genesis)
            )
            if not reset and not prev:
                print(
                    "  first run: no previous list — seeding from the genesis anchor",
                    flush=True,
                )
            elif not reset:
                print(
                    "  fallback: no previous-list node reachable — dialing the "
                    "pinned genesis anchor",
                    flush=True,
                )
            # (reset was announced above)
        else:
            seed_ids = set()
            print(
                f"  self-sustaining: {len(reached)} previous-list member(s) alive — "
                "genesis anchor not consulted",
                flush=True,
            )

        my_id = node.node_id()
        # Everything we know about: hello-learned nodes (seed + its peers)
        # ∪ previous list, deduped. Keeping un-reachable entries for a few
        # runs lets the next run retry them and keeps thin clients pointed
        # at recovering nodes — but they rank last, fill space under the
        # cap only, and age out after UNREACHABLE_MAX_MISSES.
        infos: dict[str, dict] = {}
        for info in node.table:
            if info.node_id == my_id:
                continue
            infos[info.node_id] = {
                "node_id": info.node_id,
                "relay": info.relay,
                "ticket": info.ticket,
                "images": sorted(info.images or []),
                "url": info.url,
            }
        for entry in prev.values():
            nid = entry["node_id"]
            if nid in infos:
                # Live hello data wins; carry the staleness bookkeeping over.
                for key in ("probe_fails", "misses", "seen_alive"):
                    if entry.get(key):
                        infos[nid][key] = entry[key]
            else:
                infos[nid] = dict(entry)
        for entry in prev_bastions.values():
            nid = entry["node_id"]
            if nid in infos:
                # Live hello data wins for identity; carry staleness over.
                for key in ("probe_fails", "misses", "seen_alive"):
                    if entry.get(key):
                        infos[nid][key] = entry[key]
            else:
                infos[nid] = dict(entry)

        # Dial hello-learned nodes too (they were never targets of the
        # previous list), so every entry in the list gets a live dial —
        # and, with probe enabled, a live job — this run.
        await asyncio.gather(
            *(
                _dial_one(nid, entry.get("relay"), entry.get("ticket"))
                for nid, entry in infos.items()
                if nid not in targets and (entry.get("relay") or entry.get("ticket"))
            )
        )

        now_iso = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())

        # Partition into compute nodes and bastions (nodes advertising a
        # public HTTP URL). Bastions are verified by an HTTP health check
        # instead of a compute-job probe, and are published separately so
        # paper-light clients can discover them without iroh.
        node_infos: dict[str, dict] = {}
        bastion_infos: dict[str, dict] = {}
        for nid, entry in infos.items():
            if entry.get("url"):
                bastion_infos[nid] = entry
            else:
                node_infos[nid] = entry

        async def _probe_one(node_id: str, entry: dict) -> None:
            """Submit a real job to one reachable node and record the verdict."""
            result = await run_probe(
                lambda request: node.submit_job(peers[node_id], request),
                node_id,
                timeout=probe_timeout,
            )
            entry["probe"] = "ok" if result["ok"] else "failed"
            entry["probe_at"] = now_iso
            entry["probe_detail"] = result["detail"]
            entry["probe_ms"] = result["ms"]
            # mark_probe returns a NEW entry carrying the counters — write it
            # back into node_infos, or the bookkeeping is silently lost and
            # stale nodes are never evicted.
            node_infos[node_id] = mark_probe(
                entry,
                prev.get(node_id),
                status="ok" if result["ok"] else "failed",
            )
            tag = "probe ok" if result["ok"] else "probe FAILED"
            print(f"  {tag:14} {node_id[:12]}... {result['detail']} ({result['ms']} ms)", flush=True)

        if probe:
            await asyncio.gather(
                *(
                    _probe_one(nid, entry)
                    for nid, entry in node_infos.items()
                    if nid in peers
                )
            )

        # Nodes we could not dial: record the miss (and that we tried).
        for node_id, entry in node_infos.items():
            if node_id in peers:
                continue
            if not (entry.get("relay") or entry.get("ticket")):
                entry["probe"] = "skipped"  # nothing to dial — leave untouched
                continue
            entry = node_infos[node_id] = mark_probe(
                entry, prev.get(node_id), status="unreachable"
            )
            entry["probe"] = "unreachable"
            entry["probe_at"] = now_iso
            print(f"  unreachable    {node_id[:12]}...", flush=True)

        # Evict entries that are no longer live: reachable nodes that
        # never run the probe job (PROBE_MAX_FAILS), and silent nodes
        # (UNREACHABLE_MAX_MISSES ≈ 36 h offline). The genesis anchor is
        # exempt only while it is the active bootstrap source this run
        # (first run / reset / all-prev-dead fallback); otherwise it is an
        # ordinary member and ages out like any other node.
        kept: list[dict] = []
        for node_id, entry in node_infos.items():
            if should_evict(entry, seed_ids=seed_ids):
                if (entry.get("probe_fails") or 0) >= PROBE_MAX_FAILS:
                    reason = f"{entry.get('probe_fails')} failed probes"
                else:
                    reason = f"{entry.get('misses')} unreachable runs"
                print(f"  evicting       {node_id[:12]}... ({reason})", flush=True)
                continue
            kept.append(entry)

        # Rank: genesis anchor first, then verified nodes (fastest probe /
        # hello RTT first), then reachable-but-unverified, then hello-
        # learned peers, then stale entries — capped so the address list
        # stays small and fresh.
        known = set(node.table.known_peer_ids())

        def rank_key(nid: str) -> tuple[int, float, str]:
            if nid in seed_ids:
                return (0, 0.0, nid)
            status = node_infos[nid].get("probe")
            rtt = reached.get(nid) if reached.get(nid) is not None else float("inf")
            if status == "ok":
                return (1, rtt, nid)
            if status == "failed":
                return (2, rtt, nid)
            if nid in reached:
                return (2, rtt, nid)  # dialed this run; no probe ran
            if status == "unreachable" or nid in known:
                return (3, 0.0, nid)
            return (4, 0.0, nid)

        ordered = sorted(kept, key=lambda n: rank_key(n["node_id"]))
        for n in ordered:
            rtt = reached.get(n["node_id"])
            if rtt is not None:
                n["rtt_ms"] = round(rtt * 1000)

        # --- Bastions: HTTP health check instead of a compute-job probe ---
        async def _health_one(node_id: str, entry: dict) -> None:
            url = entry.get("url")
            result = await asyncio.to_thread(http_health_check, url)
            status = (
                "ok"
                if result["ok"]
                else ("failed" if result["reachable"] else "unreachable")
            )
            entry["probe"] = status
            entry["probe_at"] = now_iso
            entry["probe_detail"] = result["detail"]
            entry["probe_ms"] = result["ms"]
            bastion_infos[node_id] = mark_probe(
                entry, prev_bastions.get(node_id), status=status
            )
            tag = "bastion ok" if result["ok"] else "bastion DOWN"
            print(
                f"  {tag:14} {url} {result['detail']} ({result['ms']} ms)",
                flush=True,
            )

        if probe:
            await asyncio.gather(
                *(
                    _health_one(nid, entry)
                    for nid, entry in bastion_infos.items()
                )
            )
        else:
            for nid, entry in bastion_infos.items():
                entry = bastion_infos[nid] = mark_probe(
                    entry, prev_bastions.get(nid), status="reached"
                )
                entry["probe"] = "skipped"
                entry["probe_at"] = now_iso

        kept_bastions: list[dict] = []
        for node_id, entry in bastion_infos.items():
            if should_evict(entry):
                if (entry.get("probe_fails") or 0) >= PROBE_MAX_FAILS:
                    reason = f"{entry.get('probe_fails')} failed health checks"
                else:
                    reason = f"{entry.get('misses')} unreachable runs"
                print(f"  evicting       {entry.get('url')} ({reason})", flush=True)
                continue
            kept_bastions.append(entry)

        def bastion_rank_key(nid: str) -> tuple[int, float, str]:
            status = bastion_infos[nid].get("probe")
            ms = bastion_infos[nid].get("probe_ms")
            if ms is None:
                ms = reached.get(nid, 0.0) * 1000
            if status == "ok":
                return (0, ms, nid)
            if status == "failed":
                return (1, ms, nid)
            return (2, 0.0, nid)

        ordered_bastions = sorted(
            kept_bastions, key=lambda b: bastion_rank_key(b["node_id"])
        )

        if probe:
            ok_n = sum(1 for n in node_infos.values() if n.get("probe") == "ok")
            fail_n = sum(1 for n in node_infos.values() if n.get("probe") == "failed")
            unreach_n = sum(1 for n in node_infos.values() if n.get("probe") == "unreachable")
            bastion_ok = sum(1 for b in bastion_infos.values() if b.get("probe") == "ok")
            print(
                f"probe: {ok_n} verified alive, {fail_n} reachable but not running "
                f"jobs, {unreach_n} unreachable; bastions: {bastion_ok} healthy",
                flush=True,
            )
            if len(reached) and ok_n == 0 and not bastion_infos:
                print(
                    "WARNING: every reachable node failed the probe — the swarm may "
                    "be down, or the probe payload itself is broken",
                    flush=True,
                )
        else:
            print(
                f"dial: {len(reached)} reached, {len(node_infos) - len(reached)} unreachable",
                flush=True,
            )

        return {
            "updated": now_iso,
            "relay": DEFAULT_RELAY,
            "max_nodes": max_nodes,
            "nodes": ordered[:max_nodes],
            "bastions": ordered_bastions[:max_nodes],
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
    parser.add_argument(
        "--no-probe",
        action="store_true",
        help="dial candidates but skip the real-job liveness probe (diagnostic runs)",
    )
    parser.add_argument(
        "--probe-timeout",
        type=float,
        default=DEFAULT_PROBE_TIMEOUT,
        help=f"per-node probe job timeout in seconds (default: {DEFAULT_PROBE_TIMEOUT})",
    )
    parser.add_argument(
        "--probe-concurrency",
        type=int,
        default=8,
        help="max simultaneous dials/probes (default: 8)",
    )
    parser.add_argument(
        "--reset",
        action="store_true",
        help="forget the previous list and regenerate fresh from the genesis anchor",
    )
    args = parser.parse_args()

    genesis = parse_genesis(args.genesis or os.environ.get("SWARM_GENESIS"))
    result = await discover(
        args.out,
        max_nodes=args.max_nodes,
        genesis=genesis,
        probe=not args.no_probe,
        probe_timeout=args.probe_timeout,
        probe_concurrency=max(1, args.probe_concurrency),
        reset=args.reset,
    )
    args.out.parent.mkdir(parents=True, exist_ok=True)
    tmp = args.out.with_name(args.out.name + ".tmp")
    tmp.write_text(json.dumps(result, indent=2) + "\n", encoding="utf-8")
    tmp.replace(args.out)
    print(
        f"swarm.json updated: {len(result['nodes'])} node(s), "
        f"{len(result.get('bastions') or [])} bastion(s) -> {args.out}",
        flush=True,
    )

    # Shields.io endpoint badge for the README's live-node counter: the
    # README is static markdown (no JS), so the badge URL points at this
    # small status file, which shields fetches server-side on every view.
    # The bot commits it together with swarm.json each refresh.
    status_path = args.out.parent / "swarm-status.json"
    payload = build_status_payload(result.get("nodes") or [])
    tmp2 = status_path.with_name(status_path.name + ".tmp")
    tmp2.write_text(json.dumps(payload) + "\n", encoding="utf-8")
    tmp2.replace(status_path)
    print(
        f"swarm-status.json: {payload['message']} verified node(s) -> {status_path}",
        flush=True,
    )

    # Keep the DNS TXT mirror in sync (optional): thin/first-time joiners
    # get an independent, tiered path to the swarm when GitHub is down.
    # Configure via repo settings: EPHEMERAL_DNS_TXT (variable) + the
    # EPHEMERAL_DNS_TOKEN secret (Cloudflare API token with DNS edit).
    dns_txt = (os.environ.get("EPHEMERAL_DNS_TXT") or "").strip()
    dns_token = (os.environ.get("EPHEMERAL_DNS_TOKEN") or "").strip()
    if dns_txt and dns_token:
        update_dns_txt(result.get("nodes") or [], dns_token, dns_txt)


if __name__ == "__main__":
    asyncio.run(main())
