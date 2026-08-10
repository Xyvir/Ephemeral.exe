"""
Swarm — shared bootstrap configuration for the default ephemeral network.

One big implicit swarm: every distributed binary — desktop client
(``main_distributed_client.py``), self-host gateway (``main_distributed.py``),
and the wasm thin client (``ephemeral-wasm-library/web/``) — joins the same
public iroh network by default and discovers the rest of it through the
**live bootstrap list** (``docs/swarm.json``, served by GitHub Pages / raw
GitHub). No configuration and no compiled-in seeds required: run a binary
and you're part of the swarm.

Why no compiled seeds? A seed compiled into every binary is a single point
of failure the operator has to edit code to change. Instead, the always-on
anchor is the *list*, not a box: ``scripts/update_swarm_json.py`` (a
scheduled GitHub Action) joins the swarm, dials the previous list's members
plus a single genesis anchor, and commits the live node list. New nodes are
picked up automatically — stand up any distributed flavor on an always-on
box and the next refresh lists it. The one genesis anchor lives in the
refresh *script* (overridable via ``SWARM_GENESIS``), never in the binaries,
and is only needed to bootstrap the very first, empty list — afterwards the
list regenerates from its own members.

Private/offline networks opt out of the public list entirely: set
``EPHEMERAL_SEED_NODES`` (``node_id@relay``) or ``EPHEMERAL_SEEDS``
(EndpointTickets) explicitly.
"""
from __future__ import annotations

import json
import os
import secrets
import urllib.parse
import urllib.request
from pathlib import Path
from typing import Sequence

# The relay every swarm node uses (n0's default). Node ids are stable
# (persisted secrets), so a (node_id, relay) pair never goes stale — the
# relay routes by node id across restarts.
DEFAULT_RELAY = "https://use1-1.relay.n0.iroh.link."

# The always-on bootstrap list: URLs where the live node list
# (docs/swarm.json, refreshed every 6 h by .github/workflows/
# swarm-bootstrap.yml) can be fetched. First reachable URL wins. The wasm
# SPA uses its own relative path first (same origin on GitHub Pages) plus
# these as fallbacks — see ephemeral-wasm-library/web/config.js.
SWARM_LIST_URLS: list[str] = [
    "https://raw.githubusercontent.com/Xyvir/Ephemeral.exe/main/docs/swarm.json",
    "https://xyvir.github.io/Ephemeral.exe/docs/swarm.json",
]

# DNS TXT fallback for first contact. When the live list (GitHub) is
# unreachable, a node can still find the swarm through a DNS TXT record
# that mirrors the list (the scheduled refresh Action keeps it in sync):
# DNS is tiered, cached infrastructure, so it is an independent path to
# the swarm. Empty (default) disables the fallback; set the
# EPHEMERAL_DNS_TXT environment variable (or edit this constant) to the
# hostname owning a TXT record whose content is the mirror format
# documented on :func:`parse_swarm_list_dns`.
SWARM_DNS_TXT: str = ""

# DNS-over-HTTPS (RFC 8484 JSON) endpoints used for the TXT lookup — no
# system-resolver parsing needed (works on Windows and in the browser),
# and the anycast providers are themselves tiered DNS infrastructure.
# Two independent providers; first reachable one wins.
DOH_ENDPOINTS: list[str] = [
    "https://cloudflare-dns.com/dns-query?name={name}&type=TXT",
    "https://dns.google/resolve?name={name}&type=TXT",
]


def fetch_swarm_list(urls: Sequence[str] | None = None) -> list[dict]:
    """
    Fetch the live swarm node list from the first reachable URL.

    Returns a list of ``{"node_id", "relay", "ticket", "images"}``-shaped
    dicts (entries missing both ``node_id`` and ``ticket`` are dropped).
    ``[]`` when no URL is reachable — callers keep whatever they already
    know and retry on the next maintenance cycle.
    """
    if urls is None:
        urls = SWARM_LIST_URLS
    for url in urls:
        try:
            with urllib.request.urlopen(url, timeout=10) as res:
                data = json.loads(res.read().decode("utf-8"))
            nodes = data.get("nodes") if isinstance(data, dict) else None
            if not isinstance(nodes, list):
                continue
            cleaned = [
                n
                for n in nodes
                if isinstance(n, dict) and (n.get("node_id") or n.get("ticket"))
            ]
            if cleaned:
                return cleaned
        except Exception:
            continue
    return []


def default_state_dir() -> Path:
    """Where nodes persist identity (and future state)."""
    env = os.getenv("EPHEMERAL_STATE_DIR")
    return Path(env).expanduser() if env else Path.home() / ".ephemeral"


def load_or_create_secret(path: Path | None = None) -> bytes:
    """
    A stable 32-byte node identity, created once and reused forever.

    A node's EndpointTicket is derived from its secret key, so persisting
    the key is what makes a node's id permanent across restarts — and why
    the list's ``node_id`` + ``relay`` entries never go stale. The key file
    is created with 0600 permissions.
    """
    p = path or (default_state_dir() / "secret_key.bin")
    if p.exists():
        data = p.read_bytes()
        if len(data) == 32:
            return data
    data = secrets.token_bytes(32)
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_bytes(data)
    try:
        os.chmod(p, 0o600)
    except OSError:  # pragma: no cover - Windows may not honor chmod
        pass
    return data


def parse_seeds(env_value: str | None) -> list[str]:
    """
    Parse the ``EPHEMERAL_SEEDS`` environment variable (EndpointTickets).

    Unset means *no* ticket bootstrap (the live-list / node-id bootstrap
    is the default, see :func:`parse_seed_nodes`); an explicit value
    (including ``\"\"``) opts into a private ticket-based network entirely.
    """
    if env_value is None:
        return []
    return [s.strip() for s in env_value.split(",") if s.strip()]


def parse_swarm_list_dns(content: str) -> list[tuple[str, str]]:
    """
    Parse the swarm-list mirror out of a DNS TXT record value.

    The record mirrors the live swarm list (``docs/swarm.json``) in
    compact form — one ``iroh1:<node_id>;<relay_url>`` entry per node,
    comma-separated, split across the record's 255-char strings as
    needed. Tickets are intentionally omitted (they're ~200+ chars and
    arrive via the hello handshake anyway; current code dials by node id
    + relay). Quotes (how some resolvers render TXT) are stripped; a
    relay may be omitted (``iroh1:<node_id>``) and :data:`DEFAULT_RELAY`
    is used; non-``iroh1`` values and malformed entries are skipped.
    """
    anchors: list[tuple[str, str]] = []
    for part in content.replace('"', "").split(","):
        part = part.strip()
        if not part.startswith("iroh1:"):
            continue
        body = part[len("iroh1:") :].strip()
        node_id, sep, relay = body.partition(";")
        node_id = node_id.strip()
        if len(node_id) != 64 or any(c not in "0123456789abcdef" for c in node_id):
            continue
        anchors.append((node_id, relay.strip() or DEFAULT_RELAY))
    return anchors


def fetch_swarm_list_dns(
    hostname: str, urls: Sequence[str] | None = None
) -> list[tuple[str, str]]:
    """
    Resolve the swarm-list mirror TXT record for ``hostname`` via DNS-over-HTTPS.

    Returns every ``(node_id, relay_url)`` entry mirrored in the record
    (deduped by node id) — the same ranked list the JSON file carries,
    minus tickets. ``[]`` when the record is missing, malformed, or no
    resolver is reachable (callers keep retrying on the next maintenance
    cycle).
    """
    if not hostname:
        return []
    if urls is None:
        quoted = urllib.parse.quote(hostname, safe=".-_")
        urls = [u.format(name=quoted) for u in DOH_ENDPOINTS]
    for url in urls:
        try:
            req = urllib.request.Request(
                url, headers={"Accept": "application/dns-json"}
            )
            with urllib.request.urlopen(req, timeout=8) as res:
                data = json.loads(res.read().decode("utf-8"))
            entries: list[tuple[str, str]] = []
            for answer in data.get("Answer") or []:
                if answer.get("type") == 16:  # TXT
                    entries.extend(parse_swarm_list_dns(answer.get("data") or ""))
            if entries:
                # Dedupe by node id across strings/answers.
                seen: set[str] = set()
                unique: list[tuple[str, str]] = []
                for node_id, relay in entries:
                    if node_id in seen:
                        continue
                    seen.add(node_id)
                    unique.append((node_id, relay))
                return unique
        except Exception:
            continue
    return []


def parse_seed_nodes(env_value: str | None) -> list[tuple[str, str]]:
    """
    Parse ``EPHEMERAL_SEED_NODES`` — comma-separated ``node_id@relay``
    pairs (``node_id`` alone uses :data:`DEFAULT_RELAY`).

    ``None`` (unset) returns ``[]`` — there are **no compiled-in seeds**;
    the caller bootstraps from the live swarm list
    (:func:`fetch_swarm_list` / ``Node.bootstrap_from_list``). Any
    explicit value (including ``\"\"``) replaces the list bootstrap
    entirely with a private node-id network.
    """
    if env_value is None:
        return []
    nodes: list[tuple[str, str]] = []
    for raw in env_value.split(","):
        raw = raw.strip()
        if not raw:
            continue
        if "@" in raw:
            node_id, relay = raw.split("@", 1)
            nodes.append((node_id.strip(), relay.strip() or DEFAULT_RELAY))
        else:
            nodes.append((raw, DEFAULT_RELAY))
    return nodes


__all__ = [
    "DEFAULT_RELAY",
    "DOH_ENDPOINTS",
    "SWARM_DNS_TXT",
    "SWARM_LIST_URLS",
    "default_state_dir",
    "fetch_swarm_list",
    "fetch_swarm_list_dns",
    "load_or_create_secret",
    "parse_seed_nodes",
    "parse_seeds",
    "parse_swarm_list_dns",
]
