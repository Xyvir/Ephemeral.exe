"""
Swarm — shared bootstrap configuration for the default ephemeral network.

One big implicit swarm: every distributed binary — desktop client
(``main_distributed_client.py``), self-host gateway (``main_distributed.py``),
and the wasm thin client (``ephemeral-wasm-library/web/``) — joins the same
public iroh network by default and discovers the rest of it through the
compiled-in seed ticket(s) below. No configuration required: run a binary and
you're part of the swarm.

A seed is an always-on compute node. Its EndpointTicket is stable only because
the node persists its secret key (see :func:`load_or_create_secret`) — a
fresh random key every run would mint a new ticket each restart and break
everyone who compiled the old one in. Stand one up (the self-host distributed
gateway prints ``SWARM SEED TICKET ...`` at startup), then swap the ticket
below into ``DEFAULT_SWARM_SEEDS`` *and* ``ephemeral-wasm-library/web/config.js``.
"""
from __future__ import annotations

import os
import secrets
from pathlib import Path

# The compiled-in swarm seed: an always-on node on the public n0 relays.
#
# Keep in sync with `ephemeral-wasm-library/web/config.js` (BOOTSTRAP.seeds).
# This is currently the original demo node's ticket — a placeholder. Replace
# it with your own always-on node's ticket (printed at gateway startup as
# ``SWARM SEED TICKET ...``) so the public Pages demo always has a live seed.
DEFAULT_SWARM_SEEDS: list[str] = [
    "endpointaaku44yiw2xtcdpvoxd4sc6i72dbi3h66a3kycmgmj3iutz4iepmkbaaenuhi5dqom5c6l3vonstcljrfzzgk3dbpexg4mbonfzg62bonruw42zof4aqasvhfs7zd3qdaeakyhcaagi64aybadakqaaushxag",
]


def default_state_dir() -> Path:
    """Where nodes persist identity (and future state)."""
    env = os.getenv("EPHEMERAL_STATE_DIR")
    return Path(env).expanduser() if env else Path.home() / ".ephemeral"


def load_or_create_secret(path: Path | None = None) -> bytes:
    """
    A stable 32-byte node identity, created once and reused forever.

    A node's EndpointTicket is derived from its secret key, so persisting
    the key is what makes a compiled-in seed ticket keep working across
    restarts. The key file is created with 0600 permissions.
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
    Parse the ``EPHEMERAL_SEEDS`` environment variable.

    ``None`` (unset) falls back to the compiled-in swarm seeds — the
    implicit-join behavior. Any explicit value (including ``""``) replaces
    them entirely.
    """
    if env_value is None:
        return list(DEFAULT_SWARM_SEEDS)
    return [s.strip() for s in env_value.split(",") if s.strip()]


__all__ = [
    "DEFAULT_SWARM_SEEDS",
    "default_state_dir",
    "load_or_create_secret",
    "parse_seeds",
]
