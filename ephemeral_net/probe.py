"""
Liveness probing for the swarm bootstrap list.

``scripts/update_swarm_json.py`` (the scheduled refresh Action) dials
every listed node each run. A successful dial + hello handshake proves a
node speaks the ephemeral wire protocol, but not that it is a *live
compute node*: a dead-but-registered peer, a foreign iroh endpoint that
somehow completed the handshake, or a machine whose executor stack is
broken would all pass.

So the refresh actually runs a real job — a tiny Python script that
prints a fresh, unpredictable per-node nonce. A node is only recorded as
verified when it executed the payload and echoed the nonce back, which a
bot that merely answers hello (or a stale entry) cannot fake.

Staleness bookkeeping is persisted in ``docs/swarm.json`` per entry:

* ``probe_fails`` — consecutive runs where the node was reachable but
  its probe job failed; the entry is evicted after :data:`PROBE_MAX_FAILS`.
* ``misses`` — consecutive runs where the node could not be dialed at
  all; the entry is evicted after :data:`UNREACHABLE_MAX_MISSES`.
* ``seen_alive`` — set the first time the node ever answered a dial
  (and ran the probe, or was verified): real nodes get the full
  recovery grace when they later go dark, while entries that have
  never once proven alive are dropped after
  :data:`UNREACHABLE_MAX_MISSES_NEVER_VERIFIED` runs instead of
  lingering for days.

The genesis anchor is exempt from eviction: it bootstraps the very
first, empty list and is operator-configured, not discovered.
"""
from __future__ import annotations

import asyncio
import base64
import secrets
import time
from typing import AsyncIterator, Callable

from .jobs import JobDoneEvent, JobErrorEvent, JobEvent, JobRequest

#: Consecutive reachable-but-failed probe runs before an entry is evicted.
PROBE_MAX_FAILS = 3

#: Consecutive undialable runs (~36 h at the 6-hourly schedule) before an
#: entry is evicted. Kept generous: a node that is merely offline retries,
#: while a node that is genuinely gone ages out instead of living forever.
UNREACHABLE_MAX_MISSES = 6

#: Consecutive undialable runs before a NEVER-verified entry is evicted
#: (~12 h at the 6-hourly schedule). Entries that have never once answered
#: a dial have no recovery to wait for — a stale address is just dead
#: weight in the list, so it gets a short leash instead of the full grace.
UNREACHABLE_MAX_MISSES_NEVER_VERIFIED = 2

#: Default per-node probe job timeout (seconds). Covers a first-run image
#: pull; already-warm nodes answer in well under a second.
DEFAULT_PROBE_TIMEOUT = 180


def probe_nonce(node_id: str) -> str:
    """
    A fresh, unpredictable token a node must print to prove it ran our code.

    Derived from the node id (so a log line is attributable) plus a random
    component (so a bot that echoes a captured or canned answer cannot
    pass). ``print()`` output containing this exact string is the proof of
    life.
    """
    return f"ephemeral-alive-{node_id[:10]}-{secrets.token_hex(5)}"


def build_probe_document(nonce: str) -> str:
    """A tiny Markdown document whose only run prints ``nonce``."""
    return f"```python\nprint({nonce!r})\n```\n"


def probe_verdict(exit_code: int, stdout: str, nonce: str) -> tuple[bool, str]:
    """
    Whether a probe job proves the node ran our code.

    True only when the job exited 0 and ``nonce`` appears in stdout —
    the remote actually executed the payload rather than replying with
    something canned. Returns ``(ok, detail)``.
    """
    if exit_code != 0:
        return False, f"exit code {exit_code}"
    if nonce not in (stdout or ""):
        return False, "output did not contain the probe nonce"
    return True, "ok"


def mark_probe(entry: dict, prev: dict | None, *, status: str) -> dict:
    """
    Apply this run's probe bookkeeping to a copy of ``entry``.

    ``status`` is one of:

    * ``"ok"`` — dialed and the job probe verified it: counters reset,
      ``seen_alive`` set.
    * ``"failed"`` — dialed, but the probe job failed: ``probe_fails``
      increments (evicted after :data:`PROBE_MAX_FAILS`), ``misses``
      resets, ``seen_alive`` set (it IS a real machine).
    * ``"reached"`` — dialed, no job probe ran (``--no-probe``):
      ``misses`` resets, ``probe_fails`` is left untouched, ``seen_alive``
      set.
    * ``"unreachable"`` — could not be dialed: ``misses`` increments
      (evicted after :data:`UNREACHABLE_MAX_MISSES`).

    ``prev`` is the previous list's entry (or None) so counters survive
    across runs.
    """
    entry = dict(entry)
    prev_fails = (prev or {}).get("probe_fails") or 0
    prev_misses = (prev or {}).get("misses") or 0
    if status == "ok":
        entry["probe_fails"] = 0
        entry["misses"] = 0
        entry["seen_alive"] = True
    elif status == "failed":
        entry["probe_fails"] = prev_fails + 1
        entry["misses"] = 0
        entry["seen_alive"] = True
    elif status == "reached":
        entry["probe_fails"] = prev_fails  # untouched — no job probe ran
        entry["misses"] = 0
        entry["seen_alive"] = True
    elif status == "unreachable":
        entry["probe_fails"] = prev_fails  # untouched — not attributable
        entry["misses"] = prev_misses + 1
        entry["seen_alive"] = bool((prev or {}).get("seen_alive"))  # carried
    else:  # pragma: no cover - programmer error
        raise ValueError(f"unknown probe status: {status!r}")
    return entry


async def run_probe(
    submit: Callable[[JobRequest], AsyncIterator[JobEvent]],
    node_id: str,
    *,
    timeout: float = DEFAULT_PROBE_TIMEOUT,
) -> dict:
    """
    Run one liveness probe over a live peer connection.

    ``submit`` is a callable that takes a :class:`JobRequest` and returns
    an async iterator of :class:`JobEvent` — in practice ``node.submit_job(
    peer, request)``. Builds the probe payload (a python ``print`` of a
    fresh nonce), submits it, and checks the verdict.

    Returns ``{"ok": bool, "detail": str, "ms": int}`` where ``ok`` is
    True only when the peer actually executed the payload and echoed the
    nonce back.
    """
    nonce = probe_nonce(node_id)
    request = JobRequest(
        job_id=f"probe-{node_id[:10]}-{int(time.monotonic() * 1000)}",
        document_blob=base64.b64encode(
            build_probe_document(nonce).encode("utf-8")
        ).decode("ascii"),
        timeout=int(timeout),
    )
    started = time.monotonic()
    ok = False
    detail = "no job_done frame"
    try:

        async def _consume() -> list[JobEvent]:
            events: list[JobEvent] = []
            async for event in submit(request):
                events.append(event)
            return events

        events = await asyncio.wait_for(_consume(), timeout=timeout + 30)
        done = next(
            (e for e in reversed(events) if isinstance(e, JobDoneEvent)), None
        )
        if done is not None:
            ok, detail = probe_verdict(done.exit_code, done.stdout, nonce)
        else:
            err = next((e for e in events if isinstance(e, JobErrorEvent)), None)
            if err is not None:
                detail = f"rejected: {err.message}"
    except asyncio.TimeoutError:
        detail = f"probe timed out after {int(timeout)}s"
    except Exception as e:
        detail = f"probe failed: {e}"
    return {"ok": ok, "detail": detail, "ms": round((time.monotonic() - started) * 1000)}


def should_evict(
    entry: dict,
    *,
    seed_ids: set[str] | None = None,
    max_fails: int = PROBE_MAX_FAILS,
    max_misses: int = UNREACHABLE_MAX_MISSES,
    max_misses_never_verified: int = UNREACHABLE_MAX_MISSES_NEVER_VERIFIED,
) -> bool:
    """
    Whether ``entry`` should be dropped from the list this run.

    The genesis anchor (``seed_ids``) is exempt — it is operator
    configuration, not a discovered member. Otherwise an entry is
    evicted once its counters pass the thresholds: reachable-but-job-
    failing entries go after ``max_fails`` runs, silent entries after
    ``max_misses`` runs (~36 h at the 6-hourly schedule) — but entries
    that have never once answered a dial (``seen_alive`` unset) are only
    given ``max_misses_never_verified`` runs (~12 h), since there is no
    recovery to wait for.
    """
    if seed_ids and entry.get("node_id") in seed_ids:
        return False
    if (entry.get("probe_fails") or 0) >= max_fails:
        return True
    misses = entry.get("misses") or 0
    if entry.get("seen_alive"):
        return misses >= max_misses
    return misses >= max_misses_never_verified


__all__ = [
    "DEFAULT_PROBE_TIMEOUT",
    "PROBE_MAX_FAILS",
    "UNREACHABLE_MAX_MISSES",
    "UNREACHABLE_MAX_MISSES_NEVER_VERIFIED",
    "build_probe_document",
    "mark_probe",
    "probe_nonce",
    "probe_verdict",
    "run_probe",
    "should_evict",
]
