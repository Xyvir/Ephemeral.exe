"""
Liveness probing for the swarm bootstrap list.

``scripts/update_swarm_json.py`` (the scheduled refresh Action) dials
every listed node each run. A successful dial + hello handshake proves a
node speaks the ephemeral wire protocol, but not that it is a *live
compute node*: a dead-but-registered peer, a foreign iroh endpoint that
somehow completed the handshake, or a machine whose executor stack is
broken would all pass.

So the refresh actually runs a real job — a tiny bash ``echo`` of a
fresh, unpredictable per-node nonce. A node is only recorded as
verified when it executed the payload and echoed the nonce back, which a
bot that merely answers hello (or a stale entry) cannot fake.

The census keeps **only live members**: an entry that cannot be dialed,
or that is dialed but fails its probe, is dropped from the list
immediately. There are no failure counters and no tombstone/blacklist —
a node that comes back online is simply re-discovered through its live
peers on the next refresh, with no recovery delay.

The refresh is tiered: previous list first, then the genesis anchor,
and if *nothing* at all is reachable the previous list is written back
verbatim as last good state (held until a later check succeeds) so a
temporary outage never wipes the census to empty.
"""
from __future__ import annotations

import asyncio
import base64
import secrets
import time
from typing import AsyncIterator, Callable

from .jobs import JobDoneEvent, JobErrorEvent, JobEvent, JobRequest

#: Default per-node probe job timeout (seconds). Covers a first-run image
#: pull; already-warm nodes answer in well under a second.
DEFAULT_PROBE_TIMEOUT = 180


def probe_nonce(node_id: str) -> str:
    """
    A fresh, unpredictable token a node must echo to prove it ran our code.

    Derived from the node id (so a log line is attributable) plus a random
    component (so a bot that echoes a captured or canned answer cannot
    pass). ``echo`` output containing this exact string is the proof of
    life.
    """
    return f"ephemeral-alive-{node_id[:10]}-{secrets.token_hex(5)}"


def build_probe_document(nonce: str) -> str:
    """A tiny Markdown document whose only run echoes ``nonce``.

    Bash is the canary because it is as spoof-proof as python (the nonce
    must round-trip through a real sandboxed container) yet needs only the
    tiny ``alpine`` image (~7 MB) that every node pre-hydrates — so a
    fresh node verifies from second zero, with no first-run pull.
    """
    return f"```bash\necho {nonce}\n```\n"


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
    peer, request)``.    Builds the probe payload (a bash ``echo`` of a
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
        # Include the exception type and fall back to repr() for errors
        # whose str() is empty (e.g. a bare TimeoutError), so the swarm
        # list never records an uninformative "probe failed: " with no cause.
        detail = f"probe failed ({type(e).__name__}): {str(e).strip() or repr(e)}"
    return {"ok": ok, "detail": detail, "ms": round((time.monotonic() - started) * 1000)}


__all__ = [
    "DEFAULT_PROBE_TIMEOUT",
    "build_probe_document",
    "probe_nonce",
    "probe_verdict",
    "run_probe",
]
