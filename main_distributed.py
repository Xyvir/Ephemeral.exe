"""
Ephemeral Distributed API Server — ``ephemeral-self-host-distributed``.

A headless backend that keeps the exact REST contract of ``main_api.py``
while joining the iroh cluster as a compute node. Requests submitted to
the REST endpoint are translated into network job payloads and executed
by the node's sandboxed executor — locally when the required images are
warm, or offloaded to the nearest neighbor that has them (with a
background pull) otherwise.

Configuration (environment variables):

    EPHEMERAL_RELAY          "n0" (default) | "minimal" | "disabled" |
                             comma-separated custom relay URLs (e.g.
                             "https://relay.myorg.com")
    EPHEMERAL_RELAY_FALLBACK "1" to ALSO use the public n0 relays when a
                             custom EPHEMERAL_RELAY is set (default "0")
    EPHEMERAL_SEED_NODES     comma-separated node_id[@relay] for a PRIVATE
                             node-id network; unset joins the public swarm via
                             the live bootstrap list (docs/swarm.json) — no
                             compiled-in seeds, fully automatic
    EPHEMERAL_SEEDS          comma-separated EndpointTickets (private networks /
                             backward compat; overrides SEED_NODES when set)
    EPHEMERAL_SECRET         hex-encoded 32-byte secret for a persistent node id;
                             unset, a stable identity is auto-persisted to disk
    EPHEMERAL_PORT           HTTP port (default 8787 — the Lithic-UK sidecar slot)
    EPHEMERAL_ALLOW_NETWORK  "1" to let remote jobs use network access (default "0")
    EPHEMERAL_PRIVATE        "1" (or ``--private``) — skip the public swarm list;
                             this node becomes its own seed for a private cluster
                             and prints a student-ready ``#seed=`` URL

Usage:
    uvicorn main_distributed:app --host 0.0.0.0 --port 8787
    python main_distributed.py --private          # direct run, private mode

Port 8787 matches the local API server and the Lithic-UK sidecar slot.

On startup the node prints ``SWARM NODE_ID`` / ``SWARM RELAY`` — this is
its stable identity for the swarm list. There is nothing to compile in:
the node bootstraps from the live ``docs/swarm.json`` list and is picked
up by the next scheduled refresh automatically.
"""
from __future__ import annotations

import os
import sys
from contextlib import asynccontextmanager

from fastapi import FastAPI, HTTPException
from main_api import RunResponse  # same wire contract as the local API server

from ephemeral_net.swarm import (
    load_or_create_secret,
    parse_seed_nodes,
    parse_seeds,
    private_mode_enabled,
    private_student_url,
)
from ephemeral_self_host import Gateway, GatewayError, RunRequest

# --- Configuration -------------------------------------------------------

EPHEMERAL_RELAY = os.getenv("EPHEMERAL_RELAY", "n0")
EPHEMERAL_RELAY_FALLBACK = os.getenv("EPHEMERAL_RELAY_FALLBACK", "0") == "1"
EPHEMERAL_SEED_NODES = parse_seed_nodes(os.getenv("EPHEMERAL_SEED_NODES"))
EPHEMERAL_SEEDS = parse_seeds(os.getenv("EPHEMERAL_SEEDS"))
if EPHEMERAL_SEEDS:
    # Explicit tickets (private network) replace the default swarm nodes.
    EPHEMERAL_SEED_NODES = []
_hex_secret = os.getenv("EPHEMERAL_SECRET", "")
EPHEMERAL_SECRET = (
    bytes.fromhex(_hex_secret) if _hex_secret else load_or_create_secret()
)
EPHEMERAL_ALLOW_NETWORK = os.getenv("EPHEMERAL_ALLOW_NETWORK", "0") == "1"

# Private mode: skip the public swarm list — this node is its own seed for a
# private cluster. Enable via ``--private`` (direct run) or
# ``EPHEMERAL_PRIVATE=1`` (systemd/uvicorn).
PRIVATE_MODE = private_mode_enabled(argv=sys.argv)


# --- Application ---------------------------------------------------------

@asynccontextmanager
async def lifespan(app: FastAPI):
    gateway = Gateway(
        secret_key=EPHEMERAL_SECRET,
        relay=EPHEMERAL_RELAY,
        relay_fallback=EPHEMERAL_RELAY_FALLBACK,
        seed_nodes=EPHEMERAL_SEED_NODES,
        seeds=EPHEMERAL_SEEDS,
        allow_network=EPHEMERAL_ALLOW_NETWORK,
        private=PRIVATE_MODE,
    )
    try:
        await gateway.start()
    except Exception as e:
        # Don't take the whole server down if the network can't join —
        # REST still works via the local sandboxed executor path.
        app.state.gateway = gateway
        app.state.gateway_error = str(e)
        yield
        await gateway.close()
        return
    app.state.gateway = gateway
    node = gateway.node
    print(f"SWARM NODE_ID {node.node_id()}", flush=True)
    print(f"SWARM RELAY {node.relay_url()}", flush=True)
    print(f"SWARM SEED TICKET {node.ticket()}", flush=True)
    if PRIVATE_MODE:
        print(f"SWARM PRIVATE URL {private_student_url(node.ticket())}", flush=True)
        print(
            "SWARM join: private mode — public swarm skipped; this node is "
            "its own seed. Share the PRIVATE URL above with students.",
            flush=True,
        )
    else:
        print(
            "SWARM join: no compiled seeds — this node bootstrapped from the "
            "live swarm list (docs/swarm.json) and will be listed automatically "
            "on the next refresh. NODE_ID/RELAY above are its stable identity.",
            flush=True,
        )
    yield
    await gateway.close()


app = FastAPI(
    title="Ephemeral Distributed API",
    description=(
        "Remote code execution engine backed by the ephemeral iroh cluster "
        "(nearest-neighbor offloading, sandboxed Podman execution)."
    ),
    version="1.0.0",
    lifespan=lifespan,
)


@app.post(
    "/ephemeral/api/v1/run",
    response_model=RunResponse,
    summary="Execute code blocks through the distributed network",
    responses={
        422: {"description": "Invalid base64, missing language, or rejected content"},
        500: {"description": "Podman infrastructure failure"},
        504: {"description": "Execution timed out"},
    },
)
async def run_code(request: RunRequest) -> RunResponse:
    """Accept a base64-encoded Markdown document and run it in the cluster."""
    gateway: Gateway = app.state.gateway
    if gateway is None or getattr(app.state, "gateway_error", None):
        raise HTTPException(
            status_code=500,
            detail=f"Gateway failed to start: {getattr(app.state, 'gateway_error', 'unknown')}",
        )
    try:
        result = await gateway.run(request.document_blob, timeout=request.timeout)
    except GatewayError as e:
        raise HTTPException(status_code=422, detail=str(e)) from e
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Unexpected error: {e}") from e

    return RunResponse(
        exit_code=result.exit_code,
        stdout=result.stdout,
        stderr=result.stderr,
        artifact_file=result.artifact_file,
        artifact_ext=result.artifact_ext,
    )


@app.get("/health")
async def health_check():
    """Check the gateway, cluster membership, and Podman backend."""
    import ephemeral_core

    gateway: Gateway = app.state.gateway
    status = gateway.status() if gateway is not None else {"status": "starting"}
    status["podman"] = (
        "running" if ephemeral_core.check_podman_alive() else "stopped"
    )
    return status


def main() -> None:
    """Run the API server directly — ``python main_distributed.py [--private]``."""
    import uvicorn

    uvicorn.run(
        "main_distributed:app",
        host="0.0.0.0",
        port=int(os.getenv("EPHEMERAL_PORT", "8787")),
    )


if __name__ == "__main__":
    main()
