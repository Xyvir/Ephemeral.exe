"""
Ephemeral Distributed API Server — ``ephemeral-self-host-distributed``.

A headless backend that keeps the exact REST contract of ``main_api.py``
while joining the iroh cluster as a compute node. Requests submitted to
the REST endpoint are translated into network job payloads and executed
by the node's sandboxed executor — locally when the required images are
warm, or offloaded to the nearest neighbor that has them (with a
background pull) otherwise.

Configuration (environment variables):

    EPHEMERAL_RELAY          "n0" (default) | "minimal" | "disabled"
    EPHEMERAL_SEEDS          comma-separated EndpointTickets to bootstrap from;
                             unset joins the default swarm (see ephemeral_net.swarm)
    EPHEMERAL_SECRET         hex-encoded 32-byte secret for a persistent node id;
                             unset, a stable identity is auto-persisted to disk
    EPHEMERAL_PORT           HTTP port (default 8787 — the Lithic-UK sidecar slot)
    EPHEMERAL_ALLOW_NETWORK  "1" to let remote jobs use network access (default "0")

Usage:
    uvicorn main_distributed:app --host 0.0.0.0 --port 8787

Port 8787 matches the local API server and the Lithic-UK sidecar slot.

On startup the node prints ``SWARM SEED TICKET ...`` — grab that ticket and
compile it into ``ephemeral_net/swarm.py`` + ``web/config.js`` to make this
gateway the swarm's always-on seed.
"""
from __future__ import annotations

import os
from contextlib import asynccontextmanager

from fastapi import FastAPI, HTTPException
from main_api import RunResponse  # same wire contract as the local API server

from ephemeral_net.swarm import load_or_create_secret, parse_seeds
from ephemeral_self_host import Gateway, GatewayError, RunRequest

# --- Configuration -------------------------------------------------------

EPHEMERAL_RELAY = os.getenv("EPHEMERAL_RELAY", "n0")
EPHEMERAL_SEEDS = parse_seeds(os.getenv("EPHEMERAL_SEEDS"))
_hex_secret = os.getenv("EPHEMERAL_SECRET", "")
EPHEMERAL_SECRET = (
    bytes.fromhex(_hex_secret) if _hex_secret else load_or_create_secret()
)
EPHEMERAL_ALLOW_NETWORK = os.getenv("EPHEMERAL_ALLOW_NETWORK", "0") == "1"


# --- Application ---------------------------------------------------------

@asynccontextmanager
async def lifespan(app: FastAPI):
    gateway = Gateway(
        secret_key=EPHEMERAL_SECRET,
        relay=EPHEMERAL_RELAY,
        seeds=EPHEMERAL_SEEDS,
        allow_network=EPHEMERAL_ALLOW_NETWORK,
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
    print(f"SWARM SEED TICKET {node.ticket()}", flush=True)
    print(
        "SWARM join: this node is part of the default ephemeral swarm; "
        "compile the ticket above into ephemeral_net/swarm.py and "
        "ephemeral-wasm-library/web/config.js to make it the always-on seed.",
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
