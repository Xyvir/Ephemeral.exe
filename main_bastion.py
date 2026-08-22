"""
Ephemeral Bastion Server — the paper-light client HTTP gateway.

A "bastion" is the public HTTP(S) face of the ephemeral swarm for
paper-light clients (curl-friendly, no WASM). It:

* joins the swarm as an iroh node and forwards each REST request through
  the same warm-image → idle-first → lowest-RTT routing the wasm SPA uses;
* optionally runs its own Podman executor so its own requests can execute
  locally as a fallback (a full-fledged coderunner node), or stays
  orchestration-only when no Podman is available (e.g. on Railway);
* advertises its public URL in ``hello`` frames so the swarm refresh can
  publish it in ``docs/swarm.json`` for paper-light clients to discover;
* rate-limits per client IP and caps concurrent jobs, and caches exact
  repeated requests so identical submissions skip compute entirely.

Configuration (environment variables):

    EPHEMERAL_RELAY           "n0" (default) | "minimal" | "disabled"
    EPHEMERAL_SEED_NODES      comma-separated node_id[@relay] for a PRIVATE
                              network; unset joins the public swarm via the
                              live bootstrap list (docs/swarm.json)
    EPHEMERAL_SEEDS           comma-separated EndpointTickets (private /
                              backward compat; overrides SEED_NODES)
    EPHEMERAL_SECRET          hex-encoded 32-byte secret for a stable node id
    EPHEMERAL_PORT            HTTP port (default 8787)
    EPHEMERAL_ALLOW_NETWORK   "1" to let remote jobs use network (default "0")
    EPHEMERAL_PRIVATE         "1" (or --private) — skip the public swarm list
    EPHEMERAL_PUBLIC_URL      this bastion's public URL (e.g. its Railway
                              domain); unset, RAILWAY_PUBLIC_DOMAIN is used
    EPHEMERAL_COMPUTE         "1" force a full compute node, "0" force
                              orchestration-only; unset auto-detects Podman
    EPHEMERAL_RATE_LIMIT_PER_MIN  per-client-IP request rate (default 60)
    EPHEMERAL_RATE_LIMIT_BURST    per-IP burst capacity (default = per-minute)
    EPHEMERAL_MAX_CONCURRENT      max simultaneous jobs (default 8)
    EPHEMERAL_CACHE_MAX           cached-response LRU size (default 512)
    EPHEMERAL_CACHE_TTL           cached-response lifetime in seconds (default 300)

Usage:
    uvicorn main_bastion:app --host 0.0.0.0 --port 8787
    python main_bastion.py
"""
from __future__ import annotations

import os
import sys
from contextlib import asynccontextmanager

from fastapi import FastAPI, HTTPException, Request, Response
from fastapi.responses import JSONResponse
from main_api import RunResponse  # same wire contract as the local API server

from ephemeral_net.swarm import (
    load_or_create_secret,
    parse_seed_nodes,
    parse_seeds,
    private_mode_enabled,
    private_student_url,
)
from ephemeral_self_host import Gateway, GatewayError, RunRequest
from ephemeral_self_host.bastion import (
    ConcurrencyLimiter,
    ResultCache,
    TokenBucketLimiter,
    client_ip,
)

# --- Configuration -------------------------------------------------------

EPHEMERAL_RELAY = os.getenv("EPHEMERAL_RELAY", "n0")
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
PRIVATE_MODE = private_mode_enabled(argv=sys.argv)

_COMPUTE_ENV = os.getenv("EPHEMERAL_COMPUTE", "").strip().lower()


def _resolve_compute() -> bool:
    """Whether this bastion also runs a local Podman executor (full node)."""
    if _COMPUTE_ENV in ("1", "true", "yes", "on"):
        return True
    if _COMPUTE_ENV in ("0", "false", "no", "off"):
        return False
    try:
        import ephemeral_core

        return ephemeral_core.check_podman_alive()
    except Exception:
        return False


def _resolve_public_url() -> str | None:
    """This bastion's public URL (explicit env, then Railway's domain)."""
    raw = os.getenv("EPHEMERAL_PUBLIC_URL", "").strip()
    if not raw:
        raw = os.getenv("RAILWAY_PUBLIC_DOMAIN", "").strip()
    if not raw:
        return None
    if not raw.startswith(("http://", "https://")):
        raw = "https://" + raw
    return raw.rstrip("/")


def _int_env(name: str, default: int) -> int:
    try:
        return int(os.getenv(name, "").strip() or default)
    except ValueError:
        return default


PUBLIC_URL = _resolve_public_url()
COMPUTE = _resolve_compute()

RATE_PER_MIN = max(1, _int_env("EPHEMERAL_RATE_LIMIT_PER_MIN", 60))
RATE_BURST = max(1, _int_env("EPHEMERAL_RATE_LIMIT_BURST", RATE_PER_MIN))
MAX_CONCURRENT = max(1, _int_env("EPHEMERAL_MAX_CONCURRENT", 8))
CACHE_MAX = max(1, _int_env("EPHEMERAL_CACHE_MAX", 512))
CACHE_TTL = max(1.0, float(_int_env("EPHEMERAL_CACHE_TTL", 300)))

rate_limiter = TokenBucketLimiter(
    rate=RATE_PER_MIN / 60.0,
    burst=RATE_BURST,
)
concurrency = ConcurrencyLimiter(limit=MAX_CONCURRENT)
cache = ResultCache(max_entries=CACHE_MAX, ttl_seconds=CACHE_TTL)


# --- Application ---------------------------------------------------------

@asynccontextmanager
async def lifespan(app: FastAPI):
    gateway = Gateway(
        secret_key=EPHEMERAL_SECRET,
        relay=EPHEMERAL_RELAY,
        seed_nodes=EPHEMERAL_SEED_NODES,
        seeds=EPHEMERAL_SEEDS,
        allow_network=EPHEMERAL_ALLOW_NETWORK,
        private=PRIVATE_MODE,
        public_url=PUBLIC_URL,
        compute=COMPUTE,
    )
    try:
        await gateway.start()
    except Exception as e:
        # REST orchestration can still forward through peers even if the
        # local network join hiccupped — but a bastion that never joined has
        # no swarm to orchestrate, so surface the error and serve degraded.
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
    if PUBLIC_URL:
        print(f"BASTION PUBLIC URL {PUBLIC_URL}", flush=True)
        print(
            "BASTION this URL is advertised in hello frames and will be "
            "published in docs/swarm.json on the next swarm refresh.",
            flush=True,
        )
    else:
        print(
            "BASTION no public URL configured (set EPHEMERAL_PUBLIC_URL or "
            "RAILWAY_PUBLIC_DOMAIN) — this bastion will not be listed.",
            flush=True,
        )
    print(f"BASTION COMPUTE {'on' if COMPUTE else 'off'} (local fallback)", flush=True)
    if PRIVATE_MODE:
        print(f"SWARM PRIVATE URL {private_student_url(node.ticket())}", flush=True)
    yield
    await gateway.close()


app = FastAPI(
    title="Ephemeral Bastion API",
    description=(
        "Paper-light HTTP gateway into the ephemeral swarm: rate-limited, "
        "cached orchestration of sandboxed Podman jobs across peer nodes."
    ),
    version="1.0.0",
    lifespan=lifespan,
)


@app.post(
    "/ephemeral/api/v1/run",
    response_model=RunResponse,
    summary="Execute code blocks through the swarm (paper-light client entry)",
    responses={
        422: {"description": "Invalid base64, missing language, or rejected content"},
        429: {"description": "Client rate limit exceeded"},
        500: {"description": "Infrastructure failure"},
        503: {"description": "Bastion is at its concurrent-job cap"},
    },
)
async def run_code(
    request: RunRequest,
    req: Request,
    response: Response,
) -> RunResponse:
    """Accept a base64-encoded Markdown document and run it in the swarm."""
    ip = client_ip(req)
    if not rate_limiter.allow(ip):
        raise HTTPException(
            status_code=429,
            detail="rate limit exceeded",
            headers={"Retry-After": "1"},
        )

    cached = cache.get(request.document_blob, request.timeout)
    if cached is not None:
        response.headers["X-Ephemeral-Cache"] = "hit"
        return RunResponse(**cached)

    gateway: Gateway = app.state.gateway
    if gateway is None or getattr(app.state, "gateway_error", None):
        raise HTTPException(
            status_code=500,
            detail=f"Gateway failed to start: {getattr(app.state, 'gateway_error', 'unknown')}",
        )

    if not await concurrency.acquire():
        raise HTTPException(
            status_code=503,
            detail="server busy — concurrent job cap reached",
            headers={"Retry-After": "1"},
        )

    try:
        try:
            result = await gateway.run(request.document_blob, timeout=request.timeout)
        except GatewayError as e:
            raise HTTPException(status_code=422, detail=str(e)) from e
        except Exception as e:
            raise HTTPException(status_code=500, detail=f"Unexpected error: {e}") from e
    finally:
        await concurrency.release()

    payload = RunResponse(
        exit_code=result.exit_code,
        stdout=result.stdout,
        stderr=result.stderr,
        artifact_file=result.artifact_file,
        artifact_ext=result.artifact_ext,
    )
    cache.put(request.document_blob, request.timeout, payload.model_dump())
    response.headers["X-Ephemeral-Cache"] = "miss"
    return payload


@app.get("/health")
async def health_check():
    """Liveness for the swarm refresh: 200 when the bastion can orchestrate."""
    gateway: Gateway = app.state.gateway
    if gateway is None or gateway.node is None:
        return JSONResponse(
            {"status": "starting", "bastion": True}, status_code=503
        )
    status = gateway.status()
    status["bastion"] = True
    status["compute"] = COMPUTE
    status["public_url"] = PUBLIC_URL
    status["cache_entries"] = len(cache)
    status["concurrent_jobs"] = concurrency.active
    return status


def main() -> None:
    """Run the API server directly — ``python main_bastion.py``."""
    import uvicorn

    uvicorn.run(
        "main_bastion:app",
        host="0.0.0.0",
        port=int(os.getenv("EPHEMERAL_PORT", "8787")),
    )


if __name__ == "__main__":
    main()
