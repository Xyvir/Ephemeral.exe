# ephemeral-self-host-distributed — headless distributed compute node + REST gateway.
#
# The image bundles the iroh networking tier and the FastAPI gateway. It
# expects a Podman runtime for job execution: mount the host's podman
# socket so the bundled podman CLI can run containers, e.g.
#
#   docker run -d -p 8787:8787 \
#     -v /run/podman/podman.sock:/run/podman/podman.sock \
#     -e EPHEMERAL_RELAY=n0 \
#     -e EPHEMERAL_SEEDS="<seed tickets, comma separated>" \
#     -e EPHEMERAL_ALLOW_NETWORK=0 \
#     ephemeral-self-host-distributed:latest
#
# Port 8787 is the Lithic-UK sidecar slot (its Caddyfile proxies
# /ephemeral/api/v1/* to 127.0.0.1:8787).
FROM python:3.12-slim

# podman CLI + CA certs (the bundled client talks to the mounted socket).
RUN apt-get update \
    && apt-get install -y --no-install-recommends podman ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Install Python deps first (layer caching). Public distributed images stay
# REST-only; private operators opt in with --build-arg INSTALL_MCP=1.
ARG INSTALL_MCP=0
COPY requirements-api.txt requirements-net.txt requirements-mcp.txt ./
RUN pip install --no-cache-dir -r requirements-api.txt -r requirements-net.txt \
    && if [ "$INSTALL_MCP" = "1" ]; then pip install --no-cache-dir -r requirements-mcp.txt; fi

# App code. main_distributed.py imports the shared API wire contract, so it
# must be present alongside the gateway and the execution/net tiers. The MCP
# source is included for private builds; public mode never registers its route.
COPY ephemeral_api/ ./ephemeral_api/
COPY ephemeral_core/ ./ephemeral_core/
COPY ephemeral_net/ ./ephemeral_net/
COPY ephemeral_mcp/ ./ephemeral_mcp/
COPY ephemeral_self_host/ ./ephemeral_self_host/
COPY main_distributed.py ./

# Runtime configuration (overridable at deploy time).
ENV EPHEMERAL_RELAY=n0 \
    EPHEMERAL_SEEDS="" \
    EPHEMERAL_ALLOW_NETWORK=0 \
    EPHEMERAL_SECRET=""

EXPOSE 8787

CMD ["uvicorn", "main_distributed:app", "--host", "0.0.0.0", "--port", "8787"]
