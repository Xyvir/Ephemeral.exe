# ephemeral-self-host-distributed — headless distributed compute node + REST gateway.
#
# The image bundles the iroh networking tier and the FastAPI gateway. It
# expects a Podman runtime for job execution: mount the host's podman
# socket so the bundled podman CLI can run containers, e.g.
#
#   docker run -d -p 8000:8000 \
#     -v /run/podman/podman.sock:/run/podman/podman.sock \
#     -e EPHEMERAL_RELAY=n0 \
#     -e EPHEMERAL_SEEDS="<seed tickets, comma separated>" \
#     -e EPHEMERAL_ALLOW_NETWORK=0 \
#     ephemeral-self-host-distributed:latest
FROM python:3.12-slim

# podman CLI + CA certs (the bundled client talks to the mounted socket).
RUN apt-get update \
    && apt-get install -y --no-install-recommends podman ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Install Python deps first (layer caching).
COPY requirements-api.txt requirements-net.txt ./
RUN pip install --no-cache-dir -r requirements-api.txt -r requirements-net.txt

# App code.
COPY ephemeral_core/ ./ephemeral_core/
COPY ephemeral_net/ ./ephemeral_net/
COPY ephemeral_self_host/ ./ephemeral_self_host/
COPY main_distributed.py ./

# Runtime configuration (overridable at deploy time).
ENV EPHEMERAL_RELAY=n0 \
    EPHEMERAL_SEEDS="" \
    EPHEMERAL_ALLOW_NETWORK=0 \
    EPHEMERAL_SECRET=""

EXPOSE 8000

CMD ["uvicorn", "main_distributed:app", "--host", "0.0.0.0", "--port", "8000"]
