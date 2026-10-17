# Ephemeral local-LLM monolith ("batteries included").
#
# One image holds the inference engine, its weights, and a data-science Python
# stack, because Ephemeral's execution cycle is deliberately pull-and-run:
# `podman run --rm -i --network none <image> sh` with nothing bind-mounted but
# /output. There is no host model cache, sidecar pod, or volume mount to hang a
# GGUF on, so the weights ride inside the image.
#
# At runtime /usr/local/bin/ephemeral-llm-entrypoint starts llama.cpp on
# 127.0.0.1:12434, waits for /health, then execs the command Ephemeral passed
# (`sh`, which then runs the block's Python). The OpenAI-compatible server is
# loopback-only, so this works under the default `--network none`.
#
# Model swaps are build args, e.g.:
#   podman build -f dockerfiles/ephemeral-qwen-monolith.dockerfile \
#     --build-arg MODEL_URL=... --build-arg MODEL_SHA256=... -t ephemeral-llm .
#
# NOTE: the model is fetched with curl in a builder stage instead of a remote
# `ADD`. Podman streams `ADD <url>` through its own barebones HTTP client with
# no retries; a ~470 MB Hugging Face LFS download dies mid-transfer with
# `unexpected EOF`. curl + retries + a sha256 check is reproducible on both
# Podman and BuildKit.

ARG LLAMA_CPP_IMAGE=ghcr.io/ggml-org/llama.cpp:server

# --- Stage 1: fetch and verify the GGUF ----------------------------------
FROM docker.io/library/debian:bookworm-slim AS modelfetch

RUN apt-get update \
    && apt-get install -y --no-install-recommends ca-certificates curl \
    && rm -rf /var/lib/apt/lists/*

# Defaults: Qwen2.5-0.5B-Instruct, Q4_K_M (~469 MiB). Small enough to run
# inside Ephemeral's 2 GB container limit (and the ~1 GB scaled limit on
# small hosts) next to a loaded pandas/scipy stack, and fast enough on 2 CPUs
# that a chat round-trip is a few seconds. Change all three args together to
# bake a different model; pass MODEL_SHA256= (empty) to skip verification.
ARG MODEL_URL=https://huggingface.co/Qwen/Qwen2.5-0.5B-Instruct-GGUF/resolve/main/qwen2.5-0.5b-instruct-q4_k_m.gguf
ARG MODEL_SHA256=74a4da8c9fdbcd15bd1f6d01d621410d31c6fc00986f5eb687824e7b93d7a9db
ARG MODEL_FILE=qwen2.5-0.5b-instruct-q4_k_m.gguf

RUN mkdir -p /models \
    && for attempt in 1 2 3 4 5; do \
         echo "model download attempt ${attempt}/5"; \
         curl -fL --retry 5 --retry-delay 2 --retry-all-errors \
              --connect-timeout 30 \
              -o "/models/${MODEL_FILE}" "${MODEL_URL}" && break; \
         sleep $((attempt * 5)); \
       done \
    && test -s "/models/${MODEL_FILE}" \
    && if [ -n "${MODEL_SHA256}" ]; then \
         echo "${MODEL_SHA256}  /models/${MODEL_FILE}" | sha256sum -c -; \
       fi \
    && ln -s "/models/${MODEL_FILE}" /models/model.gguf \
    && ls -l /models

# --- Stage 2: runtime ----------------------------------------------------
FROM ${LLAMA_CPP_IMAGE}

LABEL org.opencontainers.image.title="ephemeral-qwen-monolith" \
      org.opencontainers.image.description="llama.cpp + Qwen2.5-0.5B + a scientific Python data stack in one container for Ephemeral" \
      org.opencontainers.image.source="https://github.com/Xyvir/Ephemeral.exe"

# Ephemeral LLM knobs are read by the entrypoint and the in-image helper;
# headless/unbuffered defaults suit a one-shot sandbox.
ENV DEBIAN_FRONTEND=noninteractive \
    EPHEMERAL_LLM_PORT=12434 \
    EPHEMERAL_LLM_MODEL=/models/model.gguf \
    EPHEMERAL_LLM_MODEL_NAME=local \
    EPHEMERAL_LLM_CTX=4096 \
    EPHEMERAL_LLM_THREADS=2 \
    EPHEMERAL_LLM_LOG=/tmp/llama-server.log \
    MPLBACKEND=Agg \
    PYTHONUNBUFFERED=1 \
    PYTHONDONTWRITEBYTECODE=1 \
    PYTHONPATH=/opt/ephemeral-lib \
    PIP_DISABLE_PIP_VERSION_CHECK=1 \
    PATH=/opt/llm/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

# python3-venv pulls the matching 3.12 interpreter on Ubuntu 24.04. The data
# stack lands in a venv (not system site-packages) so `python` on PATH is
# always the one with pandas et al, and nothing shadows the distro packages
# llama.cpp's image already relies on.
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
         ca-certificates curl python3 python3-venv python3-pip \
    && rm -rf /var/lib/apt/lists/* \
    && python3 -m venv /opt/llm \
    && /opt/llm/bin/pip install --no-cache-dir --upgrade pip

COPY dockerfiles/ephemeral-qwen-monolith/requirements.txt /tmp/requirements.txt
RUN /opt/llm/bin/pip install --no-cache-dir -r /tmp/requirements.txt \
    && rm -f /tmp/requirements.txt

# Fail the build (not the user's snippet) if a wheel is broken, and warm the
# matplotlib font cache so the first plot in a run isn't slow.
RUN /opt/llm/bin/python -c "import numpy, pandas, polars, pyarrow, duckdb, scipy, sklearn, matplotlib, openai; \
matplotlib.use('Agg'); import matplotlib.pyplot as plt; plt.figure(); plt.plot([0, 1]); plt.close()"

# /app/llama-server is a launcher that loads libllama-*.so from /app; the
# entrypoint sets this per-process too, but exporting it keeps a manual
# `podman run --entrypoint /app/llama-server ...` working from any cwd.
ENV LD_LIBRARY_PATH=/app

COPY --from=modelfetch /models /models
COPY dockerfiles/ephemeral-qwen-monolith/ephemeral_llm.py /opt/ephemeral-lib/ephemeral_llm.py
COPY dockerfiles/ephemeral-qwen-monolith/entrypoint.sh /usr/local/bin/ephemeral-llm-entrypoint
RUN chmod 0755 /usr/local/bin/ephemeral-llm-entrypoint

WORKDIR /tmp
EXPOSE 12434

# The entrypoint boots the model, then hands off to whatever Ephemeral passes
# (`sh`, with the block wrapper script on stdin). It must stay the image
# ENTRYPOINT: LANG_MAP deliberately does not set `entrypoint`, so `sh` lands
# here as an argument instead of replacing this boot step.
ENTRYPOINT ["/usr/local/bin/ephemeral-llm-entrypoint"]
CMD ["sh"]

# The GGUF, the Python data stack and llama.cpp all have arm64 builds, so this
# image is published for both linux/amd64 and linux/arm64 (the CI matrix).
# Keep it that way: an amd64-only tag makes every arm64 swarm node skip the
# language (image_is_compatible fails), which looks like "the model is broken".
