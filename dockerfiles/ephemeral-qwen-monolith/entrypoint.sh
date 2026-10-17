#!/bin/sh
# Boot the in-image llama.cpp server, then hand the container over to the
# command Ephemeral asked for (`sh`, which reads the block wrapper script from
# stdin). Runs as PID 1, so `exec` keeps the container's lifetime tied to the
# user's snippet: when the shell exits, the whole namespace (and the server)
# goes with it.
#
# Why boot here instead of in LANG_MAP's `cmd`: Ephemeral always appends `sh`
# to `podman run`, so the image ENTRYPOINT is the one hook that sees every run
# (single block, multi-block, and the two-stage Python path) before any user
# code executes.
set -eu

PORT="${EPHEMERAL_LLM_PORT:-12434}"
MODEL="${EPHEMERAL_LLM_MODEL:-/models/model.gguf}"
MODEL_NAME="${EPHEMERAL_LLM_MODEL_NAME:-local}"
CTX="${EPHEMERAL_LLM_CTX:-4096}"
THREADS="${EPHEMERAL_LLM_THREADS:-2}"
LOG="${EPHEMERAL_LLM_LOG:-/tmp/llama-server.log}"
SERVER=/app/llama-server

# /health answers 503 while weights load and 200 once the model is servable.
wait_for_model() {
    waited=0
    while [ "$waited" -lt 600 ]; do
        if curl -fsS "http://127.0.0.1:${PORT}/health" >/dev/null 2>&1; then
            return 0
        fi
        kill -0 "$1" 2>/dev/null || return 1
        sleep 0.1
        waited=$((waited + 1))
    done
    return 1
}

if [ "${EPHEMERAL_LLM_DISABLE:-0}" != "1" ] && [ -x "$SERVER" ] && [ -f "$MODEL" ]; then
    # The llama.cpp `server` image ships /app/llama-server as a thin launcher
    # that resolves libllama-*.so next to itself; started from /tmp (Ephemeral's
    # workdir) it dies with "cannot open shared object file". LD_LIBRARY_PATH
    # makes the location explicit regardless of cwd. stdin must be /dev/null:
    # the shell below owns stdin (it *is* the block script), and a server that
    # read from it would swallow user code.
    LD_LIBRARY_PATH="/app${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
    "$SERVER" \
        --model "$MODEL" \
        --host 127.0.0.1 \
        --port "$PORT" \
        --ctx-size "$CTX" \
        --threads "$THREADS" \
        --parallel 1 \
        --alias "$MODEL_NAME" \
        --no-webui \
        --log-file "$LOG" \
        </dev/null >/dev/null 2>&1 &
    server_pid=$!

    if ! wait_for_model "$server_pid"; then
        # Loud on stderr but not fatal: a data-only snippet in this image
        # still has a legitimate reason to run without the model. Any actual
        # model call will fail with a connection error the user can trace
        # back to these lines.
        echo "[ephemeral-llm] WARNING: local model did not become ready on 127.0.0.1:${PORT}" >&2
        tail -n 20 "$LOG" >&2 2>/dev/null || true
    fi
fi

[ "$#" -gt 0 ] || set -- sh
exec "$@"
