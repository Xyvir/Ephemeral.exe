"""
Opt-in loopback REST bridge for the distributed tray.

Serves the same ``/ephemeral/api/v1`` contract as the self-host API server
(``main_api.py``) from inside the tray process, backed by the tray's own
cluster node — REST clients (Lithic launcher, curl, the lite client) hit
this machine's node the same way they hit a bastion, with no extra daemon
and no extra dependency: the server is stdlib ``http.server`` so a frozen
tray build gains no weight from it.

Loopback-only by construction (the socket binds to 127.0.0.1 — there is
no option to expose it). Off by default; flip it from the tray menu
(Distributed -> Local REST API) or pre-set it with ``EPHEMERAL_TRAY_API=1``
(port via ``EPHEMERAL_TRAY_API_PORT``, default 8788 — the self-host
sidecar slot owns 8787, so the tray bridge deliberately does not).
"""
from __future__ import annotations

import base64
import concurrent.futures
import json
import logging
import os
import threading
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

#: Default loopback port. 8787 belongs to the self-host API sidecar
#: (see main_api.py); the tray bridge sits next to it on 8788.
TRAY_API_PORT = 8788

#: State marker (under the node state dir) that persists the menu toggle,
#: same mechanism as private mode's ``private_mode`` marker.
TRAY_API_MARKER = "tray_api"

#: Refuse bodies larger than this before reading (a run is a base64
#: Markdown document; main_api imposes no explicit cap, uvicorn's is
#: effectively unbounded — the tray picks a generous-but-finite one).
_MAX_BODY_BYTES = 20 * 1024 * 1024

#: Concurrent REST runs allowed against the tray's own node. Hotkey runs
#: bypass this (they are user-paced); the cap only stops a client from
#: stampeding the local podman with parallel posts.
_MAX_CONCURRENT_RUNS = 2

log = logging.getLogger("ephemeral")

# --- state dir persistence (lazy: keeps the net tier out of import) ------

def _state_dir():
    from ephemeral_net.swarm import default_state_dir
    return default_state_dir()


def autostart_enabled() -> bool:
    """Whether the bridge should come up when the tray starts."""
    if os.getenv("EPHEMERAL_TRAY_API", "") == "1":
        return True
    return (_state_dir() / TRAY_API_MARKER).exists()


def persist_enabled(enabled: bool) -> None:
    """Persist (or clear) the menu toggle for the next tray start."""
    marker = _state_dir() / TRAY_API_MARKER
    try:
        if enabled:
            marker.parent.mkdir(parents=True, exist_ok=True)
            marker.touch()
        else:
            marker.unlink(missing_ok=True)
    except OSError as e:
        log.warning("tray-api marker write failed: %s", e)


# --- wire contract (mirrors main_api.RunRequest / ephemeral_api.RunResponse)

def _decode_document_blob(blob) -> str:
    """Base64 -> UTF-8 Markdown, raising ValueError like RunRequest does."""
    if not isinstance(blob, str):
        raise ValueError("document_blob must be a base64-encoded string")
    try:
        decoded = base64.b64decode(blob, validate=True)
    except Exception as e:
        raise ValueError(f"Invalid base64 encoding: {e}") from e
    try:
        return decoded.decode("utf-8")
    except UnicodeDecodeError as e:
        raise ValueError(f"Decoded content is not valid UTF-8: {e}") from e


def _validate_timeout(timeout) -> int:
    """1..600 seconds, like RunRequest.timeout's Field constraint."""
    if isinstance(timeout, bool) or not isinstance(timeout, int):
        raise ValueError("timeout must be an integer between 1 and 600")
    if not 1 <= timeout <= 600:
        raise ValueError("timeout must be an integer between 1 and 600")
    return timeout


# --- error classification ------------------------------------------------

_INFRA_HINTS = ("podman", "image", "pull", "cluster", "node", "machine")


def _error_status(e: Exception) -> int:
    """Best-effort HTTP status for a backend failure.

    main_api separates ValueError (bad input -> 422) from RuntimeError
    (infrastructure -> 500). The distributed tier raises everything as a
    RuntimeError carrying a job message, so classify by message: timeouts
    -> 504, infrastructure-sounding -> 500, everything else (missing
    language, safety rejection, no code blocks) -> 422.
    """
    message = str(e).lower()
    if "starting the cluster node" in message:
        # Bootstrap failure, not an execution timeout — the node never came
        # up, so the run was never accepted (500, not 504).
        return 500
    if "timed out" in message or "timeout" in message:
        return 504
    if any(h in message for h in _INFRA_HINTS):
        return 500
    return 422


# --- server ----------------------------------------------------------------

class TrayApiServer(ThreadingHTTPServer):
    """Loopback HTTP server bridging REST posts to the tray's node."""

    daemon_threads = True
    allow_reuse_address = True

    def __init__(self, backend, port: int) -> None:
        self.backend = backend
        self._run_slots = threading.BoundedSemaphore(_MAX_CONCURRENT_RUNS)
        super().__init__(("127.0.0.1", port), _TrayApiHandler)

    @property
    def bound_port(self) -> int:
        return self.server_address[1]

    # --- execution bridge ----------------------------------------------

    def execute(self, markdown_text: str, timeout: int) -> tuple[int, dict]:
        """Run a decoded Markdown document through the tray's node.

        Returns ``(status_code, payload)`` using the same shapes as
        main_api: a RunResponse dict on success, ``{"detail": ...}`` on
        failure. Runs via the backend's own submission path so hotkey
        runs, one-shot runs, and REST runs all land on the same executor.
        """
        blob = base64.b64encode(markdown_text.encode("utf-8")).decode("ascii")
        with self._run_slots:
            try:
                self.backend._ensure_cluster()
                result = self.backend._run_through_cluster(blob, timeout=timeout)
            except concurrent.futures.TimeoutError:
                return 504, {"detail": f"Execution timed out ({timeout}s limit)"}
            except Exception as e:
                return _error_status(e), {"detail": str(e)}
        # RunResponse fields only — ``artifact_path`` (a local file from
        # standalone execution) is the backend's internal extra.
        return 200, {
            "exit_code": result.get("exit_code", 0),
            "stdout": result.get("stdout", ""),
            "stderr": result.get("stderr", ""),
            "artifact_file": result.get("artifact_file"),
            "artifact_ext": result.get("artifact_ext"),
        }


class _TrayApiHandler(BaseHTTPRequestHandler):
    """One request per handler instance; state lives on the server."""

    server_version = "EphemeralTray"
    protocol_version = "HTTP/1.1"

    # Route the default stderr access log into the ephemeral logger — the
    # frozen exe has no console, and per-request noise is worthless there.
    def log_message(self, format, *args):  # noqa: A002 - stdlib signature
        log.debug("tray-api: " + format, *args)

    # --- helpers ---------------------------------------------------------

    def _send_json(self, status: int, payload: dict) -> None:
        body = json.dumps(payload).encode("utf-8")
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def _route(self) -> str:
        path = self.path.split("?", 1)[0]
        if len(path) > 1:
            path = path.rstrip("/") or "/"
        return path

    # --- verbs -------------------------------------------------------------

    def do_GET(self):
        if self._route() == "/ephemeral/api/v1/health":
            self._send_json(200, {
                "status": "healthy",
                "podman": "delegated (tray node)",
                "version": os.getenv("EPHEMERAL_VERSION", "") or "dev",
                "engine": "distributed-tray",
            })
        else:
            self._send_json(404, {"detail": "Not Found"})

    def do_POST(self):
        if self._route() != "/ephemeral/api/v1/run":
            self._send_json(404, {"detail": "Not Found"})
            return
        try:
            length = int(self.headers.get("Content-Length") or 0)
        except ValueError:
            length = -1
        if length <= 0:
            self._send_json(411, {"detail": "Content-Length required"})
            return
        if length > _MAX_BODY_BYTES:
            self._send_json(413, {"detail": "Request body too large"})
            return
        try:
            raw = self.rfile.read(length)
            req = json.loads(raw.decode("utf-8"))
        except Exception:
            self._send_json(400, {"detail": "Invalid JSON body"})
            return
        try:
            markdown_text = _decode_document_blob(req.get("document_blob"))
            timeout = _validate_timeout(req.get("timeout", 300))
        except ValueError as e:
            self._send_json(422, {"detail": str(e)})
            return
        status, payload = self.server.execute(markdown_text, timeout)
        self._send_json(status, payload)


# --- lifecycle owner (what the tray menu drives) ---------------------------

class TrayApi:
    """Start/stop the bridge and remember its port. One per backend."""

    def __init__(self, backend, port: int | None = None) -> None:
        self.backend = backend
        env_port = os.getenv("EPHEMERAL_TRAY_API_PORT", "")
        self.requested_port = port or (int(env_port) if env_port.isdigit() else TRAY_API_PORT)
        self.port = self.requested_port
        self._server: TrayApiServer | None = None
        self._lock = threading.Lock()

    def is_running(self) -> bool:
        return self._server is not None

    def start(self) -> tuple[bool, object]:
        """Bind and serve on a daemon thread. (True, port) or (False, reason)."""
        with self._lock:
            if self._server is not None:
                return True, self.port
            try:
                server = TrayApiServer(self.backend, self.requested_port)
            except OSError as e:
                log.warning("tray-api bind failed on port %s: %s", self.requested_port, e)
                return False, f"port {self.requested_port} unavailable ({e})"
            thread = threading.Thread(
                target=server.serve_forever,
                name="ephemeral-tray-api",
                daemon=True,
            )
            self._server = server
            self.port = server.bound_port
            thread.start()
            log.info("tray-api listening on http://127.0.0.1:%s", self.port)
            return True, self.port

    def stop(self) -> None:
        with self._lock:
            server = self._server
            self._server = None
        if server is None:
            return
        try:
            server.shutdown()
            server.server_close()
        except Exception:
            pass
        log.info("tray-api stopped (was port %s)", self.port)
