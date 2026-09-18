"""
Loopback named-pipe bridge for the distributed tray.

A same-user local IPC channel: the tray hosts the local pipe ``ephemeral-run``
and serves the same run contract as the self-host API server
(``/ephemeral/api/v1/run`` semantics) over a tiny length-prefixed JSON
frame protocol. No sockets, no HTTP, no clipboard — the pipe is a kernel
object only reachable by processes on this machine, so there is nothing
to scan, probe, or authenticate over a network.

Frame protocol (all integers little-endian):

    request : b"EPHR" + u32 length + JSON body
    response: u32 length + JSON body

Request body:  {"token": <hex>, "op": "run"|"health",
                "document_blob": <base64 markdown>, "timeout": <1..600>}
Response body: {"status": 200, ...RunResponse} on success, or
               {"status": 422|403|500|504, "detail": "..."} on failure —
               mirroring the REST contract exactly.

The bridge is on by default (the whole point is that other local tools
can find it without configuration). ``EPHEMERAL_TRAY_PIPE=0`` disables
it entirely; ``EPHEMERAL_TRAY_PIPE_NAME`` overrides the pipe path; the
Distributed menu can toggle it at runtime (persisted via a state marker,
same mechanism as private mode). Every connection must present the hex
token stored beside the state dir — a local squatter on the pipe name
still cannot run anything.
"""

from __future__ import annotations

import base64
import concurrent.futures
import ctypes
import json
import os
import secrets
import struct
import threading
import time

#: Default pipe path (override with EPHEMERAL_TRAY_PIPE_NAME).
PIPE_NAME = "\\\\.\\pipe\\ephemeral-run"

#: State marker: present -> the user toggled the bridge OFF.
TRAY_PIPE_MARKER = "tray_pipe_disabled"

#: Request cap: a markdown document with inlined base64 artifacts can be
#: large, but 16 MiB is far beyond anything a codeblock run needs.
_MAX_MESSAGE = 16 * 1024 * 1024

#: Like the REST bridge: never more than this many runs in flight.
_MAX_CONCURRENT_RUNS = 2

_MAGIC = b"EPHR"


# --- win32 (kernel32 only — no winsock, no network namespace) --------------

_kernel32 = ctypes.WinDLL("kernel32", use_last_error=True)

_HANDLE = ctypes.c_void_p
_GENERIC_READ = 0x80000000
_GENERIC_WRITE = 0x40000000
_PIPE_ACCESS_DUPLEX = 0x3
_PIPE_TYPE_BYTE = 0x0
_PIPE_UNLIMITED_INSTANCES = 255
_INVALID_HANDLE_VALUE = ctypes.c_void_p(-1).value
_ERROR_PIPE_CONNECTED = 535


def _kernel32_fn(name, restype, *params):
    fn = getattr(_kernel32, name)
    fn.restype = restype
    fn.argtypes = list(params)
    return fn


_CreateNamedPipeW = _kernel32_fn(
    "CreateNamedPipeW",
    _HANDLE,
    ctypes.c_wchar_p,          # lpName
    ctypes.c_ulong,            # dwOpenMode
    ctypes.c_ulong,            # dwPipeMode
    ctypes.c_ulong,            # nMaxInstances
    ctypes.c_ulong,            # nOutBufferSize
    ctypes.c_ulong,            # nInBufferSize
    ctypes.c_ulong,            # nDefaultTimeOut
    ctypes.c_void_p,           # lpSecurityAttributes
)
_ConnectNamedPipe = _kernel32_fn(
    "ConnectNamedPipe", ctypes.c_int, _HANDLE, ctypes.c_void_p
)
_DisconnectNamedPipe = _kernel32_fn("DisconnectNamedPipe", ctypes.c_int, _HANDLE)
_FlushFileBuffers = _kernel32_fn("FlushFileBuffers", ctypes.c_int, _HANDLE)
_CloseHandle = _kernel32_fn("CloseHandle", ctypes.c_int, _HANDLE)
_ReadFile = _kernel32_fn(
    "ReadFile",
    ctypes.c_int,
    _HANDLE,                   # hFile
    ctypes.c_char_p,           # lpBuffer
    ctypes.c_ulong,            # nNumberOfBytesToRead
    ctypes.POINTER(ctypes.c_ulong),  # lpNumberOfBytesRead
    ctypes.c_void_p,           # lpOverlapped
)
_WriteFile = _kernel32_fn(
    "WriteFile",
    ctypes.c_int,
    _HANDLE,                   # hFile
    ctypes.c_char_p,           # lpBuffer
    ctypes.c_ulong,            # nNumberOfBytesToWrite
    ctypes.POINTER(ctypes.c_ulong),  # lpNumberOfBytesWritten
    ctypes.c_void_p,           # lpOverlapped
)


def _create_pipe(name: str) -> int:
    handle = _CreateNamedPipeW(
        name,
        _PIPE_ACCESS_DUPLEX,
        _PIPE_TYPE_BYTE,
        _PIPE_UNLIMITED_INSTANCES,
        1 << 20,   # out buffer
        1 << 20,   # in buffer
        0,         # default timeout
        None,
    )
    if not handle or handle == _INVALID_HANDLE_VALUE:
        raise OSError(f"CreateNamedPipeW failed (error {ctypes.get_last_error()})")
    return handle


def _read_exact(handle: int, count: int) -> bytes:
    """Blocking read of exactly ``count`` bytes from the pipe."""
    data = b""
    buffer = ctypes.create_string_buffer(count)
    received = ctypes.c_ulong(0)
    while len(data) < count:
        buffer = ctypes.create_string_buffer(count - len(data))
        if not _ReadFile(handle, buffer, count - len(data), ctypes.byref(received), None):
            raise OSError(f"pipe read failed (error {ctypes.get_last_error()})")
        if received.value == 0:
            raise OSError("pipe closed by peer")
        data += buffer.raw[: received.value]
    return data


def _write_all(handle: int, data: bytes) -> None:
    view = (ctypes.c_char * len(data)).from_buffer_copy(data) if data else b""
    written = ctypes.c_ulong(0)
    offset = 0
    while offset < len(data):
        chunk = view[offset:]
        if not _WriteFile(handle, chunk, len(chunk), ctypes.byref(written), None):
            raise OSError(f"pipe write failed (error {ctypes.get_last_error()})")
        if written.value == 0:
            raise OSError("pipe write made no progress")
        offset += written.value


# --- state -----------------------------------------------------------------

def _state_dir():
    from ephemeral_net.swarm import default_state_dir

    return default_state_dir()


def autostart_enabled() -> bool:
    """The bridge is on unless explicitly disabled (marker or env)."""
    if os.getenv("EPHEMERAL_TRAY_PIPE", "").strip() == "0":
        return False
    return not (_state_dir() / TRAY_PIPE_MARKER).exists()


def persist_enabled(enabled: bool) -> None:
    marker = _state_dir() / TRAY_PIPE_MARKER
    if enabled:
        try:
            marker.unlink(missing_ok=True)
        except OSError:
            pass
    else:
        marker.parent.mkdir(parents=True, exist_ok=True)
        marker.write_text("disabled\n", encoding="utf-8")


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


# --- error classification ---------------------------------------------------

_INFRA_HINTS = ("podman", "image", "pull", "cluster", "node", "machine")


def _error_status(e: Exception) -> int:
    """Best-effort REST-style status for a backend failure.

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


# --- server -----------------------------------------------------------------

class TrayPipe:
    """Named-pipe bridge between local tools and the tray's own node."""

    def __init__(self, backend, pipe_name: str | None = None) -> None:
        self.backend = backend
        self.pipe_name = pipe_name or os.getenv("EPHEMERAL_TRAY_PIPE_NAME") or PIPE_NAME
        self._run_slots = threading.BoundedSemaphore(_MAX_CONCURRENT_RUNS)
        self._stop = threading.Event()
        self._thread: threading.Thread | None = None
        self._token = ""

    # --- lifecycle ----------------------------------------------------------

    def is_running(self) -> bool:
        return self._thread is not None and self._thread.is_alive()

    def start(self) -> tuple[bool, str]:
        """Start serving. Returns ``(ok, pipe_name_or_error)``."""
        if self.is_running():
            return True, self.pipe_name
        if os.getenv("EPHEMERAL_TRAY_PIPE", "").strip() == "0":
            return False, "disabled by EPHEMERAL_TRAY_PIPE=0"
        # Per-boot connection token: a local process that finds the pipe
        # name still cannot submit runs without reading this file.
        self._token = secrets.token_hex(32)
        state_dir = _state_dir()
        state_dir.mkdir(parents=True, exist_ok=True)
        (state_dir / "tray_pipe.token").write_text(self._token, encoding="utf-8")
        self._stop.clear()
        self._thread = threading.Thread(
            target=self._serve, name="ephemeral-pipe-bridge", daemon=True
        )
        self._thread.start()
        return True, self.pipe_name

    def stop(self) -> None:
        self._stop.set()
        # Open and immediately close a client connection to unblock the
        # server thread's waiting ConnectNamedPipe.
        try:
            with open(self.pipe_name, "rb", buffering=0):
                pass
        except OSError:
            pass

    # --- serving loop ---------------------------------------------------------

    def _serve(self) -> None:
        while not self._stop.is_set():
            try:
                handle = _create_pipe(self.pipe_name)
            except OSError as e:
                if self._stop.is_set():
                    break
                time.sleep(1.0)
                continue
            connected = _ConnectNamedPipe(handle, None)
            if not connected:
                error = ctypes.get_last_error()
                if error != _ERROR_PIPE_CONNECTED:
                    _CloseHandle(handle)
                    if self._stop.is_set():
                        break
                    continue
            if self._stop.is_set():
                _CloseHandle(handle)
                break
            try:
                self._serve_connection(handle)
            except Exception as e:  # never let one client kill the bridge
                try:
                    from ephemeral_ui.platform import log

                    log.warning("pipe bridge connection error: %s", e)
                except Exception:
                    pass
            finally:
                try:
                    _FlushFileBuffers(handle)
                except OSError:
                    pass
                try:
                    _DisconnectNamedPipe(handle)
                except OSError:
                    pass
                _CloseHandle(handle)

    def _serve_connection(self, handle: int) -> None:
        magic = _read_exact(handle, 4)
        (length,) = struct.unpack("<I", _read_exact(handle, 4))
        if magic != _MAGIC or not 0 < length <= _MAX_MESSAGE:
            self._reply(handle, {"status": 422, "detail": "malformed frame"})
            return
        try:
            request = json.loads(_read_exact(handle, length).decode("utf-8"))
        except (UnicodeDecodeError, json.JSONDecodeError) as e:
            self._reply(handle, {"status": 422, "detail": f"invalid request: {e}"})
            return

        if not isinstance(request, dict) or request.get("token") != self._token:
            self._reply(handle, {"status": 403, "detail": "invalid pipe token"})
            return

        if request.get("op") == "health":
            self._reply(handle, {
                "status": 200,
                "status_field": "healthy",
                "engine": "distributed-tray",
                "pipe": self.pipe_name,
            })
            return

        try:
            markdown_text = _decode_document_blob(request.get("document_blob"))
            timeout = _validate_timeout(request.get("timeout"))
        except ValueError as e:
            self._reply(handle, {"status": 422, "detail": str(e)})
            return

        status, payload = self.execute(markdown_text, timeout)
        self._reply(handle, {"status": status, **payload})

    def _reply(self, handle: int, payload: dict) -> None:
        body = json.dumps(payload).encode("utf-8")
        _write_all(handle, struct.pack("<I", len(body)) + body)

    # --- execution bridge -------------------------------------------------------

    def execute(self, markdown_text: str, timeout: int) -> tuple[int, dict]:
        """Run a decoded Markdown document through the tray's node.

        Returns ``(status, payload)`` using the same shapes as main_api:
        a RunResponse dict on success, ``{"detail": ...}`` on failure.
        Runs via the backend's own submission path so hotkey runs,
        one-shot runs, and bridge runs all land on the same executor.
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
