"""
Local double-knock pipe trigger for the distributed tray.

A doorbell with an arm/disarm step: the FIRST opening of the pipe (the
``ephemeral-run`` entry under the system's named-pipe namespace) is only
an acknowledgement — the tray is alive and the trigger arms for a short
window. A SECOND opening within that window fires Run Clipboard, exactly
as if the user had pressed ctrl+alt+x (results clobbered onto the
clipboard as usual). No payload ever travels the pipe in either
direction, so the clipboard remains the one and only data channel.

Why two knocks: the first is a fast liveness probe with no side effects,
so local tools can check for the tray without risking a run or clobbering
the clipboard, and a stray single open (a scanner, a curious tool) can
never execute anything on its own.

The trigger is on by default (local tools find it with zero
configuration). ``EPHEMERAL_TRAY_PIPE=0`` disables it entirely;
``EPHEMERAL_TRAY_PIPE_NAME`` overrides the pipe path; the Distributed
menu can toggle it at runtime (persisted via a state marker, same
mechanism as private mode).
"""

from __future__ import annotations

import ctypes
import os
import threading
import time

#: Default pipe path (override with EPHEMERAL_TRAY_PIPE_NAME).
PIPE_NAME = "\\\\.\\pipe\\ephemeral-run"

#: State marker: present -> the user toggled the trigger OFF.
TRAY_PIPE_MARKER = "tray_pipe_disabled"


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
_CloseHandle = _kernel32_fn("CloseHandle", ctypes.c_int, _HANDLE)


def _create_pipe(name: str) -> int:
    handle = _CreateNamedPipeW(
        name,
        _PIPE_ACCESS_DUPLEX,
        _PIPE_TYPE_BYTE,
        _PIPE_UNLIMITED_INSTANCES,
        4096,      # out buffer
        4096,      # in buffer
        0,         # default timeout
        None,
    )
    if not handle or handle == _INVALID_HANDLE_VALUE:
        raise OSError(f"CreateNamedPipeW failed (error {ctypes.get_last_error()})")
    return handle


# --- state -----------------------------------------------------------------

def _state_dir():
    from ephemeral_net.swarm import default_state_dir

    return default_state_dir()


def autostart_enabled() -> bool:
    """The trigger is on unless explicitly disabled (marker or env)."""
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


# --- server -----------------------------------------------------------------

class TrayPipe:
    """Double-knock doorbell that fires the tray's Run Clipboard action."""

    #: How long the first knock keeps the trigger armed (seconds).
    ARM_WINDOW_SECONDS = 3.0

    def __init__(self, backend, pipe_name: str | None = None) -> None:
        self.backend = backend
        self.pipe_name = pipe_name or os.getenv("EPHEMERAL_TRAY_PIPE_NAME") or PIPE_NAME
        self._stop = threading.Event()
        self._thread: threading.Thread | None = None
        self._icon = None
        # Monotonic timestamp of the arming knock; only touched by the
        # serve thread, so no lock is needed.
        self._armed_at: float | None = None

    # --- lifecycle ----------------------------------------------------------

    def is_running(self) -> bool:
        return self._thread is not None and self._thread.is_alive()

    def attach_icon(self, icon) -> None:
        """Give the trigger the tray icon so runs animate/notify normally."""
        self._icon = icon

    def start(self) -> tuple[bool, str]:
        """Start listening. Returns ``(ok, pipe_name_or_error)``."""
        if self.is_running():
            return True, self.pipe_name
        if os.getenv("EPHEMERAL_TRAY_PIPE", "").strip() == "0":
            return False, "disabled by EPHEMERAL_TRAY_PIPE=0"
        self._stop.clear()
        self._armed_at = None
        self._thread = threading.Thread(
            target=self._serve, name="ephemeral-pipe-trigger", daemon=True
        )
        self._thread.start()
        return True, self.pipe_name

    def stop(self) -> None:
        self._stop.set()
        # Open and immediately close a client connection to unblock the
        # server thread's waiting ConnectNamedPipe; the stop flag makes the
        # server swallow that connection without arming or firing.
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
            except OSError:
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
            # A connection IS a knock; no bytes are read or written. The
            # first knock arms (ack), a second within the window fires.
            now = time.monotonic()
            armed = (
                self._armed_at is not None
                and now - self._armed_at <= self.ARM_WINDOW_SECONDS
            )
            self._armed_at = None if armed else now
            try:
                if armed:
                    self._ring()
            except Exception:
                try:
                    from ephemeral_ui.platform import log

                    log.warning("pipe trigger error", exc_info=True)
                except Exception:
                    pass
            finally:
                try:
                    _DisconnectNamedPipe(handle)
                except OSError:
                    pass
                _CloseHandle(handle)

    def _ring(self) -> None:
        """Fire Run Clipboard exactly like a hotkey press would."""
        icon = self._icon
        if icon is None:
            return
        # base.on_hotkey spawns its own worker thread, so the serve loop
        # never blocks on a running job.
        self.backend.on_hotkey(icon)
