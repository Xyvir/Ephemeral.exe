"""
Tests for the tray pipe trigger (ephemeral_ui.tray_pipe).

The trigger is a doorbell: a client opening the pipe must fire the
backend's Run Clipboard action and nothing else — no bytes are read or
written. CI can run it directly:

    python test_tray_pipe.py
"""
import os
import sys
import tempfile
import threading
import time
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

# Hermetic state dir must exist before ephemeral_net.swarm is consulted.
_STATE = tempfile.mkdtemp(prefix="eph-tray-pipe-test-")
os.environ["EPHEMERAL_STATE_DIR"] = _STATE
_PIPE = "\\\\.\\pipe\\ephemeral-run-test-{}".format(os.getpid())

from ephemeral_ui.tray_pipe import (  # noqa: E402
    TrayPipe,
    autostart_enabled,
    persist_enabled,
)


class FakeBackend:
    """Records doorbell rings; no cluster, no icon side effects."""

    def __init__(self):
        self.calls = []

    def on_hotkey(self, icon):
        self.calls.append(icon)


def _wait_for(predicate, timeout=3.0):
    deadline = time.time() + timeout
    while time.time() < deadline:
        if predicate():
            return True
        time.sleep(0.05)
    return False


def _connect():
    """Open the pipe with retries (single listening instance)."""
    deadline = time.time() + 3
    last = None
    while time.time() < deadline:
        try:
            return open(_PIPE, "r+b", buffering=0)
        except OSError as e:
            last = e
            time.sleep(0.05)
    raise AssertionError(f"pipe never became connectable: {last}")


def test_persistence_roundtrip():
    # Default: enabled (no marker).
    assert autostart_enabled() is True
    persist_enabled(False)
    assert autostart_enabled() is False
    persist_enabled(True)
    assert autostart_enabled() is True
    # Env kill-switch wins regardless of marker.
    os.environ["EPHEMERAL_TRAY_PIPE"] = "0"
    assert autostart_enabled() is False
    del os.environ["EPHEMERAL_TRAY_PIPE"]
    assert autostart_enabled() is True


def test_ring_requires_icon():
    backend = FakeBackend()
    pipe = TrayPipe(backend, pipe_name=_PIPE)
    ok, result = pipe.start()
    assert ok, result
    try:
        # A ring before attach_icon is a no-op (no icon -> nothing to run).
        with _connect():
            pass
        assert not _wait_for(lambda: bool(backend.calls), timeout=0.6)
        assert backend.calls == []

        # After attach, a connection fires Run Clipboard with the icon.
        icon = object()
        pipe.attach_icon(icon)
        with _connect():
            pass
        assert _wait_for(lambda: backend.calls == [icon]), backend.calls
    finally:
        pipe.stop()
        assert _wait_for(lambda: not pipe.is_running()), "server did not stop"


def test_stop_connect_does_not_ring():
    backend = FakeBackend()
    pipe = TrayPipe(backend, pipe_name=_PIPE)
    pipe.attach_icon(object())
    ok, _ = pipe.start()
    assert ok
    pipe.stop()
    # The unblocking client connection used by stop() must not fire a run.
    assert not _wait_for(lambda: bool(backend.calls), timeout=0.8)
    assert _wait_for(lambda: not pipe.is_running())


def test_is_running_lifecycle():
    backend = FakeBackend()
    pipe = TrayPipe(backend, pipe_name=_PIPE)
    assert pipe.is_running() is False
    ok, _ = pipe.start()
    assert ok and pipe.is_running()
    # Starting twice is idempotent (no second server thread).
    ok2, _ = pipe.start()
    assert ok2 and pipe.is_running()
    pipe.stop()
    assert _wait_for(lambda: not pipe.is_running())
    # Restart works after stop.
    ok3, _ = pipe.start()
    assert ok3 and pipe.is_running()
    pipe.stop()
    assert _wait_for(lambda: not pipe.is_running())


def main():
    tests = [v for k, v in sorted(globals().items()) if k.startswith("test_")]
    failures = 0
    for test in tests:
        try:
            test()
            print(f"  ok  {test.__name__}")
        except Exception as e:
            failures += 1
            import traceback

            print(f"FAIL  {test.__name__}: {e}")
            traceback.print_exc()
    print("\n" + ("ALL PIPE TESTS PASSED" if failures == 0 else f"{failures} FAILURES")
          + f" ({len(tests)} tests)")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
