"""
Tests for the tray double-knock pipe trigger (ephemeral_ui.tray_pipe).

Semantics under test: knock 1 = ack/arm only, knock 2 within the arm
window = fire Run Clipboard, stale second knocks expire, and the stop
path's own connection is swallowed. CI can run it directly:

    python test_tray_pipe.py
"""
import os
import sys
import tempfile
import time
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

# Hermetic state dir must exist before ephemeral_net.swarm is consulted.
_STATE = tempfile.mkdtemp(prefix="eph-tray-pipe-test-")
os.environ["EPHEMERAL_STATE_DIR"] = _STATE
_PIPE = "\\\\.\\pipe\\ephemeral-run-test-{}".format(os.getpid())

from ephemeral_ui.tray_pipe import TrayPipe, autostart_enabled, persist_enabled  # noqa: E402


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
    assert autotart_is_on(), "env kill-switch check failed"
    # Env kill-switch wins regardless of marker.
    os.environ["EPHEMERAL_TRAY_PIPE"] = "0"
    assert autostart_enabled() is False
    del os.environ["EPHEMERAL_TRAY_PIPE"]
    assert autostart_enabled() is True


def autotart_is_on():
    return autostart_enabled() is True


def test_single_knock_never_fires():
    backend = FakeBackend()
    pipe = TrayPipe(backend, pipe_name=_PIPE)
    pipe.attach_icon(object())
    ok, _ = pipe.start()
    assert ok
    try:
        # Knock 1: ack/arm only — no run.
        with _connect():
            pass
        time.sleep(0.3)
        assert backend.calls == [], backend.calls

        # Knock 2 immediately: fires exactly once.
        with _connect():
            pass
        assert _wait_for(lambda: len(backend.calls) == 1), backend.calls
        time.sleep(0.2)
        assert len(backend.calls) == 1, backend.calls
    finally:
        pipe.stop()
        assert _wait_for(lambda: not pipe.is_running())


def test_stale_second_knock_expires():
    backend = FakeBackend()
    pipe = TrayPipe(backend, pipe_name=_PIPE)
    pipe.attach_icon(object())
    pipe.ARM_WINDOW_SECONDS = 0.4  # shrink for test speed
    ok, _ = pipe.start()
    assert ok
    try:
        with _connect():
            pass  # knock 1: arm
        time.sleep(0.7)  # let the window lapse
        with _connect():
            pass  # knock 2 arrives late: re-arms, no run
        time.sleep(0.3)
        assert backend.calls == [], backend.calls
        with _connect():
            pass  # knock 3: now armed again, fires
        assert _wait_for(lambda: len(backend.calls) == 1), backend.calls
    finally:
        pipe.stop()
        assert _wait_for(lambda: not pipe.is_running())


def test_rapid_sequence_fires_each_double_knock():
    backend = FakeBackend()
    pipe = TrayPipe(backend, pipe_name=_PIPE)
    pipe.attach_icon(object())
    ok, _ = pipe.start()
    assert ok
    try:
        for _ in range(2):
            with _connect():
                pass
            with _connect():
                pass
            assert _wait_for(lambda: len(backend.calls) >= 1), backend.calls
            time.sleep(0.2)
        assert len(backend.calls) == 2, backend.calls
    finally:
        pipe.stop()
        assert _wait_for(lambda: not pipe.is_running())


def test_stop_connection_is_swallowed():
    backend = FakeBackend()
    pipe = TrayPipe(backend, pipe_name=_PIPE)
    pipe.attach_icon(object())
    ok, _ = pipe.start()
    assert ok
    pipe.stop()
    # Even though stop() itself opens the pipe, and even if that open
    # coincided with an armed state, nothing may fire after stop.
    time.sleep(0.4)
    assert backend.calls == [], backend.calls
    assert _wait_for(lambda: not pipe.is_running())


def test_ring_requires_icon():
    backend = FakeBackend()
    pipe = TrayPipe(backend, pipe_name=_PIPE)
    ok, _ = pipe.start()
    assert ok
    try:
        with _connect():
            pass
        with _connect():
            pass  # would be a fire, but no icon -> no-op
        time.sleep(0.4)
        assert backend.calls == [], backend.calls
    finally:
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
