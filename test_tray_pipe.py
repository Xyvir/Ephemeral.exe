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

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

# Hermetic state dir must exist before ephemeral_net.swarm is consulted.
_STATE = tempfile.mkdtemp(prefix="eph-tray-pipe-test-")
os.environ["EPHEMERAL_STATE_DIR"] = _STATE

#: Each test gets its own pipe name: within one process, a lingering
#: instance from a previous test could otherwise swallow a knock meant
#: for the next test's server. (Cross-PROCESS reuse — the real takeover
#: scenario — is kernel-clean at process exit, so production is immune.)
_TEST_SEQ = iter(range(1000))


def _pipe() -> str:
    return "\\\\.\\pipe\\ephemeral-run-test-{}-{}".format(
        os.getpid(), next(_TEST_SEQ)
    )


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


def _connect(pipe_name):
    """Open the pipe with retries (single listening instance)."""
    deadline = time.time() + 3
    last = None
    while time.time() < deadline:
        try:
            return open(pipe_name, "r+b", buffering=0)
        except OSError as e:
            last = e
            time.sleep(0.05)
    raise AssertionError(f"pipe never became connectable: {last}")


def _pipe_gone(pipe_name) -> bool:
    try:
        with open(pipe_name, "rb", buffering=0):
            pass
        return False
    except OSError:
        return True


def _wait_armed(pipe, armed, timeout=2.0):
    """Wait until the server thread has (or has not) processed an arming
    knock. Client-side sleeps alone are racy: the server's processing of a
    knock can lag behind the client's next step under scheduler jitter."""
    ok = _wait_for(
        lambda: (pipe._armed_at is not None) == armed, timeout=timeout
    )
    if not ok:
        thread = pipe._thread
        print(
            f"    [diag] armed={pipe._armed_at!r} wanted={armed} "
            f"thread_alive={thread.is_alive() if thread else None} "
            f"pipe_gone={_pipe_gone(pipe.pipe_name)} "
            f"knocks={pipe.knock_log}"
        )
    return ok


def _stop_and_wait_gone(pipe):
    """Stop the server and wait until the pipe instance is truly gone, so
    nothing can land a knock on a dying server's last instance."""
    pipe.stop()
    assert _wait_for(lambda: not pipe.is_running()), "server did not stop"
    assert _wait_for(
        lambda: _pipe_gone(pipe.pipe_name), timeout=2.0
    ), "pipe still connectable"


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


def test_single_knock_never_fires():
    backend = FakeBackend()
    name = _pipe()
    pipe = TrayPipe(backend, pipe_name=name)
    pipe.attach_icon(object())
    ok, _ = pipe.start()
    assert ok
    try:
        # Knock 1: ack/arm only — no run.
        with _connect(name):
            pass
        assert _wait_armed(pipe, True), "knock 1 never armed the trigger"
        assert backend.calls == [], backend.calls

        # Knock 2 immediately: fires exactly once.
        with _connect(name):
            pass
        assert _wait_for(lambda: len(backend.calls) == 1), backend.calls
        assert _wait_armed(pipe, False), "fire must disarm the trigger"
        time.sleep(0.2)
        assert len(backend.calls) == 1, backend.calls
    finally:
        _stop_and_wait_gone(pipe)


def test_stale_second_knock_expires():
    backend = FakeBackend()
    name = _pipe()
    pipe = TrayPipe(backend, pipe_name=name)
    pipe.attach_icon(object())
    pipe.ARM_WINDOW_SECONDS = 0.4  # shrink for test speed
    ok, _ = pipe.start()
    assert ok
    try:
        with _connect(name):
            pass  # knock 1: arm
        assert _wait_armed(pipe, True), "knock 1 never armed the trigger"
        time.sleep(0.7)  # let the window lapse
        with _connect(name):
            pass  # knock 2 arrives late: re-arms, no run
        assert _wait_armed(pipe, True), "late knock must re-arm"
        assert backend.calls == [], backend.calls
        with _connect(name):
            pass  # knock 3: now armed again, fires
        assert _wait_for(lambda: len(backend.calls) == 1), backend.calls
    finally:
        _stop_and_wait_gone(pipe)


def test_rapid_sequence_fires_each_double_knock():
    backend = FakeBackend()
    name = _pipe()
    pipe = TrayPipe(backend, pipe_name=name)
    pipe.attach_icon(object())
    ok, _ = pipe.start()
    assert ok
    try:
        for _ in range(2):
            with _connect(name):
                pass
            assert _wait_armed(pipe, True), "knock 1 never armed the trigger"
            with _connect(name):
                pass
            assert _wait_for(lambda: len(backend.calls) >= 1), backend.calls
            assert _wait_armed(pipe, False), "fire must disarm the trigger"
        assert len(backend.calls) == 2, backend.calls
    finally:
        _stop_and_wait_gone(pipe)


def test_stop_connection_is_swallowed():
    backend = FakeBackend()
    name = _pipe()
    pipe = TrayPipe(backend, pipe_name=name)
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
    name = _pipe()
    pipe = TrayPipe(backend, pipe_name=name)
    ok, _ = pipe.start()
    assert ok
    try:
        with _connect(name):
            pass
        assert _wait_armed(pipe, True), "knock 1 never armed the trigger"
        with _connect(name):
            pass  # would be a fire, but no icon -> no-op
        assert _wait_armed(pipe, False), "fire must disarm the trigger"
        assert backend.calls == [], backend.calls
    finally:
        _stop_and_wait_gone(pipe)


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
