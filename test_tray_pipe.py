"""
Tests for the tray named-pipe bridge (ephemeral_ui.tray_pipe).

Covers the wire contract (framing, token gate, validation, statuses)
exactly as a local client sees it, plus persistence and a live pipe
round-trip against a fake backend (no Podman/iroh needed). CI can run it
directly:

    python test_tray_pipe.py
"""
import base64
import json
import os
import struct
import sys
import tempfile
import time
import threading
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

# Hermetic state dir must exist before ephemeral_net.swarm is consulted.
_STATE = tempfile.mkdtemp(prefix="eph-tray-pipe-test-")
os.environ["EPHEMERAL_STATE_DIR"] = _STATE
_PIPE = f"\\\\.\\pipe\\ephemeral-run-test-{os.getpid()}"

from ephemeral_ui import tray_pipe  # noqa: E402
from ephemeral_ui.tray_pipe import (  # noqa: E402
    TrayPipe,
    _decode_document_blob,
    _validate_timeout,
    _error_status,
    autostart_enabled,
    persist_enabled,
)


class FakeBackend:
    """Stands in for DistributedBackend: no cluster, instant runs."""

    def __init__(self):
        self.runs = []

    def _ensure_cluster(self):
        pass

    def _run_through_cluster(self, blob, timeout=60):
        self.runs.append((blob, timeout))
        document = base64.b64decode(blob).decode("utf-8")
        return {
            "exit_code": 0,
            "stdout": f"ran: {document.strip()!r}",
            "stderr": "",
            "artifact_file": None,
            "artifact_ext": None,
        }


def _request(token=None, **fields):
    body = {}
    if token is not None:
        body["token"] = token
    body.update(fields)
    return json.dumps(body).encode("utf-8")


def _exchange(body: bytes, magic=b"EPHR"):
    client = None
    deadline = time.time() + 3
    while client is None:
        try:
            client = open(_PIPE, "r+b", buffering=0)
        except OSError:
            # Transient: no server instance between connections, or the
            # previous one is still being torn down (MSDN says retry).
            if time.time() >= deadline:
                raise
            time.sleep(0.05)
    with client:
        client.write(magic + struct.pack("<I", len(body)) + body)
        header = _read_some(client, 4)
        (length,) = struct.unpack("<I", header)
        return json.loads(_read_some(client, length))


def _read_some(client, count):
    data = b""
    while len(data) < count:
        chunk = client.read(count - len(data))
        if not chunk:
            break
        data += chunk
    return data


def _token() -> str:
    return (Path(_STATE) / "tray_pipe.token").read_text(encoding="utf-8")


# --- pure helpers ------------------------------------------------------------

def test_decode_document_blob():
    assert _decode_document_blob(base64.b64encode("# hi".encode()).decode()) == "# hi"
    for bad in ("not-base64!!!", "aGVsbG8", None, 42, base64.b64encode(b"\xff\xfe").decode()):
        try:
            _decode_document_blob(bad)
        except ValueError:
            pass
        else:
            assert False, f"expected ValueError for {bad!r}"


def test_validate_timeout():
    assert _validate_timeout(30) == 30
    assert _validate_timeout(1) == 1
    assert _validate_timeout(600) == 600
    for bad in (0, 601, "30", 3.5, None, True):
        try:
            _validate_timeout(bad)
        except ValueError:
            pass
        else:
            assert False, f"expected ValueError for {bad!r}"


def test_error_status_classification():
    assert _error_status(RuntimeError("Execution timed out after 60s")) == 504
    assert _error_status(RuntimeError("failed starting the cluster node")) == 500
    assert _error_status(RuntimeError("podman machine is not running")) == 500
    assert _error_status(RuntimeError("no executable code blocks found")) == 422


# --- persistence ---------------------------------------------------------------

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


# --- live pipe round-trip -------------------------------------------------------

def test_live_pipe_round_trip():
    backend = FakeBackend()
    pipe = TrayPipe(backend, pipe_name=_PIPE)
    ok, result = pipe.start()
    assert ok, result
    assert result == _PIPE
    try:
        # Wait for the first server instance to exist.
        deadline = time.time() + 3
        last_error = None
        while time.time() < deadline:
            try:
                response = _exchange(_request(_token(), op="health"))
                break
            except OSError as e:
                last_error = e
                time.sleep(0.1)
        else:
            raise AssertionError(f"pipe never became connectable: {last_error}")
        assert response["status"] == 200
        assert response["engine"] == "distributed-tray"

        blob = base64.b64encode("```python\nprint('hi')\n```".encode()).decode()
        response = _exchange(_request(_token(), op="run", document_blob=blob, timeout=30))
        assert response["status"] == 200, response
        assert response["exit_code"] == 0
        assert "print('hi')" in response["stdout"]
        assert response["artifact_file"] is None
        assert len(backend.runs) == 1
        assert backend.runs[0][1] == 30  # timeout forwarded

        # Token gate: right frame, wrong token -> 403.
        response = _exchange(_request("deadbeef", op="run", document_blob=blob, timeout=30))
        assert response["status"] == 403 and "token" in response["detail"]

        # Malformed magic -> 422.
        response = _exchange(_request(_token()), magic=b"XXXX")
        assert response["status"] == 422 and "malformed" in response["detail"]

        # Contract violations mirror the REST statuses.
        response = _exchange(_request(_token(), op="run", document_blob="!!!", timeout=30))
        assert response["status"] == 422
        response = _exchange(_request(_token(), op="run", document_blob=blob, timeout=9999))
        assert response["status"] == 422

        # Backend failures classify like the REST bridge (504/500/422 shapes).
        def boom(_blob, timeout=60):
            raise RuntimeError("Execution timed out after 60s")

        backend._run_through_cluster = boom
        response = _exchange(_request(_token(), op="run", document_blob=blob, timeout=30))
        assert response["status"] == 504 and "timed out" in response["detail"]
    finally:
        pipe.stop()
        # Give the server thread a moment to notice the stop.
        for _ in range(20):
            if not pipe.is_running():
                break
            time.sleep(0.1)


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
    print(f"\n{'ALL PIPE TESTS PASSED' if failures == 0 else f'{failures} FAILURES'}"
          f" ({len(tests)} tests)")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
