"""
Smoke tests for the tray REST bridge (ephemeral_ui.tray_api).

Runs without Podman/iroh: the backend is faked, so this exercises the wire
contract (routing, validation, statuses, persistence) exactly as a REST
client sees it. CI can run it directly:

    python test_tray_api.py
"""
import base64
import json
import os
import sys
import tempfile
import urllib.error
import urllib.request
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from ephemeral_ui import tray_api  # noqa: E402


class FakeBackend:
    """Just the two attributes/methods TrayApiServer.execute touches."""

    def __init__(self):
        self.calls = []

    def _ensure_cluster(self):
        pass

    def _run_through_cluster(self, blob, timeout):
        self.calls.append({"blob": blob, "timeout": timeout})
        markdown = base64.b64decode(blob).decode("utf-8")
        if "boom" in markdown:
            raise RuntimeError("podman machine is not running")
        if "slow" in markdown:
            raise RuntimeError("job timed out after 300s")
        if "reject" in markdown:
            raise RuntimeError("no runnable code blocks found in document")
        return {
            "exit_code": 0,
            "stdout": f"ran: {markdown.strip()!r}",
            "stderr": "",
            "artifact_file": None,
            "artifact_ext": None,
        }


def post(port, path, payload=None, raw=None, headers=None):
    url = f"http://127.0.0.1:{port}{path}"
    body = raw if raw is not None else json.dumps(payload or {}).encode()
    if isinstance(body, str):
        body = body.encode()
    req = urllib.request.Request(url, data=body, method="POST",
                                 headers=headers or {"Content-Type": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=10) as resp:
            return resp.status, json.loads(resp.read().decode())
    except urllib.error.HTTPError as e:
        return e.code, json.loads(e.read().decode())


def get(port, path):
    try:
        with urllib.request.urlopen(f"http://127.0.0.1:{port}{path}", timeout=10) as resp:
            return resp.status, json.loads(resp.read().decode())
    except urllib.error.HTTPError as e:
        return e.code, json.loads(e.read().decode())


def main():
    ok = 0

    def check(name, cond):
        nonlocal ok
        assert cond, name
        ok += 1
        print(f"PASS: {name}")

    # Isolated state dir so the test never touches ~/.ephemeral.
    with tempfile.TemporaryDirectory() as state:
        os.environ["EPHEMERAL_STATE_DIR"] = state
        backend = FakeBackend()
        api = tray_api.TrayApi(backend, port=0)  # port 0: OS picks a free one

        started, result = api.start()
        check("server starts on an OS-picked port", started and isinstance(result, int))
        port = result

        # --- health -------------------------------------------------------
        status, payload = get(port, "/ephemeral/api/v1/health")
        check("health answers 200 with the engine tag",
              status == 200 and payload.get("engine") == "distributed-tray")

        # --- happy path -----------------------------------------------------
        blob = base64.b64encode(b"```python\nprint('hello')\n```").decode()
        status, payload = post(port, "/ephemeral/api/v1/run",
                               {"document_blob": blob, "timeout": 30})
        check("run returns RunResponse fields", status == 200 and payload["exit_code"] == 0)
        check("run reaches the backend with a decoded doc",
              "print('hello')" in base64.b64decode(backend.calls[0]["blob"]).decode())
        check("timeout forwarded to the backend", backend.calls[0]["timeout"] == 30)

        # --- validation (mirror of RunRequest semantics) ---------------------
        status, _ = post(port, "/ephemeral/api/v1/run", {"document_blob": "!!!", "timeout": 30})
        check("invalid base64 -> 422", status == 422)
        status, _ = post(port, "/ephemeral/api/v1/run",
                         {"document_blob": base64.b64encode(b"x").decode(), "timeout": 999})
        check("timeout > 600 -> 422", status == 422)
        status, _ = post(port, "/ephemeral/api/v1/run",
                         {"document_blob": base64.b64encode(b"x").decode(), "timeout": 0})
        check("timeout < 1 -> 422", status == 422)
        status, _ = post(port, "/ephemeral/api/v1/run", {"document_blob": blob}, raw=b"{not json")
        check("malformed JSON -> 400", status == 400)
        status, _ = post(port, "/ephemeral/api/v1/run", {"timeout": 30})
        check("missing document_blob -> 422", status == 422)

        # --- backend failure classification ---------------------------------
        status, payload = post(port, "/ephemeral/api/v1/run",
                               {"document_blob": base64.b64encode(b"boom```").decode(), "timeout": 30})
        check("infrastructure failure -> 500 + detail",
              status == 500 and "podman" in payload["detail"])
        status, _ = post(port, "/ephemeral/api/v1/run",
                         {"document_blob": base64.b64encode(b"slow```").decode(), "timeout": 30})
        check("execution timeout -> 504", status == 504)
        status, _ = post(port, "/ephemeral/api/v1/run",
                         {"document_blob": base64.b64encode(b"reject```").decode(), "timeout": 30})
        check("bad document -> 422", status == 422)

        # --- routing --------------------------------------------------------
        status, _ = get(port, "/nope")
        check("unknown GET -> 404", status == 404)
        status, _ = post(port, "/ephemeral/api/v1/other", {"document_blob": blob})
        check("unknown POST -> 404", status == 404)

        # --- persistence ------------------------------------------------------
        api.stop()
        check("stop releases the server", not api.is_running())
        tray_api.persist_enabled(True)
        check("marker persists the toggle", tray_api.autostart_enabled())
        tray_api.persist_enabled(False)
        check("marker clears the toggle", not tray_api.autostart_enabled())
        os.environ["EPHEMERAL_TRAY_API"] = "1"
        check("env override forces autostart", tray_api.autostart_enabled())
        del os.environ["EPHEMERAL_TRAY_API"]

        # --- restart on the same port (allow_reuse_address) ------------------
        api2 = tray_api.TrayApi(backend, port=port)
        started, result = api2.start()
        check("rebind on the same port works", started and result == port)
        status, payload = get(port, "/ephemeral/api/v1/health")
        check("rebound server answers health", status == 200)
        api2.stop()

    print(f"\n=== ALL TRAY API TESTS PASSED ({ok}) ===")


if __name__ == "__main__":
    main()
