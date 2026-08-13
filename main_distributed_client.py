"""
Ephemeral Distributed Client — Windows tray application (``ephemeral-distributed.exe``).

A portable Windows tray utility packaged with the ``iroh`` Python extension.

Hybrid node model (one identity per machine): when the always-on background
node (``--service``, installed from the tray menu) is running, the tray is a
thin client — it submits jobs to that node over localhost and starts no node
of its own, so a machine contributes a single identity to the swarm. When no
service is reachable, the tray falls back to running its own compute node
exactly like the original client (no admin, no service required). Either way
it runs clipboard-driven code with intelligent nearest-neighbor offloading:

* jobs execute through the node's sandboxed executor (image allowlist,
  ``unsafe`` stripped, hard container limits) — locally when the image is
  warm, or forwarded to the nearest node that has it while the image pulls
  in the background;
* the same node accepts jobs from other cluster members while idle;
* artifacts produced on this node are routed to Downloads exactly like the
  local client; remote artifacts are reported by name.

Cluster configuration (environment variables):

    EPHEMERAL_RELAY          "n0" (default) | "minimal" | "disabled"
    EPHEMERAL_SEED_NODES     comma-separated node_id[@relay] to bootstrap from;
                             unset joins the default swarm by node id
                             (see ephemeral_net.swarm) — iroh-native, no tickets
    EPHEMERAL_SEEDS          comma-separated EndpointTickets (private networks /
                             backward compat; overrides SEED_NODES when set)
    EPHEMERAL_SECRET         hex-encoded 32-byte secret for a persistent node id;
                             unset, a stable identity is auto-persisted to disk
    EPHEMERAL_ALLOW_NETWORK  "1" to let remote jobs use network access (default "0")
    EPHEMERAL_PRIVATE        "1" (or ``--private``) — skip the public swarm list;
                             this node is its own seed for a private cluster
                             (also toggled live via the tray's "Private Mode" item)

Usage:
    python main_distributed_client.py                 # Tray mode
    python main_distributed_client.py --private       # Tray mode, private swarm
    python main_distributed_client.py script.md       # One-shot mode
    python main_distributed_client.py --cli script.md # Headless CLI mode
    python main_distributed_client.py --self-check    # Print node id and exit
    python main_distributed_client.py --service       # Always-on node (background service)
    python main_distributed_client.py --install-service   # (elevated) register the service
    python main_distributed_client.py --uninstall-service # (elevated) remove the service

Localhost control API (in ``--service`` mode): the background node listens
on ``127.0.0.1:8788`` (override ``EPHEMERAL_SERVICE_PORT``) exposing
``GET /health``, ``POST /ephemeral/api/v1/run``, and ``GET /artifact`` so
any local user's tray can drive it without running its own node.
"""
from __future__ import annotations

import asyncio
import base64
import ctypes
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
import logging
import os
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
import threading
import time
import urllib.error
import urllib.parse
import urllib.request

# GUI deps are optional (CLI/self-check work without them).
try:
    import pyperclip
    import keyboard
    import pystray
    from pystray import MenuItem as item
    HAS_GUI = True
except Exception:
    # pystray's Xorg backend raises Xlib.error.DisplayNameError (not
    # ImportError) when no display is available — treat any GUI import
    # failure as headless so the app can fall back to CLI mode.
    HAS_GUI = False

# Windows-only — kept out of the GUI import chain so Linux builds stay
# GUI-capable (winreg does not exist on Linux).
try:
    import winreg
    HAS_WINREG = True
except ImportError:
    HAS_WINREG = False

import ephemeral_core
from ephemeral_net.jobs import JobDoneEvent, JobErrorEvent, JobRequest
from ephemeral_net.swarm import (
    PRIVATE_MODE_MARKER,
    default_state_dir,
    load_or_create_secret,
    parse_private_seed,
    parse_seed_nodes,
    parse_seeds,
    private_mode_enabled,
    private_student_url,
    read_private_seed,
    write_private_seed,
)

# Reuse the local client's platform plumbing (icon, clipboard, language
# prompt, artifact routing) so behavior stays identical between tiers.
from main_local import (
    create_icon_image,
    get_clipboard,
    prompt_user_for_language,
    route_artifacts_local,
    show_post_mortem_error,
    on_convert_hotkey,
    get_startupinfo,
)

# --- Configuration -------------------------------------------------------

HOTKEY = 'ctrl+alt+x'
CONVERT_HOTKEY = 'ctrl+win+x'
CLI_MODE = False

EPHEMERAL_RELAY = os.getenv("EPHEMERAL_RELAY", "n0")
EPHEMERAL_SEED_NODES = parse_seed_nodes(os.getenv("EPHEMERAL_SEED_NODES"))
EPHEMERAL_SEEDS = parse_seeds(os.getenv("EPHEMERAL_SEEDS"))
if EPHEMERAL_SEEDS:
    # Explicit tickets (private network) replace the default swarm nodes.
    EPHEMERAL_SEED_NODES = []
_hex_secret = os.getenv("EPHEMERAL_SECRET", "")
# Lazy: the tray only materializes an identity when it actually starts its
# own node (standalone fallback / service). Thin-client mode never touches
# the secret, so no second identity is ever created on disk.
EPHEMERAL_SECRET: bytes | None = (
    bytes.fromhex(_hex_secret) if _hex_secret else None
)
EPHEMERAL_ALLOW_NETWORK = os.getenv("EPHEMERAL_ALLOW_NETWORK", "0") == "1"

# Private mode (skip the public swarm list) is decided at bootstrap time via
# ``private_mode_enabled``: ``--private`` / ``EPHEMERAL_PRIVATE=1``, or a
# ``private_mode`` marker file in the node's state dir (toggled live from the
# tray's "Private Mode" menu item).

# Localhost port the background node's control API listens on, so
# thin-client trays from any local user drive this node instead of
# starting their own (one identity per machine).
SERVICE_PORT = int(os.getenv("EPHEMERAL_SERVICE_PORT", "8788"))
SERVICE_URL = f"http://127.0.0.1:{SERVICE_PORT}"


# --- Cluster lifecycle (dedicated event loop thread) ---------------------

class Cluster:
    """
    Owns the iroh ``Node`` on a dedicated asyncio loop so it can accept
    remote jobs and submit local ones from any thread.
    """

    def __init__(self) -> None:
        self.loop: asyncio.AbstractEventLoop | None = None
        self.node = None
        self._thread: threading.Thread | None = None
        self._start_error: Exception | None = None

    # --- lifecycle -------------------------------------------------------

    def start(self) -> None:
        self.loop = asyncio.new_event_loop()
        self._thread = threading.Thread(target=self._run_loop, name="ephemeral-cluster", daemon=True)
        self._thread.start()
        # Wait until the node is bound (or failed) before returning.
        for _ in range(200):
            if self.node is not None or self._start_error is not None:
                return
            time.sleep(0.05)
        raise RuntimeError("timed out starting the cluster node")

    def _run_loop(self) -> None:
        asyncio.set_event_loop(self.loop)
        try:
            self.loop.run_until_complete(self._start_node())
        except Exception as e:  # pragma: no cover - surfaced via start()
            self._start_error = e
            return
        self.loop.run_forever()

    async def _start_node(self) -> None:
        from ephemeral_net.fanout import FanoutExecutor
        from ephemeral_net.node import Node
        from ephemeral_net.offload import OffloadingExecutor
        from ephemeral_net.sandbox import CoreJobExecutor

        node = Node(
            secret_key=EPHEMERAL_SECRET or load_or_create_secret(),
            relay=EPHEMERAL_RELAY,
        )
        local = CoreJobExecutor(
            allow_network=EPHEMERAL_ALLOW_NETWORK,
            image_allowlist=None,
        )
        # Fan-out splits multi-run documents across idle warm peers; the
        # offloading stack underneath handles warmest-neighbor routing,
        # background pulls, and local execution.
        node.executor = FanoutExecutor(node, OffloadingExecutor(node, local))
        await node.start()
        if EPHEMERAL_SEED_NODES:
            await node.bootstrap_nodes(EPHEMERAL_SEED_NODES)
        elif EPHEMERAL_SEEDS:
            await node.bootstrap(EPHEMERAL_SEEDS)
        elif private_mode_enabled(argv=sys.argv):
            # Private mode: join an existing swarm when a seed is persisted,
            # otherwise this node is its own seed (a NEW swarm) — dial
            # nothing, just accept incoming connections.
            seeds, seed_nodes = parse_private_seed(read_private_seed() or "")
            if seed_nodes:
                await node.bootstrap_nodes(seed_nodes)
            elif seeds:
                await node.bootstrap(seeds)
        else:
            # No compiled-in seeds: join the public swarm via the live
            # bootstrap list (docs/swarm.json) — fully automatic.
            await node.bootstrap_from_list()
        self.node = node

    def stop(self) -> None:
        if self.loop is None or self.node is None:
            return
        node = self.node
        self.node = None

        async def _close():
            await node.close()

        fut = asyncio.run_coroutine_threadsafe(_close(), self.loop)
        try:
            fut.result(timeout=10)
        except Exception:
            pass
        self.loop.call_soon_threadsafe(self.loop.stop)

    # --- job submission --------------------------------------------------

    def submit(self, request: JobRequest) -> list:
        """Run a job through the node executor and return its events."""
        if self.loop is None or self.node is None:
            raise RuntimeError("cluster not started")

        async def _consume():
            events = []
            async for event in self.node.executor(request):
                events.append(event)
            return events

        fut = asyncio.run_coroutine_threadsafe(_consume(), self.loop)
        return fut.result(timeout=request.timeout + 60)

    # --- status ----------------------------------------------------------

    def info(self) -> str:
        if self.node is None:
            return "Cluster: starting..."
        lines = [f"Node:     {self.node.node_id()}"]
        try:
            peers = len(self.node.table)
            lines.append(f"Peers:    {peers}")
        except Exception:
            pass
        try:
            warm = self.node.warm_images()
            lines.append(f"Warm images: {len(warm)}")
        except Exception:
            pass
        return "\n".join(lines)


cluster = Cluster()


# --- Core execution bridge ----------------------------------------------

def rebuild_markdown(blocks: list[dict]) -> str:
    """Re-serialize parsed blocks back to Markdown (after header fixes)."""
    parts = []
    for b in blocks:
        if b["type"] == "seed":
            header = f"{b['name']} b64" if b.get("is_b64") else b["name"]
            parts.append(f"```{header}\n{b['content']}\n```")
        else:
            header = b.get("header") or ""
            parts.append(f"```{header}\n{b['content']}\n```")
    return "\n".join(parts)


# --- Hybrid execution: background service preferred, own node fallback ----
# One identity per machine: when the always-on background node answers, the
# tray is a thin client driving it over localhost. Otherwise the tray runs
# its own node (standalone) exactly like the original client.

service_ok = False  # True when the installed background node answers /health


def run_through_cluster(blob: str, timeout: int) -> dict:
    """Run a base64 markdown document through the local cluster node.

    Returns a RunResponse dict (``exit_code``/``stdout``/``stderr``/
    ``artifact_file``/``artifact_ext``); ``artifact_path`` is additionally
    set in standalone mode, where the produced file is on this machine.
    Raises RuntimeError on job rejection or a missing result.
    """
    request = JobRequest(
        job_id=f"local-{int(time.time() * 1000)}",
        document_blob=blob,
        timeout=timeout,
    )
    events = cluster.submit(request)
    errors = [e for e in events if isinstance(e, JobErrorEvent)]
    if errors:
        raise RuntimeError(errors[0].message)
    dones = [e for e in events if isinstance(e, JobDoneEvent)]
    if not dones:
        raise RuntimeError("job ended without a result")
    done = dones[-1]
    result = {
        "exit_code": done.exit_code,
        "stdout": done.stdout or "",
        "stderr": done.stderr or "",
        "artifact_file": done.artifact_file,
        "artifact_ext": (
            os.path.splitext(done.artifact_file or "")[1].lstrip(".") or None
        ),
    }
    if done.artifact_path and os.path.isfile(done.artifact_path):
        result["artifact_path"] = done.artifact_path
    return result


def _ensure_cluster() -> None:
    """Start the tray's own node (standalone fallback), if not already up."""
    if cluster.node is None:
        cluster.start()


def service_available() -> bool:
    """True when the installed background node's localhost API answers."""
    try:
        with urllib.request.urlopen(SERVICE_URL + "/health", timeout=2) as res:
            return res.status == 200
    except Exception:
        return False


def fetch_service_status() -> dict:
    """Health/identity snapshot from the background node."""
    with urllib.request.urlopen(SERVICE_URL + "/health", timeout=3) as res:
        return json.loads(res.read().decode("utf-8"))


def submit_via_service(blob: str, timeout: int = 300) -> dict:
    """POST a job to the background node; returns its RunResponse dict."""
    body = json.dumps({"document_blob": blob, "timeout": timeout}).encode("utf-8")
    req = urllib.request.Request(
        SERVICE_URL + "/ephemeral/api/v1/run",
        data=body,
        headers={"Content-Type": "application/json"},
        method="POST",
    )
    try:
        with urllib.request.urlopen(req, timeout=timeout + 60) as res:
            return json.loads(res.read().decode("utf-8"))
    except urllib.error.HTTPError as e:
        try:
            detail = json.loads(e.read().decode("utf-8")).get("detail", str(e))
        except Exception:
            detail = str(e)
        raise RuntimeError(detail) from e


def download_artifact(name: str) -> Path:
    """Fetch an artifact produced by the background node into a temp file."""
    dest_dir = Path(tempfile.gettempdir()) / "ephemeral-service-artifacts"
    dest_dir.mkdir(parents=True, exist_ok=True)
    dest = dest_dir / os.path.basename(name)
    url = f"{SERVICE_URL}/artifact?name={urllib.parse.quote(os.path.basename(name))}"
    with urllib.request.urlopen(url, timeout=120) as res, open(dest, "wb") as f:
        shutil.copyfileobj(res, f)
    return dest


# --- Localhost control API (runs inside --service; stdlib only) -----------
# Kept dependency-free on purpose: the distributed EXE must not grow a
# FastAPI/uvicorn dependency just to serve its own tray.

SERVICE_ARTIFACTS_DIR: Path | None = None


def service_health() -> dict | None:
    """Server-side health snapshot; None while the node is still starting."""
    if cluster.node is None:
        return None
    info = {"status": "ok", "node_id": cluster.node.node_id()}
    try:
        info["ticket"] = cluster.node.ticket()
    except Exception:
        pass
    info["private"] = private_mode_enabled(argv=sys.argv)
    try:
        info["peers"] = len(cluster.node.table)
    except Exception:
        pass
    try:
        info["warm_images"] = sorted(cluster.node.warm_images() or [])
    except Exception:
        pass
    return info


def _student_url() -> str | None:
    """The student-ready #seed= link for the current node (None if starting)."""
    if cluster.node is None:
        return None
    try:
        return private_student_url(cluster.node.ticket())
    except Exception:
        return None


def current_student_url() -> str | None:
    """Student link for the node actually serving — service or own node."""
    if service_ok:
        try:
            ticket = fetch_service_status().get("ticket")
            return private_student_url(ticket) if ticket else None
        except Exception:
            return None
    return _student_url()


def _apply_private_mode(enabled: bool) -> None:
    """Persist private mode for the current process's node (marker file)."""
    state_dir = default_state_dir()
    marker = state_dir / PRIVATE_MODE_MARKER
    if enabled:
        marker.parent.mkdir(parents=True, exist_ok=True)
        marker.touch()
    else:
        marker.unlink(missing_ok=True)


def prompt_user_for_seed(current_seed: str = "") -> str | None:
    """Ask for a seed ticket (or ``node_id@relay``) to join a private swarm.

    Empty input means "create a new swarm" (self-seed). Returns ``None``
    when the prompt could not be shown. Windows uses a cmd.exe console;
    Linux uses zenity -> kdialog -> tkinter -> stdin.
    """
    prompt = (
        "Paste the swarm's seed ticket (or node_id@relay) to JOIN it,\n"
        "or leave empty to CREATE a new private swarm."
    )
    if current_seed:
        prompt += f"\n\nCurrent seed: {current_seed}"
    if sys.platform != "win32":
        return _prompt_user_for_seed_linux(prompt)
    fd_out, path_out = tempfile.mkstemp(suffix=".txt")
    os.close(fd_out)
    fd_bat, path_bat = tempfile.mkstemp(suffix=".bat")
    os.close(fd_bat)
    try:
        with open(path_bat, "w") as f:
            f.write("@echo off\n")
            f.write("title Ephemeral: Private swarm\n")
            f.write("cls\n")
            f.write("echo.\n")
            for line in prompt.split("\n"):
                f.write(f"echo  {line}\n")
            f.write("echo.\n")
            f.write('set /p "seed= Seed (empty = create new): "\n')
            f.write(f'echo %seed%> "{path_out}"\n')
        subprocess.run(path_bat, creationflags=getattr(subprocess, "CREATE_NEW_CONSOLE", 0))
        if os.path.exists(path_out):
            with open(path_out, "r") as f:
                return f.read().strip() or ""
    except Exception as e:
        print(f"Input error: {e}")
        return None
    finally:
        if os.path.exists(path_out):
            os.remove(path_out)
        if os.path.exists(path_bat):
            os.remove(path_bat)
    return None


def _prompt_user_for_seed_linux(prompt: str) -> str | None:
    try:
        if shutil.which("zenity"):
            out = subprocess.run(
                ["zenity", "--entry", "--title=Ephemeral", "--text=" + prompt],
                capture_output=True, text=True, timeout=120,
            )
            if out.returncode == 0:
                return out.stdout.strip()
        if shutil.which("kdialog"):
            out = subprocess.run(
                ["kdialog", "--inputbox", prompt, ""],
                capture_output=True, text=True, timeout=120,
            )
            if out.returncode == 0:
                return out.stdout.strip()
        try:
            import tkinter as _tk
            from tkinter import simpledialog
            root = _tk.Tk()
            root.withdraw()
            val = simpledialog.askstring("Ephemeral", prompt)
            root.destroy()
            return (val or "").strip()
        except Exception:
            pass
        return input(prompt + " ").strip()
    except Exception as e:
        print(f"Input error: {e}")
        return None


class _ServiceHandler(BaseHTTPRequestHandler):
    """Minimal stdlib HTTP API for thin-client trays."""

    server_version = "Ephemeral-Service/1.0"

    def log_message(self, fmt, *args):  # keep tray consoles quiet
        pass

    def _send_json(self, code: int, obj: dict) -> None:
        body = json.dumps(obj).encode("utf-8")
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self) -> None:
        parsed = urllib.parse.urlparse(self.path)
        if parsed.path == "/health":
            info = service_health()
            self._send_json(200 if info else 503, info or {"status": "starting"})
            return
        if parsed.path == "/artifact":
            name = (urllib.parse.parse_qs(parsed.query).get("name") or [""])[0]
            path = (SERVICE_ARTIFACTS_DIR or Path()) / os.path.basename(name)
            if not name or not path.is_file():
                self._send_json(404, {"detail": "artifact not found"})
                return
            body = path.read_bytes()
            self.send_response(200)
            self.send_header("Content-Type", "application/octet-stream")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)
            return
        self._send_json(404, {"detail": "not found"})

    def do_POST(self) -> None:
        parsed = urllib.parse.urlparse(self.path)
        if parsed.path == "/private":
            try:
                length = int(self.headers.get("Content-Length", "0"))
                payload = json.loads(self.rfile.read(length).decode("utf-8"))
                enabled = bool(payload.get("enabled", False))
                seed = payload.get("seed")
            except Exception as e:
                self._send_json(400, {"detail": f"bad request: {e}"})
                return
            try:
                _apply_private_mode(enabled)
                # Join an existing swarm (non-empty seed), or clear the
                # seed when leaving private mode entirely.
                if enabled:
                    write_private_seed((seed or "").strip() or None)
                else:
                    write_private_seed(None)
                cluster.stop()
                cluster.start()
            except Exception as e:
                self._send_json(500, {"detail": f"toggle failed: {e}"})
                return
            self._send_json(200, {"enabled": enabled, "student_url": _student_url()})
            return
        if parsed.path != "/ephemeral/api/v1/run":
            self._send_json(404, {"detail": "not found"})
            return
        try:
            length = int(self.headers.get("Content-Length", "0"))
            payload = json.loads(self.rfile.read(length).decode("utf-8"))
            blob = payload.get("document_blob") or ""
            timeout = int(payload.get("timeout") or 300)
        except Exception as e:
            self._send_json(400, {"detail": f"bad request: {e}"})
            return
        try:
            result = run_through_cluster(blob, timeout)
        except RuntimeError as e:
            self._send_json(422, {"detail": str(e)})
            return
        except Exception as e:
            self._send_json(500, {"detail": f"unexpected error: {e}"})
            return
        # Copy the artifact into the user-profile dir so any local tray
        # (any user on this machine) can fetch it over HTTP.
        if result.get("artifact_path") and os.path.isfile(result["artifact_path"]):
            name = os.path.basename(result["artifact_path"])
            try:
                shutil.copy2(
                    result["artifact_path"], (SERVICE_ARTIFACTS_DIR or Path()) / name
                )
                result["artifact_file"] = name
            except Exception:
                pass
        result.pop("artifact_path", None)
        self._send_json(200, result)


def start_local_service_api(state_dir: Path) -> None:
    """Expose the background node over localhost for thin-client trays."""
    global SERVICE_ARTIFACTS_DIR
    SERVICE_ARTIFACTS_DIR = state_dir / "artifacts"
    SERVICE_ARTIFACTS_DIR.mkdir(parents=True, exist_ok=True)
    server = ThreadingHTTPServer(("127.0.0.1", SERVICE_PORT), _ServiceHandler)
    threading.Thread(
        target=server.serve_forever, daemon=True, name="ephemeral-local-api"
    ).start()
    logging.getLogger("ephemeral").info(
        "local API listening on http://127.0.0.1:%d", SERVICE_PORT
    )


def run_logic(icon, content=None):
    """Clipboard-driven execution through the cluster node."""
    if content is None:
        content = get_clipboard()

    # Safety check: reject previous Ephemeral output (same as local client).
    if (re.search(r"^## (Run|Result) .*[\r\n]+```text", content.strip(), re.MULTILINE)
            or re.search(r"^Result \(.*\):[\r\n]+---[\r\n]+", content.strip(), re.MULTILINE)
            or re.search(r"^--- Run \d+ \(.*\) ---\n```text", content.strip(), re.MULTILINE)):
        icon.notify("Clipboard contains previous results. Execution halted.", title="Ephemeral Safety")
        return

    blocks = ephemeral_core.parse_codeblocks(content)
    if not blocks:
        icon.notify("Clipboard is empty.", title="Ephemeral Error")
        return

    # Untagged single block: prompt for a language (local behavior).
    if len(blocks) == 1 and blocks[0]['type'] == 'code' and not blocks[0]['header']:
        from ephemeral_core.parser import strip_shebang, resolve_runtime_config
        code = strip_shebang(blocks[0]['content'])
        code = re.sub(r"```+\s*$", "", code.rstrip())
        user_input = prompt_user_for_language("python", code)
        if user_input:
            blocks[0]['header'] = user_input.strip()
            blocks[0]['config'] = resolve_runtime_config(blocks[0]['header'])
        else:
            icon.notify("Execution cancelled.", title="Ephemeral")
            return

    code_blocks = [b for b in blocks if b['type'] == 'code']
    if not code_blocks:
        icon.notify("Clipboard only contains seed files.", title="Ephemeral Error")
        return

    global service_ok

    markdown = rebuild_markdown(blocks)
    blob = base64.b64encode(markdown.encode("utf-8")).decode("ascii")

    if not service_ok:
        try:
            _ensure_cluster()
        except Exception as e:
            show_post_mortem_error(f"Cluster execution error:\n{e}")
            icon.notify("Cluster execution failed.", title="Ephemeral Failed")
            return

    set_icon_animation_state(icon, True)
    result = None
    try:
        if service_ok:
            result = submit_via_service(blob, timeout=300)
        else:
            result = run_through_cluster(blob, timeout=300)
    except Exception as e:
        # The service may have gone down mid-run: fall back to the tray's
        # own node once before surfacing the error.
        if service_ok:
            service_ok = False
            try:
                _ensure_cluster()
                result = run_through_cluster(blob, timeout=300)
            except Exception as e2:
                show_post_mortem_error(f"Cluster execution error:\n{e2}")
                icon.notify("Cluster execution failed.", title="Ephemeral Failed")
                return
        else:
            show_post_mortem_error(f"Cluster execution error:\n{e}")
            icon.notify("Cluster execution failed.", title="Ephemeral Failed")
            return
    finally:
        set_icon_animation_state(icon, False)

    artifact_local = result.get("artifact_path")
    if not artifact_local and result.get("artifact_file"):
        # Artifact produced on the background node — fetch it over localhost.
        try:
            artifact_local = str(download_artifact(result["artifact_file"]))
        except Exception as e:
            icon.notify(f"Artifact download failed: {e}", title="Ephemeral Error")

    if artifact_local:
        routed = ephemeral_core.ExecutionResult(
            stdout=result.get("stdout", ""),
            stderr=result.get("stderr", ""),
            exit_code=result.get("exit_code", 0),
            artifact_paths=[artifact_local],
            artifact_dir=os.path.dirname(artifact_local),
        )
        route_artifacts_local(routed, "distributed", icon)

    if result.get("exit_code") != 0:
        show_post_mortem_error(
            result.get("stderr") or f"Exit code {result.get('exit_code')}"
        )
        icon.notify("Execution Failed. Debug window opened.", title="Ephemeral Error")
        return

    stdout = result.get("stdout") or ""
    if stdout:
        if CLI_MODE:
            print(stdout)
        else:
            pyperclip.copy(stdout)
        icon.notify("Execution Finished. Results copied.", title="Ephemeral")


def set_icon_animation_state(icon, state):
    if not HAS_GUI or not icon:
        return
    if state:
        icon.icon = create_icon_image((255, 100, 0))
    else:
        icon.icon = create_icon_image((0, 120, 215))


# --- Startup & maintenance (parity with the local tray client) -----------

def get_install_path():
    app_data = os.getenv('LOCALAPPDATA', os.path.expanduser('~'))
    install_dir = os.path.join(app_data, 'Ephemeral-Distributed')
    is_frozen = getattr(sys, 'frozen', False)
    ext = '.exe' if is_frozen else '.py'
    return os.path.join(install_dir, f'Ephemeral-Distributed{ext}')


def _autostart_desktop_path():
    return os.path.join(os.path.expanduser("~"), ".config", "autostart", "ephemeral-distributed.desktop")


def _set_startup_linux(enable, icon=None):
    """Enable/disable login autostart via a freedesktop .desktop entry."""
    path = _autostart_desktop_path()
    try:
        if enable:
            os.makedirs(os.path.dirname(path), exist_ok=True)
            exe = os.path.realpath(
                sys.executable if getattr(sys, 'frozen', False) else os.path.abspath(__file__)
            )
            with open(path, "w", encoding="utf-8") as f:
                f.write("[Desktop Entry]\n")
                f.write("Type=Application\n")
                f.write("Name=Ephemeral-Distributed\n")
                f.write(f"Exec={shlex.quote(exe)}\n")
                f.write("X-GNOME-Autostart-enabled=true\n")
            if icon:
                icon.notify(f"Set to run on login: {path}", title="Ephemeral Setup")
        else:
            if os.path.exists(path):
                os.remove(path)
            if icon:
                icon.notify("Disabled start on login.", title="Ephemeral Setup")
    except Exception as e:
        print(f"Failed to set startup: {e}")
        if icon:
            icon.notify(f"Failed to configure startup: {e}", title="Ephemeral Error")


def set_startup(enable, icon=None):
    """Toggle login autostart (registry Run key on Windows, .desktop on Linux)."""
    if sys.platform != 'win32':
        return _set_startup_linux(enable, icon)
    app_path = sys.executable if getattr(sys, 'frozen', False) else os.path.abspath(__file__)
    install_path = get_install_path()

    try:
        key = winreg.OpenKey(winreg.HKEY_CURRENT_USER,
                             r"Software\Microsoft\Windows\CurrentVersion\Run", 0, winreg.KEY_ALL_ACCESS)
        if enable:
            if os.path.abspath(app_path) != os.path.abspath(install_path):
                os.makedirs(os.path.dirname(install_path), exist_ok=True)
                shutil.copy2(app_path, install_path)

            winreg.SetValueEx(key, "Ephemeral-Distributed", 0, winreg.REG_SZ, f'"{install_path}"')
            if icon:
                icon.notify(f"Installed to and set to run on boot from:\n{install_path}", title="Ephemeral Setup")
        else:
            try:
                winreg.DeleteValue(key, "Ephemeral-Distributed")
            except FileNotFoundError:
                pass

            if os.path.exists(install_path):
                if os.path.abspath(app_path) != os.path.abspath(install_path):
                    try:
                        os.remove(install_path)
                        if icon:
                            icon.notify("Removed installed copy and disabled start on boot.", title="Ephemeral Setup")
                    except Exception:
                        MOVEFILE_DELAY_UNTIL_REBOOT = 4
                        ctypes.windll.kernel32.MoveFileExW(install_path, None, MOVEFILE_DELAY_UNTIL_REBOOT)
                        if icon:
                            icon.notify("Disabled start on boot. File will be deleted on next restart.",
                                        title="Ephemeral Setup")
                else:
                    MOVEFILE_DELAY_UNTIL_REBOOT = 4
                    ctypes.windll.kernel32.MoveFileExW(install_path, None, MOVEFILE_DELAY_UNTIL_REBOOT)
                    if icon:
                        icon.notify("Disabled start on boot. It will be deleted on next restart.",
                                    title="Ephemeral Setup")

        winreg.CloseKey(key)
    except Exception as e:
        print(f"Failed to set startup: {e}")
        if icon:
            icon.notify(f"Failed to configure startup: {e}", title="Ephemeral Error")


def check_startup():
    if sys.platform != 'win32':
        return os.path.exists(_autostart_desktop_path())
    try:
        key = winreg.OpenKey(winreg.HKEY_CURRENT_USER,
                             r"Software\Microsoft\Windows\CurrentVersion\Run", 0, winreg.KEY_READ)
        winreg.QueryValueEx(key, "Ephemeral-Distributed")
        winreg.CloseKey(key)
        return True
    except FileNotFoundError:
        return False


def toggle_startup(icon, item_unused):
    is_enabled = check_startup()
    set_startup(not is_enabled, icon)


# --- Always-on background node (Windows service) -------------------------
# Installs the back-end (cluster node) as a boot-time scheduled task running
# as SYSTEM, so the node stays in the swarm even while the user is logged
# off. The tray app keeps acting as the front end when logged in.

SERVICE_TASK_NAME = "Ephemeral-Distributed Node"


def _service_state_dir() -> Path:
    """Private state dir for the always-on node.

    The scheduled task runs as SYSTEM, whose ambient home can resolve to
    the shared ``C:\\Users\\Public`` on Windows — never let node state
    (secret key, logs) land there. Pin it to a subdir of the installing
    user's own profile instead; SYSTEM can still read it.
    """
    return default_state_dir() / "service"


def _service_command() -> str:
    """Command line the scheduled task runs to start the always-on node.

    Points at the permanent staged copy under LOCALAPPDATA (see
    install_service), never at wherever the app was launched from — so
    moving or deleting the original exe can't break the always-on node.
    """
    state = _service_state_dir()
    if getattr(sys, "frozen", False):
        return f'"{get_install_path()}" --service "{state}"'
    return f'"{sys.executable}" "{os.path.abspath(__file__)}" --service "{state}"'


def service_installed() -> bool:
    """True when the background node is installed and serving.

    The scheduled task runs as SYSTEM with highest privileges, so a normal
    (non-elevated) tray process cannot query it: schtasks returns "Access
    is denied" and the Task Scheduler COM API hides it entirely. The task
    starts the node immediately on install and at every boot, so the
    localhost health probe is the accurate, privilege-free signal that the
    background node exists and is doing its job (it is also exactly what
    startup uses to decide thin-client vs. own-node mode).
    """
    return service_available()


def _elevate(*args: str) -> None:
    """Re-launch this app elevated (UAC prompt) for a privileged action."""
    if sys.platform != "win32" or not HAS_WINREG:
        return
    if getattr(sys, "frozen", False):
        params = " ".join(args)
        target = sys.executable
    else:
        params = f'"{os.path.abspath(__file__)}" {" ".join(args)}'
        target = sys.executable
    ctypes.windll.shell32.ShellExecuteW(None, "runas", target, params, None, 1)


def _service_feedback(message: str, error: bool = False) -> int:
    """Surface install/uninstall results from the windowless elevated child."""
    try:
        ctypes.windll.user32.MessageBoxW(
            None, message,
            "Ephemeral-Distributed" + (" - Error" if error else ""),
            0x10 if error else 0x40,
        )
    except Exception:
        pass
    return 1 if error else 0


def install_service() -> int:
    """Create a scheduled task that runs the node at boot as SYSTEM.

    The exe is first staged to a permanent location under LOCALAPPDATA
    (same spot the login-autostart feature uses), so the task never points
    at a Downloads/desktop copy that could move or be deleted.
    """
    # Stage a permanent copy so the task survives the original file moving.
    if getattr(sys, "frozen", False):
        install_path = get_install_path()
        try:
            os.makedirs(os.path.dirname(install_path), exist_ok=True)
            if os.path.abspath(sys.executable) != os.path.abspath(install_path):
                shutil.copy2(sys.executable, install_path)
        except Exception as e:
            return _service_feedback(
                f"Failed to stage the background node binary:\n{e}", error=True)

    task = subprocess.run(
        ["schtasks", "/Create", "/TN", SERVICE_TASK_NAME,
         "/TR", _service_command(), "/SC", "ONSTART", "/RU", "SYSTEM",
         "/RL", "HIGHEST", "/F"],
        capture_output=True, text=True, timeout=60, startupinfo=get_startupinfo(),
    )
    if task.returncode != 0:
        return _service_feedback(
            f"Failed to install the background node:\n"
            f"{task.stderr.strip() or task.stdout.strip()}",
            error=True,
        )
    # Start it immediately so it takes effect now, not just after reboot.
    subprocess.run(["schtasks", "/Run", "/TN", SERVICE_TASK_NAME],
                   capture_output=True, text=True, timeout=30,
                   startupinfo=get_startupinfo())
    return _service_feedback(
        "Background node installed and started.\n"
        "It will also run automatically at every boot (even while logged off)."
    )


def uninstall_service() -> int:
    """Stop and remove the background-node scheduled task.

    Also disables login autostart ("Install && Run on Boot") and deletes
    the staged exe under LOCALAPPDATA, so uninstalling the service leaves
    no task, no autostart entry, and no orphaned binary behind.
    """
    subprocess.run(["schtasks", "/End", "/TN", SERVICE_TASK_NAME],
                   capture_output=True, text=True, timeout=30,
                   startupinfo=get_startupinfo())
    task = subprocess.run(["schtasks", "/Delete", "/TN", SERVICE_TASK_NAME, "/F"],
                          capture_output=True, text=True, timeout=30,
                          startupinfo=get_startupinfo())
    if task.returncode != 0:
        return _service_feedback(
            f"Failed to remove the background node:\n"
            f"{task.stderr.strip() or task.stdout.strip()}",
            error=True,
        )
    # Full cleanup: drop the login-autostart entry (its target would
    # otherwise dangle) and delete the staged exe. set_startup(False)
    # already removes the HKCU Run value and the staged file, deferring
    # deletion to the next reboot if the binary is still locked.
    try:
        set_startup(False)
    except Exception as e:
        print(f"Failed to clean up startup entry: {e}")
    return _service_feedback("Background node removed.")


def on_install_service(icon, item_unused=None):
    _elevate("--install-service")
    icon.notify("Approve the UAC prompt to install the always-on background node.",
                title="Ephemeral")


def on_uninstall_service(icon, item_unused=None):
    _elevate("--uninstall-service")
    icon.notify("Approve the UAC prompt to remove the background node.",
                title="Ephemeral")


def toggle_service(icon, item_unused=None):
    """One menu item that installs or removes the background node.

    Shows a checkmark while the background node is installed and serving
    (localhost health probe); clicking always re-prompts UAC and flips the
    state.
    """
    if service_installed():
        on_uninstall_service(icon)
    else:
        on_install_service(icon)


def private_checked(_item=None) -> bool:
    """True when the serving node is in private mode."""
    if service_ok:
        try:
            return bool(fetch_service_status().get("private"))
        except Exception:
            return False
    return private_mode_enabled(argv=sys.argv)


def _post_private(icon, enabled, seed):
    """Toggle the background node's private mode (and optional join seed)."""
    payload = {"enabled": enabled}
    if seed is not None:
        payload["seed"] = seed
    req = urllib.request.Request(
        SERVICE_URL + "/private",
        data=json.dumps(payload).encode("utf-8"),
        headers={"Content-Type": "application/json"},
        method="POST",
    )
    with urllib.request.urlopen(req, timeout=90) as res:
        return json.loads(res.read().decode("utf-8"))


def _announce_private(icon, enabled, joined, url):
    """Copy + notify the private-mode result and (when on) the student URL."""
    if url and HAS_GUI:
        pyperclip.copy(url)
    if enabled:
        header = "Joined existing private swarm" if joined else "Private mode ON — created a new swarm"
    else:
        header = "Private mode OFF (public swarm)"
    icon.notify(
        header + (("\n\nGive students this URL:\n" + url) if url else ""),
        title="Ephemeral Private Mode",
    )


def toggle_private(icon, item_unused=None):
    """Enable/disable private mode. Enabling prompts for a seed ticket:
    paste one to JOIN an existing swarm, or leave empty to CREATE a new one."""
    if service_ok:
        try:
            currently = bool(fetch_service_status().get("private"))
        except Exception:
            currently = False
        if currently:
            try:
                _post_private(icon, False, None)
            except Exception as e:
                icon.notify(f"Could not toggle the background node: {e}", title="Ephemeral Error")
                return
            _announce_private(icon, False, False, None)
            return
        seed = prompt_user_for_seed()
        if seed is None:
            return
        seed = seed.strip() or None
        try:
            resp = _post_private(icon, True, seed or "")
        except Exception as e:
            icon.notify(f"Could not toggle the background node: {e}", title="Ephemeral Error")
            return
        _announce_private(icon, True, bool(seed), resp.get("student_url"))
        return

    if private_mode_enabled(argv=sys.argv):
        _apply_private_mode(False)
        write_private_seed(None)
        cluster.stop()
        cluster.start()
        _announce_private(icon, False, False, None)
        return
    seed = prompt_user_for_seed(read_private_seed() or "")
    if seed is None:
        return
    seed = seed.strip() or None
    _apply_private_mode(True)
    write_private_seed(seed)
    cluster.stop()
    cluster.start()
    _announce_private(icon, True, bool(seed), _student_url())


def purge_cache(icon, item_unused):
    icon.notify("Pruning unused images... this may take a moment.", title="Ephemeral Maintenance")
    startupinfo = get_startupinfo()
    try:
        subprocess.run(['podman', 'image', 'prune', '--all', '--force'],
                       startupinfo=startupinfo, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        icon.notify("Image cache cleared successfully.", title="Ephemeral")
    except Exception as e:
        icon.notify(f"Error clearing cache: {e}", title="Ephemeral Error")


def force_stop_all(icon, item_unused):
    """Kill every running Ephemeral container (runs execute inside the node
    executor rather than as tracked subprocesses, so containers are the unit
    of cancellation here — same result as the local client's force stop)."""
    startupinfo = get_startupinfo()
    killed = 0
    try:
        out = subprocess.run(['podman', 'ps', '-q'], startupinfo=startupinfo,
                             stdout=subprocess.PIPE, stderr=subprocess.PIPE).stdout
        ids = [ln for ln in out.decode(errors='replace').splitlines() if ln.strip()]
        if ids:
            subprocess.run(['podman', 'rm', '-f'] + ids,
                           startupinfo=startupinfo, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            killed = len(ids)
    except Exception:
        pass

    set_icon_animation_state(icon, False)
    if killed > 0:
        icon.notify(f"Forcefully stopped {killed} running container(s).", title="Ephemeral Stopped")
    else:
        icon.notify("No active runs to stop.", title="Ephemeral")


# --- Tray / hotkey handlers ---------------------------------------------

def on_hotkey(icon):
    def hotkey_task():
        run_logic(icon)
    threading.Thread(target=hotkey_task).start()


def on_cluster_info(icon, item_unused=None):
    global service_ok
    if service_ok:
        try:
            info = fetch_service_status()
        except Exception:
            info = None
        if info:
            lines = ["Mode: background service (one identity per machine)"]
            lines.append(f"Node:     {info.get('node_id')}")
            lines.append(f"Peers:    {info.get('peers', '?')}")
            lines.append(f"Warm images: {len(info.get('warm_images') or [])}")
            icon.notify("\n".join(lines), title="Ephemeral Cluster")
            return
        service_ok = False  # service vanished — fall back to the local node
    lines = ["Mode: standalone (tray's own node)"]
    lines.append(cluster.info())
    icon.notify("\n".join(lines), title="Ephemeral Cluster")


def quit_app(icon, item_unused=None):
    cluster.stop()
    if HAS_GUI:
        icon.stop()
    sys.exit(0)


def setup_tray_mode(icon):
    icon.visible = True
    keyboard.add_hotkey(HOTKEY, lambda: on_hotkey(icon))
    keyboard.add_hotkey(CONVERT_HOTKEY, lambda: on_convert_hotkey(icon))


class DummyIcon:
    def notify(self, msg, title=""):
        print(f"[{title}] {msg}")
    def stop(self):
        pass


def setup_headless_mode(file_path):
    icon = DummyIcon()
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        icon.notify(f"Running {os.path.basename(file_path)}...", title="Ephemeral CLI")
        run_logic(icon, content=content)
    except Exception as e:
        icon.notify(f"Headless Failed: {e}", title="Ephemeral Error")


def setup_oneshot_mode(icon, file_path):
    icon.visible = True

    def auto_run_sequence():
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
            icon.notify(f"Loading {os.path.basename(file_path)}...", title="Ephemeral One-Shot")
            run_logic(icon, content=content)
        except Exception as e:
            icon.notify(f"One-Shot Failed: {e}", title="Ephemeral Error")
            time.sleep(5)
        finally:
            cluster.stop()
            icon.stop()
            sys.exit()

    threading.Thread(target=auto_run_sequence).start()


def show_about(icon, item_unused=None):
    about_text = ("# Ephemeral-Distributed.exe\n"
                  "Version: Version number (injected from the github workflow)\n"
                  "Dev: Dunko Xyvir\nLicense: MIT License\n"
                  "URL: https://github.com/Xyvir/Ephemeral.exe")
    if private_checked():
        url = current_student_url()
        if url:
            about_text += "\n\nPrivate mode student URL:\n" + url
    if HAS_GUI:
        pyperclip.copy(about_text)
    icon.notify(about_text, title="About Ephemeral-Distributed")


# --- Entry points --------------------------------------------------------

def self_check() -> int:
    """Start a node (direct mode, no Podman needed), print identity, exit."""
    from ephemeral_net.node import Node

    async def _check():
        node = Node(relay="disabled")
        await node.start()
        print(f"SELF-CHECK OK node_id={node.node_id()} peers=0")
        await node.close()

    try:
        asyncio.run(_check())
        return 0
    except Exception as e:
        print(f"SELF-CHECK FAILED: {e}")
        return 1


if __name__ == '__main__':
    if "--self-check" in sys.argv:
        sys.exit(self_check())

    # Privileged service management (run via the UAC-elevated child).
    if "--install-service" in sys.argv:
        sys.exit(install_service())
    if "--uninstall-service" in sys.argv:
        sys.exit(uninstall_service())

    if "--service" in sys.argv:
        # Always-on headless node: join the swarm and accept remote jobs.
        # Runs under the scheduled task even while the user is logged off.
        # The installer bakes the user's private state dir into the task
        # command; use it when present so SYSTEM never writes state to its
        # own (possibly shared, e.g. C:\Users\Public) home.
        if len(sys.argv) > 2 and os.path.isabs(sys.argv[2]):
            service_state = Path(sys.argv[2])
        else:
            service_state = default_state_dir()
        os.environ["EPHEMERAL_STATE_DIR"] = str(service_state)
        EPHEMERAL_SECRET = load_or_create_secret(service_state / "secret_key.bin")
        log_path = service_state / "service.log"
        log_path.parent.mkdir(parents=True, exist_ok=True)
        logging.basicConfig(
            filename=str(log_path), level=logging.INFO,
            format="%(asctime)s %(levelname)s %(name)s: %(message)s",
        )
        logging.getLogger("ephemeral").info(
            "background node starting (relay=%s, state=%s)",
            EPHEMERAL_RELAY, service_state)
        # ``start()`` gives the node 10 s to bind and bootstrap; at boot the
        # network may not be up yet, so retry forever instead of dying.
        while True:
            try:
                cluster.start()
                break
            except Exception as e:
                logging.getLogger("ephemeral").exception(
                    "background node start failed (%s); retrying in 30s", e)
                time.sleep(30)
        logging.getLogger("ephemeral").info(
            "background node started: node_id=%s",
            cluster.node.node_id() if cluster.node else "?",
        )
        # Thin-client trays (any local user) drive this node over localhost
        # instead of starting their own — one identity per machine.
        try:
            start_local_service_api(service_state)
        except Exception as e:
            logging.getLogger("ephemeral").exception(
                "local control API failed to start: %s", e
            )
        while True:
            time.sleep(3600)

    # Hybrid identity: prefer the installed background node (one identity
    # per machine — thin client). Only when no service is reachable do we
    # start the tray's own node, preserving the zero-admin standalone
    # behavior of the original client.
    service_ok = service_available()
    if not service_ok:
        try:
            cluster.start()
        except Exception as e:
            print(f"Cluster start failed: {e}")
            sys.exit(1)

    if len(sys.argv) > 1 and os.path.exists(sys.argv[-1]):
        file_target = sys.argv[-1]
        if "--cli" in sys.argv:
            CLI_MODE = True
            setup_headless_mode(file_target)
            cluster.stop()
            sys.exit(0)
        else:
            if not HAS_GUI:
                print("GUI dependencies not found. Falling back to CLI mode.")
                CLI_MODE = True
                setup_headless_mode(file_target)
                cluster.stop()
                sys.exit(0)
            image = create_icon_image()
            menu = (
                item('Run Clipboard', lambda icon, i: on_hotkey(icon), default=True),
                item('Cluster Status', on_cluster_info),
                item('About', show_about),
                item('Quit', quit_app),
            )
            icon = pystray.Icon("Ephemeral-Distributed", image, "Ephemeral-Distributed", menu)
            icon.run(lambda icon: setup_oneshot_mode(icon, file_target))
    else:
        if not HAS_GUI:
            print("GUI dependencies not found. CLI mode requires a file argument.")
            cluster.stop()
            sys.exit(1)
        image = create_icon_image()
        menu = (
            item('Run Clipboard', lambda icon, i: on_hotkey(icon), default=True),
            item('Install && Run on Boot', toggle_startup, checked=lambda item: check_startup()),
            item('Background Service', toggle_service,
                 checked=lambda i: service_installed(),
                 visible=lambda i: sys.platform == 'win32'),
            item('Private Mode', toggle_private, checked=private_checked),
            item('Force Stop All Runs', force_stop_all),
            item('Clear Image Cache', purge_cache),
            item('Cluster Status', on_cluster_info),
            item('About', show_about),
            item('Quit', quit_app),
        )
        icon = pystray.Icon("Ephemeral-Distributed", image, "Ephemeral-Distributed", menu)
        icon.run(setup_tray_mode)
