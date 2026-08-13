"""
Ephemeral Distributed Client — Windows tray application (``ephemeral-distributed.exe``).

A portable Windows tray utility packaged with the ``iroh`` Python extension.

Merged-identity model: one node per machine. When the background service
(``--service``, installed from the tray menu) is installed, the tray is a
thin front-end to it — jobs run through the always-on node, so every user
on the box shares the one identity and the one warm image cache. The
decision is lazy and marker-based (never a launch-time probe): if the
service is unreachable at job time, the tray falls back to running its own
compute node for that job — no admin, no service required, always works.
Either way it runs clipboard-driven code with intelligent nearest-neighbor
offloading:

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
on ``127.0.0.1`` (``EPHEMERAL_SERVICE_PORT``, default 8788; auto-picked
when taken and persisted for tray discovery) exposing
``GET /health`` (plus the job/artifact/private endpoints) for diagnostics
and curl-based control.
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
# Lazy: the tray materializes its identity when it starts its own node.
EPHEMERAL_SECRET: bytes | None = (
    bytes.fromhex(_hex_secret) if _hex_secret else None
)
EPHEMERAL_ALLOW_NETWORK = os.getenv("EPHEMERAL_ALLOW_NETWORK", "0") == "1"

# Private mode (skip the public swarm list) is decided at bootstrap time via
# ``private_mode_enabled``: ``--private`` / ``EPHEMERAL_PRIVATE=1``, or a
# ``private_mode`` marker file in the node's state dir (toggled live from the
# tray's "Private Mode" menu item).

# Localhost port the background node's control API listens on (in
# ``--service`` mode) — kept for diagnostics and curl-based control. The
# actual port is dynamic: the service tries this one, scans up, then lets
# the OS assign one, and persists the winner for tray discovery.
SERVICE_PORT = int(os.getenv("EPHEMERAL_SERVICE_PORT", "8788"))


def _service_url() -> str:
    """Base URL of the background node's localhost API.

    The service persists its actual port to the shared state dir — read it
    fresh per call so a service restart that lands on a new port is always
    found, and fall back to the configured port when the file is missing.
    """
    port = SERVICE_PORT
    try:
        persisted = (_service_state_dir() / "service_port.txt").read_text().strip()
        if persisted.isdigit():
            port = int(persisted)
    except Exception:
        pass
    return f"http://127.0.0.1:{port}"


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
        if self.node is not None:
            return
        if self._thread is not None and self._thread.is_alive():
            # A bootstrap is already running in the background (e.g. the
            # launch warmup) — wait for it to settle rather than spawn a
            # second node/loop on top of it.
            for _ in range(200):
                if self.node is not None or self._start_error is not None:
                    return
                time.sleep(0.05)
            raise RuntimeError("timed out starting the cluster node")
        self._start_error = None  # clear any previous failure
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
        if self._thread is not None:
            self._thread.join(timeout=5)

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


# --- Execution bridge: the tray always runs its own node -----------------
# Two identities are fine: the tray's node is independent of the always-on
# background service (if installed). Nothing here dials the service.


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


_cluster_start_lock = threading.Lock()


def _ensure_cluster() -> None:
    """Start the tray's own node (standalone fallback), if not already up."""
    if cluster.node is not None:
        return
    with _cluster_start_lock:
        if cluster.node is not None:
            return
        cluster.start()


def _warmup_cluster() -> None:
    """Best-effort background warmup so the node joins the swarm at launch."""
    try:
        _ensure_cluster()
    except Exception as e:
        # Non-fatal — the first job retries via _ensure_cluster().
        print(f"Cluster warmup failed (will retry on first job): {e}")


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
    """Student link for the tray's own node."""
    return _student_url()


def _apply_private_mode(enabled: bool, state_dir: Path | None = None) -> None:
    """Persist private mode (marker file) for a node's state dir."""
    state_dir = state_dir or default_state_dir()
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
    """Minimal stdlib HTTP API for the background node."""

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
            ticket = None
            if cluster.node is not None:
                try:
                    ticket = cluster.node.ticket()
                except Exception:
                    ticket = None
            self._send_json(
                200,
                {"enabled": enabled, "student_url": _student_url(), "ticket": ticket},
            )
            return
        if parsed.path == "/stop":
            try:
                killed = _podman_stop_all()
                self._send_json(200, {"killed": killed})
            except Exception as e:
                self._send_json(500, {"detail": f"stop failed: {e}"})
            return
        if parsed.path == "/cache":
            try:
                _podman_prune_images()
                self._send_json(200, {"ok": True})
            except Exception as e:
                self._send_json(500, {"detail": f"cache clear failed: {e}"})
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


class _NoReuseHTTPServer(ThreadingHTTPServer):
    # Windows: SO_REUSEADDR lets a second socket bind the same port and
    # silently STEAL the connections (the cause of the Freebuff incident).
    # Keep it off on Windows so a collision always surfaces as WinError
    # 10048 and the scan-up below can dodge it; Linux keeps standard
    # behavior so restarts don't trip over TIME_WAIT.
    allow_reuse_address = sys.platform != "win32"


def start_local_service_api(state_dir: Path) -> None:
    """Expose the background node over localhost for diagnostics/curl.

    Port selection is "check then assign", Windows-safe: try the
    configured ``SERVICE_PORT``, scan upward for a free one, then let the
    OS assign an ephemeral port as the final guarantee. The winner is
    persisted to the shared state dir so every user's tray discovers it.
    """
    global SERVICE_ARTIFACTS_DIR
    SERVICE_ARTIFACTS_DIR = state_dir / "artifacts"
    SERVICE_ARTIFACTS_DIR.mkdir(parents=True, exist_ok=True)
    server = None
    for port in range(SERVICE_PORT, SERVICE_PORT + 128):
        try:
            server = _NoReuseHTTPServer(("127.0.0.1", port), _ServiceHandler)
            break
        except OSError:
            continue
    if server is None:
        server = _NoReuseHTTPServer(("127.0.0.1", 0), _ServiceHandler)
    actual = server.server_address[1]
    try:
        (state_dir / "service_port.txt").write_text(str(actual), encoding="utf-8")
    except Exception as e:
        logging.getLogger("ephemeral").warning(
            "failed to persist service port: %s", e)
    threading.Thread(
        target=server.serve_forever, daemon=True, name="ephemeral-local-api"
    ).start()
    logging.getLogger("ephemeral").info(
        "local API listening on http://127.0.0.1:%d", actual
    )


# --- Maintenance helpers (shared by the service API and the tray menus) ---
# Runs in whichever podman context the caller is in: the service process
# (SYSTEM account) operates the service's own podman; a standalone tray
# operates its own.


def _podman_stop_all() -> int:
    """Kill every running container in the caller's podman context."""
    startupinfo = get_startupinfo()
    killed = 0
    try:
        out = subprocess.run(
            ["podman", "ps", "-q"], startupinfo=startupinfo,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        ).stdout
        ids = [ln for ln in out.decode(errors="replace").splitlines() if ln.strip()]
        if ids:
            subprocess.run(
                ["podman", "rm", "-f"] + ids,
                startupinfo=startupinfo, stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            )
            killed = len(ids)
    except Exception:
        pass
    return killed


def _podman_prune_images() -> None:
    """Prune unused images from the caller's podman context (best-effort)."""
    startupinfo = get_startupinfo()
    try:
        subprocess.run(
            ["podman", "image", "prune", "--all", "--force"],
            startupinfo=startupinfo, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        )
    except Exception:
        pass


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

    markdown = rebuild_markdown(blocks)
    blob = base64.b64encode(markdown.encode("utf-8")).decode("ascii")

    set_icon_animation_state(icon, True)
    result = None
    try:
        if _service_installed_local():
            # Merged identity: one node per machine — run through the
            # always-on background service when it's installed.
            result = submit_via_service(blob, timeout=300)
        else:
            _ensure_cluster()
            result = run_through_cluster(blob, timeout=300)
    except Exception as e:
        # Lazy per-job fallback (never a launch-time decision): if the
        # service is unreachable or dies mid-run, run THIS job through the
        # tray's own node instead. A bootstrapping/down service can't
        # wedge the tray into the wrong mode.
        if _service_installed_local():
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
    """Permanent copy the login-autostart entry runs ("Install && Run on Boot")."""
    app_data = os.getenv('LOCALAPPDATA', os.path.expanduser('~'))
    install_dir = os.path.join(app_data, 'Ephemeral-Distributed')
    is_frozen = getattr(sys, 'frozen', False)
    ext = '.exe' if is_frozen else '.py'
    return os.path.join(install_dir, f'Ephemeral-Distributed{ext}')


def _service_staged_path():
    """Permanent copy the background-node scheduled task runs (``--service``).

    Deliberately a DIFFERENT file from the login-autostart copy
    (get_install_path): the service copy is a running SYSTEM exe and stays
    locked while the service is up, so autostart must never try to
    overwrite or delete it (that was the "Permission denied" when enabling
    startup with the service installed).
    """
    app_data = os.getenv('LOCALAPPDATA', os.path.expanduser('~'))
    install_dir = os.path.join(app_data, 'Ephemeral-Distributed')
    is_frozen = getattr(sys, 'frozen', False)
    ext = '.exe' if is_frozen else '.py'
    return os.path.join(install_dir, f'Ephemeral-Distributed-Service{ext}')


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
    """State dir for the always-on node (secret identity, logs, port file).

    Shared across all users — SYSTEM writes it, every user's tray reads
    and (for private-mode markers) writes it. The installing user's
    profile was per-user and hid the node from other trays; the SYSTEM
    account's own home can resolve to ``C:\\Users\\Public`` anyway, so we
    deliberately use the OS's shared folder.
    """
    return _service_root_dir()


def _service_root_dir() -> Path:
    """Machine-wide root for the always-on node's shared state.

    Lives under ``C:\\Users\\Public`` (the ``PUBLIC`` env var) — the OS's
    built-in shared folder, readable/writable by every local user and
    SYSTEM with default ACLs, so every user's tray on the box discovers
    and drives the ONE SYSTEM node (the multi-user story) with no
    permission surgery. Falls back to the user profile when PUBLIC is
    unset (non-Windows).
    """
    public = os.environ.get("PUBLIC")
    if public:
        return Path(public) / "Ephemeral-Distributed"
    return default_state_dir() / "service"


def _service_marker() -> Path:
    """Marker the elevated installer writes so ANY user's tray can show the
    service as installed without querying the SYSTEM-owned scheduled task."""
    return _service_root_dir() / "service.installed"


def _service_command() -> str:
    """Command line the scheduled task runs to start the always-on node.

    Points at the service's own permanent staged copy under LOCALAPPDATA
    (see install_service), never at wherever the app was launched from —
    so moving or deleting the original exe can't break the always-on node.
    """
    state = _service_state_dir()
    if getattr(sys, "frozen", False):
        return f'"{_service_staged_path()}" --service "{state}"'
    return f'"{sys.executable}" "{os.path.abspath(__file__)}" --service "{state}"'


def service_installed() -> bool:
    """True when the background node has been installed via the tray.

    The scheduled task runs as SYSTEM with highest privileges, so a normal
    (non-elevated) tray process cannot query it (schtasks returns "Access
    is denied") and a localhost health probe is flaky while the node is
    still bootstrapping. The elevated installer writes a marker file the
    tray can read with no privileges — deterministic and instant.
    """
    return _service_marker().exists()


#: Canary image pre-warmed at service install/boot. The swarm liveness
#: probe is (and will stay) a bash-style job, and server_mode never waits
#: for a first-run pull — so a node that starts cold on bash can fail its
#: first probe by offloading to a neighbor. Warming alpine (~7 MB) makes
#: the node advertise ``bash`` warm from second zero (list_local_images()
#: reflects it automatically), and it doubles as a warm runtime for real
#: ``sh``/``bash`` jobs.
PREHYDRATE_IMAGE = "docker.io/library/alpine:latest"


def _prehydrate_bash(timeout: int = 300) -> bool:
    """Best-effort pull of the bash canary image (never raises).

    Ensures the podman machine is up, then pulls ``PREHYDRATE_IMAGE``.
    Returns True on success. Called at service install (elevated — warms
    the user's podman context, which the tray shares) and at service boot
    (warms the node's own context, which runs as a different account).
    Failures are logged and the node falls back to on-demand pulls.
    """
    startupinfo = get_startupinfo()
    try:
        alive = subprocess.run(
            ["podman", "info"],
            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
            startupinfo=startupinfo, timeout=30,
        ).returncode == 0
        if not alive:
            subprocess.run(
                ["podman", "machine", "start"],
                stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                startupinfo=startupinfo, timeout=timeout,
            )
        pull = subprocess.run(
            ["podman", "pull", PREHYDRATE_IMAGE],
            capture_output=True, text=True,
            startupinfo=startupinfo, timeout=timeout,
        )
        return pull.returncode == 0
    except Exception as e:
        logging.getLogger("ephemeral").warning("bash pre-hydration failed: %s", e)
        return False


def _elevate(*args: str) -> int | None:
    """Re-launch this app elevated (UAC prompt) for a privileged action.

    Returns the elevated child's process handle (or ``None`` when
    elevation didn't start — non-Windows, or the user denied the prompt),
    so callers can wait for it instead of guessing.
    """
    if sys.platform != "win32" or not HAS_WINREG:
        return None
    if getattr(sys, "frozen", False):
        params = " ".join(args)
        target = sys.executable
    else:
        params = f'"{os.path.abspath(__file__)}" {" ".join(args)}'
        target = sys.executable

    class _SEI(ctypes.Structure):
        _fields_ = [
            ("cbSize", ctypes.c_ulong),
            ("fMask", ctypes.c_ulong),
            ("hwnd", ctypes.c_void_p),
            ("lpVerb", ctypes.c_wchar_p),
            ("lpFile", ctypes.c_wchar_p),
            ("lpParameters", ctypes.c_wchar_p),
            ("lpDirectory", ctypes.c_wchar_p),
            ("nShow", ctypes.c_int),
            ("hInstApp", ctypes.c_void_p),
            ("lpIDList", ctypes.c_void_p),
            ("lpClass", ctypes.c_wchar_p),
            ("hkeyClass", ctypes.c_void_p),
            ("dwHotKey", ctypes.c_ulong),
            ("hIcon", ctypes.c_void_p),
            ("hProcess", ctypes.c_void_p),
        ]

    sei = _SEI()
    sei.cbSize = ctypes.sizeof(_SEI)
    sei.fMask = 0x00000040  # SEE_MASK_NOCLOSEPROCESS
    sei.lpVerb = "runas"
    sei.lpFile = target
    sei.lpParameters = params
    sei.nShow = 1
    if not ctypes.windll.shell32.ShellExecuteExW(ctypes.byref(sei)):
        return None
    return sei.hProcess or None


def _wait_for_service_change(icon, *, installed: bool, child=None, timeout: float = 30.0) -> None:
    """Block until the service marker flips, then rebuild the tray menu.

    pystray's win32 backend caches the menu (``checked`` is evaluated once,
    at startup — reopening it never re-evaluates), so the Background
    Service box only appeared after restarting the app. The elevated child
    writes/removes the marker early (before the slow bash pre-hydration),
    so we poll briefly for the flip and then force ``update_menu()`` to
    rebuild — the checkmark updates in-session, no restart needed.

    Runs on the tray's UI thread; bounded by ``timeout`` and by the
    elevated child's exit (covers a denied UAC prompt).
    """
    deadline = time.monotonic() + timeout
    kernel32 = ctypes.windll.kernel32
    while time.monotonic() < deadline:
        if service_installed() == installed:
            break
        # Handle must be pointer-sized (a 64-bit HANDLE truncated to a C
        # int would corrupt the wait).
        if child is not None and kernel32.WaitForSingleObject(ctypes.c_void_p(child), 0) == 0:
            break  # elevated child exited (denied UAC or failure)
        time.sleep(0.25)
    if child is not None:
        kernel32.CloseHandle(ctypes.c_void_p(child))
    try:
        if HAS_GUI and icon is not None:
            if hasattr(icon, "update_menu"):
                icon.update_menu()
            else:
                icon.menu = icon.menu  # older pystray: rebuild via setter
    except Exception:
        pass


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
    # Stage a permanent copy (the service's OWN file — separate from the
    # login-autostart copy) so the task survives the original moving.
    if getattr(sys, "frozen", False):
        install_path = _service_staged_path()
        try:
            os.makedirs(os.path.dirname(install_path), exist_ok=True)
            if os.path.abspath(sys.executable) != os.path.abspath(install_path):
                shutil.copy2(sys.executable, install_path)
        except Exception as e:
            return _service_feedback(
                f"Failed to stage the background node binary:\n{e}", error=True)

    # Prepare the shared, machine-wide state dir under C:\Users\Public —
    # its default ACL already lets every local user (and SYSTEM) read and
    # write it, so no permission changes are needed here.
    try:
        shared = _service_state_dir()
        shared.mkdir(parents=True, exist_ok=True)
        (shared / "artifacts").mkdir(parents=True, exist_ok=True)
    except Exception as e:
        print(f"Failed to prepare shared service dir: {e}")
    # One-time migration: older installs kept the service state under the
    # user's profile. Copy it over so the machine keeps its stable node
    # identity across the move to the shared Public folder.
    try:
        old = default_state_dir() / "service"
        if old.is_dir() and old != _service_state_dir():
            for p in old.iterdir():
                if p.is_file() and not (_service_state_dir() / p.name).exists():
                    shutil.copy2(p, _service_state_dir() / p.name)
    except Exception as e:
        print(f"Service state migration skipped: {e}")

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
    # Marker so the (non-elevated) tray can show the service as installed
    # without querying the SYSTEM-owned task.
    try:
        marker = _service_marker()
        marker.parent.mkdir(parents=True, exist_ok=True)
        marker.write_text(str(int(time.time())), encoding="utf-8")
    except Exception as e:
        print(f"Failed to write service marker: {e}")
    # Pre-warm the bash canary so the node is warm on it from second zero
    # (the probe never waits for a first-run pull). Best-effort: a podman
    # machine that can't start just means on-demand pulls instead.
    warmed = _prehydrate_bash()
    msg = (
        "Background node installed and started.\n"
        "It will also run automatically at every boot (even while logged off)."
    )
    if warmed:
        msg += "\nBash runtime pre-warmed — first jobs (and probes) run instantly."
    else:
        msg += "\nNote: bash runtime could not be pre-warmed; the node will pull it on demand."
    return _service_feedback(msg)


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
    # Remove the service's OWN staged copy (the task no longer runs it).
    # The login-autostart copy and its Run key are left untouched — the
    # two features are independent, so uninstalling the service never
    # disables "Install && Run on Boot".
    try:
        staged = _service_staged_path()
        if os.path.exists(staged):
            try:
                os.remove(staged)
            except Exception:
                MOVEFILE_DELAY_UNTIL_REBOOT = 4
                ctypes.windll.kernel32.MoveFileExW(
                    staged, None, MOVEFILE_DELAY_UNTIL_REBOOT)
    except Exception as e:
        print(f"Failed to remove staged service copy: {e}")
    _service_marker().unlink(missing_ok=True)
    return _service_feedback("Background node removed.")


def on_install_service(icon, item_unused=None):
    child = _elevate("--install-service")
    icon.notify("Approve the UAC prompt to install the always-on background node.",
                title="Ephemeral")
    _wait_for_service_change(icon, installed=True, child=child)


def on_uninstall_service(icon, item_unused=None):
    child = _elevate("--uninstall-service")
    icon.notify("Approve the UAC prompt to remove the background node.",
                title="Ephemeral")
    _wait_for_service_change(icon, installed=False, child=child)


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
    """True when the tray's own node is in private mode."""
    return private_mode_enabled(argv=sys.argv)


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


def _service_installed_local() -> bool:
    """True when a background service is installed on this (Windows) box."""
    return sys.platform == "win32" and service_installed()


def _service_status() -> dict | None:
    """The background node's /health snapshot, or None when unreachable."""
    try:
        with urllib.request.urlopen(_service_url() + "/health", timeout=3) as res:
            return json.loads(res.read().decode("utf-8"))
    except Exception:
        return None


def _post_private(enabled: bool, seed: str = "") -> dict:
    """Toggle the background node's private mode via its localhost API."""
    payload = {"enabled": enabled}
    if seed:
        payload["seed"] = seed
    req = urllib.request.Request(
        _service_url() + "/private",
        data=json.dumps(payload).encode("utf-8"),
        headers={"Content-Type": "application/json"},
        method="POST",
    )
    with urllib.request.urlopen(req, timeout=90) as res:
        return json.loads(res.read().decode("utf-8"))


def _service_post(path: str, timeout: int = 180) -> dict:
    """POST to a background-node control endpoint; raises on failure."""
    req = urllib.request.Request(
        _service_url() + path,
        data=b"{}",
        headers={"Content-Type": "application/json"},
        method="POST",
    )
    with urllib.request.urlopen(req, timeout=timeout) as res:
        return json.loads(res.read().decode("utf-8"))


def submit_via_service(blob: str, timeout: int = 300) -> dict:
    """POST a job to the background node; returns its RunResponse dict."""
    body = json.dumps({"document_blob": blob, "timeout": timeout}).encode("utf-8")
    req = urllib.request.Request(
        _service_url() + "/ephemeral/api/v1/run",
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
    url = _service_url() + "/artifact?name=" + urllib.parse.quote(os.path.basename(name))
    with urllib.request.urlopen(url, timeout=120) as res, open(dest, "wb") as f:
        shutil.copyfileobj(res, f)
    return dest


def service_private_url() -> str | None:
    """Student URL for the background node when it's actually running private."""
    status = _service_status()
    if not status or not status.get("private"):
        return None
    ticket = status.get("ticket")
    return private_student_url(ticket) if ticket else None


def toggle_private(icon, item_unused=None):
    """Flip BOTH identities — the tray's own node and the background
    service — between public and private together, so they never disagree.
    Enabling prompts once: leave empty to CREATE a new private swarm
    (anchored on the always-on service when installed, else the tray
    node), or paste a ticket to JOIN an existing swarm with both nodes."""
    enabling = not private_mode_enabled(argv=sys.argv)

    if not enabling:
        _apply_private_mode(False)
        write_private_seed(None)
        # Restart the tray's own node only if it's actually running (it
        # exists only as a fallback when the service is installed) — never
        # spawn one just to flip a flag.
        if cluster.node is not None:
            cluster.stop()
            cluster.start()
        if _service_installed_local():
            # Clear the service's persisted state directly (so it can't come
            # back private after a reboot), then apply it live if reachable.
            try:
                _apply_private_mode(False, _service_state_dir())
                write_private_seed(None, state_dir=_service_state_dir())
            except Exception as e:
                icon.notify(
                    f"Could not persist the background node's state: {e}",
                    title="Ephemeral Warning",
                )
            try:
                _post_private(False, "")
            except Exception as e:
                icon.notify(
                    f"Could not reach the background node: {e}",
                    title="Ephemeral Warning",
                )
        _announce_private(icon, False, False, None)
        return

    seed = prompt_user_for_seed(read_private_seed() or "")
    if seed is None:
        return
    seed = seed.strip() or None

    service_ticket = None
    service_url = None
    if _service_installed_local():
        # Persist the service's state directly (works even while it's down),
        # then ask the running node to apply + restart via its localhost API.
        try:
            _apply_private_mode(True, _service_state_dir())
            write_private_seed(seed, state_dir=_service_state_dir())
        except Exception as e:
            icon.notify(
                f"Could not persist the background node's state: {e}",
                title="Ephemeral Warning",
            )
        try:
            resp = _post_private(True, seed or "")
            service_ticket = resp.get("ticket")
            service_url = resp.get("student_url")
        except Exception as e:
            icon.notify(
                f"Background node unavailable ({e}); "
                "it will go private when it next starts.",
                title="Ephemeral Warning",
            )

    # The tray's fallback node mirrors the same swarm the service just
    # anchored (create-new) or joined (join-existing), or self-seeds when
    # no service is installed.
    if seed:
        tray_seed = seed
    elif service_ticket:
        tray_seed = service_ticket
    else:
        tray_seed = None

    _apply_private_mode(True)
    write_private_seed(tray_seed)
    if cluster.node is not None:
        cluster.stop()
        cluster.start()
    _announce_private(icon, True, bool(seed), service_url or _student_url())


def purge_cache(icon, item_unused):
    """Clear the image cache of whichever podman owns the live node.

    With the background service installed (merged identity) the live cache
    belongs to the service's podman — clear it through its localhost API;
    otherwise clear the tray's own podman.
    """
    icon.notify("Pruning unused images... this may take a moment.", title="Ephemeral Maintenance")
    try:
        if _service_installed_local():
            _service_post("/cache", timeout=300)
        else:
            _podman_prune_images()
        icon.notify("Image cache cleared successfully.", title="Ephemeral")
    except Exception as e:
        icon.notify(f"Error clearing cache: {e}", title="Ephemeral Error")


def force_stop_all(icon, item_unused):
    """Kill every running Ephemeral container.

    Runs execute inside the node executor rather than as tracked
    subprocesses, so containers are the unit of cancellation here — same
    result as the local client's force stop. With the background service
    installed (merged identity) the live containers belong to the service's
    podman, so stop them through its localhost API; otherwise stop them in
    the tray's own podman.
    """
    killed = 0
    if _service_installed_local():
        try:
            resp = _service_post("/stop", timeout=120)
            killed = resp.get("killed", 0)
        except Exception as e:
            icon.notify(
                f"Could not reach the background node: {e}", title="Ephemeral Error"
            )
            set_icon_animation_state(icon, False)
            return
    else:
        killed = _podman_stop_all()

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
    if _service_installed_local():
        info = _service_status()
        if info:
            lines = ["Mode: background service (one identity per machine)"]
            lines.append(f"Node:     {info.get('node_id')}")
            lines.append(f"Peers:    {info.get('peers', '?')}")
            lines.append(f"Warm images: {len(info.get('warm_images') or [])}")
            if info.get("private"):
                lines.append("Private:  on")
            icon.notify("\n".join(lines), title="Ephemeral Cluster")
            return
        # Service installed but not answering yet — report honestly instead
        # of silently starting a second identity.
        icon.notify(
            "Mode: background service (one identity per machine)\n"
            "Status: not answering yet — jobs fall back to the tray's own "
            "node until it is reachable.",
            title="Ephemeral Cluster",
        )
        return
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
        url = service_private_url() or current_student_url()
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
        # Localhost API for diagnostics and curl-based control.
        try:
            start_local_service_api(service_state)
        except Exception as e:
            logging.getLogger("ephemeral").exception(
                "local control API failed to start: %s", e
            )
        # Pre-warm the bash canary in the node's OWN podman context — the
        # service runs as a different account than the tray, so its warm
        # image set is separate. Best-effort and non-blocking: if that
        # account's podman can't start, we just log and pull on demand.
        threading.Thread(
            target=_prehydrate_bash, name="ephemeral-prehydrate", daemon=True
        ).start()
        while True:
            time.sleep(3600)

    # Merged identity: one node per machine. When the background service is
    # installed, the tray is a thin front-end and must NOT spawn its own
    # node (that would double the identity and the warm image cache) — warm
    # the tray's own node only when no service is installed. In both cases
    # warmup runs in the BACKGROUND — never block the tray icon on cluster
    # bootstrap. A slow relay/DNS/swarm dial (or a stale swarm entry) can
    # exceed the 10 s start window; before, that delayed the icon or, on
    # timeout, silently killed the process before pystray ever ran. The
    # first job retries via _ensure_cluster().
    if not _service_installed_local():
        threading.Thread(target=_warmup_cluster, name="ephemeral-warmup", daemon=True).start()

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
