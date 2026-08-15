"""
Ephemeral Distributed Client — Windows tray application (``ephemeral-distributed.exe``).

A portable Windows tray utility packaged with the ``iroh`` Python extension.

Per-user node model: the tray runs its own compute node with a stable
identity (one per user account, persisted under the profile), available
while the user is logged in or the PC is locked. For an always-on node
that keeps serving while no one is logged in, self-host the Linux gateway
(install_self_host.sh). The tray runs clipboard-driven code with
intelligent nearest-neighbor offloading:

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
"""
from __future__ import annotations

import asyncio
import base64
import ctypes
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
from ephemeral_core.config import mapped_images
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


# --- Execution bridge: the tray runs its own per-user node ---------------


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


# --- Maintenance helpers (tray's own podman context) ---------------------


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


# --- Pre-hydrate all images ("super-seed") -------------------------------
# Worst-case download estimate (GB) for the whole language set. A single
# round max-guess anchor (~15-25 GB realistic), deliberately not per-image:
# it's a warning anchor, not a bill, and cached images get skipped anyway
# during the pull.
_HYDRATE_MAX_EST_GB = 25.0


def _hydration_free_space() -> tuple[int, str]:
    """Best-effort (free bytes, drive label) backing podman's storage.

    Prefers the real graph root when podman reports a host path, then the
    drives where a ``podman machine`` VM disk typically lives. Returns
    ``(0, "")`` when nothing can be determined.
    """
    candidates: list[str] = []
    try:
        out = subprocess.run(
            ["podman", "info", "--format", "{{.Store.GraphRoot}}"],
            capture_output=True, text=True, timeout=10,
            startupinfo=get_startupinfo(),
        )
        root = out.stdout.strip()
        # Machine mode reports a VM-internal path (not a host path) — only
        # trust it when it actually exists on this host.
        if root and os.path.exists(root):
            candidates.append(root)
    except Exception:
        pass
    for env in ("LOCALAPPDATA", "USERPROFILE", "HOME", "SystemDrive"):
        v = os.environ.get(env)
        if v:
            candidates.append(v)
    for cand in candidates:
        try:
            du = shutil.disk_usage(cand)
            return du.free, os.path.splitdrive(cand)[0] or ""
        except Exception:
            continue
    return 0, ""


def _vm_disk_cap_gb() -> float | None:
    """The podman machine VM disk cap in GB, or None when unknown."""
    try:
        out = subprocess.run(
            ["podman", "machine", "inspect", "--format", "json"],
            capture_output=True, text=True, timeout=10,
            startupinfo=get_startupinfo(),
        )
        data = json.loads(out.stdout or "[]")
        if isinstance(data, list) and data and data[0].get("DiskSize"):
            return float(data[0]["DiskSize"])
    except Exception:
        pass
    return None


def _confirm_hydration(text_lines: list[str], warn: bool) -> bool:
    """Ask the user to confirm pre-hydration; True = proceed.

    Windows shows a console with a yes/no prompt (the tray has no window);
    Linux uses zenity -> kdialog -> tkinter -> stdin.
    """
    if sys.platform != "win32":
        try:
            if shutil.which("zenity"):
                out = subprocess.run(
                    ["zenity", "--question", "--title=Ephemeral",
                     "--text=" + "\n".join(text_lines)],
                    capture_output=True, timeout=120,
                )
                return out.returncode == 0
            if shutil.which("kdialog"):
                out = subprocess.run(
                    ["kdialog", "--yesno", "\n".join(text_lines)],
                    capture_output=True, timeout=120,
                )
                return out.returncode == 0
            import tkinter as _tk
            from tkinter import messagebox
            root = _tk.Tk()
            root.withdraw()
            answer = messagebox.askyesno("Ephemeral: Pre-hydrate images", "\n".join(text_lines))
            root.destroy()
            return bool(answer)
        except Exception:
            pass
        try:
            reply = input("\n".join(text_lines) + "\nProceed? [y/N] ").strip().lower()
            return reply in ("y", "yes")
        except Exception:
            return False

    fd_out, path_out = tempfile.mkstemp(suffix=".txt")
    os.close(fd_out)
    fd_bat, path_bat = tempfile.mkstemp(suffix=".bat")
    os.close(fd_bat)
    try:
        with open(path_bat, "w") as f:
            f.write("@echo off\n")
            f.write("title Ephemeral: Pre-hydrate images\n")
            f.write("cls\n")
            f.write("echo.\n")
            for line in text_lines:
                f.write(f"echo  {line}\n")
            f.write("echo.\n")
            if warn:
                f.write("echo  *** WARNING: free space may be insufficient ***\n")
                f.write("echo.\n")
            f.write('choice /C YN /T 120 /D N /M " Proceed with pre-hydration?"\n')
            f.write(f'if errorlevel 2 echo NO> "{path_out}"\n')
            f.write(f'if not errorlevel 2 echo YES> "{path_out}"\n')
        subprocess.run(
            path_bat,
            creationflags=getattr(subprocess, "CREATE_NEW_CONSOLE", 0),
        )
        if os.path.exists(path_out):
            with open(path_out, "r") as f:
                return "YES" in f.read().upper()
    except Exception:
        return False
    finally:
        if os.path.exists(path_out):
            os.remove(path_out)
        if os.path.exists(path_bat):
            os.remove(path_bat)
    return False


def _hydrate_all_images(icon=None, images: list[str] | None = None) -> None:
    """Pull every mapped image not already cached (daemon-thread target).

    Runs in the tray's own podman context. Sequential pulls with retry +
    backoff (registry rate limits), never raises.
    """
    startupinfo = get_startupinfo()
    images = images if images is not None else mapped_images()
    if not ephemeral_core.check_podman_alive():
        try:
            subprocess.run(
                ["podman", "machine", "start"],
                stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                startupinfo=startupinfo, timeout=300,
            )
        except Exception:
            pass
        if not ephemeral_core.check_podman_alive():
            if icon:
                icon.notify(
                    "Podman could not be started — pre-hydration aborted.",
                    title="Ephemeral Error",
                )
            return
    pulled = 0
    skipped = 0
    failed: list[str] = []
    for img in images:
        if ephemeral_core.check_image_exists(img):
            skipped += 1
            continue
        ok = False
        for attempt in range(1, 4):
            try:
                rc = subprocess.run(
                    ["podman", "pull", img],
                    stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                    startupinfo=startupinfo, timeout=900,
                ).returncode
            except Exception:
                rc = -1
            if rc == 0:
                ok = True
                break
            time.sleep(5 * attempt)
        if ok:
            pulled += 1
        else:
            failed.append(img)
    if icon:
        summary = (
            f"Pre-hydration done: {pulled} pulled, "
            f"{skipped} already cached, {len(failed)} failed."
        )
        if failed:
            summary += "\nFailed: " + ", ".join(failed)
        icon.notify(summary, title="Ephemeral Pre-hydrate")


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
    try:
        _ensure_cluster()
        result = run_through_cluster(blob, timeout=300)
    except Exception as e:
        show_post_mortem_error(f"Cluster execution error:\n{e}")
        icon.notify("Cluster execution failed.", title="Ephemeral Failed")
        return
    finally:
        set_icon_animation_state(icon, False)

    artifact_local = result.get("artifact_path")

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


# --- Bash canary pre-hydration -------------------------------------------
# The swarm liveness probe is (and will stay) a bash-style job, and a node
# that starts cold on bash can fail its first probe by offloading to a
# neighbor. Warming alpine (~7 MB) makes the tray's node advertise
# ``bash`` warm from second zero (list_local_images() reflects it
# automatically), and it doubles as a warm runtime for real ``sh``/``bash``
# jobs. Best-effort at tray startup; failures just mean on-demand pulls.

PREHYDRATE_IMAGE = "docker.io/library/alpine:latest"


def _prehydrate_bash(timeout: int = 300) -> bool:
    """Best-effort pull of the bash canary image (never raises).

    Ensures the podman machine is up, then pulls ``PREHYDRATE_IMAGE``.
    Returns True on success. Called at tray startup to warm the node's own
    podman context. Failures are logged and the node falls back to
    on-demand pulls.
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


def toggle_private(icon, item_unused=None):
    """Flip the tray's own node between public and private mode.

    Enabling prompts once: leave empty to CREATE a new private swarm
    (anchored on this node), or paste a ticket to JOIN an existing swarm.
    The node restarts in the new mode."""
    enabling = not private_mode_enabled(argv=sys.argv)

    if not enabling:
        _apply_private_mode(False)
        write_private_seed(None)
        if cluster.node is not None:
            cluster.stop()
            cluster.start()
        _announce_private(icon, False, False, None)
        return

    seed = prompt_user_for_seed(read_private_seed() or "")
    if seed is None:
        return
    seed = seed.strip() or None

    # The tray's own node anchors a NEW swarm (create-new) or joins an
    # existing one (join-existing) when a seed is provided.
    _apply_private_mode(True)
    write_private_seed(seed)
    if cluster.node is not None:
        cluster.stop()
        cluster.start()
    _announce_private(icon, True, bool(seed), _student_url())


def purge_cache(icon, item_unused):
    """Clear the image cache of whichever podman owns the live node.

    Clears the tray's own podman image cache.
    """
    icon.notify("Pruning unused images... this may take a moment.", title="Ephemeral Maintenance")
    try:
        _podman_prune_images()
        icon.notify("Image cache cleared successfully.", title="Ephemeral")
    except Exception as e:
        icon.notify(f"Error clearing cache: {e}", title="Ephemeral Error")


def force_stop_all(icon, item_unused):
    """Kill every running Ephemeral container.

    Runs execute inside the node executor rather than as tracked
    subprocesses, so containers are the unit of cancellation here — same
    result as the local client's force stop. Stops them in the tray's own
    podman.
    """
    killed = _podman_stop_all()

    set_icon_animation_state(icon, False)
    if killed > 0:
        icon.notify(f"Forcefully stopped {killed} running container(s).", title="Ephemeral Stopped")
    else:
        icon.notify("No active runs to stop.", title="Ephemeral")


def on_prehydrate_all(icon, item_unused=None):
    """Pre-hydrate every language image (super-seed) with a space warning.

    Shows what's cached / missing, an estimated download size, and the free
    space on the drive backing podman's storage — then asks for
    confirmation. The pull runs in the tray's own podman, in the
    background.
    """
    images = mapped_images()

    est_gb = _HYDRATE_MAX_EST_GB
    free, drive = _hydration_free_space()
    warn = False
    lines = ["Pre-hydrate ALL language images?"]
    lines.append(f"  {len(images)} images in the language set.")
    lines.append(f"  Worst-case download: ~{est_gb:.0f} GB (skips what's already cached)")
    if free:
        lines.append(f"  Free on {drive or 'storage drive'}: {free / 2**30:.1f} GB")
        if free < est_gb * 2**30:
            warn = True
            lines.append("  WARNING: free space looks too low — pulls may fail.")
    else:
        lines.append("  (could not determine free disk space — check before proceeding)")
    cap = _vm_disk_cap_gb()
    if cap is not None:
        lines.append(f"  Podman VM disk cap: {cap:.0f} GB")
        if cap < est_gb:
            warn = True
            lines.append("  WARNING: VM disk cap is below the worst-case download.")
    lines.append("")
    lines.append("Pulls every image the cluster can request; the big science and")
    lines.append("typesetting images are multi-GB. The node keeps running during")
    lines.append("the pull — jobs just land on already-warm images.")

    if not _confirm_hydration(lines, warn):
        return

    icon.notify(
        f"Pre-hydration started — pulling up to {len(images)} image(s); "
        "already-cached ones are skipped. You'll get a summary when done.",
        title="Ephemeral Pre-hydrate",
    )
    threading.Thread(
        target=_hydrate_all_images, args=(icon,),
        name="ephemeral-hydrate", daemon=True,
    ).start()


# --- Tray / hotkey handlers ---------------------------------------------

def on_hotkey(icon):
    def hotkey_task():
        run_logic(icon)
    threading.Thread(target=hotkey_task).start()


def on_cluster_info(icon, item_unused=None):
    lines = ["Mode: per-user node (tray)"]
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

    # The tray always runs its own per-user node. Warmup runs in the
    # BACKGROUND — never block the tray icon on cluster bootstrap. A slow
    # relay/DNS/swarm dial (or a stale swarm entry) can exceed the 10 s
    # start window; before, that delayed the icon or, on timeout, silently
    # killed the process before pystray ever ran. The first job retries via
    # _ensure_cluster(). Also pre-warm the tiny bash canary so the node
    # advertises ``bash`` warm from second zero (best-effort, non-blocking).
    threading.Thread(target=_warmup_cluster, name="ephemeral-warmup", daemon=True).start()
    threading.Thread(
        target=_prehydrate_bash, name="ephemeral-prehydrate", daemon=True
    ).start()

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
            item('Distributed', pystray.Menu(
                item('Cluster Status', on_cluster_info),
                item('Private Mode', toggle_private, checked=private_checked),
                item('Pre-hydrate All Images', on_prehydrate_all),
            )),
            item('Force Stop All Runs', force_stop_all),
            item('Clear Image Cache', purge_cache),
            item('About', show_about),
            item('Quit', quit_app),
        )
        icon = pystray.Icon("Ephemeral-Distributed", image, "Ephemeral-Distributed", menu)
        icon.run(setup_tray_mode)
