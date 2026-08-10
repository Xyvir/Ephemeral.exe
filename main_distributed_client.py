"""
Ephemeral Distributed Client — Windows tray application (``ephemeral-distributed.exe``).

A portable Windows tray utility packaged with the ``iroh`` Python extension.
It joins the ephemeral cluster as a compute node and runs clipboard-driven
code with intelligent nearest-neighbor offloading:

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

Usage:
    python main_distributed_client.py                 # Tray mode
    python main_distributed_client.py script.md       # One-shot mode
    python main_distributed_client.py --cli script.md # Headless CLI mode
    python main_distributed_client.py --self-check    # Print node id and exit
"""
from __future__ import annotations

import asyncio
import base64
import ctypes
import os
import re
import shlex
import shutil
import subprocess
import sys
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
from ephemeral_net.jobs import JobDoneEvent, JobErrorEvent, JobRequest
from ephemeral_net.swarm import load_or_create_secret, parse_seed_nodes, parse_seeds

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
EPHEMERAL_SECRET = (
    bytes.fromhex(_hex_secret) if _hex_secret else load_or_create_secret()
)
EPHEMERAL_ALLOW_NETWORK = os.getenv("EPHEMERAL_ALLOW_NETWORK", "0") == "1"


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
        from ephemeral_net.node import Node
        from ephemeral_net.offload import OffloadingExecutor
        from ephemeral_net.sandbox import CoreJobExecutor

        node = Node(
            secret_key=EPHEMERAL_SECRET,
            relay=EPHEMERAL_RELAY,
        )
        local = CoreJobExecutor(
            allow_network=EPHEMERAL_ALLOW_NETWORK,
            image_allowlist=None,
        )
        node.executor = OffloadingExecutor(node, local)
        await node.start()
        if EPHEMERAL_SEED_NODES:
            await node.bootstrap_nodes(EPHEMERAL_SEED_NODES)
        elif EPHEMERAL_SEEDS:
            await node.bootstrap(EPHEMERAL_SEEDS)
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
    request = JobRequest(
        job_id=f"local-{int(time.time() * 1000)}",
        document_blob=base64.b64encode(markdown.encode("utf-8")).decode("ascii"),
        timeout=300,
    )

    set_icon_animation_state(icon, True)
    try:
        events = cluster.submit(request)
    except Exception as e:
        show_post_mortem_error(f"Cluster execution error:\n{e}")
        icon.notify("Cluster execution failed.", title="Ephemeral Failed")
        return
    finally:
        set_icon_animation_state(icon, False)

    errors = [e for e in events if isinstance(e, JobErrorEvent)]
    if errors:
        show_post_mortem_error(f"Ephemeral Error:\n{errors[0].message}")
        icon.notify("Execution rejected.", title="Ephemeral Error")
        return

    dones = [e for e in events if isinstance(e, JobDoneEvent)]
    if not dones:
        icon.notify("Job ended without a result.", title="Ephemeral Error")
        return
    done = dones[-1]

    # Route artifacts when they were produced on this node.
    if done.artifact_path and os.path.exists(done.artifact_path):
        result = ephemeral_core.ExecutionResult(
            stdout=done.stdout,
            stderr=done.stderr,
            exit_code=done.exit_code,
            artifact_paths=[done.artifact_path],
            artifact_dir=os.path.dirname(done.artifact_path),
        )
        route_artifacts_local(result, "distributed", icon)
    elif done.artifact_file:
        icon.notify(
            f"Artifact generated on the compute node: {done.artifact_file}",
            title="Ephemeral",
        )

    if done.exit_code != 0:
        show_post_mortem_error(done.stderr or f"Exit code {done.exit_code}")
        icon.notify("Execution Failed. Debug window opened.", title="Ephemeral Error")
        return

    if done.stdout:
        if CLI_MODE:
            print(done.stdout)
        else:
            pyperclip.copy(done.stdout)
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
    icon.notify(cluster.info(), title="Ephemeral Cluster")


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

    # Start the cluster node before doing anything else.
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
            item('Force Stop All Runs', force_stop_all),
            item('Clear Image Cache', purge_cache),
            item('Cluster Status', on_cluster_info),
            item('About', show_about),
            item('Quit', quit_app),
        )
        icon = pystray.Icon("Ephemeral-Distributed", image, "Ephemeral-Distributed", menu)
        icon.run(setup_tray_mode)
