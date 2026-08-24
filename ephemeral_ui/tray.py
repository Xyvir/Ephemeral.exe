"""
Unified Ephemeral tray front end.

One front end drives both desktop clients. It builds the tray menu,
registers the hotkeys, and runs the tray / one-shot / headless modes —
calling only the generic :class:`~ephemeral_ui.backends.base.Backend`
interface. It never imports ``ephemeral_core`` or ``ephemeral_net``, so a
local build stays free of the networking tier.

Entry points (``main_local.py``, ``main_distributed_client.py``) are
thin:

    from ephemeral_ui import tray
    from ephemeral_ui.backends.local import LocalBackend
    tray.run(LocalBackend())
"""
from __future__ import annotations

import os
import sys
import threading
import time

from ephemeral_ui import platform
from ephemeral_ui.platform import HAS_GUI

#: Same hotkeys for both clients (Run Clipboard / Convert to seed block).
HOTKEY = 'ctrl+alt+x'
CONVERT_HOTKEY = 'ctrl+win+x'


# --- Menu ----------------------------------------------------------------

def build_menu(backend):
    """The tray menu for a backend.

    Run Clipboard is the default (Enter) action. The backend's extra items
    (the Distributed submenu) slot in after \"Install && Run on Boot\" —
    the local backend contributes nothing, so it never appears there.
    """
    item = platform.item
    return (
        item('Run Clipboard', lambda icon, i: backend.on_hotkey(icon), default=True),
        item('Install && Run on Boot',
             lambda icon, i: backend.toggle_startup(icon, i),
             checked=lambda i: backend.check_startup()),
    ) + tuple(backend.extra_menu_items(None)) + (
        item('Force Stop All Runs', backend.force_stop_all),
        item('Clear Image Cache', backend.purge_cache),
        item('About', lambda icon, i: show_about(backend, icon, i)),
        item('Quit', lambda icon, i: backend.quit(icon, i)),
    )


def show_about(backend, icon, item_unused=None):
    # About needs review time, so it opens in a terminal window (kept
    # open until dismissed) rather than a vanishing toast. The text also
    # goes to the clipboard so it can be pasted into a ticket.
    text = backend.about()
    if HAS_GUI:
        platform.pyperclip.copy(text)
    platform.show_terminal_window(f"About {backend.display_name}", text)


# --- Modes ---------------------------------------------------------------

def setup_tray_mode(icon, backend):
    """Standard Mode: Persistent Tray Icon."""
    icon.visible = True
    keyboard = platform.keyboard
    keyboard.add_hotkey(HOTKEY, lambda: backend.on_hotkey(icon))
    keyboard.add_hotkey(CONVERT_HOTKEY, lambda: backend.on_convert_hotkey(icon))
    backend.setup_tray(icon)
    icon.notify(backend.startup_message(), title="Ephemeral")


def setup_oneshot_mode(icon, backend, file_path):
    """One-Shot Mode: Run file, respect backend state, then exit."""
    icon.visible = True

    def auto_run_sequence():
        token = backend.prepare_run(icon)
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()

            icon.notify(f"Loading {os.path.basename(file_path)}...", title="Ephemeral One-Shot")
            backend.run_logic(icon, content=content)

        except Exception as e:
            icon.notify(f"One-Shot Failed: {e}", title="Ephemeral Error")
            time.sleep(5)

        finally:
            backend.cleanup_run(icon, token)
            icon.stop()
            sys.exit()

    threading.Thread(target=auto_run_sequence).start()


def setup_headless_mode(backend, file_path):
    """Headless CLI Mode: Run file completely unattended, no GUI dependencies."""
    icon = platform.DummyIcon()
    token = backend.prepare_run(icon)

    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()

        icon.notify(f"Headless Mode: Running {os.path.basename(file_path)}...", title="Ephemeral CLI")
        backend.run_logic(icon, content=content)

    except Exception as e:
        icon.notify(f"Headless Failed: {e}", title="Ephemeral Error")

    finally:
        backend.cleanup_run(icon, token)


# --- Entry ---------------------------------------------------------------

def run(backend):
    """Shared entry point: pick a mode, drive the backend through it."""
    # SELF-CHECK: verify the install without a GUI.
    if "--self-check" in sys.argv:
        sys.exit(backend.self_check())

    # Background warmup (distributed: cluster + bash canary; local: none).
    backend.start_background()

    # DETECT MODE
    if len(sys.argv) > 1 and os.path.exists(sys.argv[-1]):
        file_target = sys.argv[-1]
        if "--cli" in sys.argv or "parse" in sys.argv:
            platform.CLI_MODE = True
            setup_headless_mode(backend, file_target)
            backend.shutdown()
            sys.exit(0)
        else:
            if not HAS_GUI:
                print("GUI dependencies not found. Falling back to CLI mode.")
                platform.CLI_MODE = True
                setup_headless_mode(backend, file_target)
                backend.shutdown()
                sys.exit(0)

            import pystray
            image = platform.create_icon_image()
            menu = build_menu(backend)
            icon = pystray.Icon(backend.app_key, image, backend.display_name, menu)
            icon.run(lambda icon: setup_oneshot_mode(icon, backend, file_target))
    else:
        if not HAS_GUI:
            print("GUI dependencies not found. CLI mode requires a file argument.")
            backend.shutdown()
            sys.exit(1)

        import pystray
        image = platform.create_icon_image()
        menu = build_menu(backend)
        icon = pystray.Icon(backend.app_key, image, backend.display_name, menu)
        icon.run(lambda icon: setup_tray_mode(icon, backend))
