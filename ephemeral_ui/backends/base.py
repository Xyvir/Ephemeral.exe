"""
Backend contract for the unified Ephemeral tray front end.

A backend is a bundle of tier-specific behavior behind a small generic
interface. The front end (``ephemeral_ui.tray``) only ever calls these
methods — it never imports ``ephemeral_core``/``ephemeral_net`` directly,
which keeps the local build free of the networking tier.

Two implementations live next door:

* :class:`ephemeral_ui.backends.local.LocalBackend` — routes execution to
  local Podman (clipboard -> sandbox -> clipboard/Downloads).
* :class:`ephemeral_ui.backends.distributed.DistributedBackend` — routes
  execution through the iroh cluster (per-user node with warmest-neighbor
  offloading).

The base class supplies everything the two tiers share: the login
autostart toggle, the convert-hotkey, the hotkey thread wrapper, the
no-op run lifecycle, and the empty extra-menu.
"""
from __future__ import annotations

import threading
from abc import ABC, abstractmethod

from ephemeral_ui.platform import StartupManager


class Backend(ABC):
    """Generic contract the unified tray front end drives."""

    #: pystray icon/app identity (registry Run key, install dir, icon name).
    app_key: str
    #: Human name shown in the tray tooltip / About title.
    display_name: str

    def __init__(self) -> None:
        # The autostart entry copies the *entry point* script/exe the user
        # launched (``sys.argv[0]``) — never this shared module.
        self.startup = StartupManager(self.app_key, self.display_name)

    # --- identity --------------------------------------------------------

    @abstractmethod
    def about(self) -> str:
        """Full About text (version placeholder is CI-injected)."""

    @abstractmethod
    def startup_message(self) -> str:
        """Toast shown when the tray finishes starting."""

    # --- execution -------------------------------------------------------

    @abstractmethod
    def run_logic(self, icon, content=None) -> None:
        """Run clipboard content (or ``content``) through this backend."""

    def on_hotkey(self, icon) -> None:
        """Hotkey handler — spawns a worker thread (default impl)."""
        def hotkey_task():
            self.run_logic(icon)
        threading.Thread(target=hotkey_task).start()

    def on_convert_hotkey(self, icon) -> None:
        """Clipboard -> seed-block converter (identical for both tiers)."""
        from ephemeral_ui import platform
        platform.on_convert_hotkey(icon)

    # --- run lifecycle (one-shot / headless) -----------------------------

    def prepare_run(self, icon):
        """Called before a one-shot/headless run; returns a cleanup token.

        Local: ensures Podman is up, returns whether it was already
        running so cleanup can restore that state. Distributed: no-op.
        """
        return None

    def cleanup_run(self, icon, token) -> None:
        """Tear down after a one-shot/headless run (inverse of prepare)."""

    def shutdown(self) -> None:
        """Teardown on process exit without a GUI icon (idempotent)."""

    # --- tray lifecycle --------------------------------------------------

    def start_background(self) -> None:
        """Background threads spawned at process start (before the icon)."""

    def setup_tray(self, icon) -> None:
        """Per-backend tray-mode setup (needs the live icon)."""

    @abstractmethod
    def quit(self, icon, item_unused=None) -> None:
        """Quit from the tray menu."""

    # --- maintenance -----------------------------------------------------

    @abstractmethod
    def force_stop_all(self, icon, item_unused=None) -> None:
        """Kill active runs/containers."""

    @abstractmethod
    def purge_cache(self, icon, item_unused=None) -> None:
        """Clear the image cache."""

    def extra_menu_items(self, icon) -> tuple:
        """Extra pystray items/submenus after \"Install && Run on Boot\".

        Local returns nothing (the Distributed submenu must not appear);
        distributed returns its \"Distributed\" submenu.
        """
        return ()

    # --- login autostart (shared) ----------------------------------------

    def set_startup(self, enable, icon=None) -> None:
        self.startup.set_startup(enable, icon)

    def check_startup(self) -> bool:
        return self.startup.check_startup()

    def toggle_startup(self, icon, item_unused=None) -> None:
        is_enabled = self.check_startup()
        self.set_startup(not is_enabled, icon)

    # --- install verification --------------------------------------------

    @abstractmethod
    def self_check(self) -> int:
        """Headless install check; returns a process exit code."""
