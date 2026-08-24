"""
Ephemeral Local Client — Windows tray application for clipboard-driven code execution.

This is now a thin entry point: the tray front end and the local backend
live in the ``ephemeral_ui`` package.

* ``ephemeral_ui.tray``            — the unified tray front end (menu,
  hotkeys, tray/one-shot/headless modes)
* ``ephemeral_ui.backends.local``  — the local backend (clipboard ->
  Podman sandbox -> clipboard/Downloads)

Usage:
    python main_local.py                    # Tray mode (persistent, hotkey-driven)
    python main_local.py script.md          # One-shot mode (run file and exit)
    python main_local.py --cli script.md    # Headless CLI mode (no GUI)
"""
from __future__ import annotations

import sys

from ephemeral_ui import tray
from ephemeral_ui.backends.local import LocalBackend


if __name__ == '__main__':
    tray.run(LocalBackend())
    # Keep a bare import safe (the distributed client used to import
    # helpers from this module; nothing should, anymore).
    sys.exit(0)
