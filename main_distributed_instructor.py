"""Ephemeral Distributed Instructor — explicit instructor-mode tray entry point.

The classroom build: the same tray as the distributed client, but its node
never joins the public swarm. It is either its own private swarm (created on
first run) or local-only — see ``ephemeral_ui.backends.distributed``.

This executable is also the first package in the setup bundle, which runs it
with ``--setup-prereqs``. Enabling WSL2 needs elevation and usually a reboot, so
the machine-level half of setup belongs to the installer the user has already
let elevate, not to the tray. That path prints a state and exits without ever
starting a tray, and returns 3010 so Burn performs the restart it implies.
"""
from __future__ import annotations

import sys


if __name__ == "__main__":
    if "--setup-prereqs" in sys.argv:
        from ephemeral_ui import prereq

        raise SystemExit(prereq.cli())
    if "--setup-dialog" in sys.argv:
        # Child process for the tkinter setup dialogs (see ephemeral_ui.setup_gui).
        # Dispatched before the tray is imported, so no tray code loads here.
        from ephemeral_ui import setup_gui

        idx = sys.argv.index("--setup-dialog")
        raise SystemExit(setup_gui.child_main(sys.argv[idx + 1:]))

    from ephemeral_ui.app import run

    run("instructor")
