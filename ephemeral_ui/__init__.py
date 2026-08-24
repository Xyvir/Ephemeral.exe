"""
Ephemeral unified desktop UI.

One front end (``ephemeral_ui.tray``) drives both Windows/Linux tray
clients. The front end calls a small set of generic functions on a
:class:`~ephemeral_ui.backends.base.Backend`; the backend decides where
those calls land:

* ``ephemeral_ui.backends.local``       — local Podman execution
* ``ephemeral_ui.backends.distributed`` — the iroh cluster (per-user node)

The two entry points (``main_local.py``, ``main_distributed_client.py``)
are thin: they pick a backend and hand it to ``tray.run()``.
"""
