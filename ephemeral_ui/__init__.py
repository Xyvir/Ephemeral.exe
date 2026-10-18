"""
Ephemeral unified desktop UI.

One front end (``ephemeral_ui.tray``) drives both Windows/Linux tray
clients. The front end calls a small set of generic functions on a
:class:`~ephemeral_ui.backends.base.Backend`; ``ephemeral_ui.app`` selects
the backend from an explicit build mode:

* ``local``       — local Podman execution
* ``distributed`` — the iroh cluster (per-user node)
* ``instructor``  — the classroom build: the distributed client without the
                    public swarm, so the node is its own private swarm or local

The entry points (``main_local.py``, ``main_distributed_client.py``,
``main_distributed_instructor.py``) are thin, immutable mode selectors. They never infer the mode from the
executable filename or a user-controlled setting.
"""
