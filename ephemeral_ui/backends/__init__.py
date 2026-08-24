"""
Backend implementations for the unified tray front end.

This package deliberately imports nothing: each entry point pulls in only
its own backend, so the local build never drags in the networking tier
(``ephemeral_net`` / ``iroh``). Import them directly:

    from ephemeral_ui.backends.local import LocalBackend
    from ephemeral_ui.backends.distributed import DistributedBackend
"""
