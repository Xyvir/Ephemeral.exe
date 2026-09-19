"""
Backend implementations for the unified tray front end.

This package deliberately imports nothing. ``ephemeral_ui.app`` selects
one backend from an explicit immutable build mode; local builds may bundle
the distributed tier for parity, but never initialize it in local mode.
Import implementations directly only when needed:

    from ephemeral_ui.backends.local import LocalBackend
    from ephemeral_ui.backends.distributed import DistributedBackend
"""
