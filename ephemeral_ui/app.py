"""Shared application entry point for the local and distributed tray builds."""
from __future__ import annotations

from typing import Literal

from ephemeral_ui import tray

BuildMode = Literal["local", "distributed"]


def run(mode: BuildMode) -> None:
    """Run the explicitly selected build mode; never infer it from a filename."""
    if mode == "local":
        from ephemeral_ui.backends.local import LocalBackend

        tray.run(LocalBackend())
        return
    if mode == "distributed":
        from ephemeral_ui.backends.distributed import DistributedBackend

        tray.run(DistributedBackend())
        return
    raise ValueError(f"unsupported build mode: {mode!r}")
