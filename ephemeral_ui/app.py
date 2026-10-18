"""Shared application entry point for the Ephemeral desktop builds."""
from __future__ import annotations

from typing import Literal

from ephemeral_ui import tray

#: ``instructor`` is the classroom build: the same front end, but a node that
#: never joins the public swarm (see
#: ``ephemeral_ui.backends.distributed.InstructorBackend``).
BuildMode = Literal["local", "distributed", "instructor"]


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
    if mode == "instructor":
        from ephemeral_ui.backends.distributed import InstructorBackend

        tray.run(InstructorBackend())
        return
    raise ValueError(f"unsupported build mode: {mode!r}")
