"""Wire models shared by Ephemeral's REST and MCP server surfaces."""
from __future__ import annotations

from pydantic import BaseModel


class RunResponse(BaseModel):
    """
    Canonical result payload for a Markdown execution.

    The same five fields are returned by REST and the trusted MCP tool.
    """

    exit_code: int
    stdout: str
    stderr: str
    artifact_file: str | None = None
    artifact_ext: str | None = None
