"""
ephemeral_core — Platform-agnostic code execution engine.

This package provides the core Podman orchestration, Markdown code-block
parsing, and artifact extraction logic for Ephemeral. It is designed to
be driven by either:

  - main_api.py  (FastAPI server for remote execution)
  - main_local.py (Windows tray client for local execution)

Public API:
    parse_and_execute(markdown_text, timeout) -> ExecutionResult
    check_podman_alive() -> bool
    ensure_podman_running() -> None
    parse_codeblocks(content) -> list[dict]
    pull_image(image_name) -> int
"""

from .models import ExecutionResult, GroupResult, BlockResult
from .executor import (
    parse_and_execute,
    check_podman_alive,
    check_image_exists,
    list_local_images,
    ensure_podman_running,
    pull_image,
)
from .parser import parse_codeblocks

__all__ = [
    "parse_and_execute",
    "check_podman_alive",
    "check_image_exists",
    "list_local_images",
    "ensure_podman_running",
    "parse_codeblocks",
    "pull_image",
    "ExecutionResult",
    "GroupResult",
    "BlockResult",
]
