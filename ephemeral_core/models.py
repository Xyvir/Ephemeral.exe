"""
Standardized data models for Ephemeral execution results.

These dataclasses define the contract between ephemeral_core and its callers
(main_api.py, main_local.py). All execution results flow through these types.
"""
from __future__ import annotations
from dataclasses import dataclass, field


@dataclass
class BlockResult:
    """Result of executing a single code block within a run."""
    step_index: int
    language: str
    stdout: str
    stderr: str
    exit_code: int


@dataclass
class GroupResult:
    """Result of a single container run (one language group, possibly multi-step)."""
    stdout_formatted: str          # Markdown-formatted output (## <Lang> Result / ## <Lang> Run N)
    stderr: str
    exit_code: int
    artifact_paths: list[str] = field(default_factory=list)
    chained_files: list[dict] = field(default_factory=list)
    image_copied: bool = False     # Only relevant for local mode (image-to-clipboard)


@dataclass
class ExecutionResult:
    """
    Aggregate result returned by parse_and_execute().
    
    This is the top-level return type that callers consume.
    - `stdout` contains the full markdown-formatted output of all runs.
    - `artifact_paths` lists absolute paths to files generated in `artifact_dir`.
    - The caller is responsible for cleaning up `artifact_dir` after use.
    """
    stdout: str                              # Aggregate formatted stdout
    stderr: str                              # Aggregate stderr
    exit_code: int                           # 0 if all runs succeeded
    artifact_paths: list[str] = field(default_factory=list)
    artifact_dir: str | None = None          # Temp dir containing artifacts; caller cleans up
    image_copied: bool = False               # True if a single image was produced (local mode hint)
    blocks: list[BlockResult] = field(default_factory=list)
