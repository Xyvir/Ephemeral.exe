"""Launch the instructor build's setup dialogs and read their answers.

The tray never imports tkinter. A dialog runs in a child process — this same
executable (frozen) or ``main_distributed_instructor.py`` (source) started with
``--setup-dialog KIND IN.json OUT.json`` — and its answer comes back through
``OUT.json``. A child that cannot load tkinter exits with
:data:`EXIT_NO_GUI`; every failure reads as "no GUI", so callers fall back to the
existing console prompts rather than leaving setup stuck.
"""
from __future__ import annotations

import importlib.util
import json
import os
import subprocess
import sys
import tempfile
from pathlib import Path

EXIT_NO_GUI = 2
_DIALOG_TIMEOUT = 3600


def available() -> bool:
    """True when tkinter is importable here. Cheap: nothing is imported."""
    return importlib.util.find_spec("tkinter") is not None


def _child_argv() -> list[str]:
    if getattr(sys, "frozen", False):
        return [sys.executable]
    entry = Path(__file__).resolve().parent.parent / "main_distributed_instructor.py"
    return [sys.executable, str(entry)]


def _run(kind: str, payload: dict) -> dict | None:
    fd_in, in_path = tempfile.mkstemp(suffix=".json")
    os.close(fd_in)
    fd_out, out_path = tempfile.mkstemp(suffix=".json")
    os.close(fd_out)
    try:
        Path(in_path).write_text(json.dumps(payload), encoding="utf-8")
        proc = subprocess.run(
            _child_argv() + ["--setup-dialog", kind, in_path, out_path],
            timeout=_DIALOG_TIMEOUT,
        )
        if proc.returncode != 0:
            return None
        return json.loads(Path(out_path).read_text(encoding="utf-8") or "null")
    except (OSError, subprocess.SubprocessError, ValueError):
        return None
    finally:
        for path in (in_path, out_path):
            try:
                os.unlink(path)
            except OSError:
                pass


def ask_seed(prompt: str) -> str | None:
    """The create-or-join question. ``""`` = create new; ``None`` = no answer."""
    result = _run("seed", {"prompt": prompt})
    if not isinstance(result, dict) or result.get("cancelled"):
        return None
    answer = result.get("answer")
    return answer if isinstance(answer, str) else None


def ask_images(images: list[str], free_bytes: int, drive: str,
               cap_gb: float | None) -> dict | None:
    """The pre-hydration picker. ``None`` when no GUI answered."""
    result = _run("images", {
        "images": images,
        "free_bytes": free_bytes,
        "drive": drive,
        "cap_gb": cap_gb,
    })
    if not isinstance(result, dict) or not isinstance(result.get("images"), list):
        return None
    return result


def child_main(argv: list[str]) -> int:
    """Entry for ``--setup-dialog KIND IN OUT`` in the child process."""
    if len(argv) != 3:
        return 64
    kind, in_path, out_path = argv
    try:
        from ephemeral_ui import setup_dialogs
    except Exception:
        return EXIT_NO_GUI
    code = setup_dialogs.run(kind, in_path, out_path)
    # Exit at once: a size probe may still be inside a podman call, and a
    # short-lived helper has nothing to wait for.
    sys.stdout.flush()
    os._exit(code)
