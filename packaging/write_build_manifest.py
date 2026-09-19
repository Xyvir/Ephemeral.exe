#!/usr/bin/env python3
"""Write reproducibility metadata and hashes for a CI build."""
from __future__ import annotations

import argparse
import hashlib
import json
import os
import platform
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def command_output(*command: str) -> str:
    try:
        return subprocess.check_output(command, text=True, stderr=subprocess.STDOUT).strip()
    except (OSError, subprocess.CalledProcessError):
        return ""


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--build-version", default="")
    parser.add_argument("--build-command", default="")
    parser.add_argument("--artifact", action="append", default=[], type=Path)
    args = parser.parse_args()

    artifacts = []
    for path in args.artifact:
        if not path.is_file():
            raise SystemExit(f"artifact does not exist: {path}")
        artifacts.append({
            "path": path.as_posix(),
            "size_bytes": path.stat().st_size,
            "sha256": sha256(path),
        })

    manifest = {
        "manifest_version": 1,
        "generated_at_utc": datetime.now(timezone.utc).isoformat(),
        "source_commit": os.environ.get("GITHUB_SHA", ""),
        "source_ref": os.environ.get("GITHUB_REF", ""),
        "workflow_run_id": os.environ.get("GITHUB_RUN_ID", ""),
        "build_version": args.build_version,
        "runner": {
            "os": platform.platform(),
            "python": sys.version,
            "python_executable": sys.executable,
        },
        "toolchain": {
            "pip": command_output(sys.executable, "-m", "pip", "--version"),
            "pyinstaller": command_output(sys.executable, "-m", "PyInstaller", "--version"),
        },
        "installed_packages": command_output(sys.executable, "-m", "pip", "freeze").splitlines(),
        "build_command": args.build_command,
        "artifacts": artifacts,
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(manifest, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
