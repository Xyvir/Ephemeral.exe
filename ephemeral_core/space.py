"""
Disk-space guardrails for image pulls.

Before pulling a container image, Ephemeral checks that the drive backing
podman's storage can hold it — with a safety margin (default 2x the
estimated download size). When space is short, the **coldest** cached
images (least-recently used, tracked in a small per-user state file) are
evicted first. Only if that still isn't enough does the pull refuse with
:class:`SpaceGuardError` instead of filling the disk.

This is the shared guardrail for every entry point that downloads an
image:

* the backend API server (``main_api.py`` -> ``ephemeral_core.pull_image``)
* the local tray client (``main_local.py`` -> ``ephemeral_core.pull_image``)
* distributed nodes (``main_distributed.py`` / ``ephemeral_net.offload``
  background pulls -> ``ephemeral_core.pull_image``)
* the distributed client's pre-hydration (``main_distributed_client.py``)

Everything is best-effort: if podman is unreachable or the free-space
probe fails, the pull proceeds as before rather than being blocked.

Configuration:

    EPHEMERAL_SPACE_MARGIN   safety margin as a multiplier of the image's
                             estimated size (default ``2.0``)
    EPHEMERAL_STORAGE_ROOT   host path backing podman's storage, used when
                             ``podman info`` is unavailable
"""
from __future__ import annotations

import json
import logging
import os
import re
import shutil
import subprocess
import tempfile
import threading
import time
from datetime import datetime
from pathlib import Path

logger = logging.getLogger("ephemeral_core.space")

#: Default safety margin: require 2x the estimated download size free.
SPACE_MARGIN = float(os.getenv("EPHEMERAL_SPACE_MARGIN", "2.0"))

#: Conservative estimate (bytes) when the image's real size can't be
#: determined — a science/typesetting image can be multi-GB, so never
#: assume small.
DEFAULT_IMAGE_EST_BYTES = 2 * 2**30

#: How long to cache a registry size probe for a given image.
_SIZE_CACHE_TTL = 600.0

#: Fallback storage roots (rootless podman default) used when
#: ``podman info`` is unavailable.
_DEFAULT_STORAGE_ROOTS = (
    os.path.expanduser("~/.local/share/containers/storage"),
)

_size_cache: dict[str, tuple[float, int | None]] = {}
_usage_lock = threading.Lock()

_UNITS = {
    "b": 1,
    "kb": 2**10,
    "mb": 2**20,
    "gb": 2**30,
    "tb": 2**40,
}


class SpaceGuardError(RuntimeError):
    """Raised when a pull is refused because there isn't enough disk space."""


# --- podman plumbing ----------------------------------------------------

def _podman(argv: list[str], timeout: int = 30) -> subprocess.CompletedProcess:
    """Run a podman command headlessly (best-effort)."""
    startupinfo = None
    if hasattr(subprocess, "STARTUPINFO"):
        startupinfo = subprocess.STARTUPINFO()
        startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
    return subprocess.run(
        ["podman", *argv],
        capture_output=True, text=True,
        timeout=timeout, startupinfo=startupinfo,
    )


def storage_root() -> str | None:
    """Host path backing podman's storage (GraphRoot), or None if unknown."""
    try:
        out = _podman(["info", "--format", "{{.Store.GraphRoot}}"])
        root = out.stdout.strip()
        if root and os.path.exists(root):
            return root
    except Exception:
        pass
    # Fallbacks: an explicit override, then env-provided home dirs (same
    # drive as the storage in practice), then the rootless default.
    for candidate in (
        os.environ.get("EPHEMERAL_STORAGE_ROOT"),
        os.environ.get("LOCALAPPDATA"),
        os.environ.get("HOME"),
        os.environ.get("SystemDrive"),
    ):
        if candidate:
            return candidate
    for root in _DEFAULT_STORAGE_ROOTS:
        if os.path.exists(root):
            return root
    return None


def free_bytes(path: str | None = None) -> int | None:
    """Free bytes on the drive backing podman storage, or None if unknown."""
    root = path or storage_root()
    if not root:
        return None
    try:
        return shutil.disk_usage(root).free
    except Exception:
        return None


# --- image size estimation ----------------------------------------------

def parse_human_size(s: str | None) -> int | None:
    """Parse a podman human size string (``"1.23 GB"``) into bytes."""
    if not s:
        return None
    m = re.match(r"^([\d.]+)\s*([kmgt]?i?b)?$", s.strip(), re.IGNORECASE)
    if not m:
        return None
    try:
        value = float(m.group(1))
    except ValueError:
        return None
    unit = (m.group(2) or "b").lower().replace("i", "")  # GiB -> gb
    return int(value * _UNITS[unit])


def estimate_pull_size(image: str, *, podman=None) -> int | None:
    """Estimated compressed download size (bytes) for ``image``, or None.

    Uses ``podman manifest inspect`` (registry metadata — no pull). The
    result is cached briefly; failures return None so callers fall back
    to a conservative default.
    """
    now = time.monotonic()
    cached = _size_cache.get(image)
    if cached and now - cached[0] < _SIZE_CACHE_TTL:
        return cached[1]
    size = _probe_pull_size(image, podman=podman)
    _size_cache[image] = (now, size)
    return size


def _probe_pull_size(image: str, *, podman=None) -> int | None:
    run = podman or _podman
    try:
        out = run(["manifest", "inspect", image])
        if out.returncode != 0:
            return None
        data = json.loads(out.stdout or "{}")
    except Exception:
        return None
    if not isinstance(data, dict):
        return None
    # Single-arch manifest: layers carry the compressed sizes.
    layers = data.get("layers")
    if isinstance(layers, list):
        total = 0
        for layer in layers:
            size = layer.get("size") if isinstance(layer, dict) else None
            if isinstance(size, (int, float)):
                total += int(size)
        return total or None
    # Manifest list: probe the platform-specific manifest (--verbose).
    if isinstance(data.get("manifests"), list):
        try:
            out = run(["manifest", "inspect", "--verbose", image])
            if out.returncode != 0:
                return None
            verb = json.loads(out.stdout or "{}")
        except Exception:
            return None
        entries = verb.get("manifests") if isinstance(verb, dict) else verb
        total = 0
        found = False
        for entry in entries or []:
            img = entry.get("ImageData") if isinstance(entry, dict) else None
            for layer in (img.get("Layers") if isinstance(img, dict) else []) or []:
                size = layer.get("size") if isinstance(layer, dict) else None
                if isinstance(size, (int, float)):
                    total += int(size)
                    found = True
        return total if found else None
    return None


# --- LRU usage tracking ------------------------------------------------

def _usage_path() -> Path:
    if os.name == "nt":
        base = os.environ.get("LOCALAPPDATA") or os.path.expanduser("~")
    else:
        base = os.environ.get("XDG_STATE_HOME") or os.path.expanduser("~/.local/state")
    return Path(base) / "ephemeral" / "image_usage.json"


def load_usage() -> dict[str, float]:
    """Last-used epoch per image name, from the per-user state file."""
    try:
        data = json.loads(_usage_path().read_text(encoding="utf-8"))
        if isinstance(data, dict):
            return {k: float(v) for k, v in data.items()
                    if isinstance(v, (int, float))}
    except Exception:
        pass
    return {}


def save_usage(usage: dict[str, float]) -> None:
    """Persist the usage map atomically (temp file + rename)."""
    try:
        path = _usage_path()
        path.parent.mkdir(parents=True, exist_ok=True)
        fd, tmp = tempfile.mkstemp(dir=str(path.parent), suffix=".tmp")
        try:
            with os.fdopen(fd, "w", encoding="utf-8") as f:
                json.dump(usage, f)
            os.replace(tmp, path)
        finally:
            if os.path.exists(tmp):
                try:
                    os.unlink(tmp)
                except OSError:
                    pass
    except Exception as e:
        logger.warning("could not persist image usage: %s", e)


def touch_image(image: str) -> None:
    """Record that ``image`` was used now (the LRU eviction signal)."""
    if not image:
        return
    with _usage_lock:
        usage = load_usage()
        usage[image] = time.time()
        save_usage(usage)


# --- cached image inventory ---------------------------------------------

def _parse_created(s) -> float | None:
    """Parse a podman image creation timestamp into an epoch, or None."""
    if not s:
        return None
    try:
        return datetime.fromisoformat(
            s.replace(" UTC", "+00:00").replace("Z", "+00:00")
        ).timestamp()
    except Exception:
        return None


def list_images(*, podman=None) -> list[dict]:
    """Cached images as ``[{name, created, size_bytes}]``, best-effort."""
    run = podman or _podman
    try:
        out = run(["images", "--format", "json"])
        if out.returncode != 0:
            return []
        entries = json.loads(out.stdout or "[]")
    except Exception:
        return []
    images: list[dict] = []
    for entry in entries if isinstance(entries, list) else []:
        names = entry.get("Names") or []
        name = names[0] if names else None
        if not name:
            continue
        created = _parse_created(entry.get("Created") or entry.get("CreatedAt"))
        images.append({
            "name": name,
            "created": created,
            "size_bytes": parse_human_size(entry.get("Size")) or 0,
        })
    return images


def eviction_order(
    usage: dict[str, float],
    images: list[dict],
    exclude: set[str] | None = None,
) -> list[str]:
    """Cached image names, coldest first (LRU), for eviction.

    Images with a recorded last-use sort by that timestamp (oldest
    first); never-used images sort after them by creation time (oldest
    first, unknown creation last). ``exclude`` names are never returned.
    """
    exclude = exclude or set()
    used: list[tuple[float, str]] = []
    unused: list[tuple[float | None, str]] = []
    for img in images:
        name = img.get("name")
        if not name or name in exclude:
            continue
        ts = usage.get(name)
        if ts is not None:
            used.append((ts, name))
        else:
            unused.append((img.get("created"), name))
    used.sort(key=lambda t: (t[0], t[1]))
    unused.sort(key=lambda t: (t[0] if t[0] is not None else float("inf"), t[1]))
    return [name for _, name in used] + [name for _, name in unused]


def evict_coldest(
    need_bytes: int,
    *,
    exclude: set[str] | None = None,
    podman=None,
    disk_free=None,
) -> list[str]:
    """Remove cached images coldest-first until ``need_bytes`` is free.

    Returns the names removed. Stops early once the drive has room and
    never removes images podman refuses (e.g. in use by a container).
    """
    run = podman or _podman
    free = (disk_free or free_bytes)()
    if free is None or free >= need_bytes:
        return []
    removed: list[str] = []
    order = eviction_order(load_usage(), list_images(podman=run), exclude)
    for name in order:
        if free is None or free >= need_bytes:
            break
        try:
            res = run(["rmi", name])
        except Exception:
            continue
        if res.returncode != 0:
            continue
        removed.append(name)
        free = (disk_free or free_bytes)()
    return removed


# --- the guardrail ------------------------------------------------------

def required_bytes(est_bytes: int | None, margin: float | None = None) -> int:
    """Free space a pull needs: the image size times the safety margin."""
    margin = margin if margin is not None else SPACE_MARGIN
    est = est_bytes if est_bytes and est_bytes > 0 else DEFAULT_IMAGE_EST_BYTES
    return max(1, int(est * max(margin, 1.0)))


def ensure_space_for_pull(
    image: str,
    *,
    margin: float | None = None,
    podman=None,
    disk_free=None,
) -> None:
    """Ensure room for ``image`` before pulling; evict coldest if needed.

    Best-effort: when free space can't be determined the call returns
    without blocking (the pull proceeds as before). Raises
    :class:`SpaceGuardError` when even after evicting every evictable
    cached image there isn't ``margin`` x the image's size free.
    """
    margin = margin if margin is not None else SPACE_MARGIN
    run = podman or _podman
    free = (disk_free or free_bytes)()
    if free is None:
        logger.warning(
            "could not determine free space for %s — pull proceeds unguarded",
            image,
        )
        return
    need = required_bytes(estimate_pull_size(image, podman=run), margin)
    if free >= need:
        return
    removed = evict_coldest(
        need, exclude={image}, podman=run, disk_free=disk_free,
    )
    free = (disk_free or free_bytes)()
    if free is not None and free >= need:
        if removed:
            logger.info(
                "evicted %d cold image(s) to make room for %s: %s",
                len(removed), image, ", ".join(removed),
            )
        return
    raise SpaceGuardError(
        f"Not enough disk space to pull {image}: need "
        f"{need / 2**30:.1f} GiB free ({margin:.1f}x safety margin), "
        f"only {free / 2**30:.1f} GiB available after evicting "
        f"{len(removed)} cold image(s)."
    )


__all__ = [
    "DEFAULT_IMAGE_EST_BYTES",
    "SPACE_MARGIN",
    "SpaceGuardError",
    "ensure_space_for_pull",
    "estimate_pull_size",
    "evict_coldest",
    "eviction_order",
    "free_bytes",
    "list_images",
    "load_usage",
    "parse_human_size",
    "required_bytes",
    "save_usage",
    "storage_root",
    "touch_image",
]
