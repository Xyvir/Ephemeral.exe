"""
Hydrate a thick node with every language-map image — a one-shot "super-seed".

Turns an always-on thick server / gateway into a warm node: once this
script completes, the box has every image the cluster's language map can
request, so offloaded jobs land on it without a registry pull, and its
hello frames advertise the full warm set (which is what makes
nearest-neighbor routing prefer it). Run once at install time, and
re-run whenever languages are added to the map.

The image set comes from ``ephemeral_core.config.mapped_images()`` — the
exact same source the receiver-side allowlist
(``ephemeral_net.sandbox.default_image_allowlist``) is derived from, so
this hydrates precisely what remote jobs are allowed to request.

Robustness:
* starts the Podman machine/socket if needed (same logic as the engine)
* skips images already cached locally
* retries each pull with backoff; a failing image never aborts the run
* optional parallelism (--parallel) for hosts that can pull concurrently
* prints a per-image progress line and a pulled/skipped/failed summary
* exits 1 when any pull failed, so automation notices

Usage:
    python scripts/hydrate_images.py [--dry-run] [--only python,node] \\
                                     [--parallel N] [--retries N] [--timeout SEC]
"""
from __future__ import annotations

import argparse
import asyncio
import subprocess
import sys
import time
from pathlib import Path

# ``python scripts/hydrate_images.py`` puts ``scripts/`` on sys.path, not
# the repo root — add the root so ``ephemeral_core`` imports.
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from ephemeral_core.config import LANG_MAP, mapped_images
from ephemeral_core.executor import (
    check_image_exists,
    check_podman_alive,
    ensure_podman_running,
    get_startupinfo,
    host_arch,
)


def _run_podman(args: list[str], timeout: int | None = None) -> int:
    """Run a podman subcommand headlessly; return its exit code."""
    try:
        result = subprocess.run(
            ["podman", *args],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            startupinfo=get_startupinfo(),
            timeout=timeout,
        )
        return result.returncode
    except Exception:
        return -1


def _resolve_langs(names: list[str]) -> list[str]:
    """Resolve ``--only`` language names (aliases included) to images."""
    images: list[str] = []
    for name in names:
        lang = name.strip().lower()
        if lang not in LANG_MAP:
            raise SystemExit(f"unknown language: {name!r}")
        while isinstance(LANG_MAP[lang], str):  # follow aliases
            lang = LANG_MAP[lang]
        entry = LANG_MAP[lang]
        if isinstance(entry, dict) and entry.get("image"):
            images.append(entry["image"])
    return images


async def pull_with_retry(image: str, retries: int, timeout: int) -> bool:
    """Pull one image, retrying with backoff. True on success."""
    for attempt in range(1, retries + 1):
        rc = await asyncio.to_thread(
            _run_podman, ["pull", "--platform", f"linux/{host_arch()}", image], timeout
        )
        if rc == 0:
            return True
        if attempt < retries:
            await asyncio.sleep(5 * attempt)  # 5s, 10s, ...
    return False


async def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="list images that would be pulled without pulling anything",
    )
    parser.add_argument(
        "--only",
        default="",
        help="comma-separated languages to hydrate (default: all mapped images)",
    )
    parser.add_argument(
        "--parallel",
        type=int,
        default=1,
        help="max concurrent pulls (default 1 — registry rate limits)",
    )
    parser.add_argument(
        "--retries",
        type=int,
        default=3,
        help="pull attempts per image (default 3)",
    )
    parser.add_argument(
        "--timeout",
        type=int,
        default=900,
        help="seconds per pull attempt (default 900)",
    )
    parser.add_argument(
        "--refresh",
        action="store_true",
        help="re-pull images even when already cached",
    )
    args = parser.parse_args()

    if args.only:
        images = _resolve_langs(args.only.split(","))
    else:
        images = mapped_images()
    if not images:
        print("No images to hydrate — nothing to do.")
        return 0

    if args.dry_run:
        print(f"{len(images)} image(s) to hydrate:")
        for img in images:
            print(f"  - {img}")
        return 0

    if not check_podman_alive():
        print("Podman is not responding — starting it...")
        try:
            await ensure_podman_running()
        except RuntimeError as e:
            print(f"FATAL: {e}", file=sys.stderr)
            return 2
        if not check_podman_alive():
            print("FATAL: Podman still unavailable after start attempt.", file=sys.stderr)
            return 2
    print("Podman OK")

    todo = [img for img in images if args.refresh or not check_image_exists(img)]
    skipped = len(images) - len(todo)
    if not todo:
        print(f"All {len(images)} image(s) already cached — nothing to pull.")
        return 0
    print(
        f"{len(todo)} image(s) to pull, {skipped} already cached "
        f"(parallel={max(1, args.parallel)}, retries={args.retries})"
    )

    sem = asyncio.Semaphore(max(1, args.parallel))
    done = 0
    failed: list[str] = []
    started = time.monotonic()

    async def worker(image: str) -> None:
        nonlocal done
        async with sem:
            ok = await pull_with_retry(image, args.retries, args.timeout)
        done += 1
        if ok:
            print(f"  [{done}/{len(todo)}] ok      {image}", flush=True)
        else:
            failed.append(image)
            print(f"  [{done}/{len(todo)}] FAILED  {image}", file=sys.stderr, flush=True)

    await asyncio.gather(*(worker(img) for img in todo))

    elapsed = time.monotonic() - started
    print()
    print(
        f"done in {elapsed:.0f}s: {len(todo) - len(failed)} pulled, "
        f"{skipped} already cached, {len(failed)} failed"
    )
    for img in failed:
        print(f"  FAILED: {img}", file=sys.stderr)
    return 1 if failed else 0


if __name__ == "__main__":
    raise SystemExit(asyncio.run(main()))
