"""
Tests for ephemeral_core.space — the disk-space guardrail for image pulls.

Everything is exercised through injected fakes (a podman runner and a
free-space probe), so no real podman or disk is touched.

Run:  python test_space.py
"""
from __future__ import annotations

import json

from ephemeral_core.space import (
    DEFAULT_IMAGE_EST_BYTES,
    SpaceGuardError,
    ensure_space_for_pull,
    estimate_pull_size,
    evict_coldest,
    eviction_order,
    parse_human_size,
    required_bytes,
)

GIB = 2**30


# --- fakes ---------------------------------------------------------------

class _Res:
    def __init__(self, returncode=0, stdout=""):
        self.returncode = returncode
        self.stdout = stdout


class _FakePodman:
    """Records calls; serves canned `manifest inspect` / `images` / `rmi`."""

    def __init__(self, manifest=None, manifest_verbose=None, images=None,
                 rmi_ok=True, evict_state=None):
        self.calls = []
        self.manifest = manifest
        self.manifest_verbose = manifest_verbose
        self.images = images
        self.rmi_ok = rmi_ok
        self.rmi_failures = 0
        self.evict_state = evict_state

    def __call__(self, argv):
        self.calls.append(list(argv))
        if argv[0] == "manifest" and argv[1] == "inspect":
            if len(argv) > 2 and argv[2] == "--verbose":
                return _Res(stdout=json.dumps(self.manifest_verbose or {}))
            return _Res(stdout=json.dumps(self.manifest or {}))
        if argv[0] == "images":
            return _Res(stdout=json.dumps(self.images or []))
        if argv[0] == "rmi":
            if self.rmi_ok is False:
                self.rmi_failures += 1
                return _Res(returncode=1)
            if self.evict_state is not None:
                self.evict_state["evicted"] += 1
            return _Res()
        return _Res()


class _FreeSpace:
    """Free-space probe that grows as images are evicted."""

    def __init__(self, start, freed_per_eviction, state):
        self.start = start
        self.per = freed_per_eviction
        self.state = state

    def __call__(self):
        return self.start + self.per * self.state["evicted"]


def _usage_state():
    return {"evicted": 0}


def _image_entry(name, created, size_gb):
    """A raw podman ``images --format json`` entry (what list_images parses)."""
    return {
        "Names": [name],
        "Created": f"{created} UTC",
        "Size": f"{size_gb:.2f} GB",
    }


def _norm_entry(name, created_epoch, size_bytes):
    """A normalized entry as ``list_images`` produces (what eviction_order eats)."""
    return {"name": name, "created": created_epoch, "size_bytes": size_bytes}


# --- size parsing / math -------------------------------------------------

def test_parse_human_size():
    assert parse_human_size("1.5 GB") == int(1.5 * GIB)
    assert parse_human_size("512 MB") == 512 * 2**20
    assert parse_human_size("2.5 GiB") == int(2.5 * GIB)
    assert parse_human_size("1024 KB") == 1024 * 2**10
    assert parse_human_size("123") == 123
    assert parse_human_size("bogus") is None
    assert parse_human_size("") is None
    assert parse_human_size(None) is None
    print("PASS: parse_human_size")


def test_required_bytes_margin():
    # 2x margin by default; estimate of None falls back to a conservative
    # multi-GB default rather than assuming small.
    assert required_bytes(1 * GIB, margin=2.0) == 2 * GIB
    assert required_bytes(1 * GIB, margin=1.0) == 1 * GIB
    assert required_bytes(1 * GIB, margin=0.5) == 1 * GIB  # margin floors at 1x
    assert required_bytes(None, margin=2.0) == 2 * DEFAULT_IMAGE_EST_BYTES
    assert required_bytes(0, margin=2.0) == 2 * DEFAULT_IMAGE_EST_BYTES
    print("PASS: required_bytes margin math")


# --- image size estimation -----------------------------------------------

def test_estimate_pull_size_single_arch():
    podman = _FakePodman(manifest={
        "mediaType": "application/vnd.oci.image.manifest.v1+json",
        "layers": [{"size": GIB // 2}, {"size": GIB // 2}],
    })
    assert estimate_pull_size("single:arch", podman=podman) == GIB
    print("PASS: estimate_pull_size single-arch manifest")


def test_estimate_pull_size_manifest_list():
    podman = _FakePodman(
        manifest={"mediaType": "application/vnd.oci.image.index.v1+json",
                  "manifests": [{"digest": "x"}]},
        manifest_verbose={"manifests": [
            {"ImageData": {"Layers": [{"size": GIB}, {"size": GIB}]}},
            {"ImageData": {"Layers": [{"size": GIB}]}},
        ]},
    )
    assert estimate_pull_size("list:arch", podman=podman) == 3 * GIB
    print("PASS: estimate_pull_size manifest list (--verbose)")


def test_estimate_pull_size_failure_is_none():
    podman = _FakePodman(manifest=None)  # manifest inspect returns {}
    assert estimate_pull_size("fail:arch", podman=podman) is None
    print("PASS: estimate_pull_size returns None on probe failure")


def test_estimate_pull_size_cached():
    podman = _FakePodman(manifest={"layers": [{"size": GIB}]})
    assert estimate_pull_size("cached:arch", podman=podman) == GIB
    assert estimate_pull_size("cached:arch", podman=podman) == GIB
    probes = [c for c in podman.calls if c[0] == "manifest"]
    assert len(probes) == 1, "size probe must be cached briefly"
    print("PASS: estimate_pull_size cached within TTL")


# --- LRU eviction ordering -----------------------------------------------

def test_eviction_order_lru_then_created():
    usage = {"img:a": 100.0, "img:b": 200.0}  # a is colder
    images = [
        _norm_entry("img:a", 1000.0, GIB),
        _norm_entry("img:b", 2000.0, GIB),
        # never-used images sort after used ones, by creation time
        _norm_entry("img:new1", 3000.0, GIB),
        _norm_entry("img:new2", 4000.0, GIB),
    ]
    order = eviction_order(usage, images)
    assert order == ["img:a", "img:b", "img:new1", "img:new2"], order
    # Unknown creation time sorts last among the never-used.
    images.append(_norm_entry("img:unknown", None, GIB))
    assert eviction_order(usage, images)[-1] == "img:unknown"
    print("PASS: eviction_order (LRU first, then created, unknown last)")


def test_eviction_order_exclude_and_names():
    images = [_norm_entry("keep", 1000.0, GIB)]
    assert eviction_order({}, images, exclude={"keep"}) == []
    # Entries with no usable name are skipped entirely.
    images.append({"name": "", "created": None, "size_bytes": 0})
    images.append({"created": 1.0, "size_bytes": 0})  # missing name key
    assert eviction_order({}, images, exclude=set()) == ["keep"]
    print("PASS: eviction_order respects exclude + skips nameless")


# --- eviction ------------------------------------------------------------

def test_evict_coldest_frees_room_in_lru_order():
    state = _usage_state()
    podman = _FakePodman(images=[
        _image_entry("img:cold", "2026-01-01 00:00:00", 1.0),
        _image_entry("img:warm", "2026-01-02 00:00:00", 1.0),
    ], evict_state=state)
    free = _FreeSpace(start=0, freed_per_eviction=GIB, state=state)
    # Need 1.5 GiB free; each eviction frees 1 GiB -> exactly two needed.
    removed = evict_coldest(
        int(1.5 * GIB), podman=podman, disk_free=free,
    )
    assert removed == ["img:cold", "img:warm"]
    rm_calls = [c for c in podman.calls if c[0] == "rmi"]
    assert [c[1] for c in rm_calls] == ["img:cold", "img:warm"]
    print("PASS: evict_coldest removes coldest-first until enough room")


def test_evict_coldest_stops_when_enough():
    state = _usage_state()
    podman = _FakePodman(images=[
        _image_entry("img:a", "2026-01-01 00:00:00", 1.0),
        _image_entry("img:b", "2026-01-02 00:00:00", 1.0),
    ], evict_state=state)
    free = _FreeSpace(start=GIB, freed_per_eviction=GIB, state=state)
    # Already 1 GiB free, need 1.2 GiB -> exactly one eviction.
    removed = evict_coldest(int(1.2 * GIB), podman=podman, disk_free=free)
    assert removed == ["img:a"]
    print("PASS: evict_coldest stops as soon as the drive has room")


def test_evict_coldest_ignores_failed_rmi():
    state = _usage_state()
    podman = _FakePodman(images=[
        _image_entry("img:a", "2026-01-01 00:00:00", 1.0),
        _image_entry("img:b", "2026-01-02 00:00:00", 1.0),
    ], rmi_ok=False, evict_state=state)
    free = _FreeSpace(start=0, freed_per_eviction=GIB, state=state)
    removed = evict_coldest(int(1.5 * GIB), podman=podman, disk_free=free)
    assert removed == [], "failed rmi must not count as space freed"
    assert podman.rmi_failures == 2
    print("PASS: evict_coldest skips images podman refuses to remove")


def test_evict_coldest_noop_when_enough_or_unknown():
    state = _usage_state()
    podman = _FakePodman(images=[_image_entry("img:a", "2026-01-01 00:00:00", 1.0)])
    # Enough space -> nothing evicted.
    assert evict_coldest(GIB, podman=podman, disk_free=lambda: 2 * GIB) == []
    # Unknown free space -> nothing evicted (best-effort).
    assert evict_coldest(GIB, podman=podman, disk_free=lambda: None) == []
    assert len(podman.calls) == 0
    print("PASS: evict_coldest no-ops when space is fine or unknown")


# --- the guardrail -------------------------------------------------------

def test_ensure_space_ok_when_enough():
    state = _usage_state()
    podman = _FakePodman(
        manifest={"layers": [{"size": GIB}]},  # 1 GiB image
        images=[_image_entry("img:a", "2026-01-01 00:00:00", 1.0)],
        evict_state=state,
    )
    ensure_space_for_pull(
        "img:latest", margin=2.0, podman=podman,
        disk_free=_FreeSpace(start=10 * GIB, freed_per_eviction=GIB, state=state),
    )
    assert not [c for c in podman.calls if c[0] == "rmi"], "no eviction needed"
    print("PASS: ensure_space_for_pull no-ops with plenty of room")


def test_ensure_space_evicts_then_proceeds():
    state = _usage_state()
    podman = _FakePodman(
        manifest={"layers": [{"size": GIB}]},  # need 2 GiB at 2x margin
        images=[_image_entry("img:cold", "2026-01-01 00:00:00", 1.0)],
        evict_state=state,
    )
    # 0.5 GiB free, each eviction frees 3 GiB -> room after one eviction.
    ensure_space_for_pull(
        "img:latest", margin=2.0, podman=podman,
        disk_free=_FreeSpace(start=GIB // 2, freed_per_eviction=3 * GIB, state=state),
    )
    rm_calls = [c for c in podman.calls if c[0] == "rmi"]
    assert [c[1] for c in rm_calls] == ["img:cold"]
    print("PASS: ensure_space_for_pull evicts coldest to make room")


def test_ensure_space_refuses_when_eviction_not_enough():
    state = _usage_state()
    podman = _FakePodman(
        manifest={"layers": [{"size": GIB}]},  # need 2 GiB at 2x margin
        images=[_image_entry("img:cold", "2026-01-01 00:00:00", 1.0)],
        evict_state=state,
    )
    # 0.5 GiB free, eviction frees only 1 GiB -> still short of 2 GiB.
    try:
        ensure_space_for_pull(
            "img:latest", margin=2.0, podman=podman,
            disk_free=_FreeSpace(start=GIB // 2, freed_per_eviction=GIB, state=state),
        )
    except SpaceGuardError as e:
        assert "img:latest" in str(e)
        assert "safety margin" in str(e)
        print("PASS: ensure_space_for_pull raises SpaceGuardError when short")
        return
    raise AssertionError("expected SpaceGuardError")


def test_ensure_space_best_effort_when_free_unknown():
    podman = _FakePodman(manifest={"layers": [{"size": GIB}]})
    # Unknown free space must never block the pull.
    ensure_space_for_pull("img:latest", margin=2.0, podman=podman, disk_free=lambda: None)
    assert podman.calls == [], "no podman calls when free space is unknown"
    print("PASS: ensure_space_for_pull best-effort when free space unknown")


def test_ensure_space_uses_default_estimate_on_probe_failure():
    state = _usage_state()
    podman = _FakePodman(manifest=None, images=[])  # size probe fails
    # Need = 2x DEFAULT_IMAGE_EST_BYTES; start with room -> no error, no eviction.
    ensure_space_for_pull(
        "img:latest", margin=2.0, podman=podman,
        disk_free=_FreeSpace(start=5 * DEFAULT_IMAGE_EST_BYTES,
                             freed_per_eviction=0, state=state),
    )
    print("PASS: ensure_space_for_pull falls back to default estimate")


def test_ensure_space_excludes_pulling_image_from_eviction():
    """The image being pulled must never be evicted to make room for itself."""
    state = _usage_state()
    podman = _FakePodman(
        manifest={"layers": [{"size": GIB}]},  # need 2 GiB
        images=[
            _image_entry("img:latest", "2026-01-01 00:00:00", 1.0),
            _image_entry("img:other", "2026-01-02 00:00:00", 1.0),
        ],
        evict_state=state,
    )
    # Evicting img:other frees only 1 GiB (still short of the 2 GiB need),
    # and img:latest itself is excluded — so the pull must be refused rather
    # than deleting the very image it is about to pull.
    try:
        ensure_space_for_pull(
            "img:latest", margin=2.0, podman=podman,
            disk_free=_FreeSpace(start=0, freed_per_eviction=GIB, state=state),
        )
    except SpaceGuardError:
        rm_calls = [c for c in podman.calls if c[0] == "rmi"]
        assert [c[1] for c in rm_calls] == ["img:other"]
        print("PASS: ensure_space_for_pull never evicts the pulling image")
        return
    raise AssertionError("expected SpaceGuardError (img:latest must not self-evict)")


def main():
    test_parse_human_size()
    test_required_bytes_margin()
    test_estimate_pull_size_single_arch()
    test_estimate_pull_size_manifest_list()
    test_estimate_pull_size_failure_is_none()
    test_estimate_pull_size_cached()
    test_eviction_order_lru_then_created()
    test_eviction_order_exclude_and_names()
    test_evict_coldest_frees_room_in_lru_order()
    test_evict_coldest_stops_when_enough()
    test_evict_coldest_ignores_failed_rmi()
    test_evict_coldest_noop_when_enough_or_unknown()
    test_ensure_space_ok_when_enough()
    test_ensure_space_evicts_then_proceeds()
    test_ensure_space_refuses_when_eviction_not_enough()
    test_ensure_space_best_effort_when_free_unknown()
    test_ensure_space_uses_default_estimate_on_probe_failure()
    test_ensure_space_excludes_pulling_image_from_eviction()
    print("\n=== ALL SPACE GUARDRAIL TESTS PASSED ===")


if __name__ == "__main__":
    main()
