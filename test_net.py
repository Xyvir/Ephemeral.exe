"""
Tests for ephemeral_net.

Two layers:

1. Pure protocol/framing tests — no iroh required, always run.
2. Two-node in-process integration test over real iroh QUIC
   connections (relay disabled — direct connection on the same host).
   Skipped cleanly when ``iroh`` is not installed or no local
   connectivity is available.

Run:  python test_net.py
"""
from __future__ import annotations

import asyncio
import base64
import hashlib
import json
import tempfile
from pathlib import Path

from ephemeral_net.protocol import (
    ALPN,
    DEFAULT_MAX_FRAME_SIZE,
    decode_frame,
    encode_frame,
    error_frame,
    hello_frame,
    read_frame,
    write_frame,
)
from ephemeral_net.errors import FrameTooLarge, JobError, ProtocolError
from ephemeral_net.jobs import JobDoneEvent, JobErrorEvent, JobLogEvent, JobRequest

# ---------------------------------------------------------------------------
# Layer 1: pure protocol tests
# ---------------------------------------------------------------------------

PY_IMG = "docker.io/tymills620/ephemeral-python-uv:latest"
BASH_IMG = "docker.io/library/alpine:latest"
NODE_IMG = "docker.io/library/node:18-alpine"


def test_sanitize_strips_unsafe_and_overrides():
    from ephemeral_net.sandbox import sanitize_markdown

    md = f"""
```python unsafe image=evil/evil:latest cmd=rm -rf / entrypoint=/bin/sh
print('hi')
```
"""
    clean, images = sanitize_markdown(md)
    assert images == [PY_IMG], f"unexpected images: {images}"
    assert "unsafe" not in clean
    assert "evil" not in clean
    assert "cmd=" not in clean and "entrypoint=" not in clean
    assert "```python" in clean and "print('hi')" in clean
    print("PASS: sanitize strips unsafe + image/cmd/entrypoint overrides")


def test_sanitize_rejects_unknown_language():
    from ephemeral_net.sandbox import sanitize_markdown

    # `image=` overrides are *stripped*, so the only way to land outside the
    # allowlist is an unknown language whose fallback image is not allowed.
    md = "```totallyfake\nprint(1)\n```"
    try:
        sanitize_markdown(md)
    except ValueError as e:
        assert "allowlist" in str(e)
        print("PASS: sanitize rejects non-allowlisted images")
        return
    raise AssertionError("expected ValueError for non-allowlisted image")


def test_sanitize_operator_network_flag():
    from ephemeral_net.sandbox import sanitize_markdown

    md = "```python\nprint(1)\n```"
    clean_off, _ = sanitize_markdown(md, allow_network=False)
    assert "unsafe" not in clean_off
    clean_on, _ = sanitize_markdown(md, allow_network=True)
    assert "unsafe" in clean_on
    print("PASS: sanitize gates network behind the node operator")


def test_sanitize_seeds_and_empty():
    from ephemeral_core.parser import parse_codeblocks
    from ephemeral_net.sandbox import sanitize_markdown

    # Seeds use the canonical fenced form (```filename ... ```).
    md = "```data.csv\na,b\n```\n\n```python\nprint(1)\n```"
    clean, images = sanitize_markdown(md)
    assert images == [PY_IMG]
    blocks = parse_codeblocks(clean)
    assert blocks[0]["type"] == "seed" and blocks[0]["name"] == "data.csv"
    assert blocks[1]["type"] == "code"
    assert "a,b" in clean

    # b64 seeds survive too.
    md2 = "```data.bin b64\nAAAA\n```\n\n```python\nprint(1)\n```"
    clean2, _ = sanitize_markdown(md2)
    blocks2 = parse_codeblocks(clean2)
    assert blocks2[0]["type"] == "seed" and blocks2[0]["is_b64"] is True

    try:
        sanitize_markdown("")
    except ValueError:
        pass
    else:
        raise AssertionError("expected ValueError for empty document")
    print("PASS: sanitize keeps fenced seeds and rejects empty documents")


def test_sanitize_custom_allowlist():
    from ephemeral_net.sandbox import sanitize_markdown

    md = f"""```python
print(1)
```
```node
console.log(1)
```"""
    # Only node is allowlisted -> the python block must be rejected
    try:
        sanitize_markdown(md, image_allowlist=[NODE_IMG])
    except ValueError as e:
        assert "allowlist" in str(e)
        print("PASS: sanitize honors a custom allowlist")
        return
    raise AssertionError("expected ValueError with a restrictive allowlist")


def test_sanitize_roundtrip_parses_cleanly():
    from ephemeral_core.parser import parse_codeblocks
    from ephemeral_net.sandbox import sanitize_markdown

    md = f"""
```python unsafe
import json
print(json.dumps({{'a': 1}}))
```
```node
console.log('x')
```
"""
    clean, images = sanitize_markdown(md)
    blocks = parse_codeblocks(clean)
    assert len(blocks) == 2
    assert all(b["type"] == "code" for b in blocks)
    assert all(not b["config"]["allow_network"] for b in blocks)
    print("PASS: sanitized markdown re-parses cleanly")


async def _fake_core_runner(markdown_text, timeout, server_mode):
    """Fake ephemeral_core.parse_and_execute returning a canned result."""
    class _Result:
        stdout = "## Result\nhello from core\n"
        stderr = ""
        exit_code = 0
        artifact_paths = []
    return _Result()


def test_core_executor_streams_result():
    import base64

    from ephemeral_net.jobs import JobDoneEvent, JobRequest
    from ephemeral_net.sandbox import CoreJobExecutor

    async def run():
        ex = CoreJobExecutor(runner=_fake_core_runner)
        req = JobRequest(
            job_id="j1",
            document_blob=base64.b64encode(b"```python\nprint(1)\n```").decode(),
        )
        events = [e async for e in ex(req)]
        dones = [e for e in events if isinstance(e, JobDoneEvent)]
        # Exactly one done, carrying the full result — the finished output is
        # NOT re-streamed as job_log chunks (that doubled it for consumers
        # that render both the log stream and the done event).
        assert len(dones) == 1
        assert dones[0].exit_code == 0
        assert dones[0].stdout == "## Result\nhello from core\n"

    asyncio.run(run())
    print("PASS: CoreJobExecutor returns the result once in the done event")


def test_core_executor_streams_artifacts_and_caps():
    import base64 as b64
    import tempfile
    from pathlib import Path

    from ephemeral_net.jobs import (
        MAX_ARTIFACT_SIZE,
        JobArtifactEvent,
        JobDoneEvent,
        JobLogEvent,
        JobRequest,
    )
    from ephemeral_net.sandbox import CoreJobExecutor

    with tempfile.TemporaryDirectory() as td:
        small = Path(td) / "chart.png"
        small.write_bytes(b"\x89PNG fake")
        big = Path(td) / "huge.bin"
        big.write_bytes(b"x" * (MAX_ARTIFACT_SIZE + 1))
        notes = Path(td) / "notes.txt"
        notes.write_bytes(b"hello")

        async def _runner(markdown_text, timeout, server_mode):
            class _Result:
                stdout = "## Result\nok\n"
                stderr = ""
                exit_code = 0
                artifact_paths = [str(small), str(big), str(notes)]

            return _Result()

        async def run():
            ex = CoreJobExecutor(runner=_runner)
            req = JobRequest(
                job_id="art-1",
                document_blob=b64.b64encode(b"```python\nx = 1\n```").decode(),
            )
            events = [e async for e in ex(req)]
            arts = [e for e in events if isinstance(e, JobArtifactEvent)]
            dones = [e for e in events if isinstance(e, JobDoneEvent)]
            warns = [
                e for e in events
                if isinstance(e, JobLogEvent) and e.channel == "stderr"
            ]
            # Small + text streamed; oversized skipped with a warning.
            assert sorted(a.name for a in arts) == ["chart.png", "notes.txt"]
            assert any(b"exceeds" in w.data for w in warns), (
                "oversized artifact must warn + skip"
            )
            # Artifacts stream BEFORE the terminating done event.
            kinds = [type(e).__name__ for e in events]
            assert kinds.index("JobArtifactEvent") < kinds.index("JobDoneEvent")
            assert len(dones) == 1
            assert dones[0].artifact_list == [
                {"name": "chart.png", "ext": ".png", "size": len(b"\x89PNG fake")},
                {"name": "notes.txt", "ext": ".txt", "size": 5},
            ]
            assert dones[0].artifact_file == "chart.png"

        asyncio.run(run())
    print("PASS: CoreJobExecutor streams one artifact frame per file before done, caps oversized")


def test_core_executor_ignores_remote_overrides():
    import base64

    from ephemeral_net.jobs import JobDoneEvent, JobRequest
    from ephemeral_net.sandbox import CoreJobExecutor

    async def run():
        ex = CoreJobExecutor(runner=_fake_core_runner)
        req = JobRequest(
            job_id="j2",
            document_blob=base64.b64encode(
                b"```python unsafe image=evil/evil:latest cmd=rm -rf /\nprint(1)\n```"
            ).decode(),
        )
        events = [e async for e in ex(req)]
        # The unsafe/override instructions are ignored; the job runs with the
        # default allowlisted image and never reaches the runner with network.
        dones = [e for e in events if isinstance(e, JobDoneEvent)]
        assert len(dones) == 1 and dones[0].exit_code == 0

    asyncio.run(run())
    print("PASS: CoreJobExecutor ignores remote override instructions")


# --- offloading unit tests (no iroh required) ---------------------------

class _FakeLocal:
    def __init__(self, images, warm, events):
        self._images = images
        self._warm = warm
        self._events = events
        self.pull_calls = []

    def prepare(self, request):
        return "md", self._images

    def is_warm(self, image):
        return image in self._warm

    async def pull(self, image):
        self.pull_calls.append(image)

    async def __call__(self, request):
        for e in self._events(request):
            yield e


class _FakePeer:
    def __init__(self, node_id="peer", images=None, rtt=0.1):
        self.node_id = node_id
        self.images = set(images or [])
        self.rtt = rtt


class _FakeNode:
    def __init__(self, peer=None):
        self.peer = peer
        self.submitted = 0

    def peer_for_images(self, images):
        if self.peer and self.peer.images & set(images):
            return self.peer
        return None

    async def submit_job(self, peer, request):
        self.submitted += 1
        yield JobLogEvent(channel="stdout", data=b"forwarded\n", job_id=request.job_id)
        yield JobDoneEvent(exit_code=0, stdout="forwarded\n", stderr="",
                           job_id=request.job_id)


def _events(request):
    yield JobLogEvent(channel="stdout", data=b"local\n", job_id=request.job_id)
    yield JobDoneEvent(exit_code=0, stdout="local\n", stderr="", job_id=request.job_id)


def test_offload_runs_locally_when_warm():
    from ephemeral_net.offload import OffloadingExecutor

    async def run():
        local = _FakeLocal([PY_IMG], warm={PY_IMG}, events=_events)
        node = _FakeNode(peer=_FakePeer(images={PY_IMG}))
        ex = OffloadingExecutor(node, local)
        events = [e async for e in ex(JobRequest(job_id="j", document_blob=""))]
        assert any(e.data == b"local\n" for e in events if hasattr(e, "data"))
        assert node.submitted == 0
        assert local.pull_calls == []

    asyncio.run(run())
    print("PASS: offload runs locally when the image is warm")


def test_offload_forwards_to_warm_neighbor_and_pulls():
    from ephemeral_net.offload import OffloadingExecutor

    async def run():
        local = _FakeLocal([PY_IMG], warm=set(), events=_events)
        node = _FakeNode(peer=_FakePeer(images={PY_IMG}))
        ex = OffloadingExecutor(node, local)
        req = JobRequest(job_id="j", document_blob="")
        events = [e async for e in ex(req)]
        assert any(e.data == b"forwarded\n" for e in events if hasattr(e, "data"))
        assert node.submitted == 1
        # background pull kicked off for the missing image
        await asyncio.sleep(0.05)
        assert local.pull_calls == [PY_IMG]

    asyncio.run(run())
    print("PASS: offload forwards to a warm neighbor and background-pulls")


def test_offload_runs_locally_when_no_warm_neighbor():
    from ephemeral_net.offload import OffloadingExecutor

    async def run():
        local = _FakeLocal([PY_IMG], warm=set(), events=_events)
        node = _FakeNode(peer=None)
        ex = OffloadingExecutor(node, local)
        events = [e async for e in ex(JobRequest(job_id="j", document_blob=""))]
        assert any(e.data == b"local\n" for e in events if hasattr(e, "data"))
        assert node.submitted == 0
        assert local.pull_calls == []

    asyncio.run(run())
    print("PASS: offload runs locally when no neighbor has the image")


def test_offload_falls_back_to_local_when_forward_fails():
    from ephemeral_net.offload import OffloadingExecutor

    class _FailNode(_FakeNode):
        async def submit_job(self, peer, request):
            self.submitted += 1
            if False:  # pragma: no cover - keep this an async generator
                yield
            raise RuntimeError("dial timed out")

    async def run():
        local = _FakeLocal([PY_IMG], warm=set(), events=_events)
        node = _FailNode(peer=_FakePeer(images={PY_IMG}))
        ex = OffloadingExecutor(node, local)
        req = JobRequest(job_id="j", document_blob="")
        events = [e async for e in ex(req)]
        # A failed neighbor forward must not fail the job — run locally.
        assert any(e.data == b"local\n" for e in events if hasattr(e, "data"))
        assert node.submitted == 1
        await asyncio.sleep(0.05)  # let the background pull task settle

    asyncio.run(run())
    print("PASS: offload falls back to local when the neighbor forward fails")


# --- mesh image pull unit tests (no iroh / podman required) ------------

def test_image_ref_parsing():
    from ephemeral_net.image_pull import parse_image_ref

    assert parse_image_ref("docker.io/library/alpine:latest") == (
        "registry-1.docker.io", "library/alpine", "latest")
    assert parse_image_ref("alpine") == ("registry-1.docker.io", "library/alpine", "latest")
    assert parse_image_ref("ubuntu:22.04") == ("registry-1.docker.io", "library/ubuntu", "22.04")
    assert parse_image_ref("ghcr.io/org/img:v1") == ("ghcr.io", "org/img", "v1")
    assert parse_image_ref("mcr.microsoft.com/powershell") == (
        "mcr.microsoft.com", "powershell", "latest")
    print("PASS: image ref parsing (registry/repo/tag)")


def test_blob_frame_helpers():
    from ephemeral_net.protocol import (
        blob_chunk_frame, blob_done_frame, blob_request_frame, decode_frame, encode_frame,
    )

    req = blob_request_frame("docker.io/library/alpine:latest", "sha256:" + "a" * 64, 1234)
    assert req["type"] == "blob_request" and req["size"] == 1234
    assert decode_frame(encode_frame(req))["digest"] == "sha256:" + "a" * 64
    chunk = blob_chunk_frame("img", "sha256:" + "b" * 64, 0, b"\x00\x01hello", 7)
    assert base64.b64decode(chunk["data"]) == b"\x00\x01hello"
    assert chunk["offset"] == 0 and chunk["total"] == 7
    done = blob_done_frame("img", "sha256:" + "c" * 64, 7)
    assert done["type"] == "blob_done" and done["total"] == 7
    print("PASS: blob frame helpers round-trip")


def _stage_blob(root, digest, data):
    p = root / "src" / digest
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_bytes(data)
    return p


def _sample_manifest():
    """A tiny docker-v2 platform manifest with one config + one layer blob."""
    layer = b"layer-bytes" * 100
    config = b'{"architecture":"amd64"}'
    manifest = {
        "mediaType": "application/vnd.docker.distribution.manifest.v2+json",
        "config": {"digest": "sha256:" + hashlib.sha256(config).hexdigest(), "size": len(config)},
        "layers": [{"digest": "sha256:" + hashlib.sha256(layer).hexdigest(), "size": len(layer)}],
    }
    mbytes = json.dumps(manifest).encode()
    return (
        layer, config, manifest,
        "sha256:" + hashlib.sha256(mbytes).hexdigest(), mbytes,
    )


def test_oci_layout_assembly_and_verification():
    import tempfile

    from ephemeral_net.image_pull import ImagePullError, assemble_oci_layout, verify_blob

    layer, config, manifest, mdigest, mbytes = _sample_manifest()
    config_d = manifest["config"]["digest"]
    layer_d = manifest["layers"][0]["digest"]
    with tempfile.TemporaryDirectory(prefix="mesh-layout-") as d:
        root = Path(d)
        blobs = {
            config_d: _stage_blob(root, config_d, config),
            layer_d: _stage_blob(root, layer_d, layer),
        }
        assemble_oci_layout(
            root, "docker.io/library/alpine:latest", manifest, mdigest, mbytes, blobs
        )
        assert (root / "blobs" / "sha256" / layer_d).read_bytes() == layer
        idx = json.loads((root / "index.json").read_text())
        assert idx["manifests"][0]["annotations"]["org.opencontainers.image.ref.name"] \
            == "docker.io/library/alpine:latest"
        assert json.loads((root / "oci-layout").read_text())["imageLayoutVersion"] == "1.0.0"
        assert verify_blob(root / "blobs" / "sha256" / layer_d, layer_d)

        # A tampered layer must fail assembly (never reaches podman load).
        bad = root / "src" / "bad"
        bad.write_bytes(b"TAMPERED")
        try:
            assemble_oci_layout(root, "ref", manifest, mdigest, mbytes, {config_d: blobs[config_d], layer_d: bad})
            raise AssertionError("expected ImagePullError on tampered blob")
        except ImagePullError:
            pass
    print("PASS: OCI layout assembly + sha256 verification (tampered blob refused)")


def test_mesh_pull_orchestration():
    import tempfile

    from ephemeral_net.image_pull import MeshImagePuller

    layer, config, manifest, mdigest, mbytes = _sample_manifest()
    config_d = manifest["config"]["digest"]
    layer_d = manifest["layers"][0]["digest"]
    blobs = {config_d: config, layer_d: layer}

    class _Peer:
        node_id = "peer"

    class _Node:
        def __init__(self, has_peer=True):
            self.peer = _Peer() if has_peer else None
            self._tamper = False

        def peer_for_images(self, images):
            return self.peer

        async def fetch_blob(self, peer, image, digest, size, dest):
            data = blobs[digest]
            if self._tamper and digest == layer_d:
                data = b"TAMPERED"
            Path(dest).write_bytes(data)

    async def run():
        # Success: blobs fetched + verified, loader called with the ref.
        calls = []
        puller = MeshImagePuller(
            _Node(),
            manifest_fetcher=lambda ref: (manifest, mdigest, mbytes),
            loader=lambda root, ref: (calls.append(ref) or True),
        )
        assert await puller.pull("docker.io/library/alpine:latest") is True
        assert calls == ["docker.io/library/alpine:latest"]

        # No warm peer: False, loader untouched.
        calls2 = []
        puller2 = MeshImagePuller(
            _Node(has_peer=False),
            manifest_fetcher=lambda ref: (manifest, mdigest, mbytes),
            loader=lambda root, ref: (calls2.append(ref) or True),
        )
        assert await puller2.pull("img") is False and calls2 == []

        # Tampered layer: sha256 verification refuses, loader never runs.
        node3 = _Node()
        node3._tamper = True
        calls3 = []
        puller3 = MeshImagePuller(
            node3,
            manifest_fetcher=lambda ref: (manifest, mdigest, mbytes),
            loader=lambda root, ref: (calls3.append(ref) or True),
        )
        assert await puller3.pull("img") is False and calls3 == []

        # Registry manifest unavailable: False (caller keeps registry pull).
        def boom(ref):
            raise RuntimeError("registry unreachable")

        puller4 = MeshImagePuller(_Node(), manifest_fetcher=boom)
        assert await puller4.pull("img") is False

    asyncio.run(run())
    print("PASS: mesh pull orchestration (success / no peer / tampered / manifest down)")


def test_registry_manifest_fetch_auth_and_index():
    import ephemeral_net.image_pull as ip

    layer, config, manifest, _mdigest, mbytes = _sample_manifest()
    index = {
        "mediaType": "application/vnd.docker.distribution.manifest.list.v2+json",
        "manifests": [
            {"digest": "sha256:" + "p" * 64, "platform": {"os": "linux", "architecture": "amd64"}},
            {"digest": "sha256:" + "q" * 64, "platform": {"os": "linux", "architecture": "arm64"}},
        ],
    }
    idx_bytes = json.dumps(index).encode()

    def fake_get(registry, path, headers, timeout=20.0):
        if "Authorization" not in headers:
            return 401, {
                "WWW-Authenticate": 'Bearer realm="https://auth.example/token",service="s",scope="repository:library/alpine:pull"'
            }, b""
        if path == "library/alpine/manifests/latest":
            return 200, {}, idx_bytes
        if path.endswith("/manifests/sha256:" + "p" * 64):
            return 200, {}, mbytes
        return 404, {}, b""

    orig_get, orig_token = ip._registry_get, ip._fetch_token
    ip._registry_get = fake_get
    ip._fetch_token = lambda challenge, registry, repo, timeout=20.0: "tok-123"
    try:
        manifest_out, digest_out, body_out = ip.fetch_manifest(
            "docker.io/library/alpine:latest", arch="amd64"
        )
        assert manifest_out["layers"][0]["digest"] == manifest["layers"][0]["digest"]
        assert body_out == mbytes
        assert digest_out == "sha256:" + hashlib.sha256(mbytes).hexdigest()

        # 404 -> ImagePullError (no mesh pull without a trust anchor).
        try:
            ip.fetch_manifest("docker.io/library/nope:latest", arch="amd64")
            raise AssertionError("expected ImagePullError on 404")
        except ip.ImagePullError:
            pass
    finally:
        ip._registry_get, ip._fetch_token = orig_get, orig_token
    print("PASS: registry manifest fetch (401->token dance, index->platform, 404)")


def test_offload_background_pull_prefers_mesh():
    from ephemeral_net.offload import OffloadingExecutor

    class _MeshNode(_FakeNode):
        def __init__(self):
            super().__init__(peer=_FakePeer(images={PY_IMG}))
            self.mesh_calls = []

        async def mesh_pull_image(self, image, preferred_peer=None):
            self.mesh_calls.append((image, preferred_peer))
            return True

    async def run():
        local = _FakeLocal([PY_IMG], warm=set(), events=_events)
        node = _MeshNode()
        ex = OffloadingExecutor(node, local)
        events = [e async for e in ex(JobRequest(job_id="j", document_blob=""))]
        assert any(e.data == b"forwarded\n" for e in events if hasattr(e, "data"))
        await asyncio.sleep(0.05)
        assert node.mesh_calls == [(PY_IMG, node.peer)], f"got {node.mesh_calls}"
        assert local.pull_calls == [], "mesh success must skip the registry pull"

    asyncio.run(run())
    print("PASS: offload background pull prefers mesh (registry skipped on success)")


def test_offload_background_pull_falls_back_to_registry():
    from ephemeral_net.offload import OffloadingExecutor

    class _MeshFailNode(_FakeNode):
        def __init__(self):
            super().__init__(peer=_FakePeer(images={PY_IMG}))

        async def mesh_pull_image(self, image, preferred_peer=None):
            return False

    async def run():
        local = _FakeLocal([PY_IMG], warm=set(), events=_events)
        node = _MeshFailNode()
        ex = OffloadingExecutor(node, local)
        events = [e async for e in ex(JobRequest(job_id="j", document_blob=""))]
        assert any(e.data == b"forwarded\n" for e in events if hasattr(e, "data"))
        await asyncio.sleep(0.05)
        assert local.pull_calls == [PY_IMG], "mesh failure must fall back to the registry"

    asyncio.run(run())
    print("PASS: offload background pull falls back to the registry when mesh fails")


# --- busy/idle routing unit tests (no iroh required) -------------------

class _RP:
    """Fake routable peer exposing the attributes select_peer_for_images uses."""

    def __init__(self, node_id="p", images=None, rtt=1.0, active=0, max_jobs=None):
        self.node_id = node_id
        self.images = set(images or [])
        self.rtt = rtt
        self.active_jobs = active
        self.max_jobs = max_jobs


def test_select_peer_prefers_idle_then_fast():
    from ephemeral_net.node import select_peer_for_images

    idle = _RP("idle", images=[PY_IMG], rtt=5.0)
    busy = _RP("busy", images=[PY_IMG], rtt=0.1, active=3, max_jobs=4)
    fast = _RP("fast", images=[PY_IMG], rtt=0.05, active=1, max_jobs=4)

    # Idle wins over faster-but-busy peers.
    assert select_peer_for_images([fast, busy, idle], [PY_IMG]).node_id == "idle"
    # A busier peer beats one that doesn't advertise the image at all.
    assert select_peer_for_images([busy, _RP("cold", images=[NODE_IMG])], [PY_IMG]).node_id == "busy"
    # Saturated peers are never chosen.
    sat = _RP("sat", images=[PY_IMG], rtt=0.01, active=4, max_jobs=4)
    assert select_peer_for_images([sat, fast], [PY_IMG]).node_id == "fast"
    # Peers with no image list are treated as not warm.
    assert select_peer_for_images([_RP("unknown", images=None)], [PY_IMG]) is None
    assert select_peer_for_images([], [PY_IMG]) is None
    print("PASS: idle-first peer selection (saturation, RTT, warm-only)")


# --- fan-out unit tests (no iroh required) ------------------------------


def test_fanout_split_runs():
    from ephemeral_net.fanout import split_runs

    # Multi-language document splits into per-run docs, seeds attached.
    md = "```data.csv\na,b\n```\n\n```python\nprint(1)\n```\n\n```node\nconsole.log(2)\n```"
    docs, chained = split_runs(md)
    assert chained is False
    assert docs is not None and len(docs) == 2, docs
    assert "data.csv" in docs[0] and "print(1)" in docs[0] and "console.log" not in docs[0]
    assert "console.log(2)" in docs[1]

    # Chaining declared anywhere -> never split.
    md_chain = "```python chain\nprint(1)\n```\n\n```node\nconsole.log(2)\n```"
    docs, chained = split_runs(md_chain)
    assert chained is True and docs is None

    # Single run or seeds-only -> never split.
    assert split_runs("```python\nprint(1)\n```")[0] is None
    assert split_runs("```data.csv\na\n```")[0] is None
    assert split_runs("")[0] is None
    print("PASS: split_runs (independent runs split; chained/single do not)")


class _FanoutLocal:
    """Fake local executor: echoes a per-run id in its output."""

    def __init__(self):
        self.calls = []

    async def __call__(self, request):
        self.calls.append(request.job_id)
        yield JobLogEvent(channel="stdout", data=b"local\n", job_id=request.job_id)
        yield JobDoneEvent(exit_code=0, stdout="local\n", stderr="", job_id=request.job_id)


class _FanoutNode:
    """Fake node: hands every warm-image request to a fake peer."""

    def __init__(self):
        self.peer = _RP("peer", images=[PY_IMG, NODE_IMG], rtt=0.1, active=0, max_jobs=2)
        self.submitted = []

    def peer_for_images(self, images):
        return self.peer

    async def submit_job(self, peer, request):
        self.submitted.append(request.job_id)
        yield JobLogEvent(channel="stdout", data=b"remote\n", job_id=request.job_id)
        yield JobDoneEvent(
            exit_code=0, stdout="remote\n", stderr="", job_id=request.job_id
        )


def _fanout_request(doc):
    return JobRequest(
        job_id="fan-1",
        document_blob=base64.b64encode(doc.encode("utf-8")).decode("ascii"),
        timeout=60,
    )


def test_fanout_splits_and_merges():
    from ephemeral_net.fanout import FanoutExecutor

    async def run():
        md = "```python\nprint(1)\n```\n\n```node\nconsole.log(2)\n```"
        node = _FanoutNode()
        local = _FanoutLocal()
        ex = FanoutExecutor(node, local)
        events = [e async for e in ex(_fanout_request(md))]

        # Both runs went to the peer, not local.
        assert len(node.submitted) == 2, node.submitted
        assert local.calls == []
        assert node.submitted[0].endswith("-0") and node.submitted[1].endswith("-1")

        logs = [e for e in events if isinstance(e, JobLogEvent)]
        dones = [e for e in events if isinstance(e, JobDoneEvent)]
        assert len(logs) == 2 and all(e.data == b"remote\n" for e in logs)
        assert len(dones) == 1, "one merged done event"
        merged = dones[0]
        assert merged.stdout.count("remote") == 2, merged.stdout
        assert merged.exit_code == 0
        assert merged.job_id == "fan-1"

    asyncio.run(run())
    print("PASS: FanoutExecutor splits a multi-run doc across peers and merges")


def test_fanout_relays_artifacts():
    from ephemeral_net.fanout import FanoutExecutor
    from ephemeral_net.jobs import JobArtifactEvent, JobDoneEvent, JobLogEvent

    class _ArtPeerNode:
        """Fake node whose peer runs also emit an artifact per run."""

        def __init__(self):
            self.submitted = []
            self.peer = _RP(
                "peer", images=[PY_IMG, NODE_IMG], rtt=0.1, active=0, max_jobs=2
            )

        def peer_for_images(self, images):
            return self.peer

        async def submit_job(self, peer, request):
            self.submitted.append(request.job_id)
            yield JobLogEvent(channel="stdout", data=b"remote\n", job_id=request.job_id)
            yield JobArtifactEvent(
                name=request.job_id + ".png",
                ext=".png",
                data=b"\x89PNG " + request.job_id.encode(),
                job_id=request.job_id,
            )
            yield JobDoneEvent(
                exit_code=0, stdout="remote\n", stderr="", job_id=request.job_id
            )

    async def run():
        md = "```python\nprint(1)\n```\n\n```node\nconsole.log(2)\n```"
        node = _ArtPeerNode()
        ex = FanoutExecutor(node, _FanoutLocal())
        events = [e async for e in ex(_fanout_request(md))]
        arts = [e for e in events if isinstance(e, JobArtifactEvent)]
        dones = [e for e in events if isinstance(e, JobDoneEvent)]
        # Both peer runs' artifacts relayed in document order.
        assert len(node.submitted) == 2
        assert [a.name for a in arts] == ["fan-1-0.png", "fan-1-1.png"], [
            a.name for a in arts
        ]
        assert all(a.size == len(a.data) for a in arts)
        assert dones[0].artifact_list == [
            {"name": "fan-1-0.png", "ext": ".png", "size": 12},
            {"name": "fan-1-1.png", "ext": ".png", "size": 12},
        ]
        # Artifacts stream before the merged done.
        kinds = [type(e).__name__ for e in events]
        assert kinds.index("JobArtifactEvent") < kinds.index("JobDoneEvent")

    asyncio.run(run())
    print("PASS: FanoutExecutor relays artifact frames and merges artifact_list")


def test_fanout_falls_back_locally():
    from ephemeral_net.fanout import FanoutExecutor

    async def run():
        md = "```python\nprint(1)\n```\n\n```node\nconsole.log(2)\n```"

        class _NoPeerNode:
            def peer_for_images(self, images):
                return None

            async def submit_job(self, peer, request):
                raise AssertionError("must not submit")

        local = _FanoutLocal()
        ex = FanoutExecutor(_NoPeerNode(), local)
        events = [e async for e in ex(_fanout_request(md))]
        assert len(local.calls) == 2, local.calls
        dones = [e for e in events if isinstance(e, JobDoneEvent)]
        assert len(dones) == 1 and dones[0].stdout.count("local") == 2

    asyncio.run(run())
    print("PASS: FanoutExecutor falls back to the local executor with no peers")


def test_fanout_chained_runs_unsplit():
    from ephemeral_net.fanout import FanoutExecutor

    async def run():
        md = "```python chain\nprint(1)\n```\n\n```node\nconsole.log(2)\n```"
        node = _FanoutNode()
        local = _FanoutLocal()
        ex = FanoutExecutor(node, local)
        events = [e async for e in ex(_fanout_request(md))]
        # Chained request runs whole through the local executor — never split.
        assert node.submitted == []
        assert local.calls == ["fan-1"]
        assert any(isinstance(e, JobDoneEvent) for e in events)

    asyncio.run(run())
    print("PASS: FanoutExecutor never splits a chained request")


def test_frame_roundtrip():
    msg = {"type": "job_request", "job_id": "abc", "document_blob": "aGVsbG8=", "timeout": 30}
    data = encode_frame(msg)
    assert decode_frame(data) == msg
    print("PASS: frame round-trip")


def test_frame_binary_payload():
    msg = {"type": "job_log", "channel": "stdout", "data": "AAEC/w=="}
    assert decode_frame(encode_frame(msg)) == msg
    print("PASS: frame binary payload")


def test_frame_too_large():
    big = {"type": "x", "payload": "a" * (DEFAULT_MAX_FRAME_SIZE + 1)}
    data = encode_frame(big)
    try:
        decode_frame(data, max_size=1024)
    except FrameTooLarge:
        print("PASS: frame too large rejected")
        return
    raise AssertionError("expected FrameTooLarge")


def test_artifact_frame_roundtrip():
    from ephemeral_net.jobs import JobArtifactEvent, JobDoneEvent, parse_job_frame

    ev = JobArtifactEvent(
        name="chart.png", ext=".png", data=b"\x89PNG\r\n\x1a\nfake", job_id="j1"
    )
    frame = ev.to_frame()
    assert frame["type"] == "artifact" and frame["name"] == "chart.png"
    back = parse_job_frame(frame)
    assert isinstance(back, JobArtifactEvent)
    assert back.data == ev.data and back.size == len(ev.data)

    done = JobDoneEvent(
        exit_code=0,
        stdout="ok",
        stderr="",
        artifact_file="chart.png",
        artifact_ext=".png",
        artifact_list=[{"name": "chart.png", "ext": ".png", "size": 14}],
    )
    dframe = done.to_frame()
    assert dframe["artifact_list"] == [{"name": "chart.png", "ext": ".png", "size": 14}]
    dback = JobDoneEvent.from_frame(dframe)
    assert dback.artifact_list == done.artifact_list
    # Legacy frames without artifact_list still parse.
    legacy = JobDoneEvent.from_frame(
        {"type": "job_done", "exit_code": 0, "stdout": "", "stderr": ""}
    )
    assert legacy.artifact_list is None
    print("PASS: artifact frame + done artifact_list round-trip (legacy-safe)")


def test_frame_malformed():
    # truncated header
    try:
        decode_frame(b"\x00\x00")
    except ProtocolError:
        pass
    else:
        raise AssertionError("expected ProtocolError on truncated header")
    # declares more than present
    try:
        decode_frame(b"\x00\x00\x00\x10" + b"abc")
    except ProtocolError:
        pass
    else:
        raise AssertionError("expected ProtocolError on truncated body")
    # invalid JSON
    try:
        decode_frame(b"\x00\x00\x00\x05" + b"{nope")
    except ProtocolError:
        pass
    else:
        raise AssertionError("expected ProtocolError on invalid JSON")
    # not a dict
    try:
        decode_frame(b"\x00\x00\x00\x02" + b"[]")
    except ProtocolError:
        pass
    else:
        raise AssertionError("expected ProtocolError on non-dict payload")
    print("PASS: malformed frames rejected")


def test_hello_frame():
    f = hello_frame(
        "node-a", "ticket-a", [{"node_id": "node-b", "ticket": "ticket-b"}],
        relay="https://relay.example.com.",
    )
    assert f["type"] == "hello"
    assert f["node_id"] == "node-a"
    assert f["relay"] == "https://relay.example.com."
    assert f["peers"][0]["node_id"] == "node-b"
    assert f["active_jobs"] == 0 and f["max_jobs"] is None, "load fields default"
    f2 = hello_frame("node-a", None, [], active_jobs=3, max_jobs=4)
    assert f2["active_jobs"] == 3 and f2["max_jobs"] == 4, "load fields advertised"
    assert error_frame("boom", job_id="j1")["job_id"] == "j1"
    print("PASS: hello/error frame helpers (incl. relay + load)")


def test_hello_frame_url_and_peer_table():
    """Bastion public URLs flow through hello frames into the peer table."""
    import time

    from ephemeral_net.discovery import PeerInfo, PeerTable
    from ephemeral_net.protocol import hello_frame, peer_entries_from_hello

    f = hello_frame(
        "node-a",
        None,
        [],
        relay="https://relay.example.com.",
        url="https://bastion.example.com",
    )
    assert f["url"] == "https://bastion.example.com"
    entries = peer_entries_from_hello(f)
    assert entries[0]["url"] == "https://bastion.example.com"

    table = PeerTable()
    table.merge(
        [
            PeerInfo(
                node_id="b",
                relay="https://relay.example.com.",
                url="https://bastion.example.com",
                last_seen=time.monotonic(),
            )
        ]
    )
    snap = table.snapshot()
    assert snap[0]["url"] == "https://bastion.example.com"
    print("PASS: bastion public URL flows through hello frames into the peer table")


def test_job_messages():
    from ephemeral_net.jobs import (
        JobDoneEvent,
        JobErrorEvent,
        JobLogEvent,
        JobRequest,
        parse_job_frame,
    )

    req = JobRequest(job_id="j1", document_blob="bWFyaw==", timeout=42)
    assert JobRequest.from_frame(req.to_frame()) == req

    log = JobLogEvent(channel="stdout", data=b"hello\n", job_id="j1")
    back = parse_job_frame(log.to_frame())
    assert back.channel == "stdout" and back.data == b"hello\n"

    done = JobDoneEvent(exit_code=0, stdout="ok", stderr="", artifact_file="a.zip",
                        artifact_ext=".zip", artifact_path="C:/tmp/a.zip", job_id="j1")
    back = parse_job_frame(done.to_frame())
    assert back.exit_code == 0 and back.artifact_file == "a.zip"
    assert back.artifact_path == "C:/tmp/a.zip"

    err = JobErrorEvent(message="boom", job_id="j1")
    back = parse_job_frame(err.to_frame())
    assert back.message == "boom"

    # base64 payload round-trips binary
    blob = b"\x00\x01\xfe\xff"
    log2 = JobLogEvent(channel="stderr", data=blob, job_id="j1")
    assert parse_job_frame(log2.to_frame()).data == blob

    print("PASS: job message schemas")


def test_peer_table_ttl_eviction():
    """PeerTable prunes entries unseen (direct or gossip) past the TTL, and
    stale entries never propagate through snapshots."""
    import time

    from ephemeral_net.discovery import PEER_TTL_SECONDS, PeerInfo, PeerTable

    t = PeerTable()
    now = time.monotonic()
    fresh = PeerInfo(node_id="live", relay="https://relay.example/", last_seen=now)
    t.merge([fresh])
    assert "live" in t.known_peer_ids()

    # A peer whose last_seen is past the TTL is evicted on the next merge.
    stale = PeerInfo(
        node_id="dead",
        relay="https://relay.example/",
        last_seen=now - PEER_TTL_SECONDS - 60.0,
    )
    t.merge([stale, fresh])  # fresh re-seen now, dead is not
    assert "live" in t.known_peer_ids()
    assert "dead" not in t.known_peer_ids()

    # Snapshot also prunes, so a stale entry can never be re-gossiped to
    # other nodes once the table has gone quiet.
    t2 = PeerTable()
    t2.merge([stale])
    assert len(t2.snapshot()) == 0
    assert len(t2.known_peer_ids()) == 0

    # Prune is idempotent on an empty table and reports eviction counts.
    assert t2.prune() == 0
    print("PASS: peer table TTL eviction")


def test_peer_table_gossip_does_not_refresh_ttl():
    """Regression: a peer refreshed only via GOSSIP (last_seen=0.0) must not
    have its TTL re-stamped, and must never downgrade a directly-seen peer.
    Before the fix, every hello stamped last_seen=now on ALL entries, so a
    dead peer circulated forever — each gossip receipt refreshed its TTL on
    every node that heard about it, defeating prune()."""
    import ephemeral_net.discovery as discovery
    from ephemeral_net.discovery import PEER_TTL_SECONDS, PeerInfo, PeerTable

    # Deterministic clock. The table's TTL is compared against
    # time.monotonic(), which starts near zero on a fresh CI runner — a
    # never-directly-seen entry (last_seen=0.0) then survives every prune
    # until the machine has been up past the TTL, making the test pass on
    # long-lived dev boxes and fail in CI. Pin the clock instead.
    clock = [1000.0]  # pretend the machine has been up well past the TTL

    def fake_monotonic() -> float:
        return clock[0]

    real_monotonic = discovery.time.monotonic
    discovery.time.monotonic = fake_monotonic
    try:
        t = PeerTable()

        # 1. A peer directly seen in the past, then quiet, receives endless
        #    gossip (last_seen=0.0): its last_seen must NOT be refreshed,
        #    and it must age out once the TTL passes.
        t.merge(
            [
                PeerInfo(
                    node_id="zombie",
                    relay="https://relay.example/",
                    last_seen=clock[0] - 100.0,
                )
            ]
        )
        assert "zombie" in t.known_peer_ids()

        clock[0] += PEER_TTL_SECONDS + 60.0  # sit idle past the TTL
        t.merge(
            [
                PeerInfo(
                    node_id="zombie", relay="https://relay.example/", last_seen=0.0
                )
            ]
        )  # endless gossip re-mentions of the dead peer
        assert "zombie" not in t.known_peer_ids(), \
            "gossip must not refresh a stale peer's last_seen"

        # 2. A directly-seen peer is never downgraded by gossip mentioning it
        #    with last_seen=0.0.
        t.merge(
            [
                PeerInfo(
                    node_id="live", relay="https://relay.example/",
                    last_seen=clock[0],
                )
            ]
        )
        t.merge([PeerInfo(node_id="live", relay="https://relay.example/", last_seen=0.0)])
        assert "live" in t.known_peer_ids(), \
            "gossip must not downgrade a directly-seen peer"

        # 3. A never-directly-seen peer (last_seen=0.0) is prunable.
        t3 = PeerTable()
        t3._peers["ghost"] = PeerInfo(node_id="ghost", last_seen=0.0)
        assert t3.prune(ttl=-1.0) == 1, "0.0 last_seen must be prunable"
        print("PASS: gossip never refreshes last_seen (dead peers age out)")
    finally:
        discovery.time.monotonic = real_monotonic


def test_peer_table_gossiped_new_peer_survives_merge():
    """Regression: a brand-new peer arriving via GOSSIP (last_seen=0.0)
    must survive the very merge that added it, then age out after the TTL.

    last_seen is compared against time.monotonic(), an uptime clock that
    is thousands of seconds on any real machine. Storing 0.0 for a
    gossiped peer made ``now - 0.0 > TTL`` true immediately, so every
    gossip-learned peer was pruned in the same merge that inserted it —
    discovery could never learn any peer from a hello. New entries now
    get ``last_seen = now`` so they have a real TTL window to be dialed;
    only *repeated* gossip (an existing entry) is barred from refreshing.
    """
    import ephemeral_net.discovery as discovery
    from ephemeral_net.discovery import PEER_TTL_SECONDS, PeerInfo, PeerTable

    clock = [74316.0]  # a long-lived machine, well past the TTL

    def fake_monotonic() -> float:
        return clock[0]

    real_monotonic = discovery.time.monotonic
    discovery.time.monotonic = fake_monotonic
    try:
        # 1. Gossiped newcomer survives the merge that added it.
        t = PeerTable()
        t.merge([PeerInfo(node_id="gossiped", relay="https://relay.example/", last_seen=0.0)])
        assert "gossiped" in t.known_peer_ids(), \
            "a newly gossiped peer must survive the merge that added it"

        # 2. ...but ages out once the TTL passes if never dialed directly.
        clock[0] += PEER_TTL_SECONDS + 60.0
        t.merge([])  # prune on next merge
        assert "gossiped" not in t.known_peer_ids(), \
            "a gossiped-only peer must age out after the TTL"

        # 3. Repeated gossip (0.0) must not refresh a directly-seen peer.
        t2 = PeerTable()
        t2.merge(
            [PeerInfo(node_id="live", relay="https://relay.example/", last_seen=clock[0])]
        )
        clock[0] += PEER_TTL_SECONDS - 30.0  # still within the TTL
        t2.merge([PeerInfo(node_id="live", relay="https://relay.example/", last_seen=0.0)])
        assert "live" in t2.known_peer_ids(), \
            "gossip must never refresh a directly-seen peer"
        print("PASS: gossiped newcomers survive merge, age out after TTL")
    finally:
        discovery.time.monotonic = real_monotonic


def test_genesis_fallback_plan():
    """The previous list is the primary census source; the pinned genesis
    anchor is only consulted when the list is empty or every member is
    unreachable — and once the swarm is self-sustaining, the anchor is not
    exempt from eviction."""
    import scripts.update_swarm_json as upd
    from ephemeral_net.probe import UNREACHABLE_MAX_MISSES, should_evict

    genesis = [("g" * 64, "https://relay.example")]
    assert upd.genesis_anchor_required(
        reset=False, has_prev=True, prev_reached=1
    ) is False, "one live previous member must be enough — no genesis needed"
    assert upd.genesis_anchor_required(
        reset=False, has_prev=True, prev_reached=0
    ) is True, "all previous members dead must trigger the genesis fallback"
    assert upd.genesis_anchor_required(
        reset=False, has_prev=False, prev_reached=0
    ) is True, "first run seeds from the genesis anchor"
    assert upd.genesis_anchor_required(
        reset=True, has_prev=True, prev_reached=3
    ) is True, "manual reset always regenerates from the genesis anchor"

    # De-pinned: while the swarm regenerates from its own members the
    # genesis node is an ordinary member and ages out like any other.
    entry = {
        "node_id": genesis[0][0],
        "misses": UNREACHABLE_MAX_MISSES,
        "seen_alive": True,
    }
    assert should_evict(entry, seed_ids=set()) is True, \
        "genesis must age out when it is not the active anchor"
    assert should_evict(entry, seed_ids={genesis[0][0]}) is False, \
        "genesis is exempt only while it is the active anchor"
    print("PASS: genesis fallback plan (prev-list first, anchor only as fallback)")


def test_resolve_genesis_from_url():
    """A bastion URL alone bootstraps the refresh — node identity is read
    from its /health, so no node id is hard-coded in code."""
    import scripts.update_swarm_json as upd

    real_fetch = upd.fetch_genesis_from_url
    try:
        # No explicit genesis ids -> the URL is the sole source.
        upd.fetch_genesis_from_url = lambda url, timeout=15.0: (
            "a" * 64, "https://euc1-1.relay.n0.iroh.link./", "ticket-1"
        )
        targets, tickets = upd.resolve_genesis([], "https://bastion.example")
        assert targets and len(targets) == 1, targets
        nid, relay = targets[0]
        assert nid == "a" * 64
        assert relay.startswith("https://")
        assert tickets == {"a" * 64: "ticket-1"}

        # Explicit genesis ids win outright and carry no ticket.
        explicit, t2 = upd.resolve_genesis(
            [("b" * 64, "https://relay.example")], "https://bastion.example"
        )
        assert explicit == [("b" * 64, "https://relay.example")] and t2 == {}

        # URL that fails to resolve its identity -> empty anchor.
        upd.fetch_genesis_from_url = lambda url, timeout=15.0: None
        targets3, _ = upd.resolve_genesis([], "https://down.example")
        assert targets3 == []
    finally:
        upd.fetch_genesis_from_url = real_fetch

    print("PASS: genesis resolved from a bastion URL (no hardcoded node id)")


def test_evicted_tombstones_ttl():
    """Eviction tombstones expire so a recovered node can rejoin, and the
    loader tolerates the transitional plain-list format."""
    import json
    import tempfile
    import time
    from pathlib import Path

    import scripts.update_swarm_json as upd

    now = time.time()
    with tempfile.TemporaryDirectory(prefix="ephemeral-evict-ttl-") as d:
        out = Path(d) / "swarm.json"

        # Fresh tombstone kept; expired one dropped.
        out.write_text(
            json.dumps(
                {
                    "evicted": {
                        "a" * 64: now - 60.0,                            # fresh
                        "b" * 64: now - upd.EVICT_TTL_SECONDS - 10.0,   # expired
                    }
                }
            ),
            encoding="utf-8",
        )
        loaded = upd._load_evicted(out)
        assert "a" * 64 in loaded and "b" * 64 not in loaded, \
            "expired tombstones must be dropped so recovered nodes can rejoin"

        # Transitional plain-list format is tolerated (evicted as of now).
        out.write_text(json.dumps({"evicted": ["c" * 64]}), encoding="utf-8")
        loaded = upd._load_evicted(out)
        assert "c" * 64 in loaded, "plain-list format must be accepted"

        # Malformed file yields an empty set.
        out.write_text("{not json", encoding="utf-8")
        assert upd._load_evicted(out) == {}
        print("PASS: evicted tombstones TTL-expire and tolerate legacy format")


def test_swarm_status_badge_payload():
    """The README's live-node badge counts only probe-verified nodes, and
    turns red when the swarm has none."""
    import scripts.update_swarm_json as upd

    nodes = [
        {"node_id": "a" * 64, "probe": "ok"},
        {"node_id": "b" * 64, "probe": "ok"},
        {"node_id": "c" * 64, "probe": "unreachable"},
        {"node_id": "d" * 64, "probe": "failed"},
        {"node_id": "e" * 64, "probe": "skipped"},
        {},
    ]
    payload = upd.build_status_payload(nodes)
    assert payload["schemaVersion"] == 1
    assert payload["label"] == "live nodes"
    assert payload["message"] == "2", "only probe-verified nodes count"
    assert payload["color"] == "brightgreen"
    empty = upd.build_status_payload([])
    assert empty["message"] == "0"
    assert empty["color"] == "red"
    print("PASS: swarm status badge payload (verified-only count)")


def test_private_mode_helpers():
    """``--private`` / ``EPHEMERAL_PRIVATE`` / marker decide private mode,
    and the student URL is the hosted SPA with the ticket in the fragment."""
    import os

    from ephemeral_net.swarm import (
        PRIVATE_MODE_MARKER,
        parse_private_seed,
        private_mode_enabled,
        private_student_url,
        read_private_seed,
        write_private_seed,
    )

    url = private_student_url("ticket123")
    assert url.endswith("#seed=ticket123"), url
    assert url.startswith("http"), url

    with tempfile.TemporaryDirectory() as td:
        state = Path(td)
        old = os.environ.pop("EPHEMERAL_PRIVATE", None)
        try:
            # Nothing set → public swarm (no marker, no flag).
            assert private_mode_enabled(state, argv=[]) is False
            # --private argv flag forces it on.
            assert private_mode_enabled(state, argv=["x", "--private"]) is True
            # A persisted marker file turns it on.
            (state / PRIVATE_MODE_MARKER).touch()
            assert private_mode_enabled(state, argv=[]) is True
            # The env var wins even without a marker.
            (state / PRIVATE_MODE_MARKER).unlink()
            os.environ["EPHEMERAL_PRIVATE"] = "1"
            assert private_mode_enabled(state, argv=[]) is True
        finally:
            if old is None:
                os.environ.pop("EPHEMERAL_PRIVATE", None)
            else:
                os.environ["EPHEMERAL_PRIVATE"] = old

        # Private swarm join seed: persist/clear + parse into bootstrap args.
        assert read_private_seed(state) is None
        write_private_seed("endpoint-ticket-abc", state)
        assert read_private_seed(state) == "endpoint-ticket-abc"
        seeds, seed_nodes = parse_private_seed(read_private_seed(state))
        assert seeds == ["endpoint-ticket-abc"] and seed_nodes == []
        # node_id@relay parses as a node-id bootstrap.
        nid = "a" * 64
        write_private_seed(f"{nid}@https://relay.example.com.", state)
        seeds, seed_nodes = parse_private_seed(read_private_seed(state))
        assert seeds == [] and seed_nodes == [(nid, "https://relay.example.com.")]
        # Clearing removes the marker.
        write_private_seed(None, state)
        assert read_private_seed(state) is None
    print("PASS: private mode helpers (flag/env/marker, join seed, student URL)")


# ---------------------------------------------------------------------------
# Layer 2: two-node integration test (requires iroh + local connectivity)
# ---------------------------------------------------------------------------

async def _fake_executor(request):
    """Async-generator JobExecutor: emits two logs then a done event."""
    yield JobLogEvent(channel="stdout", data=b"computing...\n", job_id=request.job_id)
    await asyncio.sleep(0.2)
    yield JobLogEvent(channel="stderr", data=b"warning: none\n", job_id=request.job_id)
    yield JobDoneEvent(
        exit_code=0,
        stdout="## Result\n42\n",
        stderr="warning: none\n",
        job_id=request.job_id,
    )


async def _failing_executor(request):
    yield JobLogEvent(channel="stdout", data=b"about to fail\n", job_id=request.job_id)
    raise RuntimeError("sandbox exploded")


async def _run_integration() -> bool:
    from ephemeral_net.node import Node

    alice = Node(relay="disabled", idle_timeout=5.0)
    bob = Node(relay="disabled", executor=_fake_executor, idle_timeout=5.0)
    try:
        await alice.start()
        await bob.start()
        print(f"  alice node_id={alice.node_id()[:8]}...")
        print(f"  bob   node_id={bob.node_id()[:8]}...")

        # --- handshake / discovery ---
        try:
            peer = await asyncio.wait_for(alice.dial(bob.ticket()), timeout=30)
        except asyncio.TimeoutError:
            print("  SKIP: no local connectivity (dial timed out)")
            return False
        print("  dial + hello handshake ok")
        assert peer.node_id == bob.node_id()
        assert bob.node_id() in alice.table.known_peer_ids(), "alice learned bob via hello"
        assert alice.node_id() in bob.table.known_peer_ids(), "bob learned alice via hello"
        assert alice.table.ticket_for(bob.node_id()) is not None

        # --- first job ---
        req = JobRequest(
            job_id="job-1",
            document_blob=base64.b64encode(b"```python\nprint(42)\n```").decode(),
            timeout=30,
        )
        events = [e async for e in alice.submit_job(peer, req)]
        logs = [e for e in events if isinstance(e, JobLogEvent)]
        dones = [e for e in events if isinstance(e, JobDoneEvent)]
        assert len(logs) == 2, f"expected 2 log events, got {len(logs)}"
        assert logs[0].data == b"computing...\n"
        assert dones and dones[0].exit_code == 0 and dones[0].stdout == "## Result\n42\n"
        print("  job-1 streamed 2 logs + done (exit 0)")

        # --- second job on the same connection ---
        req2 = JobRequest(job_id="job-2", document_blob="", timeout=30)
        events2 = [e async for e in alice.submit_job(peer, req2)]
        assert any(isinstance(e, JobDoneEvent) for e in events2)
        print("  job-2 reused the same connection")

        # --- remote executor failure surfaces as JobError ---
        failing = Node(relay="disabled", executor=_failing_executor, idle_timeout=5.0)
        await failing.start()
        try:
            fpeer = await asyncio.wait_for(alice.dial(failing.ticket()), timeout=30)
            try:
                async for _ in alice.submit_job(fpeer, JobRequest(job_id="job-3", document_blob="")):
                    pass
            except JobError as e:
                assert "sandbox exploded" in str(e)
                print("  failing executor surfaced as JobError on the client")
            else:
                raise AssertionError("expected JobError")
        finally:
            await failing.close()

        print("  INTEGRATION OK")
        return True
    finally:
        await alice.close()
        await bob.close()


async def _offload_worker_executor(request):
    """The warm worker's executor: records that it ran the job."""
    _offload_worker_executor.ran.append(request.job_id)
    yield JobLogEvent(channel="stdout", data=b"ran on worker\n", job_id=request.job_id)
    yield JobDoneEvent(exit_code=0, stdout="ran on worker\n", stderr="",
                       job_id=request.job_id)


_offload_worker_executor.ran = []


def test_fetch_swarm_list():
    """The live bootstrap list is fetched, parsed, and first-URL-wins."""
    from ephemeral_net.swarm import fetch_swarm_list

    with tempfile.TemporaryDirectory(prefix="ephemeral-list-") as d:
        good = Path(d) / "swarm.json"
        good.write_text(
            json.dumps(
                {
                    "updated": "2026-08-10T00:00:00Z",
                    "nodes": [
                        {"node_id": "a" * 64, "relay": "https://relay.example.", "ticket": "t1"},
                        {"node_id": "b" * 64, "relay": None, "ticket": "t2"},
                        {"relay": "no-identity-entry"},  # dropped
                    ],
                }
            ),
            encoding="utf-8",
        )
        bad = Path(d) / "bad.json"
        bad.write_text("not json", encoding="utf-8")

        # First URL that parses wins.
        nodes = fetch_swarm_list([bad.as_uri(), good.as_uri()])
        assert len(nodes) == 2, nodes
        assert nodes[0]["node_id"] == "a" * 64
        assert nodes[1]["ticket"] == "t2"

        # No reachable/parseable URL -> [] (callers retry next cycle).
        assert fetch_swarm_list([bad.as_uri()]) == []
        assert fetch_swarm_list([]) == []
    print("  fetch_swarm_list: parsing + fallback OK")


def test_parse_swarm_list_dns():
    """TXT mirror parsing: entries, quotes, multi-string, defaults."""
    from ephemeral_net.swarm import DEFAULT_RELAY, parse_swarm_list_dns

    # One entry with an explicit relay.
    good = "iroh1:" + "a" * 64 + ";https://relay.example."
    assert parse_swarm_list_dns(good) == [("a" * 64, "https://relay.example.")]

    # Relay-less entries default to DEFAULT_RELAY; quotes (how resolvers
    # render TXT, including mid-content) are stripped.
    quoted = '"iroh1:' + "b" * 64 + ';https://r1.,iroh1:' + "c" * 64 + '"'
    assert parse_swarm_list_dns(quoted) == [
        ("b" * 64, "https://r1."),
        ("c" * 64, DEFAULT_RELAY),
    ]

    # Comma-separated multiple entries, mixed relays.
    multi = "iroh1:" + "d" * 64 + ";https://r1.,iroh1:" + "e" * 64
    assert parse_swarm_list_dns(multi) == [
        ("d" * 64, "https://r1."),
        ("e" * 64, DEFAULT_RELAY),
    ]

    # Non-iroh1 records (SPF, DKIM, ...) and malformed entries are skipped.
    assert parse_swarm_list_dns("v=spf1 include:_spf.example.com ~all") == []
    assert parse_swarm_list_dns("iroh1:not-a-node-id;https://relay.example.") == []
    print("PASS: parse_swarm_list_dns")


def test_fetch_swarm_list_dns():
    """DoH TXT lookup: mirrors the full list, falls through on failure."""
    from unittest import mock

    from ephemeral_net.swarm import fetch_swarm_list_dns

    class _Res:
        def __enter__(self):
            return self

        def __exit__(self, *a):
            return False

        def read(self):
            return (
                b'{"Status":0,"Answer":[{"name":"x","type":16,"TTL":120,'
                b'"data":"iroh1:'
                + b"c" * 64
                + b';https://relay.example.,iroh1:'
                + b"d" * 64
                + b';https://relay2.example."}]}'
            )

    with mock.patch("urllib.request.urlopen", return_value=_Res()):
        entries = fetch_swarm_list_dns("_ephemeral-swarm.example.com")
    assert entries == [
        ("c" * 64, "https://relay.example."),
        ("d" * 64, "https://relay2.example."),
    ]

    # Duplicate node ids across strings/answers are deduped.
    class _Dup(_Res):
        def read(self):
            return (
                b'{"Status":0,"Answer":[{"name":"x","type":16,"TTL":120,'
                b'"data":"iroh1:'
                + b"c" * 64
                + b';https://r1.,iroh1:'
                + b"c" * 64
                + b';https://r2."}]}'
            )

    with mock.patch("urllib.request.urlopen", return_value=_Dup()):
        assert fetch_swarm_list_dns("_ephemeral-swarm.example.com") == [
            ("c" * 64, "https://r1.")
        ]

    # Unreachable resolvers / missing record -> [] (callers retry).
    with mock.patch("urllib.request.urlopen", side_effect=OSError("no network")):
        assert fetch_swarm_list_dns("_ephemeral-swarm.example.com") == []

    class _NoAnswer(_Res):
        def read(self):
            return b'{"Status":0,"Answer":[{"name":"x","type":16,"TTL":120,' \
                b'"data":"v=spf1 ~all"}]}'

    with mock.patch("urllib.request.urlopen", return_value=_NoAnswer()):
        assert fetch_swarm_list_dns("_ephemeral-swarm.example.com") == []

    # No hostname configured -> no lookup at all.
    with mock.patch("urllib.request.urlopen", side_effect=AssertionError("must not call")):
        assert fetch_swarm_list_dns("") == []
    print("PASS: fetch_swarm_list_dns (DoH JSON)")


def test_probe_helpers():
    """Liveness-probe payload, verdict, and staleness bookkeeping are pure."""
    from ephemeral_net.probe import (
        PROBE_MAX_FAILS,
        UNREACHABLE_MAX_MISSES,
        UNREACHABLE_MAX_MISSES_NEVER_VERIFIED,
        build_probe_document,
        mark_probe,
        probe_nonce,
        probe_verdict,
        should_evict,
    )
    from ephemeral_net.sandbox import sanitize_markdown

    # The probe document is a minimal bash run that passes sanitization on
    # a real node (image resolves to the allowlisted alpine image, which
    # every node pre-hydrates so first probes verify without a pull).
    nonce = probe_nonce("a" * 64)
    assert nonce.startswith("ephemeral-alive-") and "a" * 10 in nonce
    assert probe_nonce("a" * 64) != nonce, "nonces must be unpredictable"
    doc = build_probe_document(nonce)
    assert f"echo {nonce}" in doc
    clean, images = sanitize_markdown(doc)
    assert images == [BASH_IMG], f"probe must run on the bash image, got {images}"
    assert nonce in clean

    # Verdict: exit 0 AND the exact nonce in stdout — a canned/captured
    # answer or a crash must fail.
    ok, detail = probe_verdict(0, f"## Result\n{nonce}\n", nonce)
    assert ok and detail == "ok"
    assert probe_verdict(0, "alive\n", nonce)[0] is False
    assert probe_verdict(0, "", nonce)[0] is False
    assert probe_verdict(1, nonce, nonce)[0] is False
    assert probe_verdict(0, nonce, "other-nonce")[0] is False

    # Bookkeeping: counters carry over from the previous list entry;
    # any successful dial (ok / failed / reached) marks the node alive.
    prev = {"node_id": "x", "probe_fails": 2, "misses": 3}
    assert mark_probe({}, prev, status="ok") == {
        "probe_fails": 0, "misses": 0, "seen_alive": True}
    failed = mark_probe({}, prev, status="failed")
    assert failed == {"probe_fails": 3, "misses": 0, "seen_alive": True}
    reached = mark_probe({}, prev, status="reached")
    assert reached["misses"] == 0 and reached["probe_fails"] == 2
    assert reached["seen_alive"] is True
    unreach = mark_probe({}, prev, status="unreachable")
    assert unreach == {"probe_fails": 2, "misses": 4, "seen_alive": False}
    assert mark_probe({}, None, status="failed")["probe_fails"] == 1
    # seen_alive is only ever set by an actual dial, and survives misses.
    assert mark_probe({}, None, status="unreachable") == {
        "probe_fails": 0, "misses": 1, "seen_alive": False}

    # Eviction: only when a counter crosses its threshold; never-verified
    # entries get a short leash; the genesis anchor is always exempt.
    assert should_evict({"node_id": "x", "probe_fails": PROBE_MAX_FAILS})
    assert not should_evict({"node_id": "x", "probe_fails": PROBE_MAX_FAILS - 1})
    # Never dialed once: dropped after the short leash, not the long one.
    assert should_evict({"node_id": "x", "misses": UNREACHABLE_MAX_MISSES_NEVER_VERIFIED})
    assert not should_evict(
        {"node_id": "x", "misses": UNREACHABLE_MAX_MISSES_NEVER_VERIFIED - 1})
    # Was alive once: gets the full recovery grace.
    assert not should_evict(
        {"node_id": "x", "seen_alive": True, "misses": UNREACHABLE_MAX_MISSES - 1})
    assert should_evict(
        {"node_id": "x", "seen_alive": True, "misses": UNREACHABLE_MAX_MISSES})
    assert not should_evict({"node_id": "x"})
    assert not should_evict(
        {"node_id": "genesis", "probe_fails": 99, "misses": 99},
        seed_ids={"genesis"},
    )
    print("PASS: probe helpers (payload, verdict, counters, eviction)")


async def _run_list_bootstrap_integration() -> bool:
    """
    No compiled seeds: a node bootstraps from the LIVE SWARM LIST
    (docs/swarm.json shape) and connects to a listed member.
    """
    from ephemeral_net.node import Node

    b = Node(relay="disabled", idle_timeout=5.0)
    await b.start()
    a = Node(relay="disabled", idle_timeout=5.0)
    try:
        with tempfile.TemporaryDirectory(prefix="ephemeral-list-boot-") as d:
            lst = Path(d) / "swarm.json"
            lst.write_text(
                json.dumps(
                    {
                        "updated": "2026-08-10T00:00:00Z",
                        "nodes": [
                            {"node_id": b.node_id(), "relay": None, "ticket": b.ticket()},
                        ],
                    }
                ),
                encoding="utf-8",
            )
            await a.start()
            await asyncio.wait_for(a.bootstrap_from_list([lst.as_uri()]), timeout=30)
        assert b.node_id() in a._peers, "A should have dialed B from the list"
        print("  LIST-BOOTSTRAP OK: dialed a listed member with no compiled seed")
        return True
    finally:
        await a.close()
        await b.close()


async def _run_dial_by_id_integration() -> bool:
    """
    iroh-native identity: dial a node by its STABLE NODE ID + relay URL
    (no ticket), across a restart of the target. The relay routes by
    node id, so a compiled-in id never goes stale.
    """
    from ephemeral_net.node import Node
    from ephemeral_net.swarm import load_or_create_secret

    state_dir = tempfile.mkdtemp(prefix="ephemeral-dialbyid-")
    secret = load_or_create_secret(Path(state_dir) / "secret.bin")

    async def _spawn() -> Node:
        node = Node(secret_key=secret, relay="n0")
        await node.start()
        return node

    b = await _spawn()
    b_id = b.node_id()
    b_relay = b.relay_url()
    print(f"  B id={b_id[:12]}... relay={b_relay}")
    await b.close()

    # B restarts (same secret -> same id, new ports). A dials by id + relay.
    b2 = await _spawn()
    a = Node(relay="n0")
    await a.start()
    try:
        peer = await asyncio.wait_for(a.dial_node(b_id, b_relay), timeout=30)
        assert peer.node_id == b_id
        print("  DIAL-BY-ID OK: dialed restarted node by node id + relay, no ticket")
        return True
    finally:
        await a.close()
        await b2.close()


async def _run_mesh_heal_integration() -> bool:
    """
    Mesh healing: after a peer connection drops, the node re-dials the
    peer straight from its peer table (no seed involved) and repairs the
    mesh — the swarm should heal around a dead seed.
    """
    from ephemeral_net.node import Node

    a = Node(relay="disabled", idle_timeout=5.0)
    b = Node(relay="disabled", idle_timeout=5.0)
    try:
        await a.start()
        await b.start()

        # B dials A directly; both learn each other's ticket via hello.
        await asyncio.wait_for(b.dial(a.ticket()), timeout=30)
        b_id = b.node_id()
        assert b_id in a._peers, "A holds B's connection"
        assert a.table.ticket_for(b_id), "A learned B's dial-back ticket"
        print("  A + B connected; A learned B's ticket via hello")

        # Simulate a dropped connection.
        a._peers[b_id].connection.close(0, b"test drop")
        for _ in range(50):
            if b_id not in a._peers:
                break
            await asyncio.sleep(0.1)
        assert b_id not in a._peers, "A's registry dropped the dead connection"
        print("  connection dropped; A's registry cleaned up")

        # Heal: A re-dials B straight from its peer table.
        await a._mesh_heal_once()
        assert b_id in a._peers, "A reconnected to B via mesh healing"
        print("  MESH HEAL OK: A re-dialed dropped peer B from its table")
        return True
    finally:
        await a.close()
        await b.close()


async def _run_offload_integration() -> bool:
    """
    Three-node offloading test: requester -> offloader (image not warm)
    -> worker (image warm). The offloader must forward and background-pull.
    """
    from ephemeral_net.node import Node
    from ephemeral_net.offload import OffloadingExecutor
    from ephemeral_net.sandbox import CoreJobExecutor

    async def _fake_runner(markdown_text, timeout, server_mode):
        class _Result:
            stdout = "## Result\nlocal-core\n"
            stderr = ""
            exit_code = 0
            artifact_paths = []
        return _Result()

    pulled = []

    async def _fake_pull(image):
        pulled.append(image)

    worker = Node(
        relay="disabled",
        executor=_offload_worker_executor,
        idle_timeout=5.0,
        list_images=lambda: [PY_IMG],
    )
    local = CoreJobExecutor(
        runner=_fake_runner,
        image_exists=lambda image: False,  # never warm locally
        pull=_fake_pull,
    )
    # mesh_pull=False: the offload path must keep using the registry-pull
    # fallback here (a live registry fetch would race this test).
    offloader = Node(
        relay="disabled", idle_timeout=5.0, list_images=lambda: [], mesh_pull=False
    )
    offloader.executor = OffloadingExecutor(offloader, local)
    requester = Node(relay="disabled", idle_timeout=5.0)
    try:
        await worker.start()
        await offloader.start()
        await requester.start()

        # Offloader learns the worker is warm.
        wpeer = await asyncio.wait_for(offloader.dial(worker.ticket()), timeout=30)
        assert wpeer.images == {PY_IMG}, f"worker should advertise warm image, got {wpeer.images}"
        assert wpeer.rtt is not None
        print("  offloader learned worker's warm image + rtt")

        # Requester submits through the offloader.
        rpeer = await asyncio.wait_for(requester.dial(offloader.ticket()), timeout=30)
        req = JobRequest(
            job_id="offload-1",
            document_blob=base64.b64encode(b"```python\nprint(1)\n```").decode(),
            timeout=30,
        )
        events = [e async for e in requester.submit_job(rpeer, req)]
        dones = [e for e in events if isinstance(e, JobDoneEvent)]
        assert dones and dones[0].stdout == "ran on worker\n", f"got {dones}"
        assert _offload_worker_executor.ran == ["offload-1"]
        await asyncio.sleep(0.05)
        assert pulled == [PY_IMG], f"offloader should background-pull, got {pulled}"
        print("  offload-1 forwarded to warm worker; background pull started")

        # A second job with the same image still offloads while pulling.
        _offload_worker_executor.ran.clear()
        req2 = JobRequest(
            job_id="offload-2",
            document_blob=base64.b64encode(b"```python\nprint(2)\n```").decode(),
            timeout=30,
        )
        events2 = [e async for e in requester.submit_job(rpeer, req2)]
        assert any(isinstance(e, JobDoneEvent) for e in events2)
        assert _offload_worker_executor.ran == ["offload-2"]
        print("  offload-2 forwarded again while pull continues")

        print("  OFFLOAD INTEGRATION OK")
        return True
    finally:
        await worker.close()
        await offloader.close()
        await requester.close()


async def _run_probe_integration() -> bool:
    """
    Liveness probe over a real connection: a worker that executes the
    payload verifies; a hello-only node (no executor) must fail it — the
    exact bot-detection the refresh workflow relies on.
    """
    import re

    from ephemeral_net.node import Node
    from ephemeral_net.probe import run_probe
    from ephemeral_net.sandbox import CoreJobExecutor

    async def _echo_runner(markdown_text, timeout, server_mode):
        """Simulate a real node: run the bash payload, echo its output."""
        m = re.search(r"echo\s+([A-Za-z0-9_-]+)", markdown_text)
        class _Result:
            stdout = (m.group(1) + "\n") if m else ""
            stderr = ""
            exit_code = 0
            artifact_paths = []
        return _Result()

    worker = Node(relay="disabled", idle_timeout=5.0)
    worker.executor = CoreJobExecutor(runner=_echo_runner)
    # A plain node that speaks hello but has no executor — a bot that
    # merely answers the handshake looks exactly like this.
    hollow = Node(relay="disabled", idle_timeout=5.0)
    probe = Node(relay="disabled", idle_timeout=5.0)
    try:
        await worker.start()
        await hollow.start()
        await probe.start()

        wpeer = await asyncio.wait_for(probe.dial(worker.ticket()), timeout=30)
        result = await asyncio.wait_for(
            run_probe(lambda req: probe.submit_job(wpeer, req), worker.node_id(),
                      timeout=30),
            timeout=70,
        )
        assert result["ok"], f"worker should verify, got {result}"
        assert result["detail"] == "ok"
        print(f"  probe ok against a real worker ({result['ms']} ms)")

        hpeer = await asyncio.wait_for(probe.dial(hollow.ticket()), timeout=30)
        result = await asyncio.wait_for(
            run_probe(lambda req: probe.submit_job(hpeer, req), hollow.node_id(),
                      timeout=30),
            timeout=70,
        )
        assert not result["ok"], f"hello-only node must fail the probe, got {result}"
        assert "does not run jobs" in result["detail"]
        print(f"  probe FAILED against hello-only node as expected: {result['detail']}")

        print("  PROBE INTEGRATION OK")
        return True
    finally:
        await worker.close()
        await hollow.close()
        await probe.close()


async def _run_eviction_integration() -> bool:
    """
    Staleness bookkeeping across real ``discover()`` runs: unreachable
    entries accumulate ``misses`` in the written list and are evicted
    once they cross the threshold — the refresh script must not keep
    dead entries forever. Regression test: mark_probe() returns a NEW
    entry with the counters, and discover() must write that back into
    the list (discarding it silently froze all counters at 0).
    """
    import scripts.update_swarm_json as upd  # noqa: F401
    from ephemeral_net.probe import (
        UNREACHABLE_MAX_MISSES,
        UNREACHABLE_MAX_MISSES_NEVER_VERIFIED,
    )

    with tempfile.TemporaryDirectory(prefix="ephemeral-evict-") as d:
        out = Path(d) / "swarm.json"
        genesis_id = "g" * 64
        never_id = "n" * 64
        recovering_id = "r" * 64
        out.write_text(
            json.dumps(
                {
                    "updated": "2026-08-12T00:00:00Z",
                    "nodes": [
                        # Never answered a dial: one miss short of the short
                        # leash — this run pushes it over and it must go.
                        {
                            "node_id": never_id,
                            "relay": None,
                            "ticket": "bogus-ticket",
                            "probe_fails": 0,
                            "misses": UNREACHABLE_MAX_MISSES_NEVER_VERIFIED - 1,
                        },
                        # Was alive once (seen_alive) and offline since:
                        # keeps the full recovery grace, counters accumulate.
                        {
                            "node_id": recovering_id,
                            "relay": None,
                            "ticket": "bogus-ticket",
                            "probe_fails": 0,
                            "misses": 1,
                            "seen_alive": True,
                        },
                        # The genesis anchor is operator config: exempt even
                        # when it has been silent for a very long time.
                        {
                            "node_id": genesis_id,
                            "relay": None,
                            "ticket": "bogus-ticket",
                            "probe_fails": 0,
                            "misses": 99,
                        },
                    ],
                }
            ),
            encoding="utf-8",
        )

        # A bogus relay + bogus tickets make every dial fail fast (no 20 s
        # per-node timeouts); the genesis is passed as operator config.
        genesis = [(genesis_id, "https://127.0.0.1:1")]
        r1 = await upd.discover(out, max_nodes=50, genesis=genesis)
        by_id = {n["node_id"]: n for n in r1["nodes"]}
        assert never_id not in by_id, "never-verified entry must be evicted"
        assert genesis_id in by_id and by_id[genesis_id]["misses"] == 100, \
            "genesis is exempt from eviction"
        assert by_id[recovering_id]["misses"] == 2, \
            f"previously-alive entry should carry 2 misses, got {by_id[recovering_id]}"
        # The evicted set must be written to the output so the next run
        # can filter gossip discoveries against it.
        evicted_out = set(r1.get("evicted") or {})
        assert never_id in evicted_out, "evicted node must appear in evicted set"
        assert genesis_id not in evicted_out, "genesis must never be in evicted set"
        assert isinstance(r1["evicted"], dict), "evicted must be a timestamped map"
        assert all(
            isinstance(ts, (int, float)) for ts in r1["evicted"].values()
        ), "evicted timestamps must be numeric"
        print("  run 1: never-verified evicted; recovering kept; genesis kept")

        # Persist run 1 the way main() does (discover() returns the list;
        # the workflow writes it before the next run reads it back).
        out.write_text(json.dumps(r1, indent=2) + "\n", encoding="utf-8")

        # A second run reads the file back: counters survive across runs,
        # and the recovering entry is still within its grace period.
        r2 = await upd.discover(out, max_nodes=50, genesis=genesis)
        by_id2 = {n["node_id"]: n for n in r2["nodes"]}
        assert by_id2[recovering_id]["misses"] == 3, \
            f"misses must accumulate across runs, got {by_id2[recovering_id]}"
        # The evicted set persists across runs.
        evicted_r2 = set(r2.get("evicted") or {})
        assert never_id in evicted_r2, "evicted set must persist across runs"
        print("  run 2: counters persisted; recovering entry still kept")

        # Reset: forget the whole list and regenerate from the genesis
        # anchor alone — every previous entry is dropped regardless of
        # its counters (nothing is reachable here, so the fresh census
        # is empty).
        r3 = await upd.discover(out, max_nodes=50, genesis=genesis, reset=True)
        assert r3["nodes"] == [], "reset must drop every previous entry"
        print("  run 3: reset regenerated from scratch (empty fresh census)")
        print("  EVICTION INTEGRATION OK")
        return True


async def _run_mesh_blob_integration() -> bool:
    """
    Mesh image blob transfer over real iroh QUIC connections: a node with
    a warm image (fake OCI exporter) serves a content-addressed blob; the
    client fetches it and verifies its sha256; a missing blob surfaces as
    an ImagePullError; a node with serving disabled refuses the request.
    """
    from ephemeral_net.image_pull import ImagePullError
    from ephemeral_net.node import Node

    layer = b"mesh-integration-layer-" * 1000
    digest = "sha256:" + hashlib.sha256(layer).hexdigest()
    image = "docker.io/library/alpine:latest"

    def fake_exporter(image_name, out):
        out = Path(out)
        blobs = out / "blobs" / "sha256"
        blobs.mkdir(parents=True, exist_ok=True)
        (blobs / digest).write_bytes(layer)
        return out

    server = Node(
        relay="disabled", idle_timeout=5.0,
        list_images=lambda: [image],
        image_exporter=fake_exporter,
    )
    client = Node(relay="disabled", idle_timeout=5.0)
    noserve = Node(relay="disabled", idle_timeout=5.0, serve_blobs=False)
    try:
        await server.start()
        await client.start()
        await noserve.start()
        peer = await asyncio.wait_for(client.dial(server.ticket()), timeout=30)

        with tempfile.TemporaryDirectory(prefix="mesh-blob-") as d:
            dest = Path(d) / "layer"
            await asyncio.wait_for(
                client.fetch_blob(peer, image, digest, len(layer), dest), timeout=30
            )
            assert dest.read_bytes() == layer, "fetched blob bytes mismatch"
            print("  blob fetched over iroh and sha256-verified")

            # Missing blob -> ImagePullError carrying the peer's refusal.
            try:
                await client.fetch_blob(
                    peer, image, "sha256:" + "0" * 64, 1, Path(d) / "missing"
                )
                raise AssertionError("expected ImagePullError for missing blob")
            except ImagePullError as e:
                assert "not available" in str(e)
            print("  missing blob surfaced as ImagePullError")

            # A node with serving disabled refuses blob requests.
            npeer = await asyncio.wait_for(client.dial(noserve.ticket()), timeout=30)
            try:
                await client.fetch_blob(
                    npeer, image, digest, len(layer), Path(d) / "refused"
                )
                raise AssertionError("expected ImagePullError from non-serving node")
            except ImagePullError as e:
                assert "does not serve" in str(e)
            print("  non-serving node refused the blob request")

        print("  MESH BLOB INTEGRATION OK")
        return True
    except asyncio.TimeoutError:
        print("  SKIP: no local connectivity (dial timed out)")
        return False
    finally:
        await server.close()
        await client.close()
        await noserve.close()


def main():
    # Layer 1
    test_frame_roundtrip()
    test_frame_binary_payload()
    test_frame_too_large()
    test_frame_malformed()
    test_artifact_frame_roundtrip()
    test_hello_frame()
    test_fetch_swarm_list()
    test_parse_swarm_list_dns()
    test_fetch_swarm_list_dns()
    test_probe_helpers()
    test_job_messages()
    test_peer_table_ttl_eviction()
    test_peer_table_gossip_does_not_refresh_ttl()
    test_peer_table_gossiped_new_peer_survives_merge()
    test_genesis_fallback_plan()
    test_resolve_genesis_from_url()
    test_evicted_tombstones_ttl()
    test_swarm_status_badge_payload()
    test_private_mode_helpers()
    test_sanitize_strips_unsafe_and_overrides()
    test_sanitize_rejects_unknown_language()
    test_sanitize_operator_network_flag()
    test_sanitize_seeds_and_empty()
    test_sanitize_custom_allowlist()
    test_sanitize_roundtrip_parses_cleanly()
    test_core_executor_streams_result()
    test_core_executor_streams_artifacts_and_caps()
    test_core_executor_ignores_remote_overrides()
    test_offload_runs_locally_when_warm()
    test_offload_forwards_to_warm_neighbor_and_pulls()
    test_offload_runs_locally_when_no_warm_neighbor()
    test_image_ref_parsing()
    test_blob_frame_helpers()
    test_oci_layout_assembly_and_verification()
    test_mesh_pull_orchestration()
    test_registry_manifest_fetch_auth_and_index()
    test_offload_background_pull_prefers_mesh()
    test_offload_background_pull_falls_back_to_registry()
    test_select_peer_prefers_idle_then_fast()
    test_fanout_split_runs()
    test_fanout_splits_and_merges()
    test_fanout_relays_artifacts()
    test_fanout_falls_back_locally()
    test_fanout_chained_runs_unsplit()

    # Layer 2
    try:
        import iroh  # noqa: F401
    except ImportError:
        print("\nSKIP: integration test requires iroh (pip install -r requirements-net.txt)")
        return

    print("\n--- two-node integration ---")
    ok = asyncio.run(_run_integration())
    if not ok:
        print("SKIP: integration test — no local connectivity")

    print("\n--- three-node offloading integration ---")
    ok = asyncio.run(_run_offload_integration())
    if not ok:
        print("SKIP: offload integration test — no local connectivity")

    print("\n--- liveness-probe integration (worker vs hello-only node) ---")
    ok = asyncio.run(_run_probe_integration())
    if not ok:
        print("SKIP: probe integration test — no local connectivity")

    print("\n--- staleness-eviction integration (discover() twice) ---")
    ok = asyncio.run(_run_eviction_integration())
    if not ok:
        print("SKIP: eviction integration test — no local connectivity")

    print("\n--- mesh image-blob integration ---")
    ok = asyncio.run(_run_mesh_blob_integration())
    if not ok:
        print("SKIP: mesh-blob integration test — no local connectivity")

    print("\n--- mesh-heal integration ---")
    ok = asyncio.run(_run_mesh_heal_integration())
    if not ok:
        print("SKIP: mesh-heal integration test — no local connectivity")

    print("\n--- dial-by-node-id integration (iroh-native identity) ---")
    ok = asyncio.run(_run_dial_by_id_integration())
    if not ok:
        print("SKIP: dial-by-id integration test — no local connectivity")

    print("\n--- live-list bootstrap integration (no compiled seeds) ---")
    ok = asyncio.run(_run_list_bootstrap_integration())
    if not ok:
        print("SKIP: list-bootstrap integration test — no local connectivity")


if __name__ == "__main__":
    main()
