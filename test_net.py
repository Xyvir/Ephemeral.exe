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

    from ephemeral_net.jobs import JobDoneEvent, JobLogEvent, JobRequest
    from ephemeral_net.sandbox import CoreJobExecutor

    async def run():
        ex = CoreJobExecutor(runner=_fake_core_runner)
        req = JobRequest(
            job_id="j1",
            document_blob=base64.b64encode(b"```python\nprint(1)\n```").decode(),
        )
        events = [e async for e in ex(req)]
        logs = [e for e in events if isinstance(e, JobLogEvent)]
        dones = [e for e in events if isinstance(e, JobDoneEvent)]
        assert logs and b"hello from core" in logs[0].data
        assert dones and dones[0].exit_code == 0
        assert dones[0].stdout == "## Result\nhello from core\n"

    asyncio.run(run())
    print("PASS: CoreJobExecutor streams logs + done event")


def test_core_executor_ignores_remote_overrides():
    import base64

    from ephemeral_net.jobs import JobDoneEvent, JobLogEvent, JobRequest
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
        logs = [e for e in events if isinstance(e, JobLogEvent)]
        dones = [e for e in events if isinstance(e, JobDoneEvent)]
        assert logs and b"hello from core" in logs[0].data
        assert dones and dones[0].exit_code == 0

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
        build_probe_document,
        mark_probe,
        probe_nonce,
        probe_verdict,
        should_evict,
    )
    from ephemeral_net.sandbox import sanitize_markdown

    # The probe document is a minimal python run that passes sanitization
    # on a real node (image resolves to the allowlisted python image).
    nonce = probe_nonce("a" * 64)
    assert nonce.startswith("ephemeral-alive-") and "a" * 10 in nonce
    assert probe_nonce("a" * 64) != nonce, "nonces must be unpredictable"
    doc = build_probe_document(nonce)
    assert f"print({nonce!r})" in doc
    clean, images = sanitize_markdown(doc)
    assert images == [PY_IMG], f"probe must run on the python image, got {images}"
    assert nonce in clean

    # Verdict: exit 0 AND the exact nonce in stdout — a canned/captured
    # answer or a crash must fail.
    ok, detail = probe_verdict(0, f"## Result\n{nonce}\n", nonce)
    assert ok and detail == "ok"
    assert probe_verdict(0, "alive\n", nonce)[0] is False
    assert probe_verdict(0, "", nonce)[0] is False
    assert probe_verdict(1, nonce, nonce)[0] is False
    assert probe_verdict(0, nonce, "other-nonce")[0] is False

    # Bookkeeping: counters carry over from the previous list entry.
    prev = {"node_id": "x", "probe_fails": 2, "misses": 3}
    assert mark_probe({}, prev, status="ok") == {"probe_fails": 0, "misses": 0}
    failed = mark_probe({}, prev, status="failed")
    assert failed == {"probe_fails": 3, "misses": 0}
    reached = mark_probe({}, prev, status="reached")
    assert reached["misses"] == 0 and reached["probe_fails"] == 2
    unreach = mark_probe({}, prev, status="unreachable")
    assert unreach == {"probe_fails": 2, "misses": 4}
    assert mark_probe({}, None, status="failed")["probe_fails"] == 1

    # Eviction: only when a counter crosses its threshold, and the
    # genesis anchor is always exempt.
    assert should_evict({"node_id": "x", "probe_fails": PROBE_MAX_FAILS})
    assert not should_evict({"node_id": "x", "probe_fails": PROBE_MAX_FAILS - 1})
    assert should_evict({"node_id": "x", "misses": UNREACHABLE_MAX_MISSES})
    assert not should_evict({"node_id": "x", "misses": UNREACHABLE_MAX_MISSES - 1})
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
    offloader = Node(relay="disabled", idle_timeout=5.0, list_images=lambda: [])
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
        """Simulate a real node: run the python payload, echo its print."""
        m = re.search(r"print\('([^']+)'\)", markdown_text)
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
    from ephemeral_net.probe import UNREACHABLE_MAX_MISSES

    with tempfile.TemporaryDirectory(prefix="ephemeral-evict-") as d:
        out = Path(d) / "swarm.json"
        genesis_id = "g" * 64
        stale_id = "s" * 64
        fresh_id = "f" * 64
        out.write_text(
            json.dumps(
                {
                    "updated": "2026-08-12T00:00:00Z",
                    "nodes": [
                        # One miss short of the threshold: this run pushes it
                        # over and it must be dropped from the list.
                        {
                            "node_id": stale_id,
                            "relay": None,
                            "ticket": "bogus-ticket",
                            "probe_fails": 0,
                            "misses": UNREACHABLE_MAX_MISSES - 1,
                        },
                        # Never dialed successfully before: first miss recorded.
                        {
                            "node_id": fresh_id,
                            "relay": None,
                            "ticket": "bogus-ticket",
                            "probe_fails": 0,
                            "misses": 0,
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
        assert stale_id not in by_id, "entry over the miss threshold must be evicted"
        assert genesis_id in by_id and by_id[genesis_id]["misses"] == 100, \
            "genesis is exempt from eviction"
        assert by_id[fresh_id]["misses"] == 1, \
            f"fresh entry should carry 1 miss, got {by_id[fresh_id]}"
        print("  run 1: stale entry evicted; fresh miss recorded; genesis kept")

        # Persist run 1 the way main() does (discover() returns the list;
        # the workflow writes it before the next run reads it back).
        out.write_text(json.dumps(r1, indent=2) + "\n", encoding="utf-8")

        # A second run reads the file back: counters survive across runs.
        r2 = await upd.discover(out, max_nodes=50, genesis=genesis)
        by_id2 = {n["node_id"]: n for n in r2["nodes"]}
        assert by_id2[fresh_id]["misses"] == 2, \
            f"misses must accumulate across runs, got {by_id2[fresh_id]}"
        print("  run 2: misses persisted across runs")
        print("  EVICTION INTEGRATION OK")
        return True


def main():
    # Layer 1
    test_frame_roundtrip()
    test_frame_binary_payload()
    test_frame_too_large()
    test_frame_malformed()
    test_hello_frame()
    test_fetch_swarm_list()
    test_parse_swarm_list_dns()
    test_fetch_swarm_list_dns()
    test_probe_helpers()
    test_job_messages()
    test_sanitize_strips_unsafe_and_overrides()
    test_sanitize_rejects_unknown_language()
    test_sanitize_operator_network_flag()
    test_sanitize_seeds_and_empty()
    test_sanitize_custom_allowlist()
    test_sanitize_roundtrip_parses_cleanly()
    test_core_executor_streams_result()
    test_core_executor_ignores_remote_overrides()
    test_offload_runs_locally_when_warm()
    test_offload_forwards_to_warm_neighbor_and_pulls()
    test_offload_runs_locally_when_no_warm_neighbor()
    test_select_peer_prefers_idle_then_fast()
    test_fanout_split_runs()
    test_fanout_splits_and_merges()
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
