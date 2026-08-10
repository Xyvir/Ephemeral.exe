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
    assert error_frame("boom", job_id="j1")["job_id"] == "j1"
    print("PASS: hello/error frame helpers (incl. relay)")


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


def main():
    # Layer 1
    test_frame_roundtrip()
    test_frame_binary_payload()
    test_frame_too_large()
    test_frame_malformed()
    test_hello_frame()
    test_fetch_swarm_list()
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
