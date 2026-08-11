"""
Tests for ephemeral_self_host (the distributed self-host gateway).

Pure tests — no iroh, no Podman, no network. The Gateway is exercised
with a fake node so the REST -> cluster-job bridge is verified directly.

Run:  python test_self_host.py
"""
from __future__ import annotations

import asyncio
import base64

from ephemeral_self_host import Gateway, GatewayError, GatewayResult, RunRequest
from ephemeral_net.jobs import JobDoneEvent, JobErrorEvent, JobLogEvent


class _FakeTable:
    def __len__(self):
        return 2


class _FakeNode:
    """Node-like object recording lifecycle calls for the Gateway."""

    def __init__(self):
        self.executor = None
        self.started = False
        self.closed = False
        self.bootstrapped = []
        self.table = _FakeTable()

    async def start(self):
        self.started = True

    async def close(self):
        self.closed = True

    async def bootstrap(self, seeds):
        self.bootstrapped = list(seeds)

    def node_id(self):
        return "fake-node-id"

    def warm_images(self):
        return ["docker.io/library/node:18-alpine"]


async def _ok_events(request):
    yield JobLogEvent(channel="stdout", data=b"working...\n", job_id=request.job_id)
    yield JobDoneEvent(
        exit_code=0,
        stdout="## Result\n42\n",
        stderr="",
        artifact_file="out.txt",
        artifact_ext=".txt",
        job_id=request.job_id,
    )


async def _err_events(request):
    yield JobErrorEvent(message="no language detected", job_id=request.job_id)


def test_gateway_start_wires_executor_and_bootstraps():
    async def run():
        fake = _FakeNode()
        gw = Gateway(seeds=["ticket-a", "ticket-b"], node_factory=lambda **kw: fake)
        await gw.start()
        assert fake.started
        assert gw.node is fake
        assert fake.bootstrapped == ["ticket-a", "ticket-b"]
        # executor is FanoutExecutor -> OffloadingExecutor -> sandboxed core
        from ephemeral_net.fanout import FanoutExecutor
        from ephemeral_net.offload import OffloadingExecutor
        from ephemeral_net.sandbox import CoreJobExecutor

        assert isinstance(fake.executor, FanoutExecutor)
        assert isinstance(fake.executor.local, OffloadingExecutor)
        assert isinstance(fake.executor.local.local, CoreJobExecutor)
        await gw.close()
        assert fake.closed

    asyncio.run(run())
    print("PASS: gateway start wires fan-out+offloading sandboxed executor and bootstraps")


def test_gateway_run_maps_done_event():
    async def run():
        fake = _FakeNode()
        fake.executor = _ok_events
        gw = Gateway(node_factory=lambda **kw: fake)
        gw._node = fake  # pretend started
        result = await gw.run(
            base64.b64encode(b"```python\nprint(42)\n```").decode(), timeout=30
        )
        assert isinstance(result, GatewayResult)
        assert result.exit_code == 0
        assert result.stdout == "## Result\n42\n"
        assert result.artifact_file == "out.txt"
        assert result.artifact_ext == ".txt"

    asyncio.run(run())
    print("PASS: gateway run maps job events to a result")


def test_gateway_run_surfaces_errors():
    async def run():
        fake = _FakeNode()
        fake.executor = _err_events
        gw = Gateway(node_factory=lambda **kw: fake)
        gw._node = fake
        try:
            await gw.run(base64.b64encode(b"```python\nx\n```").decode())
        except GatewayError as e:
            assert "no language detected" in str(e)
            return
        raise AssertionError("expected GatewayError")

    asyncio.run(run())
    print("PASS: gateway run surfaces remote errors as GatewayError")


def test_run_request_validates_base64():
    ok = RunRequest(document_blob=base64.b64encode(b"x").decode())
    assert ok.document_blob  # stays base64-encoded for forwarding
    try:
        RunRequest(document_blob="!!!not-base64!!!")
    except Exception:
        pass
    else:
        raise AssertionError("expected validation error for bad base64")
    print("PASS: RunRequest validates base64 and keeps the raw blob")


def test_endpoint_maps_to_run_response():
    """The FastAPI endpoint function returns the same wire contract."""
    import main_distributed
    from main_api import RunResponse

    class _FakeGateway:
        async def run(self, document_blob, timeout=300):
            return GatewayResult(
                exit_code=0, stdout="## Result\nhi\n", stderr="",
                artifact_file=None, artifact_ext=None,
            )

    async def run():
        main_distributed.app.state.gateway = _FakeGateway()
        main_distributed.app.state.gateway_error = None
        req = RunRequest(document_blob=base64.b64encode(b"x").decode())
        resp = await main_distributed.run_code(req)
        assert isinstance(resp, RunResponse)
        assert resp.exit_code == 0 and resp.stdout == "## Result\nhi\n"

    asyncio.run(run())
    print("PASS: REST endpoint returns the RunResponse contract")


def main():
    test_gateway_start_wires_executor_and_bootstraps()
    test_gateway_run_maps_done_event()
    test_gateway_run_surfaces_errors()
    test_run_request_validates_base64()
    test_endpoint_maps_to_run_response()
    print("\n=== ALL SELF-HOST TESTS PASSED ===")


if __name__ == "__main__":
    main()
