"""Scratch: Python compute node on the public n0 relay for the WASM interop test.

Runs a fake (no-Podman) executor so the browser thin client can be verified
end-to-end: wasm SPA -> n0 relay -> this Python node -> events back.
"""
import asyncio

from ephemeral_net.jobs import JobDoneEvent, JobLogEvent
from ephemeral_net.node import Node


async def demo_executor(request):
    yield JobLogEvent(
        channel="stdout",
        data=b"hello from the python compute node\n",
        job_id=request.job_id,
    )
    await asyncio.sleep(0.4)
    yield JobLogEvent(
        channel="stderr",
        data=b"(demo executor: no podman involved)\n",
        job_id=request.job_id,
    )
    yield JobLogEvent(
        channel="stdout",
        data=f"processed job {request.job_id}\n".encode(),
        job_id=request.job_id,
    )
    yield JobDoneEvent(
        exit_code=0,
        stdout="## Result\nhello from the python compute node\n",
        stderr="",
        job_id=request.job_id,
    )


async def main():
    node = Node(relay="n0", executor=demo_executor, idle_timeout=300.0)
    await node.start()
    print(f"NODE_ID {node.node_id()}")
    print(f"TICKET {node.ticket()}")
    print("READY serving jobs (Ctrl+C to stop)", flush=True)
    try:
        await asyncio.Event().wait()
    except (KeyboardInterrupt, asyncio.CancelledError):
        pass
    finally:
        await node.close()


if __name__ == "__main__":
    asyncio.run(main())
