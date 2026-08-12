"""
Cross-node fan-out for multi-run documents.

With artifact chaining off by default, a multi-block multi-language
request is a set of *independent* runs. Rather than executing them all
on one node, :class:`FanoutExecutor` splits the document into per-run
jobs and submits each to the best (idle-first, warm) neighbor it can
reach, merging the event streams back into one response.

When any block declares chaining (``chain``/``piping``/``pipe``), the
document is NOT split: it runs in-order on a single node exactly as
before, so artifacts keep flowing run-to-run. Single-run documents are
also never split. If a peer submission fails, that run falls back to
the local executor (which itself offloads warmest-neighbor-first), so a
multi-run request never fails harder than the pre-split path.

The executor chain wraps the existing offloading stack::

    FanoutExecutor(node, OffloadingExecutor(node, CoreJobExecutor(...)))

Every node wires the same chain, so any entry point (REST gateway,
desktop client, compute node) fans out multi-run work uniformly. A
fanned-out run is a single-run document, so receivers never split
again — no recursion.
"""
from __future__ import annotations

import asyncio
import base64
import logging
from typing import AsyncIterator

from .jobs import (
    JobArtifactEvent,
    JobDoneEvent,
    JobEvent,
    JobLogEvent,
    JobRequest,
)

logger = logging.getLogger(__name__)


def _run_images(doc: str) -> list[str]:
    """Required images for one run document (in order, deduped)."""
    from ephemeral_core.parser import parse_codeblocks

    try:
        blocks = parse_codeblocks(doc)
    except Exception:
        return []
    images: list[str] = []
    for b in blocks:
        if b["type"] == "code" and b.get("config") and b["config"].get("image"):
            image = b["config"]["image"]
            if image not in images:
                images.append(image)
    return images


def split_runs(markdown: str) -> tuple[list[str] | None, bool]:
    """
    Split a document into per-run Markdown documents.

    Returns ``(docs, any_chained)`` where ``docs`` is None when the
    document must NOT be split (empty/unparseable, a single run, or any
    block declaring chaining) and ``any_chained`` records whether
    chaining was declared. Runs group by identical resolved config, with
    seeds attached to the run that follows them — the same grouping the
    local executor uses.
    """
    from ephemeral_core.executor import group_into_runs
    from ephemeral_core.parser import parse_codeblocks

    try:
        blocks = parse_codeblocks(markdown)
    except Exception:
        return None, False
    if not blocks:
        return None, False
    try:
        runs = group_into_runs(blocks)
    except ValueError:
        return None, False

    code_blocks = [b for b in blocks if b["type"] == "code"]
    if not code_blocks:
        return None, False
    any_chained = any(
        b.get("config") and b["config"].get("allow_chain") for b in code_blocks
    )
    if len(runs) <= 1 or any_chained:
        return None, any_chained

    docs: list[str] = []
    for run in runs:
        parts = []
        for b in run:
            if b["type"] == "seed":
                header = f"{b['name']} b64" if b.get("is_b64") else b["name"]
                parts.append(f"```{header}\n{b['content']}\n```")
            else:
                parts.append(f"```{b.get('header') or ''}\n{b['content']}\n```")
        docs.append("\n".join(parts) + "\n")
    return docs, False


class FanoutExecutor:
    """
    JobExecutor that splits multi-run documents across idle warm peers.

    ``node`` exposes ``peer_for_images(images)`` and ``submit_job(peer,
    request)``; ``local_executor`` runs the fallback path (and the whole
    document when it must not be split). One merged ``job_done`` event
    terminates the stream, preserving the wire contract for consumers
    (gateway / desktop / SPA) unchanged.
    """

    def __init__(self, node, local_executor) -> None:
        self.node = node
        self.local = local_executor

    # --- JobExecutor protocol --------------------------------------------

    async def __call__(self, request: JobRequest) -> AsyncIterator[JobEvent]:
        try:
            markdown = base64.b64decode(
                request.document_blob, validate=True
            ).decode("utf-8")
        except Exception:
            async for event in self.local(request):
                yield event
            return

        docs, any_chained = split_runs(markdown)
        if docs is None:
            # Single run, chained (must stay in-order), or unparseable —
            # run the whole document as before.
            async for event in self.local(request):
                yield event
            return

        async def _run_one(index: int, doc: str) -> tuple[int, list[JobEvent]]:
            per_run = JobRequest(
                job_id=f"{request.job_id}-{index}",
                document_blob=base64.b64encode(doc.encode("utf-8")).decode("ascii"),
                timeout=request.timeout,
            )
            images = _run_images(doc)
            peer = self.node.peer_for_images(images) if images else None
            events: list[JobEvent] = []
            if peer is not None:
                try:
                    async for event in self.node.submit_job(peer, per_run):
                        events.append(event)
                    return index, events
                except Exception as e:
                    logger.warning(
                        "fan-out to %s failed, running locally: %s",
                        peer.node_id[:8] if getattr(peer, "node_id", None) else "?",
                        e,
                    )
                    events = []
            async for event in self.local(per_run):
                events.append(event)
            return index, events

        results = await asyncio.gather(
            *(_run_one(i, d) for i, d in enumerate(docs))
        )
        results.sort(key=lambda r: r[0])

        # Stream all runs' logs in document order, then the artifact frames,
        # then one merged done.
        for _index, events in results:
            for event in events:
                if isinstance(event, JobLogEvent):
                    yield event

        artifact_list: list[dict] = []
        for _index, events in results:
            for event in events:
                if isinstance(event, JobArtifactEvent):
                    yield event
                    artifact_list.append(
                        {"name": event.name, "ext": event.ext, "size": event.size}
                    )

        stdout_parts: list[str] = []
        stderr_parts: list[str] = []
        exit_code = 0
        artifact_file = artifact_ext = artifact_path = None
        for _index, events in results:
            for event in events:
                if not isinstance(event, JobDoneEvent):
                    continue
                if event.stdout:
                    stdout_parts.append(event.stdout)
                if event.stderr:
                    stderr_parts.append(event.stderr)
                if event.exit_code != 0 and exit_code == 0:
                    exit_code = event.exit_code
                if artifact_file is None and event.artifact_file:
                    artifact_file = event.artifact_file
                    artifact_ext = event.artifact_ext
                    artifact_path = event.artifact_path
        yield JobDoneEvent(
            exit_code=exit_code,
            stdout="\n".join(stdout_parts),
            stderr="\n".join(stderr_parts),
            artifact_file=artifact_file,
            artifact_ext=artifact_ext,
            artifact_path=artifact_path,
            artifact_list=artifact_list,
            job_id=request.job_id,
        )


__all__ = ["FanoutExecutor", "split_runs"]
