"""
Nearest-neighbor offloading for ephemeral_net.

When a thick node receives a job that needs a container image it does
not have cached locally, it must not block on a slow pull. Instead it:

1. forwards the job to the nearest neighbor that advertises the image
   warm, streaming that neighbor's events straight back to the requester,
   and
2. quietly pulls the image in the background so it is warm next time.

If no neighbor has the image warm, the node runs the job locally and
lets ``ephemeral_core``'s ``server_mode`` background-pull behavior
report the delay. If the forward to a neighbor fails, the node also
falls back to running locally instead of failing the job. If every
required image is already warm locally, the job runs locally immediately.

``OffloadingExecutor`` wraps a local :class:`CoreJobExecutor` (or any
executor exposing ``prepare(request)``, ``is_warm(image)``, and the
``JobExecutor`` protocol) and uses the owning :class:`Node` to find and
dial warm neighbors.
"""
from __future__ import annotations

import asyncio
import logging
from typing import AsyncIterator

from .jobs import JobEvent, JobRequest

logger = logging.getLogger(__name__)


class OffloadingExecutor:
    """JobExecutor that runs locally or forwards to a warm neighbor."""

    def __init__(self, node, local_executor, *, background_pull: bool = True) -> None:
        self.node = node
        self.local = local_executor
        self.background_pull = background_pull

    # --- helpers ---------------------------------------------------------

    def _needed_images(self, request: JobRequest) -> list[str] | None:
        """Required images for ``request``, or None if it cannot run at all."""
        try:
            _markdown, images = self.local.prepare(request)
            return images
        except Exception as e:
            logger.warning("offload prepare failed for %s: %s", request.job_id, e)
            return None

    def _start_background_pull(self, images: list[str], peer=None) -> None:
        """
        Pull ``images`` in the background so they are warm next time.

        When ``peer`` (the warm neighbor the job was forwarded to) is
        available, the image is first pulled from THAT peer over iroh —
        assembled from its blobs, verified against the registry manifest
        (``node.mesh_pull_image``) — and only falls back to the registry
        pull if the mesh pull fails or is disabled.
        """
        if not self.background_pull:
            return
        for image in images:
            pull = getattr(self.local, "pull", None)
            if pull is None:
                import ephemeral_core

                pull = ephemeral_core.pull_image

            async def _pull(img: str) -> None:
                pulled = False
                try:
                    mesh = getattr(self.node, "mesh_pull_image", None)
                    if mesh is not None:
                        pulled = await mesh(img, preferred_peer=peer)
                except Exception as e:  # pragma: no cover - best effort
                    logger.warning("mesh pull of %s failed: %s", img, e)
                if not pulled:
                    try:
                        await pull(img)
                    except Exception as e:  # pragma: no cover - best effort
                        logger.warning("background pull of %s failed: %s", img, e)

            asyncio.create_task(_pull(image))

    # --- JobExecutor protocol --------------------------------------------

    async def __call__(self, request: JobRequest) -> AsyncIterator[JobEvent]:
        images = self._needed_images(request)
        if images is None:
            # Let the local executor produce the rejection event.
            async for event in self.local(request):
                yield event
            return

        missing = [i for i in images if not self.local.is_warm(i)]
        if not missing:
            async for event in self.local(request):
                yield event
            return

        peer = self.node.peer_for_images(missing) if missing else None
        if peer is not None:
            # Forward to the warm neighbor; pull locally while it runs
            # (mesh pull from this same peer when available).
            self._start_background_pull(missing, peer=peer)
            logger.info(
                "offloading job %s for %s to %s (pulling %s locally)",
                request.job_id,
                missing,
                peer.node_id[:8],
                missing,
            )
            yielded = False
            try:
                async for event in self.node.submit_job(peer, request):
                    yielded = True
                    yield event
                return
            except Exception as e:
                if yielded:
                    # A partial stream already reached the requester — re-running
                    # locally would duplicate it. Surface the failure instead.
                    logger.exception(
                        "offload of %s to %s failed mid-stream",
                        request.job_id, peer.node_id[:8],
                    )
                    raise
                logger.exception(
                    "offload of %s to %s failed (%s); running locally",
                    request.job_id, peer.node_id[:8], e,
                )

        # No warm neighbor (or the forward failed): run locally
        # (server_mode background-pulls and reports the delay).
        async for event in self.local(request):
            yield event


__all__ = ["OffloadingExecutor"]
