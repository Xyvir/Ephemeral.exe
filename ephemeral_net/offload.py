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
report the delay. If every required image is already warm locally, the
job runs locally immediately.

``OffloadingExecutor`` wraps a local :class:`CoreJobExecutor` (or any
executor exposing ``prepare(request)``, ``is_warm(image)``, and the
``JobExecutor`` protocol) and uses the owning :class:`Node` to find and
dial warm neighbors.
"""
from __future__ import annotations

import asyncio
import logging
from typing import AsyncIterator

from .jobs import JobErrorEvent, JobEvent, JobRequest

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

    def _start_background_pull(self, images: list[str]) -> None:
        """Pull ``images`` in the background so they are warm next time."""
        if not self.background_pull:
            return
        for image in images:
            pull = getattr(self.local, "pull", None)
            if pull is None:
                import ephemeral_core

                pull = ephemeral_core.pull_image

            async def _pull(img: str) -> None:
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
            # Forward to the warm neighbor; pull locally while it runs.
            self._start_background_pull(missing)
            logger.info(
                "offloading job %s for %s to %s (pulling %s locally)",
                request.job_id,
                missing,
                peer.node_id[:8],
                missing,
            )
            try:
                async for event in self.node.submit_job(peer, request):
                    yield event
                return
            except Exception as e:
                logger.exception("offload of %s to %s failed", request.job_id, peer.node_id)
                yield JobErrorEvent(
                    message=f"offload to neighbor failed: {e}",
                    job_id=request.job_id,
                )
                return

        # No warm neighbor: run locally (server_mode background-pulls and
        # reports the delay in the response).
        async for event in self.local(request):
            yield event


__all__ = ["OffloadingExecutor"]
