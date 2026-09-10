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

from .jobs import JobErrorEvent, JobEvent, JobRequest

logger = logging.getLogger(__name__)


class OffloadingExecutor:
    """JobExecutor that runs locally or forwards to a warm neighbor."""

    def __init__(self, node, local_executor, *, background_pull: bool = True) -> None:
        self.node = node
        self.local = local_executor
        self.background_pull = background_pull

    # --- helpers ---------------------------------------------------------

    def _needed_images(
        self, request: JobRequest
    ) -> tuple[list[str] | None, Exception | None]:
        """Required images for ``request`` plus any prepare failure.

        Returns ``(images, None)`` on success, ``(None, error)`` when the
        document itself is rejected (bad payload, unknown or missing
        language, disallowed image, ...). Callers must surface ``error``
        directly — falling through to the local executor would mask the
        real reason with a misleading "no warm peer" message on
        orchestration-only bastions.
        """
        try:
            _markdown, images = self.local.prepare(request)
            return images, None
        except Exception as e:
            logger.warning("offload prepare failed for %s: %s", request.job_id, e)
            return None, e

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
        images, prepare_error = self._needed_images(request)
        if images is None:
            if prepare_error is not None:
                # The document itself was rejected (bad payload, unknown or
                # missing language, disallowed image, ...). Surface the REAL
                # reason — falling through to the local executor would
                # produce a misleading "no warm peer" error on
                # orchestration-only bastions and hide the actual problem
                # from the client.
                yield JobErrorEvent(
                    message=f"job rejected: {prepare_error}",
                    job_id=request.job_id,
                )
                return
            # No images to route on and no failure — let the local executor
            # produce its own outcome.
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
            # (mesh pull from this same peer when available). If the
            # forward fails before anything is streamed, the connection is
            # likely a zombie the peer's side dropped (idle timeout): evict
            # it and re-dial the peer once — an idle-dropped peer comes
            # back instantly and the job runs, a genuinely dead peer falls
            # through to the local path below.
            evict = getattr(self.node, "drop_peer", None)
            reestablish = getattr(self.node, "reestablish_peer", None)
            target = peer
            attempt = 0
            while target is not None:
                if attempt == 0:
                    self._start_background_pull(missing, peer=target)
                logger.info(
                    "offloading job %s for %s to %s (pulling %s locally)",
                    request.job_id,
                    missing,
                    target.node_id[:8],
                    missing,
                )
                yielded = False
                try:
                    async for event in self.node.submit_job(target, request):
                        yielded = True
                        yield event
                    return
                except Exception as e:
                    if yielded:
                        # A partial stream already reached the requester —
                        # re-running locally would duplicate it. Surface it.
                        logger.exception(
                            "offload of %s to %s failed mid-stream",
                            request.job_id, target.node_id[:8],
                        )
                        raise
                    logger.warning(
                        "offload of %s to %s failed (%s); %s",
                        request.job_id, target.node_id[:8], e,
                        "re-dialing the peer" if attempt == 0 else "giving up",
                    )
                    if evict is not None:
                        evict(target.node_id)
                    target = (
                        await reestablish(target)
                        if reestablish is not None and attempt == 0
                        else None
                    )
                    attempt += 1

        # No warm neighbor (or every forward failed): run locally
        # (server_mode background-pulls and reports the delay).
        async for event in self.local(request):
            yield event


__all__ = ["OffloadingExecutor"]
