"""
Gateway — REST bridge + compute node for ``ephemeral-self-host-distributed``.

The gateway keeps the exact REST contract of ``main_api.py``
(``RunRequest``/``RunResponse``) while translating each request into a
network job payload. It joins the cluster as a compute node: jobs run in
the sandboxed local executor, and when an image is not warm locally the
job is offloaded to the nearest neighbor that has it (with a background
pull), so REST clients get fast turnaround without blocking on pulls.
"""
from __future__ import annotations

import base64
from dataclasses import dataclass
from typing import Callable, Sequence

from pydantic import BaseModel, Field, field_validator


class RunRequest(BaseModel):
    """
    Request payload for the distributed /ephemeral/api/v1/run endpoint.

    Identical shape to ``main_api.RunRequest``, but ``document_blob``
    stays base64-encoded so it can be forwarded to the network
    unchanged (``JobRequest.document_blob`` is base64 too).
    """

    document_blob: str
    timeout: int = Field(default=300, ge=1, le=600)

    @field_validator("document_blob", mode="before")
    @classmethod
    def validate_base64(cls, v: str) -> str:
        if not isinstance(v, str):
            raise ValueError("document_blob must be a base64-encoded string")
        try:
            base64.b64decode(v, validate=True)
        except Exception as e:
            raise ValueError(f"Invalid base64 encoding: {e}") from e
        return v


class GatewayError(Exception):
    """A job could not be run (bad input or infrastructure failure)."""


@dataclass
class GatewayResult:
    """Outcome of a gateway run — mirrors ``RunResponse`` fields."""

    exit_code: int
    stdout: str
    stderr: str
    artifact_file: str | None = None
    artifact_ext: str | None = None


class Gateway:
    """
    Owns a cluster node and translates REST runs into network jobs.

    Args:
        secret_key: 32 raw bytes for a persistent node identity
            (hex-encoded on the command line / env).
        relay: ``"n0"`` (public relays), ``"minimal"``, or ``"disabled"``
            (direct connections only).
        seeds: EndpointTicket strings of seed nodes to bootstrap from.
        allow_network: whether remote jobs may use network access.
        image_allowlist: allowed images for remote jobs; defaults to the
            ``ephemeral_core`` language map.
        node_factory: injectable for tests — callable returning a
            ``Node``-like object (``start``, ``close``, ``executor``,
            ``bootstrap``, ``node_id``).
    """

    def __init__(
        self,
        *,
        secret_key: bytes | None = None,
        relay: str = "n0",
        seeds: Sequence[str] = (),
        allow_network: bool = False,
        image_allowlist: Sequence[str] | None = None,
        node_factory: Callable[..., object] | None = None,
    ) -> None:
        self.secret_key = secret_key
        self.relay = relay
        self.seeds = list(seeds)
        self.allow_network = allow_network
        self.image_allowlist = image_allowlist
        self.node_factory = node_factory
        self._node = None

    # --- lifecycle -------------------------------------------------------

    @property
    def node(self):
        """The underlying cluster node (None before :meth:`start`)."""
        return self._node

    async def start(self) -> None:
        """Build the node with the sandboxed + offloading executor and join."""
        from ephemeral_net.node import Node
        from ephemeral_net.offload import OffloadingExecutor
        from ephemeral_net.sandbox import CoreJobExecutor

        factory = self.node_factory or (
            lambda **kw: Node(secret_key=kw["secret_key"], relay=kw["relay"])
        )
        node = factory(secret_key=self.secret_key, relay=self.relay)
        local = CoreJobExecutor(
            allow_network=self.allow_network,
            image_allowlist=self.image_allowlist,
        )
        node.executor = OffloadingExecutor(node, local)
        await node.start()
        if self.seeds:
            await node.bootstrap(self.seeds)
        self._node = node

    async def close(self) -> None:
        if self._node is not None:
            await self._node.close()
            self._node = None

    # --- REST bridge -----------------------------------------------------

    async def run(self, document_blob: str, timeout: int = 300) -> GatewayResult:
        """
        Run a base64-encoded Markdown document through the cluster.

        The request is submitted to this node's own executor, which runs
        it locally when the required images are warm and otherwise
        offloads to the nearest warm neighbor while pulling in the
        background.
        """
        from ephemeral_net.jobs import JobErrorEvent, JobRequest

        if self._node is None:
            raise GatewayError("gateway is not started")
        if self._node.executor is None:
            raise GatewayError("gateway node has no executor")

        request = JobRequest(
            job_id=f"rest-{id(self)}",
            document_blob=document_blob,
            timeout=timeout,
        )
        try:
            events = [e async for e in self._node.executor(request)]
        except Exception as e:
            raise GatewayError(f"job failed: {e}") from e

        errors = [e for e in events if isinstance(e, JobErrorEvent)]
        if errors:
            raise GatewayError(errors[0].message)

        done = [e for e in events if not isinstance(e, JobErrorEvent)
                and getattr(e, "exit_code", None) is not None]
        if not done:
            raise GatewayError("job ended without a done event")
        final = done[-1]
        return GatewayResult(
            exit_code=final.exit_code,
            stdout=final.stdout or "",
            stderr=final.stderr or "",
            artifact_file=final.artifact_file,
            artifact_ext=final.artifact_ext,
        )

    # --- status ----------------------------------------------------------

    def status(self) -> dict:
        """Cluster status for a /health endpoint."""
        node = self._node
        if node is None:
            return {"status": "starting", "node_id": None, "peers": 0}
        try:
            node_id = node.node_id() if hasattr(node, "node_id") else None
            peers = len(node.table) if hasattr(node, "table") else 0
            warm = node.warm_images() if hasattr(node, "warm_images") else []
        except Exception:
            node_id, peers, warm = None, 0, []
        return {
            "status": "healthy",
            "node_id": node_id,
            "peers": peers,
            "warm_images": warm,
        }


__all__ = ["Gateway", "GatewayError", "GatewayResult", "RunRequest"]
