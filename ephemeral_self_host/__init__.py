"""
ephemeral_self_host — the ``ephemeral-self-host-distributed`` tier.

A headless backend for self-hosting (Docker/Coolify on a server) that
acts as a heavy compute node for direct REST requests *and* joins the
iroh cluster to serve distributed clients while idle.

The :class:`~ephemeral_self_host.gateway.Gateway` owns an
``ephemeral_net.Node`` wired with the sandboxed
:class:`~ephemeral_net.sandbox.CoreJobExecutor` (receiver-side safety:
image allowlist, ``unsafe`` stripped, overrides ignored, hard container
limits) wrapped in the :class:`~ephemeral_net.offload.OffloadingExecutor`
(forward to a warm neighbor + background pull when an image is missing).
REST requests are translated into network job payloads with the same
wire contract as ``main_api.py`` (``RunRequest``/``RunResponse``).
"""
from .gateway import Gateway, GatewayError, GatewayResult, RunRequest

__all__ = ["Gateway", "GatewayError", "GatewayResult", "RunRequest"]
