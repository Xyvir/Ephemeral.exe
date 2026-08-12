"""
ephemeral_net — distributed execution network core for Ephemeral.

Built on the ``iroh`` Python bindings, this package is the shared
networking layer for the distributed tiers:

  * ``ephemeral-distributed.exe`` (Windows tray app)
  * ``ephemeral-self-host-distributed`` (headless compute node/gateway)
  * ``ephemeral-wasm-library`` (browser client — same wire protocol,
    implemented in Rust/wasm-bindgen)

Transport: iroh QUIC connections (public n0 relays by default, direct
connection when possible). Protocol: one length-prefixed JSON frame
exchange per QUIC bi-stream — a ``hello`` handshake on connection
establishment for seed-mediated discovery, then ``job_request`` streams
that stream ``job_log`` / ``job_done`` / ``error`` frames back. The job
contract mirrors the existing REST contract (RunRequest/RunResponse).

Phase 1: transport + protocol + discovery + job streaming.
Phase 2: receiver-side sandboxing (:class:`ephemeral_net.sandbox.CoreJobExecutor`
— image allowlist, ``unsafe`` stripped, overrides ignored, hard container
limits) and nearest-neighbor offloading
(:class:`ephemeral_net.offload.OffloadingExecutor` — forward to a warm
neighbor + background pull).
Phase 2.5: the self-host REST gateway in ``ephemeral_self_host`` bridges
``RunRequest``/``RunResponse`` into cluster jobs.
"""
from __future__ import annotations

from .discovery import PeerInfo, PeerTable
from .errors import (
    ConnectionClosed,
    FrameTooLarge,
    HandshakeError,
    JobError,
    NetError,
    ProtocolError,
)
from .jobs import (
    JobDoneEvent,
    JobErrorEvent,
    JobEvent,
    JobExecutor,
    JobLogEvent,
    JobRequest,
    parse_job_frame,
)
from .node import Node, PeerConnection
from .offload import OffloadingExecutor
from .probe import (
    DEFAULT_PROBE_TIMEOUT,
    PROBE_MAX_FAILS,
    UNREACHABLE_MAX_MISSES,
    UNREACHABLE_MAX_MISSES_NEVER_VERIFIED,
    build_probe_document,
    mark_probe,
    probe_nonce,
    probe_verdict,
    run_probe,
    should_evict,
)
from .protocol import ALPN, PROTOCOL_VERSION, encode_frame, decode_frame
from .sandbox import CoreJobExecutor, default_image_allowlist, sanitize_markdown

__all__ = [
    "ALPN",
    "DEFAULT_PROBE_TIMEOUT",
    "PROBE_MAX_FAILS",
    "PROTOCOL_VERSION",
    "UNREACHABLE_MAX_MISSES",
    "UNREACHABLE_MAX_MISSES_NEVER_VERIFIED",
    "ConnectionClosed",
    "CoreJobExecutor",
    "FrameTooLarge",
    "HandshakeError",
    "JobDoneEvent",
    "JobError",
    "JobErrorEvent",
    "JobEvent",
    "JobExecutor",
    "JobLogEvent",
    "JobRequest",
    "NetError",
    "Node",
    "OffloadingExecutor",
    "PeerConnection",
    "PeerInfo",
    "PeerTable",
    "ProtocolError",
    "build_probe_document",
    "decode_frame",
    "default_image_allowlist",
    "encode_frame",
    "mark_probe",
    "parse_job_frame",
    "probe_nonce",
    "probe_verdict",
    "run_probe",
    "sanitize_markdown",
    "should_evict",
]
