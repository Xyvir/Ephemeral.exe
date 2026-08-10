"""
Wire protocol for ephemeral_net.

Every logical exchange (handshake or job) runs on its own QUIC
bi-directional stream. Streams carry length-prefixed JSON frames::

    frame := u32be(len(payload)) || payload
    payload := UTF-8 JSON object with a "type" field

One exchange per stream, so a stream that sends a ``job_request``
receives a sequence of ``job_log`` / ``job_done`` / ``error`` frames
back and then both sides finish the stream.
"""
from __future__ import annotations

import json
import struct

from .errors import FrameTooLarge, ProtocolError

#: Default maximum frame payload size (16 MiB) — guards against memory abuse.
DEFAULT_MAX_FRAME_SIZE = 16 * 1024 * 1024

#: ALPN advertised by ephemeral nodes.
ALPN = b"ephemeral/1"

#: Protocol version reported in handshakes.
PROTOCOL_VERSION = 1


# --- Framing -------------------------------------------------------------

def encode_frame(msg: dict) -> bytes:
    """Serialize a message dict into a length-prefixed JSON frame."""
    payload = json.dumps(msg, separators=(",", ":")).encode("utf-8")
    return struct.pack(">I", len(payload)) + payload


def decode_frame(data: bytes, max_size: int = DEFAULT_MAX_FRAME_SIZE) -> dict:
    """
    Parse a single length-prefixed frame from ``data``.

    Raises :class:`FrameTooLarge` if the declared length exceeds
    ``max_size`` and :class:`ProtocolError` on malformed content.
    """
    if len(data) < 4:
        raise ProtocolError(f"frame shorter than 4-byte header: {len(data)} bytes")
    (size,) = struct.unpack(">I", data[:4])
    if size > max_size:
        raise FrameTooLarge(f"frame payload {size} exceeds limit {max_size}")
    if len(data) < 4 + size:
        raise ProtocolError(f"frame truncated: header says {size}, got {len(data) - 4}")
    try:
        msg = json.loads(data[4 : 4 + size])
    except ValueError as e:
        raise ProtocolError(f"invalid JSON frame: {e}") from e
    if not isinstance(msg, dict) or "type" not in msg:
        raise ProtocolError("frame payload must be a JSON object with a 'type' field")
    return msg


async def read_frame(
    recv,
    max_size: int = DEFAULT_MAX_FRAME_SIZE,
) -> dict:
    """Read one frame from a iroh ``RecvStream`` (or any ``read_exact``-like object)."""
    header = await recv.read_exact(4)
    (size,) = struct.unpack(">I", header)
    if size > max_size:
        raise FrameTooLarge(f"frame payload {size} exceeds limit {max_size}")
    payload = await recv.read_exact(size)
    return decode_frame(header + payload, max_size=max_size)


async def write_frame(send, msg: dict) -> None:
    """Write one frame to a iroh ``SendStream``."""
    await send.write_all(encode_frame(msg))


def error_frame(message: str, job_id: str | None = None) -> dict:
    """Build a wire ``error`` frame."""
    frame = {"type": "error", "message": message}
    if job_id is not None:
        frame["job_id"] = job_id
    return frame


# --- Handshake messages --------------------------------------------------

def hello_frame(
    node_id: str,
    ticket: str | None,
    peers: list[dict],
    images: list[str] | None = None,
) -> dict:
    """
    Build a ``hello`` frame.

    ``peers`` is a list of ``{"node_id": ..., "ticket": ...}`` entries
    the sender knows about, used for seed-mediated discovery.
    ``images`` is the sender's list of locally-cached container images
    ("warm" images), used for nearest-neighbor offloading.
    """
    return {
        "type": "hello",
        "v": PROTOCOL_VERSION,
        "node_id": node_id,
        "ticket": ticket,
        "peers": peers,
        "images": list(images or []),
    }


def peer_entries_from_hello(frame: dict) -> list[dict]:
    """Extract the peer entries a hello frame carries (excluding the sender)."""
    entries = list(frame.get("peers") or [])
    entries.append({"node_id": frame.get("node_id"), "ticket": frame.get("ticket")})
    return [e for e in entries if e and e.get("node_id")]
