"""
Error hierarchy for ``ephemeral_net``.

All networking-layer failures raise subclasses of :class:`NetError` so
callers can catch one base type while still distinguishing protocol,
handshake, and job-level problems.
"""
from __future__ import annotations


class NetError(Exception):
    """Base class for all ephemeral_net errors."""


class ProtocolError(NetError):
    """A peer sent something that does not conform to the wire protocol."""


class FrameTooLarge(ProtocolError):
    """An incoming frame exceeded the configured size limit."""


class HandshakeError(NetError):
    """The hello handshake with a peer failed or timed out."""


class ConnectionClosed(NetError):
    """The underlying QUIC connection closed before the exchange finished."""


class JobError(NetError):
    """The remote side reported an error while running a job."""
