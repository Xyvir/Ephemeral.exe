"""
Node — one iroh endpoint participating in the ephemeral cluster.

Responsibilities:

* own the ``iroh.Endpoint`` lifecycle (bind, accept loop, close)
* keep a connection registry: QUIC connections must stay alive for the
  duration of a conversation, otherwise the stream aborts mid-read
* run the ``hello`` handshake on the first bi-stream of every connection
  (node id, dial-back ticket, known peers) for seed-mediated discovery
* dispatch ``job_request`` streams to the configured :class:`JobExecutor`
  and stream ``job_log``/``job_done``/``error`` frames back

A node both dials (client role) and accepts (server role); the same
class covers desktop clients, compute nodes, and gateways.
"""
from __future__ import annotations

import asyncio
import logging
import time
from typing import AsyncIterator, Sequence

from .discovery import PeerInfo, PeerTable
from .errors import HandshakeError, JobError, ProtocolError
from .jobs import JobErrorEvent, JobExecutor, JobRequest, parse_job_frame
from .protocol import (
    ALPN,
    DEFAULT_MAX_FRAME_SIZE,
    error_frame,
    hello_frame,
    peer_entries_from_hello,
    read_frame,
    write_frame,
)

logger = logging.getLogger(__name__)


class PeerConnection:
    """A live, registry-held connection to another ephemeral node."""

    def __init__(self, node: "Node", conn, node_id: str) -> None:
        self.node = node
        self.connection = conn
        self.node_id = node_id
        self.ticket: str | None = None      # dial-back ticket from hello
        self.hello: dict | None = None      # raw hello frame received
        self.images: set[str] | None = None  # warm images advertised in hello
        self.rtt: float | None = None       # hello round-trip seconds (dial side)
        self.connected_at: float = time.monotonic()

    def __repr__(self) -> str:  # pragma: no cover - debug aid
        return f"<PeerConnection {self.node_id[:8]}...>"


class Node:
    """
    An ephemeral cluster node built on an ``iroh.Endpoint``.

    Args:
        secret_key: 32 raw bytes for a persistent node identity; a fresh
            random key is generated when omitted.
        relay: ``"n0"`` (public relays, default), ``"minimal"`` (n0
            relays, no address lookup), or ``"disabled"`` (direct
            connections only — used by tests and closed networks).
        executor: callable(request) -> async iterator of JobEvents; when
            set, the node accepts and runs jobs from peers.
        idle_timeout: seconds a peer may sit idle on an accepted
            connection before it is dropped.
        list_images: callable returning the node's locally-cached image
            names, advertised in hello frames for nearest-neighbor
            offloading (defaults to ``ephemeral_core.list_local_images``).
        warm_cache_ttl: seconds to cache the warm-image list between
            hello frames.
    """

    def __init__(
        self,
        *,
        secret_key: bytes | None = None,
        relay: str = "n0",
        executor: JobExecutor | None = None,
        idle_timeout: float = 60.0,
        max_frame_size: int = DEFAULT_MAX_FRAME_SIZE,
        list_images=None,
        warm_cache_ttl: float = 30.0,
    ) -> None:
        import iroh  # deferred so non-net code paths don't require the wheel

        self._iroh = iroh
        self.relay = relay
        self.executor = executor
        self.idle_timeout = idle_timeout
        self.max_frame_size = max_frame_size
        self._list_images = list_images
        self._warm_cache_ttl = warm_cache_ttl
        self._warm_cache: tuple[float, list[str]] | None = None

        builder = iroh.EndpointBuilder()
        builder.alpns([ALPN])
        if relay == "disabled":
            # The FFI only installs a rustls crypto provider via the
            # presets, so apply the minimal preset first, then disable
            # relays for direct-only operation.
            builder.apply_minimal()
            builder.relay_mode(iroh.RelayMode.disabled())
        elif relay == "n0":
            builder.apply_n0()
        elif relay == "minimal":
            builder.apply_minimal()
        else:
            raise ValueError(f"unknown relay mode: {relay!r}")
        if secret_key is not None:
            builder.secret_key(iroh.SecretKey.from_bytes(secret_key))
        self._builder = builder

        self._ep = None
        self._accept_task: asyncio.Task | None = None
        self._bootstrap_task: asyncio.Task | None = None
        self._peers: dict[str, PeerConnection] = {}
        self._peers_lock = asyncio.Lock()
        self.table = PeerTable()
        self._closed = False

    # --- identity --------------------------------------------------------

    @property
    def ep(self):  # the bound iroh Endpoint (None before start())
        return self._ep

    def node_id(self) -> str:
        if self._ep is None:
            raise RuntimeError("node not started")
        return str(self._ep.id())

    def secret_key_bytes(self) -> bytes:
        if self._ep is None:
            raise RuntimeError("node not started")
        return self._ep.secret_key().to_bytes()

    def ticket(self) -> str:
        """A serialized EndpointTicket others can dial us with."""
        if self._ep is None:
            raise RuntimeError("node not started")
        addr = self._ep.addr()
        return str(self._iroh.EndpointTicket.from_addr(addr))

    def warm_images(self) -> list[str]:
        """Locally-cached image names, refreshed at most every ``warm_cache_ttl``."""
        now = time.monotonic()
        if self._warm_cache is not None and now - self._warm_cache[0] < self._warm_cache_ttl:
            return self._warm_cache[1]
        probe = self._list_images
        if probe is None:
            try:
                import ephemeral_core

                probe = ephemeral_core.list_local_images
            except Exception:
                probe = lambda: []  # noqa: E731 - podman absent; nothing is warm
        try:
            images = list(probe())
        except Exception:
            images = []
        self._warm_cache = (now, images)
        return images

    # --- lifecycle -------------------------------------------------------

    async def start(self) -> None:
        """Bind the endpoint and begin accepting connections."""
        if self._ep is not None:
            return
        self._ep = await self._builder.bind()
        self._accept_task = asyncio.create_task(self._accept_loop())
        # Let the network report settle so tickets carry usable addresses.
        await asyncio.sleep(1.0)

    async def close(self) -> None:
        """Stop accepting, drop peers, and release the endpoint."""
        self._closed = True
        for task in (self._accept_task, self._bootstrap_task):
            if task is not None:
                task.cancel()
        for task in (self._accept_task, self._bootstrap_task):
            if task is not None:
                try:
                    await task
                except (asyncio.CancelledError, Exception):
                    pass
        async with self._peers_lock:
            peers = list(self._peers.values())
            self._peers.clear()
        for peer in peers:
            try:
                peer.connection.close(0, b"shutdown")
            except Exception:  # pragma: no cover - best effort
                pass
        if self._ep is not None:
            await self._ep.close()
            self._ep = None

    # --- dialing / discovery ---------------------------------------------

    async def dial(self, ticket: str) -> PeerConnection:
        """
        Dial a peer by its EndpointTicket string and complete the hello
        handshake. The resulting connection is held in the registry.
        """
        if self._ep is None:
            raise RuntimeError("node not started")
        ticket_obj = self._iroh.EndpointTicket.from_string(ticket)
        addr = ticket_obj.endpoint_addr()
        conn = await self._ep.connect(addr, ALPN)
        peer = PeerConnection(self, conn, str(conn.remote_id()))
        async with self._peers_lock:
            self._peers[peer.node_id] = peer
        try:
            started = time.monotonic()
            bs = await conn.open_bi()
            await write_frame(
                bs.send(),
                hello_frame(
                    self.node_id(),
                    self.ticket(),
                    self.table.snapshot(),
                    self.warm_images(),
                ),
            )
            reply = await asyncio.wait_for(
                read_frame(bs.recv(), max_size=self.max_frame_size),
                timeout=self.idle_timeout,
            )
            peer.rtt = time.monotonic() - started
        except Exception:
            async with self._peers_lock:
                self._peers.pop(peer.node_id, None)
            raise
        if reply.get("type") != "hello":
            raise HandshakeError(f"peer replied {reply.get('type')!r}, expected hello")
        peer.hello = reply
        peer.ticket = reply.get("ticket")
        peer.images = set(reply.get("images") or [])
        self._merge_hello(reply)
        return peer

    async def bootstrap(self, seed_tickets: Sequence[str], interval: float = 60.0) -> None:
        """
        Dial the given seed nodes to discover peers, then refresh every
        ``interval`` seconds in the background.
        """
        self._seed_tickets = list(seed_tickets)
        await self._bootstrap_once()
        self._bootstrap_task = asyncio.create_task(self._bootstrap_loop(interval))

    async def _bootstrap_loop(self, interval: float) -> None:
        while not self._closed:
            await asyncio.sleep(interval)
            await self._bootstrap_once()

    async def _bootstrap_once(self) -> None:
        for ticket in getattr(self, "_seed_tickets", []):
            try:
                await self.dial(ticket)
            except Exception as e:
                logger.warning("bootstrap dial failed: %s", e)

    # --- internals -------------------------------------------------------

    def _merge_hello(self, hello: dict) -> None:
        now = time.monotonic()
        infos = []
        for entry in peer_entries_from_hello(hello):
            infos.append(
                PeerInfo(
                    node_id=entry["node_id"],
                    ticket=entry.get("ticket"),
                    images=set(entry.get("images") or []),
                    last_seen=now,
                )
            )
        self.table.merge(infos)

    async def _accept_loop(self) -> None:
        while not self._closed:
            try:
                incoming = await asyncio.wait_for(self._ep.accept_next(), timeout=1.0)
            except asyncio.TimeoutError:
                continue
            except Exception:
                break  # endpoint closed
            try:
                accepting = await incoming.accept()
                conn = await accepting.connect()
            except Exception as e:
                logger.debug("accept failed: %s", e)
                continue
            self._spawn_conn_handler(conn)

    def _spawn_conn_handler(self, conn) -> None:
        asyncio.create_task(self._conn_handler(conn))

    async def _conn_handler(self, conn) -> None:
        node_id = str(conn.remote_id())
        peer = PeerConnection(self, conn, node_id)
        async with self._peers_lock:
            self._peers[node_id] = peer  # hold the connection alive
        try:
            # First stream: hello handshake (dialer always sends hello first).
            try:
                bs = await asyncio.wait_for(conn.accept_bi(), timeout=self.idle_timeout)
            except asyncio.TimeoutError:
                return
            frame = await read_frame(bs.recv(), max_size=self.max_frame_size)
            if frame.get("type") == "hello":
                await self._handle_hello(peer, frame, bs)
            elif frame.get("type") == "job_request":
                await self._handle_job(peer, frame, bs)
            else:
                await write_frame(bs.send(), error_frame("expected hello or job_request"))

            # Subsequent streams: jobs.
            while not self._closed:
                try:
                    bs = await asyncio.wait_for(conn.accept_bi(), timeout=self.idle_timeout)
                except asyncio.TimeoutError:
                    return
                except Exception:
                    return
                frame = await read_frame(bs.recv(), max_size=self.max_frame_size)
                if frame.get("type") == "hello":
                    await self._handle_hello(peer, frame, bs)
                elif frame.get("type") == "job_request":
                    await self._handle_job(peer, frame, bs)
                else:
                    await write_frame(bs.send(), error_frame("unexpected frame type"))
        except (asyncio.CancelledError, Exception):
            pass
        finally:
            async with self._peers_lock:
                self._peers.pop(node_id, None)

    async def _handle_hello(self, peer: PeerConnection, frame: dict, bs) -> None:
        peer.hello = frame
        peer.ticket = frame.get("ticket")
        peer.images = set(frame.get("images") or [])
        self._merge_hello(frame)
        send = bs.send()
        try:
            await write_frame(
                send,
                hello_frame(
                    self.node_id(),
                    self.ticket(),
                    self.table.snapshot(),
                    self.warm_images(),
                ),
            )
        finally:
            await send.finish()

    async def _handle_job(self, peer: PeerConnection, frame: dict, bs) -> None:
        send = bs.send()
        if self.executor is None:
            await write_frame(send, error_frame("this node does not run jobs"))
            await send.finish()
            return
        try:
            request = JobRequest.from_frame(frame)
        except ProtocolError as e:
            await write_frame(send, error_frame(f"bad request: {e}"))
            await send.finish()
            return
        try:
            async for event in self.executor(request):
                await write_frame(send, event.to_frame())
        except JobError as e:
            await write_frame(send, error_frame(str(e), job_id=request.job_id))
        except Exception as e:
            logger.exception("job %s failed", request.job_id)
            await write_frame(send, error_frame(f"job failed: {e}", job_id=request.job_id))
        finally:
            await send.finish()

    # --- nearest-neighbor offloading -------------------------------------

    def peer_for_images(self, images: Sequence[str]) -> PeerConnection | None:
        """
        The warmest registered peer advertising any of ``images``.

        "Nearest" is the peer with the lowest measured hello RTT; peers
        that did not advertise an image list are never chosen (unknown
        warm state is treated as "not warm"). Returns None when no peer
        advertises any of the required images.
        """
        wanted = set(images)
        best: PeerConnection | None = None
        for peer in self._peers.values():
            if peer.images is None:
                continue
            if not (wanted & peer.images):
                continue
            if best is None or (peer.rtt is not None and (best.rtt is None or peer.rtt < best.rtt)):
                best = peer
        return best

    # --- client-side job submission --------------------------------------

    async def submit_job(
        self,
        peer: PeerConnection,
        request: JobRequest,
    ) -> AsyncIterator:
        """
        Submit a job to ``peer`` and stream back its events. Yields
        :class:`JobLogEvent` frames as they arrive and terminates with a
        :class:`JobDoneEvent`; raises :class:`JobError` on a remote
        ``error`` frame.
        """
        if self._ep is None:
            raise RuntimeError("node not started")
        conn = peer.connection
        bs = await conn.open_bi()
        await write_frame(bs.send(), request.to_frame())
        recv = bs.recv()
        while True:
            frame = await read_frame(recv, max_size=self.max_frame_size)
            event = parse_job_frame(frame)
            if isinstance(event, JobErrorEvent):
                raise JobError(event.message)
            yield event
            if frame.get("type") == "job_done":
                return
