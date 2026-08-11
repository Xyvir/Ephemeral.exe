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
import os
import time
from typing import AsyncIterator, Sequence

from .discovery import PeerInfo, PeerTable
from .errors import HandshakeError, JobError, ProtocolError
from .jobs import JobErrorEvent, JobExecutor, JobRequest, parse_job_frame
from .swarm import (
    SWARM_DNS_TXT,
    SWARM_LIST_URLS,
    fetch_swarm_list,
    fetch_swarm_list_dns,
)
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


def _peer_load_factor(peer) -> float:
    """
    A peer's normalized load factor (lower is better).

    Peers advertising a concurrency ceiling report active/max; peers
    without one are ranked by raw active-job count, so an idle peer (0)
    is always preferred over a busy one regardless of advertisement.
    """
    active = getattr(peer, "active_jobs", 0) or 0
    max_jobs = getattr(peer, "max_jobs", None)
    if max_jobs and max_jobs > 0:
        return active / max_jobs
    return float(active)


def select_peer_for_images(peers, images: Sequence[str]):
    """
    Best peer advertising any of ``images`` — idle-first, then RTT.

    Pure (no iroh) so routing is unit-testable: takes any iterable of
    objects exposing ``images``/``rtt``/``active_jobs``/``max_jobs``.
    Returns None when no peer advertises a required image.
    """
    wanted = set(images)
    best = None
    best_key = None
    for peer in peers:
        if peer.images is None:
            continue  # unknown warm state treated as "not warm"
        if not (wanted & peer.images):
            continue
        max_jobs = getattr(peer, "max_jobs", None)
        active = getattr(peer, "active_jobs", 0) or 0
        if max_jobs and max_jobs > 0 and active >= max_jobs:
            continue  # saturated — never route a new job here
        key = (_peer_load_factor(peer), peer.rtt if peer.rtt is not None else float("inf"))
        if best_key is None or key < best_key:
            best, best_key = peer, key
    return best


class PeerConnection:
    """A live, registry-held connection to another ephemeral node."""

    def __init__(self, node: "Node", conn, node_id: str) -> None:
        self.node = node
        self.connection = conn
        self.node_id = node_id
        self.ticket: str | None = None      # dial-back ticket from hello (fallback)
        self.relay: str | None = None       # peer's relay URL — dial by id + relay
        self.hello: dict | None = None      # raw hello frame received
        self.images: set[str] | None = None  # warm images advertised in hello
        self.active_jobs: int = 0           # jobs the peer is currently running
        self.max_jobs: int | None = None    # peer's concurrency ceiling (None = unknown)
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
        max_jobs: int | None = None,
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
        # Busy/idle tracking: jobs currently running on this node and the
        # concurrency ceiling advertised to peers (None = no cap). Both are
        # only touched from the node's own event loop.
        self._max_jobs = max_jobs
        self._active_jobs = 0

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
            # The Python FFI takes the raw 32 bytes (unlike the Rust API,
            # which takes a SecretKey value).
            builder.secret_key(secret_key)
        self._builder = builder

        self._ep = None
        self._accept_task: asyncio.Task | None = None
        self._maintenance_task: asyncio.Task | None = None
        self._peers: dict[str, PeerConnection] = {}
        self._peers_lock = asyncio.Lock()
        self.table = PeerTable()
        self._seed_tickets: list[str] = []
        self._seed_nodes: list[tuple[str, str | None]] = []
        self._list_urls: list[str] = []
        self._dns_txt: str = ""
        self._interval: float = 60.0
        # Mesh healing: when a peer dial fails, back off for this long so
        # the maintenance loop doesn't hammer dead peers every pass.
        self._heal_cooldown: float = 180.0
        self._dial_timeout: float = 20.0
        self._dial_backoff: dict[str, float] = {}
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

    def relay_url(self) -> str | None:
        """This node's current relay URL (None when relays are disabled)."""
        if self._ep is None:
            raise RuntimeError("node not started")
        try:
            value = str(self._ep.addr().relay_url())
        except Exception:
            return None
        # The FFI stringifies a missing relay to "None".
        if not value or value == "None":
            return None
        return value

    def load_info(self) -> dict:
        """This node's current load, advertised in hello frames."""
        return {"active_jobs": self._active_jobs, "max_jobs": self._max_jobs}

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
        # One background loop handles both seed refresh and mesh healing
        # (re-dialing known peers whose connections dropped).
        self._maintenance_task = asyncio.create_task(self._maintenance_loop())
        # Let the network report settle so tickets carry usable addresses.
        await asyncio.sleep(1.0)

    async def close(self) -> None:
        """Stop accepting, drop peers, and release the endpoint."""
        self._closed = True
        for task in (self._accept_task, self._maintenance_task):
            if task is not None:
                task.cancel()
        for task in (self._accept_task, self._maintenance_task):
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

    async def _dial_addr(self, addr) -> PeerConnection:
        """Connect to an EndpointAddr and complete the hello handshake."""
        if self._ep is None:
            raise RuntimeError("node not started")
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
                    self.relay_url(),
                    self._active_jobs,
                    self._max_jobs,
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
        peer.relay = reply.get("relay")
        peer.images = set(reply.get("images") or [])
        peer.active_jobs = int(reply.get("active_jobs") or 0)
        peer.max_jobs = reply.get("max_jobs")
        self._merge_hello(reply)
        return peer

    async def dial(self, ticket: str) -> PeerConnection:
        """
        Dial a peer by its EndpointTicket string and complete the hello
        handshake. The resulting connection is held in the registry.
        """
        if self._ep is None:
            raise RuntimeError("node not started")
        ticket_obj = self._iroh.EndpointTicket.from_string(ticket)
        return await self._dial_addr(ticket_obj.endpoint_addr())

    async def dial_node(self, node_id: str, relay_url: str | None = None) -> PeerConnection:
        """
        Dial a peer by its stable node id + relay URL — iroh-native
        identity, no ticket. The relay routes by node id, so this works
        across the peer's restarts. ``relay_url`` defaults to this node's
        own relay; pass it explicitly when dialing a peer on another relay.
        """
        if self._ep is None:
            raise RuntimeError("node not started")
        relay = relay_url or self.relay_url()
        if not relay:
            raise ValueError("dial_node needs a relay URL (pass relay_url=...)")
        addr = self._iroh.EndpointAddr(
            self._iroh.EndpointId.from_string(node_id), relay, []
        )
        return await self._dial_addr(addr)

    async def bootstrap(self, seed_tickets: Sequence[str], interval: float = 60.0) -> None:
        """
        Dial the given seed nodes (by EndpointTicket) to discover peers
        immediately. The maintenance loop (started in :meth:`start`) then
        re-dials the seeds and heals the mesh every ``interval`` seconds.
        """
        self._seed_tickets = list(seed_tickets)
        self._interval = interval
        await self._bootstrap_once()

    async def bootstrap_nodes(
        self, seed_nodes: Sequence[tuple[str, str | None]], interval: float = 60.0
    ) -> None:
        """
        Dial seed nodes by ``(node_id, relay_url)`` — iroh-native
        bootstrap with no tickets. The maintenance loop re-dials them
        every ``interval`` seconds.
        """
        self._seed_nodes = [(nid, relay) for nid, relay in seed_nodes]
        self._interval = interval
        await self._bootstrap_once()

    async def _maintenance_loop(self) -> None:
        while not self._closed:
            await asyncio.sleep(self._interval)
            if self._closed:
                return
            await self._bootstrap_once()
            await self._bootstrap_list_once()
            await self._mesh_heal_once()

    async def _bootstrap_once(self) -> None:
        for ticket in self._seed_tickets:
            try:
                await asyncio.wait_for(self.dial(ticket), timeout=self._dial_timeout)
            except Exception as e:
                logger.warning("bootstrap dial failed: %s", e)
        for node_id, relay in self._seed_nodes:
            try:
                await asyncio.wait_for(
                    self.dial_node(node_id, relay), timeout=self._dial_timeout
                )
            except Exception as e:
                logger.warning("bootstrap dial (node id) failed: %s", e)

    async def bootstrap_from_list(
        self,
        urls: Sequence[str] | None = None,
        interval: float = 60.0,
        dns_txt: str | None = None,
    ) -> None:
        """
        Bootstrap from the live swarm list (``docs/swarm.json``).

        There are no compiled-in seeds — the always-on list (GitHub Pages /
        raw GitHub, refreshed every 6 h by a scheduled Action) *is* the
        bootstrap. Fetches the list, dials the listed members by stable
        node id + relay (ticket fallback for legacy entries), and
        re-fetches + dials new members every ``interval`` seconds so
        freshly-picked-up nodes are learned without a restart.

        ``dns_txt``: when the list is unreachable (e.g. GitHub is down),
        resolve this hostname's TXT record via DNS-over-HTTPS and dial
        the members it mirrors — DNS is tiered/cached infrastructure, an
        independent path to first contact. ``None`` (default) uses
        ``EPHEMERAL_DNS_TXT`` or :data:`SWARM_DNS_TXT`; empty disables
        the fallback.
        """
        self._list_urls = list(urls if urls is not None else SWARM_LIST_URLS)
        self._dns_txt = (
            dns_txt
            if dns_txt is not None
            else (os.environ.get("EPHEMERAL_DNS_TXT") or SWARM_DNS_TXT)
        )
        self._interval = interval
        await self._bootstrap_list_once()

    async def _bootstrap_list_once(self) -> None:
        """Fetch the live list (or DNS anchor) and dial unknown members."""
        if (not self._list_urls and not self._dns_txt) or self._ep is None or self._closed:
            return
        entries: list[dict] = []
        if self._list_urls:
            try:
                entries = await asyncio.wait_for(
                    asyncio.to_thread(fetch_swarm_list, self._list_urls),
                    timeout=self._dial_timeout + 5.0,
                )
            except Exception as e:
                logger.warning("swarm list fetch failed: %s", e)
        now = time.monotonic()
        targets: list[tuple[str | None, str | None, str | None]] = []
        if entries:
            for entry in entries:
                node_id = entry.get("node_id")
                relay = entry.get("relay")
                ticket = entry.get("ticket")
                if relay and relay != "None":
                    pass
                else:
                    relay = None
                key = node_id or ticket
                if not key or key in self._peers:
                    continue
                if node_id and node_id == self.node_id():
                    continue
                last = self._dial_backoff.get(key)
                if last is not None and now - last < self._heal_cooldown:
                    continue
                targets.append((node_id, relay, ticket))
        elif self._dns_txt:
            # GitHub unreachable — DNS TXT fallback: the refresh Action
            # keeps a TXT mirror of the list (compact node_id + relay
            # entries) at this hostname, and dialing any of them reveals
            # the whole swarm via hello.
            try:
                anchors = await asyncio.wait_for(
                    asyncio.to_thread(fetch_swarm_list_dns, self._dns_txt),
                    timeout=self._dial_timeout,
                )
            except Exception as e:
                logger.warning("swarm DNS mirror fetch failed: %s", e)
                anchors = []
            for node_id, relay in anchors:
                if node_id == self.node_id():
                    continue
                if node_id in self._peers:
                    continue
                last = self._dial_backoff.get(node_id)
                if last is not None and now - last < self._heal_cooldown:
                    continue
                targets.append((node_id, relay, None))
        if not targets:
            return

        async def _try(node_id: str | None, relay: str | None, ticket: str | None) -> None:
            key = node_id or ticket
            try:
                if node_id and relay:
                    await asyncio.wait_for(
                        self.dial_node(node_id, relay), timeout=self._dial_timeout
                    )
                elif ticket:
                    await asyncio.wait_for(self.dial(ticket), timeout=self._dial_timeout)
                else:
                    return
                logger.info("swarm list: connected to %s", str(key)[:12])
            except Exception as e:
                self._dial_backoff[key] = time.monotonic()
                logger.debug("swarm list dial failed for %s: %s", str(key)[:12], e)

        await asyncio.gather(*(_try(nid, rel, tk) for nid, rel, tk in targets))

    async def _mesh_heal_once(self) -> None:
        """
        Re-dial known peers we are not currently connected to.

        The mesh heals around a dead seed: members re-establish dropped
        connections directly from their peer table, so only the very
        first contact for brand-new nodes ever needs a live seed.
        """
        if self._ep is None or self._closed:
            return
        my_id = self.node_id()
        now = time.monotonic()
        targets: list[tuple[str, str | None, str | None]] = []
        for info in self.table:
            if info.node_id == my_id:
                continue  # never dial ourselves
            if info.node_id in self._peers:
                continue  # already connected
            relay = info.relay if (info.relay and info.relay != "None") else None
            if not info.ticket and not relay:
                continue
            last = self._dial_backoff.get(info.node_id)
            if last is not None and now - last < self._heal_cooldown:
                continue
            targets.append((info.node_id, info.ticket, relay))
        if not targets:
            return

        async def _try(node_id: str, ticket: str | None, relay: str | None) -> None:
            try:
                if relay:
                    # iroh-native: re-dial by stable id + relay, no ticket.
                    await asyncio.wait_for(
                        self.dial_node(node_id, relay), timeout=self._dial_timeout
                    )
                else:
                    await asyncio.wait_for(self.dial(ticket), timeout=self._dial_timeout)
            except Exception:
                self._dial_backoff[node_id] = time.monotonic()

        await asyncio.gather(*(_try(nid, tk, rl) for nid, tk, rl in targets))

    # --- internals -------------------------------------------------------

    def _merge_hello(self, hello: dict) -> None:
        now = time.monotonic()
        infos = []
        for entry in peer_entries_from_hello(hello):
            infos.append(
                PeerInfo(
                    node_id=entry["node_id"],
                    ticket=entry.get("ticket"),
                    relay=entry.get("relay"),
                    images=set(entry.get("images") or []),
                    active_jobs=int(entry.get("active_jobs") or 0),
                    max_jobs=entry.get("max_jobs"),
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
        peer.relay = frame.get("relay")
        peer.images = set(frame.get("images") or [])
        peer.active_jobs = int(frame.get("active_jobs") or 0)
        peer.max_jobs = frame.get("max_jobs")
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
                    self.relay_url(),
                    self._active_jobs,
                    self._max_jobs,
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
        # Busy/idle: count this job so hello frames advertise current load.
        self._active_jobs += 1
        try:
            async for event in self.executor(request):
                await write_frame(send, event.to_frame())
        except JobError as e:
            await write_frame(send, error_frame(str(e), job_id=request.job_id))
        except Exception as e:
            logger.exception("job %s failed", request.job_id)
            await write_frame(send, error_frame(f"job failed: {e}", job_id=request.job_id))
        finally:
            self._active_jobs -= 1
            await send.finish()

    # --- nearest-neighbor offloading -------------------------------------

    def peer_for_images(self, images: Sequence[str]) -> PeerConnection | None:
        """
        The best registered peer advertising any of ``images``.

        Idle-first: saturated peers are never chosen, then the lowest
        load factor wins (active/max jobs; peers without a max are ranked
        by raw active count), then the lowest measured hello RTT. Peers
        that did not advertise an image list are never chosen (unknown
        warm state is treated as "not warm"). Returns None when no peer
        advertises any of the required images.
        """
        return select_peer_for_images(self._peers.values(), images)

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
