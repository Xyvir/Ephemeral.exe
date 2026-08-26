"""Shared MCP surface for trusted Ephemeral deployments.

This module is imported only by local or explicitly private server builds.
The tool accepts the same human-readable Markdown document that Ephemeral
executes everywhere and returns the canonical REST ``RunResponse`` model.
"""
from __future__ import annotations

import os
import secrets
from collections.abc import Awaitable, Callable
from typing import Annotated

from starlette.datastructures import Headers

from mcp.server import MCPServer
from mcp.server.transport_security import TransportSecuritySettings
from pydantic import Field

from ephemeral_api import RunResponse

RunMarkdown = Callable[[str, int], Awaitable[RunResponse]]


def create_mcp_server(
    run_markdown_callback: RunMarkdown,
    *,
    name: str = "Ephemeral",
) -> MCPServer:
    """Create the single trusted ``run_markdown`` MCP tool."""
    server = MCPServer(name)

    @server.tool()
    async def run_markdown(
        markdown: str,
        timeout: Annotated[int, Field(ge=1, le=600)] = 300,
    ) -> RunResponse:
        """Execute an Ephemeral Markdown document in a disposable sandbox."""
        return await run_markdown_callback(markdown, timeout)

    return server


def mcp_allowed_hosts() -> list[str] | None:
    """Return an explicit MCP Host allowlist, or None for localhost-only."""
    raw = os.getenv("EPHEMERAL_MCP_ALLOWED_HOSTS", "").strip()
    if not raw:
        return None
    return [host.strip() for host in raw.split(",") if host.strip()]


def mcp_allowed_origins() -> list[str] | None:
    """Return optional Origin values accepted by the MCP transport."""
    raw = os.getenv("EPHEMERAL_MCP_ALLOWED_ORIGINS", "").strip()
    if not raw:
        return None
    return [origin.strip() for origin in raw.split(",") if origin.strip()]


class MCPBearerMiddleware:
    """Optionally protect the mounted MCP endpoint with a bearer token."""

    def __init__(self, app, token: str) -> None:
        self.app = app
        self.token = token

    async def __call__(self, scope, receive, send):
        if scope.get("type") != "http":
            await self.app(scope, receive, send)
            return
        path = scope.get("path", "")
        if path == "/mcp" or path.startswith("/mcp/"):
            auth = Headers(scope=scope).get("authorization", "")
            scheme, _, value = auth.partition(" ")
            if scheme.lower() != "bearer" or not secrets.compare_digest(
                value, self.token
            ):
                await self._unauthorized(send)
                return
        await self.app(scope, receive, send)

    @staticmethod
    async def _unauthorized(send) -> None:
        body = b'{"detail":"MCP authentication required"}'
        await send({
            "type": "http.response.start",
            "status": 401,
            "headers": [
                (b"content-type", b"application/json"),
                (b"www-authenticate", b'Bearer realm="ephemeral-mcp"'),
            ],
        })
        await send({"type": "http.response.body", "body": body})


def mcp_bearer_token() -> str | None:
    """Return the optional private-deployment MCP bearer token."""
    token = os.getenv("EPHEMERAL_MCP_TOKEN", "").strip()
    return token or None


def mcp_transport_security() -> TransportSecuritySettings | None:
    """Build transport DNS-rebinding/CORS security from operator settings."""
    hosts = mcp_allowed_hosts()
    origins = mcp_allowed_origins()
    if hosts is None and origins is None:
        return None
    # Keep localhost usable when an operator specifies only origins; the SDK
    # treats an empty allowlist as deny-all once explicit settings are passed.
    return TransportSecuritySettings(
        allowed_hosts=hosts or [
            "localhost",
            "localhost:*",
            "127.0.0.1",
            "127.0.0.1:*",
        ],
        allowed_origins=origins or [],
    )


__all__ = [
    "RunMarkdown",
    "create_mcp_server",
    "mcp_allowed_hosts",
    "mcp_allowed_origins",
    "mcp_bearer_token",
    "mcp_transport_security",
    "MCPBearerMiddleware",
]
