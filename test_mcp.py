"""Tests for the trusted-only MCP integration.

The repository's normal test environment may omit the optional MCP SDK. The
policy and response-contract tests still run in that case; SDK-specific tests
run when ``requirements-mcp.txt`` is installed.
"""
from __future__ import annotations

import asyncio
import base64
import importlib.util
import os
from pathlib import Path

from ephemeral_api import RunResponse


def _paths(app):
    return [getattr(route, "path", None) for route in app.routes]


def test_public_bastion_has_no_mcp_route():
    import main_bastion

    assert "/mcp" not in _paths(main_bastion.app)
    print("PASS: public bastion has no MCP route")


def test_public_builds_do_not_enable_mcp():
    dockerfile = Path("Dockerfile").read_text(encoding="utf-8")
    bastion = Path("Dockerfile.bastion").read_text(encoding="utf-8")
    workflow = Path(".github/workflows/build.yml").read_text(encoding="utf-8")

    assert "ARG INSTALL_MCP=0" in dockerfile
    assert 'if [ "$INSTALL_MCP" = "1" ]' in dockerfile
    assert "requirements-mcp.txt" not in bastion
    distributed_client_job = workflow.split("build-exe-distributed:", 1)[1].split(
        "build-linux-appimage:", 1
    )[0]
    assert "requirements-mcp.txt" not in distributed_client_job
    print("PASS: public Docker, bastion, and desktop builds do not enable MCP")


def test_mcp_settings_are_parsed_and_localhost_is_preserved():
    # This test is available whenever the optional SDK is installed.
    if importlib.util.find_spec("mcp") is None:
        print("SKIP: MCP settings test (SDK not installed)")
        return
    from ephemeral_mcp import mcp_transport_security

    previous_hosts = os.environ.get("EPHEMERAL_MCP_ALLOWED_HOSTS")
    previous_origins = os.environ.get("EPHEMERAL_MCP_ALLOWED_ORIGINS")
    try:
        os.environ.pop("EPHEMERAL_MCP_ALLOWED_HOSTS", None)
        os.environ["EPHEMERAL_MCP_ALLOWED_ORIGINS"] = "https://agent.example"
        settings = mcp_transport_security()
        assert settings is not None
        assert "localhost" in settings.allowed_hosts
        assert "https://agent.example" in settings.allowed_origins
    finally:
        if previous_hosts is None:
            os.environ.pop("EPHEMERAL_MCP_ALLOWED_HOSTS", None)
        else:
            os.environ["EPHEMERAL_MCP_ALLOWED_HOSTS"] = previous_hosts
        if previous_origins is None:
            os.environ.pop("EPHEMERAL_MCP_ALLOWED_ORIGINS", None)
        else:
            os.environ["EPHEMERAL_MCP_ALLOWED_ORIGINS"] = previous_origins
    print("PASS: MCP transport settings preserve localhost with explicit origins")


def test_run_response_is_canonical_five_field_contract():
    response = RunResponse(
        exit_code=0,
        stdout="## Python Result\n\n```text\n42\n```\n",
        stderr="",
        artifact_file="run-artifact.zip",
        artifact_ext=".zip",
    )
    assert list(response.model_dump()) == [
        "exit_code",
        "stdout",
        "stderr",
        "artifact_file",
        "artifact_ext",
    ]
    assert response.model_dump()["artifact_file"] == "run-artifact.zip"
    print("PASS: MCP and REST share the canonical five-field RunResponse")


if importlib.util.find_spec("mcp") is not None:
    from ephemeral_mcp import create_mcp_server

    async def test_run_markdown_tool_uses_callback_and_response_model():
        calls = []

        async def callback(markdown: str, timeout: int) -> RunResponse:
            calls.append((markdown, timeout))
            return RunResponse(
                exit_code=0,
                stdout=markdown,
                stderr="",
                artifact_file=None,
                artifact_ext=None,
            )

        server = create_mcp_server(callback)
        tool = server._tool_manager.get_tool("run_markdown")
        assert tool is not None
        result = await tool.run(
            {"markdown": "```python\nprint(1)\n```", "timeout": 7},
            context=None,
        )
        assert isinstance(result, RunResponse)
        assert result.model_dump() == {
            "exit_code": 0,
            "stdout": "```python\nprint(1)\n```",
            "stderr": "",
            "artifact_file": None,
            "artifact_ext": None,
        }
        assert calls == [("```python\nprint(1)\n```", 7)]
        print("PASS: run_markdown MCP tool delegates and returns RunResponse")


def main():
    test_public_bastion_has_no_mcp_route()
    test_public_builds_do_not_enable_mcp()
    test_mcp_settings_are_parsed_and_localhost_is_preserved()
    test_run_response_is_canonical_five_field_contract()
    if importlib.util.find_spec("mcp") is not None:
        asyncio.run(test_run_markdown_tool_uses_callback_and_response_model())
    else:
        print("SKIP: MCP SDK not installed (install requirements-mcp.txt)")
    print("\n=== MCP TESTS PASSED ===")


if __name__ == "__main__":
    main()
