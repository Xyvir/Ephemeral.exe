"""
Receiver-side sandbox for ephemeral_net.

A thick node must never trust the content of a remote job. Before a
document reaches ``ephemeral_core``, :class:`CoreJobExecutor` sanitizes it:

* the ``unsafe`` network flag is stripped — a remote peer can never
  grant its own payload network access (the node operator decides via
  ``allow_network``)
* raw command-line overrides (``image=``, ``cmd=``, ``entrypoint=``)
  are dropped — a remote peer can never dictate what image runs or how
* the resolved image must be on the node's allowlist (default: the
  images defined in ``ephemeral_core.config.LANG_MAP``)
* ``--memory``, ``--cpus``, ``--pids-limit``, and ``--network none``
  are enforced by the underlying container builder. ``--network none``
  is unconditional; the cgroup resource limits are applied whenever the
  host can enforce them and skipped (with a one-time warning) on hosts
  that cannot — e.g. a stock WSL2 + ``podman machine`` setup, which
  doesn't delegate the cgroup controllers to rootless containers

The sanitizer re-serializes the parsed blocks back to Markdown so the
existing ``ephemeral_core`` pipeline (language grouping, dependency
inference, PEP 723 injection, two-stage Python resolution) runs
unchanged.
"""
from __future__ import annotations

import base64
import os
from typing import AsyncIterator, Callable, Iterable, Sequence

from .errors import JobError
from .jobs import (
    MAX_ARTIFACT_SIZE,
    JobArtifactEvent,
    JobDoneEvent,
    JobErrorEvent,
    JobEvent,
    JobLogEvent,
    JobRequest,
)

#: Header tokens that grant capabilities a remote peer must not control.
NETWORK_TOKENS = frozenset({"unsafe"})

#: Header ``key=value`` overrides a remote peer must not control.
DANGEROUS_OVERRIDES = frozenset({"image", "cmd", "entrypoint"})


def default_image_allowlist() -> frozenset[str]:
    """The images defined in ``ephemeral_core``'s language map."""
    from ephemeral_core.config import LANG_MAP

    images = set()
    for value in LANG_MAP.values():
        if isinstance(value, dict) and value.get("image"):
            images.add(value["image"])
    return frozenset(images)


def sanitize_markdown(
    markdown: str,
    *,
    allow_network: bool = False,
    image_allowlist: Iterable[str] | None = None,
) -> tuple[str, list[str]]:
    """
    Sanitize untrusted Markdown for local execution.

    Strips network/override tokens from every codeblock header, validates
    the resolved image against ``image_allowlist`` (default: the
    ``ephemeral_core`` language map), and re-serializes the document.

    Returns ``(sanitized_markdown, required_images)``. Raises
    :class:`ValueError` with a human-readable message when the document
    is empty or requests an image outside the allowlist.
    """
    from ephemeral_core.parser import parse_codeblocks, resolve_runtime_config

    allowlist = (
        frozenset(image_allowlist)
        if image_allowlist is not None
        else default_image_allowlist()
    )

    blocks = parse_codeblocks(markdown)
    if not blocks:
        raise ValueError("No code blocks found in the provided Markdown text.")

    sanitized: list[str] = []
    required_images: list[str] = []

    for block in blocks:
        if block["type"] == "seed":
            # Re-serialize in the canonical fenced form (```name ... ```).
            # A shebang form would be dropped by the parser when the
            # document also contains fenced code blocks.
            name = block["name"]
            header = f"{name} b64" if block.get("is_b64") else name
            sanitized.append(f"```{header}\n{block['content']}\n```")
            continue

        header = block.get("header") or ""
        cleaned_tokens = []
        for token in header.split():
            low = token.lower()
            if low in NETWORK_TOKENS:
                continue
            if token.startswith("-"):
                continue  # stray flag tokens are inert, drop them
            if "=" in token:
                key = token.split("=", 1)[0].lower()
                if key in DANGEROUS_OVERRIDES:
                    continue
            cleaned_tokens.append(token)
        new_header = " ".join(cleaned_tokens)
        # Network policy belongs to the node operator, not the requester:
        # when the operator allows network for remote jobs, re-grant it via
        # the existing `unsafe` token so the core pipeline honors it.
        if allow_network:
            new_header = (new_header + " unsafe").strip()

        config = resolve_runtime_config(new_header)
        if not config or not config.get("image"):
            raise ValueError(f"Configuration failed for block with header: '{header}'")

        image = config["image"]
        if image not in allowlist:
            raise ValueError(
                f"Image '{image}' is not on this node's allowlist. "
                "Remote jobs may only use the node's built-in language images."
            )
        if image not in required_images:
            required_images.append(image)

        # Network policy is decided by the node operator, never the requester.
        if new_header:
            sanitized.append(f"```{new_header}\n{block['content']}\n```")
        else:
            sanitized.append(f"```\n{block['content']}\n```")

    return "\n".join(sanitized) + "\n", required_images


class CoreJobExecutor:
    """
    The sandboxed executor that runs remote jobs on a thick node.

    Wires :class:`ephemeral_net.jobs.JobRequest` to
    ``ephemeral_core.parse_and_execute`` after sanitizing the document
    (see :func:`sanitize_markdown`). Results stream back as
    a :class:`JobDoneEvent` carrying the finished result (server-mode
    results are not re-streamed as :class:`JobLogEvent` chunks — that
    would duplicate the output for stream-and-done consumers);
    rejections and runner failures surface as :class:`JobErrorEvent`.

    ``runner``, ``image_exists``, and ``pull`` are injectable for tests
    (they default to the ``ephemeral_core`` implementations).
    """

    def __init__(
        self,
        *,
        runner: Callable[..., object] | None = None,
        image_exists: Callable[[str], bool] | None = None,
        pull: Callable[[str], object] | None = None,
        allow_network: bool = False,
        image_allowlist: Iterable[str] | None = None,
        log_chunk_size: int = 4096,
    ) -> None:
        self.runner = runner
        self.image_exists = image_exists
        self.pull = pull
        self.allow_network = allow_network
        self.image_allowlist = image_allowlist
        self.log_chunk_size = log_chunk_size

    # --- shared helpers (used by the offloading executor too) ------------

    def prepare(self, request: JobRequest) -> tuple[str, list[str]]:
        """
        Decode and sanitize a request.

        Returns ``(sanitized_markdown, required_images)``; raises
        :class:`JobError` on undecodable or rejected content.
        """
        try:
            markdown = base64.b64decode(
                request.document_blob, validate=True
            ).decode("utf-8")
        except Exception as e:
            raise JobError(f"bad document_blob: {e}") from e
        try:
            return sanitize_markdown(
                markdown,
                allow_network=self.allow_network,
                image_allowlist=self.image_allowlist,
            )
        except ValueError as e:
            raise JobError(str(e)) from e

    def is_warm(self, image: str) -> bool:
        """Whether ``image`` is cached locally (injectable probe)."""
        probe = self.image_exists
        if probe is None:
            import ephemeral_core

            probe = ephemeral_core.check_image_exists
        try:
            return bool(probe(image))
        except Exception:
            return False

    async def run(self, markdown: str, timeout: int) -> object:
        """Execute sanitized Markdown and return the runner's result."""
        runner = self.runner
        if runner is None:
            import ephemeral_core

            runner = ephemeral_core.parse_and_execute
        return await runner(markdown_text=markdown, timeout=timeout, server_mode=True)

    # --- JobExecutor protocol --------------------------------------------

    async def __call__(self, request: JobRequest) -> AsyncIterator[JobEvent]:
        try:
            markdown, _images = self.prepare(request)
        except JobError as e:
            yield JobErrorEvent(message=str(e), job_id=request.job_id)
            return

        try:
            result = await self.run(markdown, request.timeout)
        except Exception as e:  # runner/infrastructure failure
            yield JobErrorEvent(message=str(e), job_id=request.job_id)
            return

        # The runner (server mode) returns the fully-rendered result envelope
        # in result.stdout — do NOT also stream it as job_log chunks, or every
        # consumer that renders both the log stream AND the done event (the
        # wasm SPA, future clients) shows the result twice. The JobDoneEvent
        # is the single carrier of the finished output.
        #
        # Artifacts stream as one JobArtifactEvent per file, BEFORE the done
        # event (clients stop reading the stream when done lands). Each file
        # is capped at MAX_ARTIFACT_SIZE; oversized files are skipped with a
        # warning rather than transferred.
        artifact_list: list[dict] = []
        for path in result.artifact_paths or []:
            if not os.path.isfile(path):
                continue
            name = os.path.basename(path)
            size = os.path.getsize(path)
            if size > MAX_ARTIFACT_SIZE:
                yield JobLogEvent(
                    channel="stderr",
                    data=(
                        f"\nWarning: artifact {name} ({size} bytes) exceeds the "
                        f"{MAX_ARTIFACT_SIZE} byte transfer cap — skipped.\n"
                    ).encode(),
                    job_id=request.job_id,
                )
                continue
            ext = os.path.splitext(path)[1]
            with open(path, "rb") as f:
                data = f.read()
            artifact_list.append({"name": name, "ext": ext, "size": size})
            yield JobArtifactEvent(
                name=name, ext=ext, data=data, job_id=request.job_id
            )

        first = artifact_list[0] if artifact_list else None
        yield JobDoneEvent(
            exit_code=result.exit_code,
            stdout=result.stdout or "",
            stderr=result.stderr or "",
            artifact_file=first["name"] if first else None,
            artifact_ext=first["ext"] if first else None,
            artifact_path=(
                result.artifact_paths[0] if result.artifact_paths else None
            ),
            artifact_list=artifact_list,
            job_id=request.job_id,
        )


__all__ = [
    "CoreJobExecutor",
    "DANGEROUS_OVERRIDES",
    "NETWORK_TOKENS",
    "default_image_allowlist",
    "sanitize_markdown",
]
