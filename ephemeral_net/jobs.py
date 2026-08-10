"""
Job protocol for ephemeral_net.

The wire contract deliberately mirrors the existing REST contract
(``RunRequest``/``RunResponse`` in main_api.py): a request carries a
base64-encoded Markdown document plus a timeout; the response streams
``stdout``/``stderr`` log chunks followed by a final ``job_done`` frame
carrying the exit code and artifact metadata.

Phase 2 wires :class:`JobExecutor` to ``ephemeral_core.parse_and_execute``
with the receiver-side sandbox enforcement.
"""
from __future__ import annotations

import base64
from dataclasses import dataclass
from typing import AsyncIterator, Callable

from .errors import ProtocolError


@dataclass
class JobRequest:
    """Request payload for a distributed job — mirrors ``RunRequest``."""

    job_id: str
    document_blob: str  # base64-encoded UTF-8 Markdown (same as RunRequest)
    timeout: int = 300  # seconds, 1-600

    def to_frame(self) -> dict:
        return {
            "type": "job_request",
            "job_id": self.job_id,
            "document_blob": self.document_blob,
            "timeout": self.timeout,
        }

    @classmethod
    def from_frame(cls, frame: dict) -> "JobRequest":
        if frame.get("type") != "job_request":
            raise ProtocolError(f"expected job_request frame, got {frame.get('type')!r}")
        job_id = frame.get("job_id")
        document_blob = frame.get("document_blob")
        if not job_id or not isinstance(document_blob, str):
            raise ProtocolError("job_request missing job_id or document_blob")
        return cls(
            job_id=str(job_id),
            document_blob=document_blob,
            timeout=int(frame.get("timeout", 300)),
        )


class JobEvent:
    """Base marker for events streamed back from a running job."""


@dataclass
class JobLogEvent(JobEvent):
    """A chunk of stdout/stderr produced by the job."""

    channel: str  # "stdout" | "stderr"
    data: bytes
    job_id: str = ""

    def to_frame(self) -> dict:
        return {
            "type": "job_log",
            "job_id": self.job_id,
            "channel": self.channel,
            "data": base64.b64encode(self.data).decode("ascii"),
        }

    @classmethod
    def from_frame(cls, frame: dict) -> "JobLogEvent":
        if frame.get("type") != "job_log":
            raise ProtocolError(f"expected job_log frame, got {frame.get('type')!r}")
        channel = frame.get("channel")
        if channel not in ("stdout", "stderr"):
            raise ProtocolError(f"bad job_log channel: {channel!r}")
        try:
            data = base64.b64decode(frame["data"], validate=True)
        except Exception as e:
            raise ProtocolError(f"bad job_log data: {e}") from e
        return cls(channel=channel, data=data, job_id=str(frame.get("job_id", "")))


@dataclass
class JobDoneEvent(JobEvent):
    """Terminal success event — mirrors ``RunResponse``."""

    exit_code: int
    stdout: str
    stderr: str
    artifact_file: str | None = None
    artifact_ext: str | None = None
    #: Absolute path to the artifact on the executing node (local consumers
    #: can route it; remote consumers only see the basename via artifact_file).
    artifact_path: str | None = None
    job_id: str = ""

    def to_frame(self) -> dict:
        return {
            "type": "job_done",
            "job_id": self.job_id,
            "exit_code": self.exit_code,
            "stdout": self.stdout,
            "stderr": self.stderr,
            "artifact_file": self.artifact_file,
            "artifact_ext": self.artifact_ext,
            "artifact_path": self.artifact_path,
        }

    @classmethod
    def from_frame(cls, frame: dict) -> "JobDoneEvent":
        if frame.get("type") != "job_done":
            raise ProtocolError(f"expected job_done frame, got {frame.get('type')!r}")
        return cls(
            exit_code=int(frame.get("exit_code", 1)),
            stdout=str(frame.get("stdout", "")),
            stderr=str(frame.get("stderr", "")),
            artifact_file=frame.get("artifact_file"),
            artifact_ext=frame.get("artifact_ext"),
            artifact_path=frame.get("artifact_path"),
            job_id=str(frame.get("job_id", "")),
        )


@dataclass
class JobErrorEvent(JobEvent):
    """Terminal failure event carrying a human-readable message."""

    message: str
    job_id: str = ""

    def to_frame(self) -> dict:
        return {"type": "error", "job_id": self.job_id, "message": self.message}

    @classmethod
    def from_frame(cls, frame: dict) -> "JobErrorEvent":
        if frame.get("type") != "error":
            raise ProtocolError(f"expected error frame, got {frame.get('type')!r}")
        return cls(message=str(frame.get("message", "unknown error")),
                   job_id=str(frame.get("job_id", "")))


def parse_job_frame(frame: dict) -> JobEvent:
    """Parse any server-side job frame into a :class:`JobEvent`."""
    kind = frame.get("type")
    if kind == "job_log":
        return JobLogEvent.from_frame(frame)
    if kind == "job_done":
        return JobDoneEvent.from_frame(frame)
    if kind == "error":
        return JobErrorEvent.from_frame(frame)
    raise ProtocolError(f"unexpected frame type on job stream: {kind!r}")


#: A job executor turns a request into a stream of events. Compute nodes
#: supply one; Phase 2 wires it to ephemeral_core's sandboxed runner.
JobExecutor = Callable[[JobRequest], AsyncIterator[JobEvent]]
