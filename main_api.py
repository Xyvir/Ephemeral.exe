"""
Ephemeral API Server — FastAPI backend for remote code execution.

This module provides a REST API that accepts base64-encoded Markdown documents,
passes them to ephemeral_core for sandboxed execution in Podman containers,
and returns structured JSON results with optional artifact routing to a
local WebDAV mount at /data/ephemeral/.

Usage:
    uvicorn main_api:app --host 0.0.0.0 --port 8787

Port 8787 is the Lithic-UK sidecar slot: its generated Caddyfile proxies
/ephemeral/api/v1/* to 127.0.0.1:8787 (see install_self_host.sh).
"""
from __future__ import annotations

import base64
import os
import shutil
import tempfile
from datetime import datetime, timezone

from fastapi import FastAPI, HTTPException
from pydantic import BaseModel, Field, field_validator

import ephemeral_core

# --- Configuration ---
WEBDAV_PATH = "/data/ephemeral"

# --- Pydantic Models ---

class RunRequest(BaseModel):
    """
    Request payload for the /ephemeral/api/v1/run endpoint.
    
    Attributes:
        document_blob: Base64-encoded UTF-8 Markdown string containing code blocks.
                       Automatically decoded to plaintext by the field_validator.
        timeout: Maximum execution time per container in seconds (1-600, default 300).
    """
    document_blob: str
    timeout: int = Field(default=300, ge=1, le=600)

    @field_validator("document_blob", mode="before")
    @classmethod
    def decode_base64(cls, v: str) -> str:
        """Decode the base64-encoded document_blob into a UTF-8 Markdown string."""
        if not isinstance(v, str):
            raise ValueError("document_blob must be a base64-encoded string")
        try:
            decoded_bytes = base64.b64decode(v, validate=True)
        except Exception as e:
            raise ValueError(f"Invalid base64 encoding: {e}") from e
        try:
            return decoded_bytes.decode("utf-8")
        except UnicodeDecodeError as e:
            raise ValueError(f"Decoded content is not valid UTF-8: {e}") from e


class RunResponse(BaseModel):
    """
    Response payload from the /ephemeral/api/v1/run endpoint.
    
    Attributes:
        exit_code: Overall exit code (0 = all runs succeeded).
        stdout: Markdown-formatted execution output.
        stderr: Aggregate stderr from all runs.
        artifact_file: Filename of the generated artifact zip on the WebDAV share,
                       or null if no artifacts were produced.
        artifact_ext: File extension (e.g. '.png', '.svg', '.zip') to help the front-end
                      understand how to present the data, or null.
    """
    exit_code: int
    stdout: str
    stderr: str
    artifact_file: str | None = None
    artifact_ext: str | None = None


# --- FastAPI Application ---

app = FastAPI(
    title="Ephemeral API",
    description="Remote code execution engine powered by Podman containers.",
    version="1.0.0",
)


@app.post(
    "/ephemeral/api/v1/run",
    response_model=RunResponse,
    summary="Execute code blocks from a Markdown document",
    responses={
        422: {"description": "Invalid base64 or non-UTF-8 content"},
        500: {"description": "Podman infrastructure failure"},
        504: {"description": "Execution timed out"},
    },
)
async def run_code(request: RunRequest) -> RunResponse:
    """
    Accept a base64-encoded Markdown document, execute all code blocks
    in sandboxed Podman containers, and return the results.
    
    If artifacts are generated, they are zipped and written to the
    WebDAV share at /data/ephemeral/ with a timestamped filename.
    """
    try:
        result = await ephemeral_core.parse_and_execute(
            markdown_text=request.document_blob,
            timeout=request.timeout,
            server_mode=True,
        )
    except ValueError as e:
        # Bad input: missing language, no code blocks, safety rejection, etc.
        raise HTTPException(status_code=422, detail=str(e)) from e
    except RuntimeError as e:
        # Infrastructure failure: Podman won't start, image pull failed, etc.
        raise HTTPException(status_code=500, detail=str(e)) from e
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Unexpected error: {e}") from e

    # Route artifacts to the WebDAV share
    artifact_filename = None
    if result.artifact_paths and result.artifact_dir:
        # --- Special single-artifact handling ---
        if len(result.artifact_paths) == 1:
            single_file = result.artifact_paths[0]
            ext = os.path.splitext(single_file)[1].lower()
            image_exts = {'.png', '.jpeg', '.jpg', '.gif', '.bmp', '.webp', '.ico', '.tif', '.tiff'}
            
            handled_as_inline = False
            try:
                if ext in image_exts:
                    with open(single_file, 'rb') as f:
                        b64_content = base64.b64encode(f.read()).decode('utf-8')
                    result.stdout = b64_content
                    handled_as_inline = True
                else:
                    # Check if it is plaintext or SVG by attempting UTF-8 decode
                    with open(single_file, 'rb') as f:
                        raw_data = f.read()
                    text_content = raw_data.decode('utf-8')
                    result.stdout = text_content
                    handled_as_inline = True
                    
                if handled_as_inline:
                    shutil.rmtree(result.artifact_dir, ignore_errors=True)
                    return RunResponse(
                        exit_code=result.exit_code,
                        stdout=result.stdout,
                        stderr=result.stderr,
                        artifact_file=None,
                        artifact_ext=ext if ext else '.txt',
                    )
            except UnicodeDecodeError:
                # Not a valid UTF-8 plaintext file, fallback to zipping
                pass
            except Exception as e:
                # Any other error, fallback to zipping
                result.stderr += f"\nWarning: Failed to process single artifact inline: {e}"

        # --- Standard artifact routing (zip to WebDAV) ---
        try:
            timestamp = datetime.now(timezone.utc).strftime("%Y%m%d-%H%M%S")
            artifact_filename = f"{timestamp}-artifact.zip"
            
            os.makedirs(WEBDAV_PATH, exist_ok=True)
            
            # Create a zip from the artifact directory
            zip_base = os.path.join(
                tempfile.gettempdir(),
                f"ephemeral_api_{timestamp}"
            )
            final_zip = shutil.make_archive(zip_base, 'zip', result.artifact_dir)
            
            # Move the zip to the WebDAV mount
            target_path = os.path.join(WEBDAV_PATH, artifact_filename)
            shutil.move(final_zip, target_path)
            
        except Exception as e:
            # Don't fail the whole request if artifact routing fails
            artifact_filename = None
            result_stderr = f"{result.stderr}\nArtifact routing error: {e}"
        else:
            result_stderr = result.stderr
        finally:
            # Clean up the temp artifact directory
            if result.artifact_dir:
                try:
                    shutil.rmtree(result.artifact_dir, ignore_errors=True)
                except Exception:
                    pass
    else:
        result_stderr = result.stderr

    return RunResponse(
        exit_code=result.exit_code,
        stdout=result.stdout,
        stderr=result_stderr,
        artifact_file=artifact_filename,
        artifact_ext='.zip' if artifact_filename else None,
    )


@app.get("/health")
async def health_check():
    """Check if the API and Podman backend are operational."""
    podman_alive = ephemeral_core.check_podman_alive()
    return {
        "status": "healthy" if podman_alive else "degraded",
        "podman": "running" if podman_alive else "stopped",
        "version": (
            os.getenv("RAILWAY_GIT_COMMIT_SHA", "")
            or os.getenv("GIT_SHA", "")
            or os.getenv("EPHEMERAL_VERSION", "")
            or "dev"
        ),
    }
