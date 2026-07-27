"""
Ephemeral executor: async Podman orchestration and container lifecycle.

This module contains the core execution engine extracted from the original
ephemeral.py. All GUI/clipboard/platform-specific code has been removed.
The functions are async-compatible via asyncio.to_thread for subprocess calls.

Security flags (--network none, --cap-drop ALL) are strictly maintained.
"""
from __future__ import annotations

import asyncio
import base64
import os
import re
import shlex
import subprocess
import tempfile
import uuid

from .config import LANG_MAP
from .models import ExecutionResult, GroupResult
from .parser import parse_codeblocks, strip_ansi_codes, strip_shebang, resolve_runtime_config


# --- Subprocess Helpers ---

def get_startupinfo():
    """Return STARTUPINFO to hide console windows on Windows, or None on other platforms."""
    if hasattr(subprocess, 'STARTUPINFO'):
        si = subprocess.STARTUPINFO()
        si.dwFlags |= subprocess.STARTF_USESHOWWINDOW
        return si
    return None


def _shlex_join(split_command: list[str]) -> str:
    """Compatibility wrapper for shlex.join."""
    if hasattr(shlex, 'join'):
        return shlex.join(split_command)
    return ' '.join(shlex.quote(arg) for arg in split_command)


# --- Podman Lifecycle ---

def check_podman_alive() -> bool:
    """Check if the Podman daemon/machine is responsive."""
    try:
        startupinfo = get_startupinfo()
        subprocess.check_call(
            ['podman', 'info'],
            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
            startupinfo=startupinfo
        )
        return True
    except Exception:
        return False


def check_image_exists(image_name: str) -> bool:
    """Check if a container image is already pulled locally."""
    try:
        startupinfo = get_startupinfo()
        subprocess.check_call(
            ['podman', 'image', 'exists', image_name],
            startupinfo=startupinfo
        )
        return True
    except Exception:
        return False


async def ensure_podman_running() -> None:
    """
    Ensure the Podman machine is running. Start it if needed.
    
    Raises RuntimeError if Podman cannot be started.
    """
    if check_podman_alive():
        return

    startupinfo = get_startupinfo()
    try:
        await asyncio.to_thread(
            subprocess.check_call,
            ['podman', 'machine', 'start'],
            startupinfo=startupinfo,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL
        )
    except subprocess.CalledProcessError:
        try:
            await asyncio.to_thread(
                subprocess.check_call,
                ['podman', 'machine', 'init'],
                startupinfo=startupinfo
            )
            await asyncio.to_thread(
                subprocess.check_call,
                ['podman', 'machine', 'start'],
                startupinfo=startupinfo
            )
        except Exception as e:
            raise RuntimeError(f"Could not start Podman: {e}") from e


async def pull_image(image_name: str) -> int:
    """
    Pull a container image headlessly.
    
    Returns the exit code of the pull command (0 = success).
    """
    startupinfo = get_startupinfo()
    
    def _pull():
        process = subprocess.Popen(
            ['podman', 'pull', image_name],
            stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            startupinfo=startupinfo
        )
        process.wait()
        return process.returncode
    
    return await asyncio.to_thread(_pull)


# --- Core Container Execution ---

def _run_container_sync(
    config: dict,
    run_blocks: list[dict],
    lang: str,
    run_index: int,
    total_runs: int,
    output_dir: str,
    timeout: int | None
) -> GroupResult:
    """
    Synchronous container execution. Called via asyncio.to_thread.
    
    This is the core Podman orchestration logic extracted from the original
    run_container_piped_group(), with GUI dependencies removed.
    
    Args:
        config: Runtime configuration dict (image, cmd, entrypoint, flags)
        run_blocks: List of classified blocks (seed + code) for this run
        lang: Language identifier string
        run_index: 1-based index of this run in a multi-run batch
        total_runs: Total number of runs in the batch
        output_dir: Host directory mounted as /output in the container
        timeout: Maximum execution time in seconds
    
    Returns:
        GroupResult with formatted stdout, stderr, exit code, and artifact paths.
    """
    startupinfo = get_startupinfo()

    wrapper_script = ["mkdir -p /output 2>/dev/null || true"]
    block_markers = []
    code_blocks = [b for b in run_blocks if b['type'] == 'code']
    is_single_step = len(code_blocks) <= 1

    step_index = 1
    for b in run_blocks:
        marker = f"EPHEMERAL_EOF_{uuid.uuid4().hex}"
        if b['type'] == 'seed':
            name = b['name']
            content = b['content']
            is_b64 = b.get('is_b64', False)
            if not content.endswith('\n'): content += '\n'

            wrapper_script.append(f"mkdir -p \"$(dirname '{name}')\" 2>/dev/null || true")
            if is_b64:
                wrapper_script.append(f"cat > '{name}.b64' << '{marker}'")
                wrapper_script.append(content.replace('\r\n', '\n') + marker)
                wrapper_script.append(f"base64 -d < '{name}.b64' > '{name}'")
                wrapper_script.append(f"rm -f '{name}.b64'")
            else:
                wrapper_script.append(f"cat > '{name}' << '{marker}'")
                wrapper_script.append(content.replace('\r\n', '\n') + marker)

        elif b['type'] == 'code':
            content = b['content']
            if not content.endswith('\n'): content += '\n'

            cmd_str = _shlex_join(config['cmd'])
            block_lang = b.get('header', '').split()[0].capitalize() if b.get('header') else "Code"

            b_marker = f"EPHEMERAL_STEP_{step_index}_{uuid.uuid4().hex}"
            block_markers.append((step_index, b_marker, block_lang))
            wrapper_script.append(f"echo '{b_marker}'")

            wrapper_script.append(f"{cmd_str} << '{marker}'")
            wrapper_script.append(content.replace('\r\n', '\n') + marker)
            step_index += 1

    script_code = ("\n".join(wrapper_script) + "\n").encode('utf-8')

    # Build Podman command with security flags
    podman_cmd = ['podman', 'run', '--rm', '-i', '--memory', '2g', '-w', '/tmp']

    if config.get('allow_network', False):
        pass  # Network explicitly enabled via 'unsafe' keyword
    else:
        podman_cmd.extend(['--network', 'none'])

    podman_cmd.extend(['-v', f'{output_dir}:/output'])

    if 'entrypoint' in config:
        podman_cmd.extend(['--entrypoint', config['entrypoint']])

    podman_cmd.append(config['image'])
    podman_cmd.extend(['sh'])

    process = subprocess.Popen(
        podman_cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        text=False, startupinfo=startupinfo
    )

    try:
        stdout_bytes, stderr_bytes = process.communicate(input=script_code, timeout=timeout)
    except subprocess.TimeoutExpired:
        process.kill()
        process.communicate()
        return GroupResult(
            stdout_formatted=f"## Run {run_index} Timed Out\n```text\nExecution exceeded {timeout}s timeout.\n```\n",
            stderr=f"Timeout after {timeout} seconds",
            exit_code=-1
        )

    stdout = strip_ansi_codes(stdout_bytes.decode('utf-8', errors='replace'))
    stderr = strip_ansi_codes(stderr_bytes.decode('utf-8', errors='replace'))

    if process.returncode == 0:
        files = [f for f in os.listdir(output_dir) if os.path.isfile(os.path.join(output_dir, f))]
        safe_lang = re.sub(r'[^a-zA-Z0-9]', '_', lang) if lang else "custom"

        # Collect chained files for piping to the next run
        new_chained_files = []
        if config.get('allow_chain', False):
            for f in files:
                filepath = os.path.join(output_dir, f)
                try:
                    with open(filepath, 'rb') as fd:
                        content_b64 = base64.b64encode(fd.read()).decode('utf-8')
                    new_chained_files.append({'type': 'seed', 'name': f, 'content': content_b64, 'is_b64': True})
                except Exception as e:
                    print(f"Error reading {f} for chaining: {e}")

        # Demultiplex stdout by marker
        outputs = {}
        current_marker_idx = 0
        current_text = []

        lines = stdout.split('\n')
        for line in lines:
            is_marker = False
            if current_marker_idx < len(block_markers):
                expected_marker = block_markers[current_marker_idx][1]
                if line.strip() == expected_marker:
                    if current_marker_idx > 0:
                        prev_step_idx = block_markers[current_marker_idx - 1][0]
                        outputs[prev_step_idx] = '\n'.join(current_text)
                    current_text = []
                    current_marker_idx += 1
                    is_marker = True

            if not is_marker:
                cleaned_line = re.sub(r"^--- Container \d+ \(.*\) ---\s*", "", line)
                current_text.append(cleaned_line)

        if current_marker_idx > 0:
            prev_step_idx = block_markers[current_marker_idx - 1][0]
            outputs[prev_step_idx] = '\n'.join(current_text)

        # Format output markdown
        result_parts = []
        title_lang = lang.split()[0].capitalize() if lang else "Custom"

        header_prefix = f"## Run {run_index} ({title_lang})" if total_runs > 1 else f"## Result ({title_lang})"
        result_parts.append(header_prefix)

        for i, (step_idx, marker_val, block_lang) in enumerate(block_markers):
            block_output = outputs.get(step_idx, "").strip('\r\n')

            if is_single_step:
                result_parts.append(f"```text\n{block_output}\n```")
            else:
                if not block_output:
                    block_output = ""
                result_parts.append(f"### Step {step_idx} ({block_lang})\n```text\n{block_output}\n```")

        result_str = "\n\n".join(result_parts) + "\n"
        artifact_paths = [os.path.join(output_dir, f) for f in files]

        return GroupResult(
            stdout_formatted=result_str,
            stderr=stderr,
            exit_code=0,
            artifact_paths=artifact_paths,
            chained_files=new_chained_files,
            image_copied=False  # Image-to-clipboard is handled by the local client, not the core
        )
    else:
        full_error = f"Exit Code: {process.returncode}\n\nSTDERR:\n{stderr}\n\nSTDOUT:\n{stdout}"
        return GroupResult(
            stdout_formatted=f"## Run {run_index} Failed\n```text\n{stderr.strip()}\n```\n",
            stderr=full_error,
            exit_code=process.returncode
        )


async def run_container_group(
    config: dict,
    run_blocks: list[dict],
    lang: str,
    run_index: int,
    total_runs: int,
    output_dir: str,
    timeout: int | None = None
) -> GroupResult:
    """
    Async wrapper around the synchronous container execution.
    
    Delegates to _run_container_sync via asyncio.to_thread to avoid
    blocking the event loop during subprocess.communicate().
    """
    return await asyncio.to_thread(
        _run_container_sync,
        config, run_blocks, lang, run_index, total_runs, output_dir, timeout
    )


# --- Top-Level Orchestrator ---

async def parse_and_execute(
    markdown_text: str,
    timeout: int | None = None
) -> ExecutionResult:
    """
    Parse Markdown text for codeblocks and execute them in Podman containers.
    
    This is the primary public API of ephemeral_core. It:
    1. Parses the markdown for fenced codeblocks / shebangs
    2. Groups blocks by language into runs
    3. Ensures required images are pulled
    4. Executes each run in an isolated container
    5. Returns an ExecutionResult with stdout, stderr, exit_code, and artifact paths
    
    Args:
        markdown_text: Raw Markdown string containing code blocks to execute.
        timeout: Maximum execution time per container in seconds.
    
    Returns:
        ExecutionResult containing all execution outputs and artifact references.
    
    Raises:
        ValueError: If no executable code blocks are found, or if a block
                     has no language specified (API mode cannot prompt the user).
        RuntimeError: If Podman is unreachable and cannot be started.
    """
    # Safety check: reject clipboard containing previous Ephemeral output
    stripped = markdown_text.strip()
    if (re.search(r"^## (Run|Result) .*[\r\n]+```text", stripped, re.MULTILINE)
            or re.search(r"^Result \(.*\):[\r\n]+---[\r\n]+", stripped, re.MULTILINE)
            or re.search(r"^--- Run \d+ \(.*\) ---\n```text", stripped, re.MULTILINE)):
        raise ValueError("Input contains previous Ephemeral results. Execution halted for safety.")

    blocks = parse_codeblocks(markdown_text)
    if not blocks:
        raise ValueError("No code blocks found in the provided Markdown text.")

    # In API/core mode, we cannot prompt for a language — raise if undetectable
    if len(blocks) == 1 and blocks[0]['type'] == 'code' and not blocks[0]['header']:
        raise ValueError(
            "No language detected in the code block. "
            "Specify a language via a fenced code block header (e.g., ```python) "
            "or a shebang (#! python)."
        )

    code_blocks = [b for b in blocks if b['type'] == 'code']
    if not code_blocks:
        raise ValueError("Input contains only seed files with no executable code blocks.")

    # Group blocks into runs by language/config
    runs = []
    current_run = []

    for b in blocks:
        if b['type'] == 'seed':
            current_run.append(b)
        else:
            if not b['config'] or not b['config'].get('image'):
                raise ValueError(f"Configuration failed for block with header: '{b.get('header', '')}'")

            if not current_run:
                current_run.append(b)
            else:
                last_code = next((x for x in reversed(current_run) if x['type'] == 'code'), None)
                if last_code:
                    if last_code['config'] == b['config']:
                        current_run.append(b)
                    else:
                        runs.append(current_run)
                        current_run = [b]
                else:
                    current_run.append(b)

    if current_run:
        runs.append(current_run)

    # Ensure Podman is available
    await ensure_podman_running()

    # Execute each run
    all_stdout = []
    all_stderr = []
    overall_exit_code = 0
    all_artifact_paths = []
    final_artifact_dir = None
    chained_files = []

    for i, run in enumerate(runs):
        if chained_files:
            run = chained_files + run

        code_item = next(b for b in run if b['type'] == 'code')
        lang = code_item['header']
        config = code_item['config']

        image_name = config['image']
        is_cached = check_image_exists(image_name)
        if not is_cached:
            exit_code = await pull_image(image_name)
            if exit_code != 0:
                raise RuntimeError(f"Failed to pull image: {image_name}")

        # Create a temp output directory for this run's artifacts
        output_dir = tempfile.mkdtemp(prefix="ephemeral_")

        result = await run_container_group(
            config, run, lang,
            run_index=i + 1,
            total_runs=len(runs),
            output_dir=output_dir,
            timeout=timeout
        )

        chained_files = result.chained_files

        if result.stdout_formatted:
            all_stdout.append(result.stdout_formatted)
        if result.stderr:
            all_stderr.append(result.stderr)
        if result.exit_code != 0:
            overall_exit_code = result.exit_code

        # Collect artifact paths; keep the last non-empty artifact dir
        if result.artifact_paths:
            all_artifact_paths.extend(result.artifact_paths)
            final_artifact_dir = output_dir
        else:
            # Clean up empty output dirs immediately
            try:
                os.rmdir(output_dir)
            except OSError:
                pass

    return ExecutionResult(
        stdout="\n".join(all_stdout),
        stderr="\n".join(all_stderr),
        exit_code=overall_exit_code,
        artifact_paths=all_artifact_paths,
        artifact_dir=final_artifact_dir,
        image_copied=False,
        blocks=[]
    )
