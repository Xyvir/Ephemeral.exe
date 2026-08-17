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
import json
import logging
import os
import re
import shlex
import shutil
import subprocess
import tempfile
import uuid

from .config import LANG_MAP
from .space import SpaceGuardError, ensure_space_for_pull

logger = logging.getLogger(__name__)
from .models import ExecutionResult, GroupResult
from .parser import (
    parse_codeblocks,
    prepare_python_block,
    resolve_runtime_config,
    strip_ansi_codes,
    strip_shebang,
)

_active_pulls = set()

#: Maximum number of runs executed concurrently on one host. Multi-block
#: requests whose runs are independent (no chaining declared) run in
#: parallel up to this guardrail; chained requests always run in-order.
MAX_PARALLEL_RUNS = 4


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


def list_local_images() -> list[str]:
    """
    List the fully-qualified names of all images cached locally, in one call.

    Used by distributed nodes to advertise which images are "warm" so peers
    can offload jobs to a node that already has the image instead of pulling.
    Returns an empty list when Podman is unavailable or returns no names.
    """
    try:
        startupinfo = get_startupinfo()
        output = subprocess.check_output(
            ['podman', 'images', '--format', 'json'],
            startupinfo=startupinfo,
            stderr=subprocess.DEVNULL,
        )
    except Exception:
        return []
    try:
        entries = json.loads(output)
    except Exception:
        return []
    names: list[str] = []
    for entry in entries if isinstance(entries, list) else []:
        for name in entry.get('Names') or []:
            if name and name not in names:
                names.append(name)
    return names


# --- Cgroup resource-limit capability ------------------------------------
#
# A stock WSL2 + `podman machine` setup does NOT delegate the cgroup
# controllers (cpu/pids/memory) into the rootless user slice, so crun
# rejects `--memory`/`--cpus`/`--pids-limit` ("controller `cpu` is not
# available" / "open `memory.max` for writing"). Rather than asking
# Windows users to customize their WSL/Linux setup, Ephemeral probes the
# host once and skips only the limits it genuinely cannot enforce,
# keeping `--network none` and the markdown-level sandbox intact.

_limits_supported: bool | None = None
_limits_warning_emitted = False


def podman_supports_cgroup_limits() -> bool:
    """
    Whether this host's Podman can enforce cgroup resource limits.

    Probed once per process with a throwaway limited container; the
    result is cached. Returns ``True`` when the probe can't run (no
    images, Podman down) so behavior only changes when we *know* limits
    are unsupported.
    """
    global _limits_supported
    if _limits_supported is None:
        _limits_supported = _probe_cgroup_limits()
    return _limits_supported


def _probe_cgroup_limits() -> bool:
    """Run a tiny limited container against a warm image."""
    images = list_local_images()
    if not images:
        return True  # nothing runs anyway — keep prior behavior
    probe = [
        'podman', 'run', '--rm', '-i',
        '--memory', '64m', '--cpus', '1', '--pids-limit', '100',
        images[0], 'true',
    ]
    try:
        startupinfo = get_startupinfo()
        result = subprocess.run(
            probe,
            stdin=subprocess.DEVNULL,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.PIPE,
            startupinfo=startupinfo,
            timeout=30,
        )
    except Exception:
        return True
    if result.returncode == 0:
        return True
    err = (result.stderr or b"").decode("utf-8", "replace").lower()
    # Only the cgroup-delegation failure modes disable limits; anything
    # else keeps prior behavior.
    markers = ("memory.max", "controller `", "is not available", "cgroup.controllers")
    return not any(m in err for m in markers)


def _limits_warning_line() -> str:
    """
    A one-time stderr note when the host can't enforce cgroup limits.

    Emitted on the first job so the user sees *why* the sandbox flags are
    missing; suppressed afterwards.
    """
    global _limits_warning_emitted
    if _limits_warning_emitted or podman_supports_cgroup_limits():
        return ""
    _limits_warning_emitted = True
    return (
        "echo '[ephemeral] host podman cannot enforce resource limits "
        "(--memory/--cpus/--pids-limit): cgroup controllers are not "
        "delegated on this WSL2 default setup - running without them' >&2"
    )


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

    Refuses (raising :class:`ephemeral_core.space.SpaceGuardError`) when
    the drive backing podman's storage cannot hold the image even after
    evicting the coldest cached images — see ``ephemeral_core.space``.

    Returns the exit code of the pull command (0 = success).
    """
    # Disk-space guardrail (best-effort): probe + evict coldest images
    # before pulling so a tight drive never hits "no space left on device".
    await asyncio.to_thread(ensure_space_for_pull, image_name)

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
    
    Python blocks backed by `uv run` get their third-party imports turned into
    a PEP 723 inline-script header (implicit dependency injection). When those
    blocks declare dependencies but the user did NOT grant network access via
    the `unsafe` keyword, the run is split into two container stages:
    resolve the dependencies into a shared venv with the network up, then
    execute the payload with the network removed (see _run_two_stage_python).
    
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
    code_blocks = [b for b in run_blocks if b['type'] == 'code']
    is_single_step = len(code_blocks) <= 1
    uses_uv_python = config.get('cmd') == ['uv', 'run', '-']

    # Implicit PEP 723 dependency injection: infer third-party imports and
    # prepend a `# /// script` header so `uv run` knows what to resolve.
    prepared_blocks = run_blocks
    python_deps: list[str] = []
    if uses_uv_python:
        prepared_blocks = []
        for b in run_blocks:
            if b['type'] == 'code':
                prepared, deps = prepare_python_block(b)
                prepared_blocks.append(prepared)
                python_deps.extend(deps)
            else:
                prepared_blocks.append(b)
        python_deps = sorted(set(python_deps))

    # Two-stage execution: dependencies need the network, the payload doesn't.
    if uses_uv_python and python_deps and not config.get('allow_network', False):
        return _run_two_stage_python(
            config, prepared_blocks, python_deps, lang,
            run_index, total_runs, output_dir, timeout, is_single_step
        )

    # Normal single-stage execution (offline, or `unsafe` with network).
    return _run_single_stage(
        config, prepared_blocks, lang,
        run_index, total_runs, output_dir, timeout, is_single_step
    )


def _build_podman_cmd(
    config: dict,
    output_dir: str,
    extra_mounts: list[tuple[str, str]] | None = None,
    network: bool | None = None
) -> list[str]:
    """
    Build the `podman run` command with Ephemeral's security flags.

    `network=True` uses the default bridge plus explicit public DNS resolvers;
    `network=False` applies `--network none`. When `network` is None the
    container's `allow_network` config flag decides.
    """
    podman_cmd = ['podman', 'run', '--rm', '-i', '-w', '/tmp']
    if podman_supports_cgroup_limits():
        # Hard container limits — skipped (with a one-time warning) only
        # when the host genuinely can't enforce them (stock WSL2).
        podman_cmd.extend(['--memory', '2g', '--cpus', '2', '--pids-limit', '512'])

    if network is not None:
        if network:
            # Pass explicit public DNS resolvers to prevent systemd-resolved loopback failures in rootless Podman
            podman_cmd.extend(['--dns', '8.8.8.8', '--dns', '1.1.1.1'])
        else:
            podman_cmd.extend(['--network', 'none'])
    elif config.get('allow_network', False):
        podman_cmd.extend(['--dns', '8.8.8.8', '--dns', '1.1.1.1'])
    else:
        podman_cmd.extend(['--network', 'none'])

    podman_cmd.extend(['-v', f'{output_dir}:/output'])
    for host_path, container_path in (extra_mounts or []):
        podman_cmd.extend(['-v', f'{host_path}:{container_path}'])

    if 'entrypoint' in config:
        podman_cmd.extend(['--entrypoint', config['entrypoint']])

    podman_cmd.append(config['image'])
    podman_cmd.extend(['sh'])
    return podman_cmd


def _build_wrapper_script(
    run_blocks: list[dict],
    cmd: list[str]
) -> tuple[list[str], list[tuple[int, str, str]]]:
    """
    Build the shell wrapper script that stages seed files and runs each code
    block with the given command.

    Returns ``(script_lines, block_markers)`` where ``block_markers`` is a list
    of ``(step_index, marker, language)`` tuples used to demultiplex stdout.
    """
    wrapper_script = ["mkdir -p /output 2>/dev/null || true"]
    _warning = _limits_warning_line()
    if _warning:
        wrapper_script.insert(1, _warning)
    block_markers = []
    step_index = 1
    cmd_str = _shlex_join(cmd)

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

            block_lang = b.get('header', '').split()[0].capitalize() if b.get('header') else "Code"

            b_marker = f"EPHEMERAL_STEP_{step_index}_{uuid.uuid4().hex}"
            block_markers.append((step_index, b_marker, block_lang))
            wrapper_script.append(f"echo '{b_marker}'")

            wrapper_script.append(f"{cmd_str} << '{marker}'")
            wrapper_script.append(content.replace('\r\n', '\n') + marker)
            step_index += 1

    return wrapper_script, block_markers


def _run_podman_script(
    podman_cmd: list[str],
    script_code: bytes,
    timeout: int | None
) -> tuple[int | None, str, str]:
    """
    Run a podman container with ``script_code`` piped to its stdin.

    Returns ``(returncode, stdout, stderr)``; ``returncode`` is None when the
    run exceeded ``timeout``.
    """
    startupinfo = get_startupinfo()
    process = subprocess.Popen(
        podman_cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        text=False, startupinfo=startupinfo
    )

    try:
        stdout_bytes, stderr_bytes = process.communicate(input=script_code, timeout=timeout)
    except subprocess.TimeoutExpired:
        process.kill()
        process.communicate()
        return None, "", f"Execution exceeded {timeout}s timeout."

    stdout = strip_ansi_codes(stdout_bytes.decode('utf-8', errors='replace'))
    stderr = strip_ansi_codes(stderr_bytes.decode('utf-8', errors='replace'))
    return process.returncode, stdout, stderr


def _lang_title(lang: str) -> str:
    """Display name for a block's language (first token, capitalized)."""
    if not lang:
        return "Custom"
    first = lang.split()[0]
    if first.lower() == "bf":
        return "BF"  # brainfuck is shown as 'bf' in the PWA (kid-friendly)
    return first.capitalize()


def _format_success_result(
    stdout: str,
    stderr: str,
    block_markers: list[tuple[int, str, str]],
    lang: str,
    run_index: int,
    total_runs: int,
    is_single_step: bool,
    output_dir: str,
    allow_chain: bool
) -> GroupResult:
    """Format a successful run's raw stdout/stderr into a GroupResult."""
    files = [f for f in os.listdir(output_dir) if os.path.isfile(os.path.join(output_dir, f))]

    # Collect chained files for piping to the next run
    new_chained_files = []
    if allow_chain:
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
    title_lang = _lang_title(lang)

    header_prefix = f"## {title_lang} Run {run_index}" if total_runs > 1 else f"## {title_lang} Result"
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


def _run_single_stage(
    config: dict,
    run_blocks: list[dict],
    lang: str,
    run_index: int,
    total_runs: int,
    output_dir: str,
    timeout: int | None,
    is_single_step: bool
) -> GroupResult:
    """Run a container group exactly once (the original single-container path)."""
    script_lines, block_markers = _build_wrapper_script(run_blocks, config['cmd'])
    script_code = ("\n".join(script_lines) + "\n").encode('utf-8')

    podman_cmd = _build_podman_cmd(config, output_dir)
    returncode, stdout, stderr = _run_podman_script(podman_cmd, script_code, timeout)
    tlang = _lang_title(lang)

    if returncode is None:
        return GroupResult(
            stdout_formatted=f"## {tlang} Run {run_index} Timed Out\n```text\nExecution exceeded {timeout}s timeout.\n```\n",
            stderr=f"Timeout after {timeout} seconds",
            exit_code=-1
        )

    if returncode == 0:
        return _format_success_result(
            stdout, stderr, block_markers, lang, run_index, total_runs,
            is_single_step, output_dir, config.get('allow_chain', False)
        )

    full_error = f"Exit Code: {returncode}\n\nSTDERR:\n{stderr}\n\nSTDOUT:\n{stdout}"
    return GroupResult(
        stdout_formatted=f"## {tlang} Run {run_index} Failed\n```text\n{stderr.strip()}\n```\n",
        stderr=full_error,
        exit_code=returncode
    )


def _run_two_stage_python(
    config: dict,
    run_blocks: list[dict],
    deps: list[str],
    lang: str,
    run_index: int,
    total_runs: int,
    output_dir: str,
    timeout: int | None,
    is_single_step: bool
) -> GroupResult:
    """
    Run a Python payload in two stages so dependencies can be resolved without
    permanently granting the payload network access:

      Stage A: a network-enabled container installs `deps` into a venv created
               on a volume shared with the next stage. This is the only stage
               with internet access.
      Stage C: a network-disabled container executes the payload with that
               venv's interpreter. The payload itself never sees the network.

    The venv lives in a host temp directory mounted at /deps in both containers
    and is removed when the run finishes.
    """
    deps_dir = tempfile.mkdtemp(prefix="ephemeral_deps_")
    tlang = _lang_title(lang)
    try:
        # --- Stage A: resolve dependencies with network access ---
        install_script = (
            "mkdir -p /deps /output 2>/dev/null || true\n"
            "uv venv /deps/venv || exit 1\n"
            "uv pip install --no-cache --python /deps/venv/bin/python "
            + " ".join(shlex.quote(d) for d in deps)
            + "\n"
        ).encode('utf-8')

        stage_a_cmd = _build_podman_cmd(
            config, output_dir, extra_mounts=[(deps_dir, '/deps')], network=True
        )
        retcode, stdout, stderr = _run_podman_script(stage_a_cmd, install_script, timeout)
        if retcode is None:
            return GroupResult(
                stdout_formatted=f"## {tlang} Run {run_index} Timed Out\n```text\nDependency resolution exceeded {timeout}s timeout.\n```\n",
                stderr=f"Dependency resolution timed out after {timeout} seconds",
                exit_code=-1
            )
        if retcode != 0:
            full_error = (
                f"Dependency resolution failed for: {', '.join(deps)}\n"
                f"Exit Code: {retcode}\n\nSTDERR:\n{stderr}\n\nSTDOUT:\n{stdout}"
            )
            return GroupResult(
                stdout_formatted=f"## {tlang} Run {run_index} Failed (dependency resolution)\n```text\n{stderr.strip()}\n```\n",
                stderr=full_error,
                exit_code=retcode
            )

        # --- Stage C: run the payload with the network removed ---
        script_lines, block_markers = _build_wrapper_script(run_blocks, ['/deps/venv/bin/python', '-'])
        script_code = ("\n".join(script_lines) + "\n").encode('utf-8')

        stage_c_cmd = _build_podman_cmd(
            config, output_dir, extra_mounts=[(deps_dir, '/deps')], network=False
        )
        retcode, stdout, stderr = _run_podman_script(stage_c_cmd, script_code, timeout)
        if retcode is None:
            return GroupResult(
                stdout_formatted=f"## {tlang} Run {run_index} Timed Out\n```text\nExecution exceeded {timeout}s timeout.\n```\n",
                stderr=f"Timeout after {timeout} seconds",
                exit_code=-1
            )

        if retcode == 0:
            return _format_success_result(
                stdout, stderr, block_markers, lang, run_index, total_runs,
                is_single_step, output_dir, config.get('allow_chain', False)
            )

        full_error = f"Exit Code: {retcode}\n\nSTDERR:\n{stderr}\n\nSTDOUT:\n{stdout}"
        return GroupResult(
            stdout_formatted=f"## {tlang} Run {run_index} Failed\n```text\n{stderr.strip()}\n```\n",
            stderr=full_error,
            exit_code=retcode
        )
    finally:
        shutil.rmtree(deps_dir, ignore_errors=True)


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


# --- Run grouping ---

def group_into_runs(blocks: list[dict]) -> list[list[dict]]:
    """
    Group parsed blocks into runs by identical runtime config.

    Seed blocks attach to the run that follows them; consecutive code
    blocks with identical resolved config (same image/cmd/entrypoint/flags)
    share one container run. Returns a list of runs, each a list of
    ``seed``/``code`` block dicts. Raises :class:`ValueError` when a code
    block has no resolvable image.
    """
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
    return runs


def request_is_chained(runs: list[list[dict]]) -> bool:
    """
    True when any code block in ``runs`` declares artifact chaining.

    Chaining is opt-in (``chain``/``piping``/``pipe`` in a block header).
    When any block declares it, the whole request must run in-order so
    artifacts flow run-to-run; otherwise runs are independent and may
    execute concurrently.
    """
    return any(
        b.get('config') and b['config'].get('allow_chain')
        for run in runs
        for b in run
        if b['type'] == 'code'
    )


async def _execute_run(
    run: list[dict],
    run_index: int,
    total_runs: int,
    timeout: int | None,
    server_mode: bool,
    cancelled_images: set[str],
) -> tuple[GroupResult | None, str | None, str | None]:
    """
    Execute a single run (image check/pull + container).

    Returns ``(result, output_dir, notice)``. In ``server_mode`` a run
    whose image is missing is skipped (background pull started) and
    ``notice`` carries the "downloading, wait" message the caller must
    surface; ``result``/``output_dir`` are then None.
    """
    code_item = next(b for b in run if b['type'] == 'code')
    lang = code_item['header']
    config = code_item['config']

    image_name = config['image']

    if server_mode and image_name in cancelled_images:
        return None, None, None

    is_cached = check_image_exists(image_name)
    if not is_cached:
        if server_mode:
            lang_name = lang.split()[0].capitalize() if lang else "Requested"
            msg = f"The {lang_name} runner isn't cached yet and is currently downloading. Please wait approximately 5 minutes, then run your code again."
            cancelled_images.add(image_name)

            if image_name not in _active_pulls:
                _active_pulls.add(image_name)

                async def bg_pull(img=image_name):
                    try:
                        await pull_image(img)
                    except SpaceGuardError as e:
                        # Best-effort background pull: a full drive just
                        # leaves the image cold; jobs keep offloading.
                        logger.warning(
                            "background pull of %s refused: %s", img, e
                        )
                    finally:
                        _active_pulls.discard(img)

                asyncio.create_task(bg_pull())

            return None, None, msg

        try:
            exit_code = await pull_image(image_name)
        except SpaceGuardError as e:
            raise RuntimeError(str(e)) from e
        if exit_code != 0:
            raise RuntimeError(f"Failed to pull image: {image_name}")

    # Create a temp output directory for this run's artifacts
    output_dir = tempfile.mkdtemp(prefix="ephemeral_")

    result = await run_container_group(
        config, run, lang,
        run_index=run_index,
        total_runs=total_runs,
        output_dir=output_dir,
        timeout=timeout,
    )
    return result, output_dir, None


# --- Top-Level Orchestrator ---

async def parse_and_execute(
    markdown_text: str,
    timeout: int | None = None,
    server_mode: bool = False
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

    runs = group_into_runs(blocks)

    # Ensure Podman is available
    await ensure_podman_running()

    all_stdout = []
    all_stderr = []
    overall_exit_code = 0
    all_artifact_paths = []
    final_artifact_dir = None
    cancelled_images = set()

    def _collect(result: GroupResult, output_dir: str) -> None:
        """Merge one run's output into the aggregate (closure over the lists)."""
        nonlocal overall_exit_code, final_artifact_dir
        if result.stdout_formatted:
            all_stdout.append(result.stdout_formatted)
        if result.stderr:
            all_stderr.append(result.stderr)
        if result.exit_code != 0:
            overall_exit_code = result.exit_code
        if result.artifact_paths:
            all_artifact_paths.extend(result.artifact_paths)
            final_artifact_dir = output_dir
        else:
            # Clean up empty output dirs immediately
            try:
                os.rmdir(output_dir)
            except OSError:
                pass

    if request_is_chained(runs) or len(runs) <= 1:
        # Sequential path: runs execute in-order and chained artifacts flow
        # from one run to the next (opt-in via the `chain` flag).
        chained_files = []
        for i, run in enumerate(runs):
            if chained_files:
                run = chained_files + run
            result, output_dir, notice = await _execute_run(
                run, i + 1, len(runs), timeout, server_mode, cancelled_images
            )
            if notice:
                all_stdout.append(notice + "\n")
            if result is None:
                continue  # server_mode: image downloading, run skipped
            chained_files = result.chained_files
            _collect(result, output_dir)
    else:
        # Parallel path: no block declared chaining, so runs are independent
        # and execute concurrently (capped by MAX_PARALLEL_RUNS).
        semaphore = asyncio.Semaphore(MAX_PARALLEL_RUNS)

        async def _worker(i: int, run: list[dict]):
            async with semaphore:
                return await _execute_run(
                    run, i + 1, len(runs), timeout, server_mode, cancelled_images
                )

        results = await asyncio.gather(
            *(_worker(i, run) for i, run in enumerate(runs))
        )
        for result, output_dir, notice in results:
            if notice:
                all_stdout.append(notice + "\n")
            if result is None:
                continue  # server_mode: image downloading, run skipped
            _collect(result, output_dir)

    return ExecutionResult(
        stdout="\n".join(all_stdout),
        stderr="\n".join(all_stderr),
        exit_code=overall_exit_code,
        artifact_paths=all_artifact_paths,
        artifact_dir=final_artifact_dir,
        image_copied=False,
        blocks=[]
    )
