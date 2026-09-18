"""
Local backend — routes execution to local Podman.

Everything the tray can do in local mode lives here: clipboard parsing,
the untagged-language prompt, sandboxed container runs via
``ephemeral_core``, artifact routing, Podman lifecycle (machine
start/stop, idle sleep), and cache maintenance. The front end treats it
as a generic :class:`~ephemeral_ui.backends.base.Backend`.
"""
from __future__ import annotations

import asyncio
import os
import re
import shutil
import subprocess
import sys
import tempfile
import threading
import time

import ephemeral_core
from ephemeral_core.executor import host_arch, run_container_group, check_image_exists
from ephemeral_core.parser import strip_shebang, resolve_runtime_config
from ephemeral_core.space import SpaceGuardError, ensure_space_for_pull

from ephemeral_ui import platform
from ephemeral_ui.backends.base import Backend


class LocalBackend(Backend):
    """Tray backend for the local (non-distributed) client."""

    app_key = "Ephemeral"
    display_name = "Ephemeral"

    def __init__(self) -> None:
        super().__init__()
        self._last_detected_lang = "python"
        self._last_activity_time = time.time()
        self.active_processes = []

    # --- identity --------------------------------------------------------

    def about(self) -> str:
        return ("# Ephemeral.exe\n"
                "Version: Version number (injected from the github workflow)\n"
                "Dev: Dunko Xyvir\nLicense: MIT License\n"
                "URL: https://github.com/Xyvir/Ephemeral.exe")

    def startup_message(self) -> str:
        return "Ephemeral tray started — ready for clipboard runs."

    # --- Podman lifecycle (local-specific, with icon notifications) ------

    def _ensure_podman_running(self, icon):
        if ephemeral_core.check_podman_alive():
            return
        icon.notify("Podman is not running. Attempting to start...", title="Ephemeral Init")
        startupinfo = platform.get_startupinfo()
        try:
            if sys.platform == 'win32':
                subprocess.check_call(['podman', 'machine', 'start'], startupinfo=startupinfo,
                                      stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            else:
                # Linux: podman runs natively — start the rootless socket when systemd is present.
                subprocess.check_call(['systemctl', '--user', 'start', 'podman.socket'],
                                      stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            icon.notify("Podman started successfully.", title="Ephemeral Init")
        except subprocess.CalledProcessError:
            if sys.platform != 'win32':
                icon.notify("Podman is not running and could not be started. Is podman installed?",
                            title="Ephemeral Fatal Error")
                return
            icon.notify("Start failed. Initializing new machine...", title="Ephemeral Init")
            try:
                subprocess.check_call(['podman', 'machine', 'init'], startupinfo=startupinfo)
                subprocess.check_call(['podman', 'machine', 'start'], startupinfo=startupinfo)
                icon.notify("Podman machine initialized and started.", title="Ephemeral Init")
            except Exception as e:
                icon.notify(f"Could not start Podman: {e}", title="Ephemeral Fatal Error")

    def _stop_podman(self, icon):
        if sys.platform not in ('win32', 'darwin'):
            return  # Podman runs natively on Linux — nothing to stop
        icon.notify("Stopping Podman machine...", title="Ephemeral Shutdown")
        startupinfo = platform.get_startupinfo()
        try:
            subprocess.run(['podman', 'machine', 'stop'], startupinfo=startupinfo)
        except Exception as e:
            print(f"Error stopping podman: {e}")

    def _perform_visible_pull(self, image_name):
        """Pull a container image with a visible console window on Windows."""
        # Disk-space guardrail: refuse when the drive can't hold the image even
        # after evicting the coldest cached images (see ephemeral_core.space).
        try:
            ensure_space_for_pull(image_name)
        except SpaceGuardError as e:
            print(f"[Ephemeral] {e}", file=sys.stderr)
            return 1
        if sys.platform == 'win32':
            cmd_line = f'cmd /C "echo [Ephemeral] Image {image_name} not found. Downloading... && podman pull --platform linux/{host_arch()} {image_name} || pause"'
            process = subprocess.Popen(cmd_line, creationflags=getattr(subprocess, 'CREATE_NEW_CONSOLE', 0))
        else:
            cmd_line = ['podman', 'pull', '--platform', f'linux/{host_arch()}', image_name]
            process = subprocess.Popen(cmd_line)
        return process.wait()

    # --- maintenance -----------------------------------------------------

    def purge_cache(self, icon, item_unused=None):
        icon.notify("Pruning unused images... this may take a moment.", title="Ephemeral Maintenance")
        startupinfo = platform.get_startupinfo()
        try:
            subprocess.run(['podman', 'image', 'prune', '--all', '--force'],
                           startupinfo=startupinfo, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            icon.notify("Image cache cleared successfully.", title="Ephemeral")
        except Exception as e:
            icon.notify(f"Error clearing cache: {e}", title="Ephemeral Error")

    def force_stop_all(self, icon, item_unused=None):
        killed_count = 0
        for p in list(self.active_processes):
            try:
                p.kill()
                killed_count += 1
            except Exception:
                pass
        self.active_processes.clear()

        try:
            startupinfo = platform.get_startupinfo()
            subprocess.run(['podman', 'rm', '-f', '$(podman ps -q)'], shell=True,
                           startupinfo=startupinfo, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        except Exception:
            pass

        platform.set_icon_animation_state(icon, False)
        if killed_count > 0:
            icon.notify(f"Forcefully stopped {killed_count} active runs.", title="Ephemeral Stopped")
        else:
            icon.notify("No active runs to stop.", title="Ephemeral")

    # --- run lifecycle ---------------------------------------------------

    def prepare_run(self, icon):
        was_running = ephemeral_core.check_podman_alive()
        if not was_running:
            self._ensure_podman_running(icon)
        return was_running

    def cleanup_run(self, icon, was_running):
        if not was_running:
            self._stop_podman(icon)

    def shutdown(self) -> None:
        # Local client never stops Podman on a bare process exit (matches
        # the original CLI fallback path).
        pass

    def quit(self, icon, item_unused=None):
        self._stop_podman(icon)
        icon.stop()
        sys.exit()

    # --- execution -------------------------------------------------------

    def run_logic(self, icon, content=None):
        """
        Bridge between the local tray UI and ephemeral_core.

        Reads from clipboard if no content provided, handles the language
        prompt for untagged blocks, delegates execution to the core, then
        routes artifacts and results back through local channels.
        """
        if content is None:
            content = platform.get_clipboard()

        # Safety check: reject previous Ephemeral output (same as original)
        if (re.search(r"^## (Run|Result) .*[\r\n]+```text", content.strip(), re.MULTILINE)
                or re.search(r"^Result \(.*\):[\r\n]+---[\r\n]+", content.strip(), re.MULTILINE)
                or re.search(r"^--- Run \d+ \(.*\) ---\n```text", content.strip(), re.MULTILINE)):
            icon.notify("Clipboard contains previous results. Execution halted.", title="Ephemeral Safety")
            return

        blocks = ephemeral_core.parse_codeblocks(content)
        if not blocks:
            icon.notify("Clipboard is empty.", title="Ephemeral Error")
            return

        # Handle untagged single block: prompt user for language (local-only behavior)
        if len(blocks) == 1 and blocks[0]['type'] == 'code' and not blocks[0]['header']:
            code = strip_shebang(blocks[0]['content'])
            code = re.sub(r"```+\s*$", "", code.rstrip())
            user_input = platform.prompt_user_for_language(self._last_detected_lang, code)
            if user_input:
                blocks[0]['header'] = user_input.strip()
                blocks[0]['config'] = resolve_runtime_config(blocks[0]['header'])
            else:
                icon.notify("Execution cancelled.", title="Ephemeral")
                return

        code_blocks = [b for b in blocks if b['type'] == 'code']
        if not code_blocks:
            icon.notify("Clipboard only contains seed files.", title="Ephemeral Error")
            return

        # Group blocks into runs (same grouping logic as original)
        runs = []
        current_run = []

        for b in blocks:
            if b['type'] == 'seed':
                current_run.append(b)
            else:
                if not b['config'] or not b['config'].get('image'):
                    icon.notify("Configuration failed for a block.", title="Ephemeral Error")
                    return

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

        if len(runs) > 1:
            icon.notify(f"Executing {len(runs)} grouped runs...", title="Ephemeral Status")
        else:
            lang = next(b for b in runs[0] if b['type'] == 'code')['header']
            self._last_detected_lang = lang.split()[0] if lang else self._last_detected_lang
            icon.notify(f"Launching {self._last_detected_lang}...", title="Ephemeral Status")

        platform.set_icon_animation_state(icon, True)

        # Execute via the core engine (run synchronously in this thread via asyncio.run)
        try:
            all_stdout = []
            executed_langs = []
            image_was_copied = False
            chained_files = []

            for i, run in enumerate(runs):
                if chained_files:
                    run = chained_files + run

                code_item = next(b for b in run if b['type'] == 'code')
                lang = code_item['header']
                self._last_detected_lang = lang.split()[0] if lang else self._last_detected_lang
                config = code_item['config']

                image_name = config['image']
                is_cached = check_image_exists(image_name)
                if not is_cached:
                    exit_code = self._perform_visible_pull(image_name)
                    if exit_code != 0:
                        icon.notify("Image download failed.", title="Ephemeral Error")
                        return

                output_dir = tempfile.mkdtemp(prefix="ephemeral_")

                # Run synchronously using asyncio.run for the async core function
                group_result = asyncio.run(
                    run_container_group(
                        config, run, lang,
                        run_index=i + 1,
                        total_runs=len(runs),
                        output_dir=output_dir,
                        timeout=None
                    )
                )

                chained_files = group_result.chained_files

                # Route artifacts locally
                if group_result.artifact_paths:
                    # Build a mini ExecutionResult for the routing function
                    mini_result = ephemeral_core.ExecutionResult(
                        stdout=group_result.stdout_formatted,
                        stderr=group_result.stderr,
                        exit_code=group_result.exit_code,
                        artifact_paths=group_result.artifact_paths,
                        artifact_dir=output_dir,
                    )
                    img_copied = platform.route_artifacts_local(mini_result, lang, icon)
                    if img_copied:
                        image_was_copied = True
                else:
                    # Clean up empty output dir
                    try:
                        shutil.rmtree(output_dir, ignore_errors=True)
                    except Exception:
                        pass

                if group_result.exit_code != 0:
                    platform.show_terminal_window(
                        "Ephemeral Error",
                        group_result.stderr,
                        header="EPHEMERAL EXECUTION ERROR",
                    )

                if group_result.stdout_formatted:
                    all_stdout.append(group_result.stdout_formatted)
                    title_lang = lang.split()[0].capitalize() if lang else "Custom"
                    if title_lang not in executed_langs:
                        executed_langs.append(title_lang)

            if all_stdout:
                final_result = "\n".join(all_stdout)

                if image_was_copied and len(runs) == 1:
                    lang_str = ", ".join(executed_langs) if executed_langs else "Custom"
                    icon.notify(f"{lang_str} Execution Finished. Image preserved in clipboard.", title="Ephemeral")
                else:
                    if not platform.CLI_MODE:
                        platform.pyperclip.copy(final_result)
                    else:
                        print(final_result)
                    lang_str = ", ".join(executed_langs) if executed_langs else "Custom"
                    icon.notify(f"{lang_str} Execution Finished. Results copied.", title="Ephemeral")

        except Exception as e:
            platform.show_terminal_window(
                "Ephemeral Error",
                f"System Exception:\n{str(e)}",
                header="EPHEMERAL EXECUTION ERROR",
            )

        finally:
            platform.set_icon_animation_state(icon, False)

    # --- hotkeys / idle monitor ------------------------------------------

    def on_hotkey(self, icon):
        self._last_activity_time = time.time()
        def hotkey_task():
            self._ensure_podman_running(icon)
            self.run_logic(icon)
        threading.Thread(target=hotkey_task).start()

    def _idle_monitor(self, icon):
        while True:
            time.sleep(60)
            if time.time() - self._last_activity_time > 1800:
                if not self.active_processes and ephemeral_core.check_podman_alive():
                    icon.notify("Idling for 30 minutes. Stopping Podman VM.", title="Ephemeral Sleep")
                    self._stop_podman(icon)

    # --- tray lifecycle --------------------------------------------------

    def setup_tray(self, icon):
        threading.Thread(target=self._idle_monitor, args=(icon,), daemon=True).start()

    def extra_menu_items(self, icon) -> tuple:
        # No Distributed submenu on the local client.
        return ()

    # --- install verification --------------------------------------------

    def self_check(self) -> int:
        blocks = ephemeral_core.parse_codeblocks("```python\nprint(1)\n```")
        assert blocks and blocks[0]["type"] == "code", "parser round-trip failed"
        print(f"SELF-CHECK OK parser_ok podman_alive={ephemeral_core.check_podman_alive()}")
        return 0
