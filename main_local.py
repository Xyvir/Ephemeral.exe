"""
Ephemeral Local Client — Windows tray application for clipboard-driven code execution.

This module provides the Windows-native front-end that monitors the clipboard,
invokes ephemeral_core for sandboxed Podman execution, and routes results
back to the clipboard and artifacts to the Downloads folder.

All platform-specific code (pystray, pyperclip, keyboard, winreg, ctypes)
lives exclusively in this module.

Usage:
    python main_local.py                    # Tray mode (persistent, hotkey-driven)
    python main_local.py script.md          # One-shot mode (run file and exit)
    python main_local.py --cli script.md    # Headless CLI mode (no GUI)
"""
from __future__ import annotations

import asyncio
import base64
import ctypes
import os
import re
import shutil
import subprocess
import sys
import tempfile
import threading
import time

try:
    import pystray
    from pystray import MenuItem as item
    from PIL import Image, ImageDraw, ImageGrab
    import pyperclip
    import keyboard
    import winreg
    HAS_GUI = True
except ImportError:
    HAS_GUI = False

import ephemeral_core
from ephemeral_core.parser import strip_shebang, resolve_runtime_config
from ephemeral_core.config import LANG_MAP

# --- Configuration ---
CLI_MODE = False
HOTKEY = 'ctrl+alt+x'
CONVERT_HOTKEY = 'ctrl+win+x'
LAST_DETECTED_LANG = "python"

# --- Globals and State ---
active_processes = []
last_activity_time = time.time()


# --- Tray Icon ---

def create_icon_image(color=(0, 120, 215)):
    image = Image.new('RGB', (64, 64), (30, 30, 30))
    dc = ImageDraw.Draw(image)
    dc.rectangle((16, 16, 48, 48), fill=(255, 255, 255))
    dc.rectangle((20, 20, 44, 28), fill=color)
    return image


def set_icon_animation_state(icon, state):
    if not HAS_GUI or not icon: return
    if state:
        icon.icon = create_icon_image((255, 100, 0))  # Solid Orange
    else:
        icon.icon = create_icon_image((0, 120, 215))   # Solid Blue


# --- Clipboard ---

def get_clipboard():
    return pyperclip.paste()


def copy_image_to_clipboard(image_path):
    """Copy an image file to the Windows clipboard via Win32 API."""
    try:
        from io import BytesIO
        img = Image.open(image_path)
        output = BytesIO()
        img.convert("RGB").save(output, "BMP")
        data = output.getvalue()[14:]
        output.close()
        user32 = ctypes.windll.user32
        kernel32 = ctypes.windll.kernel32
        OpenClipboard = user32.OpenClipboard
        EmptyClipboard = user32.EmptyClipboard
        SetClipboardData = user32.SetClipboardData
        CloseClipboard = user32.CloseClipboard
        GlobalAlloc = kernel32.GlobalAlloc
        GlobalLock = kernel32.GlobalLock
        GlobalUnlock = kernel32.GlobalUnlock
        GMEM_MOVEABLE = 0x0002
        CF_DIB = 8
        OpenClipboard(0)
        EmptyClipboard()
        hCd = GlobalAlloc(GMEM_MOVEABLE, len(data))
        pchData = GlobalLock(hCd)
        ctypes.memmove(pchData, data, len(data))
        GlobalUnlock(hCd)
        SetClipboardData(CF_DIB, hCd)
        CloseClipboard()
        return True
    except Exception as e:
        print(f"Image copy failed: {e}")
        return False


# --- Windows-Specific Helpers ---

def get_startupinfo():
    if hasattr(subprocess, 'STARTUPINFO'):
        si = subprocess.STARTUPINFO()
        si.dwFlags |= subprocess.STARTF_USESHOWWINDOW
        return si
    return None


def prompt_user_for_language(default_lang, code_preview=""):
    """Show a cmd.exe prompt asking the user to specify a runtime language."""
    fd_out, path_out = tempfile.mkstemp(suffix='.txt')
    os.close(fd_out)
    fd_bat, path_bat = tempfile.mkstemp(suffix='.bat')
    os.close(fd_bat)
    fd_ctx, path_ctx = tempfile.mkstemp(suffix='.ctx')
    os.close(fd_ctx)
    detected_lang = None
    try:
        if code_preview:
            lines = code_preview.strip().splitlines()
            last_lines = lines[-5:] if len(lines) > 5 else lines
            with open(path_ctx, 'w', encoding='utf-8') as f:
                f.write('\n'.join(last_lines))
        with open(path_bat, 'w') as f:
            f.write('@echo off\n')
            f.write('title Ephemeral: No language specified\n')
            f.write('cls\n')
            f.write('echo.\n')
            f.write('echo  --------------------------------------------------\n')
            f.write('echo   No language detected in clipboard.\n')
            f.write('echo  --------------------------------------------------\n')
            if code_preview:
                f.write('echo   Context (Last 5 lines of clipboard):\n')
                f.write('echo   ------------------------------------\n')
                f.write(f'type "{path_ctx}"\n')
                f.write('echo.\n')
                f.write('echo   ------------------------------------\n')
            f.write('echo.\n')
            f.write(f'set /p "lang= Enter Language [Default: {default_lang}]: "\n')
            f.write(f'if "%lang%"=="" set lang={default_lang}\n')
            f.write(f'echo %lang%> "{path_out}"\n')
        subprocess.run(path_bat, creationflags=getattr(subprocess, 'CREATE_NEW_CONSOLE', 0))
        if os.path.exists(path_out):
            with open(path_out, 'r') as f:
                val = f.read().strip()
                if val: detected_lang = val
    except Exception as e:
        print(f"Input error: {e}")
        return None
    finally:
        if os.path.exists(path_out): os.remove(path_out)
        if os.path.exists(path_bat): os.remove(path_bat)
        if os.path.exists(path_ctx): os.remove(path_ctx)
    return detected_lang


def show_post_mortem_error(error_text):
    """Open a persistent cmd.exe window showing an error message."""
    try:
        if sys.platform != 'win32':
            print("--- EPHEMERAL EXECUTION ERROR ---\n" + error_text, file=sys.stderr)
            return
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as tmp:
            tmp.write("--- EPHEMERAL EXECUTION ERROR ---\n\n")
            tmp.write(error_text)
            tmp_path = tmp.name
        subprocess.Popen(f'start cmd /K "type "{tmp_path}" && echo. && echo. && echo [Ephemeral Debug] Window persisted due to error. Close to dismiss."', shell=True)
    except Exception as e:
        print(f"Failed to show error window: {e}")


def perform_visible_pull(image_name):
    """Pull a container image with a visible console window on Windows."""
    if sys.platform == 'win32':
        cmd_line = f'cmd /C "echo [Ephemeral] Image {image_name} not found. Downloading... && podman pull {image_name} || pause"'
        process = subprocess.Popen(cmd_line, creationflags=getattr(subprocess, 'CREATE_NEW_CONSOLE', 0))
    else:
        cmd_line = ['podman', 'pull', image_name]
        process = subprocess.Popen(cmd_line)
    return process.wait()


# --- Podman Lifecycle (Local-specific wrappers with icon notifications) ---

def ensure_podman_running_local(icon):
    """Ensure Podman is running, with tray notifications for status."""
    if ephemeral_core.check_podman_alive():
        return
    icon.notify("Podman is not running. Attempting to start...", title="Ephemeral Init")
    startupinfo = get_startupinfo()
    try:
        subprocess.check_call(['podman', 'machine', 'start'], startupinfo=startupinfo,
                              stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        icon.notify("Podman machine started successfully.", title="Ephemeral Init")
    except subprocess.CalledProcessError:
        icon.notify("Start failed. Initializing new machine...", title="Ephemeral Init")
        try:
            subprocess.check_call(['podman', 'machine', 'init'], startupinfo=startupinfo)
            subprocess.check_call(['podman', 'machine', 'start'], startupinfo=startupinfo)
            icon.notify("Podman machine initialized and started.", title="Ephemeral Init")
        except Exception as e:
            icon.notify(f"Could not start Podman: {e}", title="Ephemeral Fatal Error")


def stop_podman_machine(icon):
    icon.notify("Stopping Podman machine...", title="Ephemeral Shutdown")
    startupinfo = get_startupinfo()
    try:
        subprocess.run(['podman', 'machine', 'stop'], startupinfo=startupinfo)
    except Exception as e:
        print(f"Error stopping podman: {e}")


def purge_cache(icon, item_unused):
    icon.notify("Pruning unused images... this may take a moment.", title="Ephemeral Maintenance")
    startupinfo = get_startupinfo()
    try:
        subprocess.run(['podman', 'image', 'prune', '--all', '--force'],
                       startupinfo=startupinfo, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        icon.notify("Image cache cleared successfully.", title="Ephemeral")
    except Exception as e:
        icon.notify(f"Error clearing cache: {e}", title="Ephemeral Error")


def force_stop_all(icon, item_unused):
    global active_processes
    killed_count = 0
    for p in list(active_processes):
        try:
            p.kill()
            killed_count += 1
        except:
            pass
    active_processes.clear()

    try:
        startupinfo = get_startupinfo()
        subprocess.run(['podman', 'rm', '-f', '$(podman ps -q)'], shell=True,
                       startupinfo=startupinfo, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    except:
        pass

    set_icon_animation_state(icon, False)
    if killed_count > 0:
        icon.notify(f"Forcefully stopped {killed_count} active runs.", title="Ephemeral Stopped")
    else:
        icon.notify("No active runs to stop.", title="Ephemeral")


# --- Local Artifact Routing ---

def route_artifacts_local(result, lang, icon):
    """
    Route artifacts from an ExecutionResult to the user's Downloads folder.
    
    Preserves the original behavior:
    - Single image file → copy to clipboard
    - Single non-image file → move to Downloads with Ephemeral_ prefix
    - Multiple files → zip to Downloads
    
    Returns True if an image was copied to clipboard.
    """
    if not result.artifact_paths or not result.artifact_dir:
        return False

    files = [os.path.basename(p) for p in result.artifact_paths if os.path.isfile(p)]
    if not files:
        return False

    if CLI_MODE:
        downloads_dir = os.path.abspath(os.getcwd())
    else:
        downloads_dir = os.path.join(os.path.expanduser("~"), "Downloads")

    safe_lang = re.sub(r'[^a-zA-Z0-9]', '_', lang) if lang else "custom"
    image_copied = False

    if len(files) == 1:
        filename = files[0]
        filepath = os.path.join(result.artifact_dir, filename)

        if filename.lower().endswith(('.png', '.jpg', '.jpeg', '.bmp')):
            if copy_image_to_clipboard(filepath):
                icon.notify("Image generated and copied to clipboard!", title="Ephemeral")
                image_copied = True
            else:
                icon.notify("Failed to copy image. Check debug.", title="Ephemeral Error")
        else:
            target_name = f"Ephemeral_{safe_lang}_{filename}"
            target_path = os.path.join(downloads_dir, target_name)

            base_name, ext = os.path.splitext(target_path)
            counter = 1
            while os.path.exists(target_path):
                target_path = f"{base_name}_{counter}{ext}"
                counter += 1

            shutil.move(filepath, target_path)
            icon.notify(f"File saved to Downloads:\n{os.path.basename(target_path)}", title="Ephemeral")

    elif len(files) > 1:
        timestamp = int(time.time())
        zip_base_name = f"Ephemeral_{safe_lang}_Artifacts_{timestamp}"
        zip_base_path = os.path.join(downloads_dir, zip_base_name)
        final_zip = shutil.make_archive(zip_base_path, 'zip', result.artifact_dir)
        icon.notify(f"Artifacts zipped to Downloads:\n{os.path.basename(final_zip)}", title="Ephemeral")

    return image_copied


# --- Core Execution Bridge ---

def run_logic(icon, content=None):
    """
    Bridge between the local tray UI and ephemeral_core.
    
    Reads from clipboard if no content provided, handles the language prompt
    for untagged blocks, delegates execution to the core, then routes
    artifacts and results back through local channels.
    """
    global LAST_DETECTED_LANG

    if content is None:
        content = get_clipboard()

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
        user_input = prompt_user_for_language(LAST_DETECTED_LANG, code)
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
        LAST_DETECTED_LANG = lang.split()[0] if lang else LAST_DETECTED_LANG
        icon.notify(f"Launching {LAST_DETECTED_LANG}...", title="Ephemeral Status")

    set_icon_animation_state(icon, True)

    # Execute via the core engine (run synchronously in this thread via asyncio.run)
    try:
        # Build the full markdown with any language fixes applied, then delegate
        # We need to reconstruct the text or call the core directly with blocks.
        # Since the user may have modified the header, we use the core's lower-level API.
        from ephemeral_core.executor import (
            run_container_group, check_image_exists
        )

        all_stdout = []
        executed_langs = []
        image_was_copied = False
        chained_files = []

        for i, run in enumerate(runs):
            if chained_files:
                run = chained_files + run

            code_item = next(b for b in run if b['type'] == 'code')
            lang = code_item['header']
            LAST_DETECTED_LANG = lang.split()[0] if lang else LAST_DETECTED_LANG
            config = code_item['config']

            image_name = config['image']
            is_cached = check_image_exists(image_name)
            if not is_cached:
                exit_code = perform_visible_pull(image_name)
                if exit_code != 0:
                    icon.notify("Image download failed.", title="Ephemeral Error")
                    return

            import tempfile as _tempfile
            output_dir = _tempfile.mkdtemp(prefix="ephemeral_")

            # Run synchronously using asyncio.run for the async core function
            group_result = asyncio.run(
                run_container_group(
                    config, run, lang,
                    run_index=i + 1,
                    total_runs=len(runs),
                    output_dir=output_dir,
                    timeout=60
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
                img_copied = route_artifacts_local(mini_result, lang, icon)
                if img_copied:
                    image_was_copied = True
            else:
                # Clean up empty output dir
                try:
                    shutil.rmtree(output_dir, ignore_errors=True)
                except:
                    pass

            if group_result.exit_code != 0:
                show_post_mortem_error(group_result.stderr)
                icon.notify(f"Run {i + 1} Failed. Debug window opened.", title="Ephemeral Error")

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
                if not CLI_MODE:
                    pyperclip.copy(final_result)
                else:
                    print(final_result)
                lang_str = ", ".join(executed_langs) if executed_langs else "Custom"
                icon.notify(f"{lang_str} Execution Finished. Results copied.", title="Ephemeral")

    except Exception as e:
        show_post_mortem_error(f"System Exception:\n{str(e)}")
        icon.notify("Critical System Error", title="Ephemeral Failed")

    finally:
        set_icon_animation_state(icon, False)


# --- Idle Monitor ---

def idle_monitor(icon):
    global last_activity_time
    while True:
        time.sleep(60)
        if time.time() - last_activity_time > 1800:
            if not active_processes and ephemeral_core.check_podman_alive():
                icon.notify("Idling for 30 minutes. Stopping Podman VM.", title="Ephemeral Sleep")
                stop_podman_machine(icon)


# --- Hotkey Handlers ---

def on_hotkey(icon):
    global last_activity_time
    last_activity_time = time.time()
    def hotkey_task():
        ensure_podman_running_local(icon)
        run_logic(icon)
    threading.Thread(target=hotkey_task).start()


def on_convert_hotkey(icon):
    global last_activity_time
    last_activity_time = time.time()
    def hotkey_task():
        try:
            clip_data = ImageGrab.grabclipboard()
        except Exception:
            clip_data = None

        result_text = ""

        if isinstance(clip_data, list) and len(clip_data) > 0:
            file_path = clip_data[0]
            filename = os.path.basename(file_path)
            try:
                with open(file_path, 'r', encoding='utf-8') as f:
                    content = f.read()
                result_text = f"```seed.{filename.split('.')[-1] if '.' in filename else 'txt'} origin={filename}\n{content}\n```"
            except UnicodeDecodeError:
                with open(file_path, 'rb') as f:
                    b64_content = base64.b64encode(f.read()).decode('utf-8')
                result_text = f"```seed.{filename.split('.')[-1] if '.' in filename else 'bin'} origin={filename} b64\n{b64_content}\n```"

        elif hasattr(clip_data, 'save'):
            import io
            buf = io.BytesIO()
            clip_data.save(buf, format='PNG')
            b64_content = base64.b64encode(buf.getvalue()).decode('utf-8')
            result_text = f"```seed.png b64\n{b64_content}\n```"

        else:
            text = pyperclip.paste()
            if text:
                result_text = f"```seed.txt\n{text}\n```"

        if result_text:
            pyperclip.copy(result_text)
            if icon:
                icon.notify("Converted clipboard to Ephemeral format.", title="Ephemeral")
        else:
            if icon:
                icon.notify("Nothing valid in clipboard to convert.", title="Ephemeral Error")

    threading.Thread(target=hotkey_task).start()


# --- Startup/Install (Windows Registry) ---

def get_install_path():
    app_data = os.getenv('LOCALAPPDATA', os.path.expanduser('~'))
    install_dir = os.path.join(app_data, 'Ephemeral')
    is_frozen = getattr(sys, 'frozen', False)
    ext = '.exe' if is_frozen else '.py'
    return os.path.join(install_dir, f'Ephemeral{ext}')


def set_startup(enable, icon=None):
    app_path = sys.executable if getattr(sys, 'frozen', False) else os.path.abspath(__file__)
    install_path = get_install_path()

    try:
        key = winreg.OpenKey(winreg.HKEY_CURRENT_USER,
                             r"Software\Microsoft\Windows\CurrentVersion\Run", 0, winreg.KEY_ALL_ACCESS)
        if enable:
            if os.path.abspath(app_path) != os.path.abspath(install_path):
                os.makedirs(os.path.dirname(install_path), exist_ok=True)
                shutil.copy2(app_path, install_path)

            winreg.SetValueEx(key, "Ephemeral", 0, winreg.REG_SZ, f'"{install_path}"')
            if icon:
                icon.notify(f"Installed to and set to run on boot from:\n{install_path}", title="Ephemeral Setup")
        else:
            try:
                winreg.DeleteValue(key, "Ephemeral")
            except FileNotFoundError:
                pass

            if os.path.exists(install_path):
                if os.path.abspath(app_path) != os.path.abspath(install_path):
                    try:
                        os.remove(install_path)
                        if icon:
                            icon.notify("Removed installed copy and disabled start on boot.", title="Ephemeral Setup")
                    except Exception:
                        MOVEFILE_DELAY_UNTIL_REBOOT = 4
                        ctypes.windll.kernel32.MoveFileExW(install_path, None, MOVEFILE_DELAY_UNTIL_REBOOT)
                        if icon:
                            icon.notify("Disabled start on boot. File will be deleted on next restart.",
                                        title="Ephemeral Setup")
                else:
                    MOVEFILE_DELAY_UNTIL_REBOOT = 4
                    ctypes.windll.kernel32.MoveFileExW(install_path, None, MOVEFILE_DELAY_UNTIL_REBOOT)
                    if icon:
                        icon.notify("Disabled start on boot. It will be deleted on next restart.",
                                    title="Ephemeral Setup")

        winreg.CloseKey(key)
    except Exception as e:
        print(f"Failed to set startup: {e}")
        if icon:
            icon.notify(f"Failed to configure startup: {e}", title="Ephemeral Error")


def check_startup():
    try:
        key = winreg.OpenKey(winreg.HKEY_CURRENT_USER,
                             r"Software\Microsoft\Windows\CurrentVersion\Run", 0, winreg.KEY_READ)
        winreg.QueryValueEx(key, "Ephemeral")
        winreg.CloseKey(key)
        return True
    except FileNotFoundError:
        return False


def toggle_startup(icon, item_unused):
    is_enabled = check_startup()
    set_startup(not is_enabled, icon)


# --- Main Entry Points ---

def setup_tray_mode(icon):
    """Standard Mode: Persistent Tray Icon"""
    icon.visible = True
    threading.Thread(target=idle_monitor, args=(icon,), daemon=True).start()
    keyboard.add_hotkey(HOTKEY, lambda: on_hotkey(icon))
    keyboard.add_hotkey(CONVERT_HOTKEY, lambda: on_convert_hotkey(icon))


def setup_oneshot_mode(icon, file_path):
    """One-Shot Mode: Run file, respect Podman state, then exit."""
    icon.visible = True

    def auto_run_sequence():
        was_podman_running = ephemeral_core.check_podman_alive()

        if not was_podman_running:
            ensure_podman_running_local(icon)

        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()

            pyperclip.copy(content)
            time.sleep(0.5)

            icon.notify(f"Loading {os.path.basename(file_path)}...", title="Ephemeral One-Shot")
            run_logic(icon)

        except Exception as e:
            icon.notify(f"One-Shot Failed: {e}", title="Ephemeral Error")
            time.sleep(5)

        finally:
            if not was_podman_running:
                icon.notify("Cleaning up...", title="Ephemeral Shutdown")
                stop_podman_machine(icon)

            icon.stop()
            sys.exit()

    threading.Thread(target=auto_run_sequence).start()


def show_about(icon, item_unused):
    about_text = ("# Ephemeral.exe\nVersion: Version number (injected from the github workflow)\n"
                  "Dev: Dunko Xyvir\nLicense: MIT License\nURL: https://github.com/Xyvir/Ephemeral.exe")
    pyperclip.copy(about_text)
    icon.notify(about_text, title="About Ephemeral")


def quit_app(icon, item_unused):
    stop_podman_machine(icon)
    icon.stop()
    sys.exit()


def setup_headless_mode(file_path):
    """Headless CLI Mode: Run file completely unattended, no GUI dependencies."""
    class DummyIcon:
        def notify(self, msg, title=""):
            print(f"[{title}] {msg}")
        def stop(self):
            pass

    icon = DummyIcon()

    was_podman_running = ephemeral_core.check_podman_alive()
    if not was_podman_running:
        ensure_podman_running_local(icon)

    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()

        icon.notify(f"Headless Mode: Running {os.path.basename(file_path)}...", title="Ephemeral CLI")
        run_logic(icon, content=content)

    except Exception as e:
        icon.notify(f"Headless Failed: {e}", title="Ephemeral Error")

    finally:
        if not was_podman_running:
            stop_podman_machine(icon)


if __name__ == '__main__':
    # DETECT MODE
    if len(sys.argv) > 1 and os.path.exists(sys.argv[-1]):
        file_target = sys.argv[-1]
        if "--cli" in sys.argv or "parse" in sys.argv:
            CLI_MODE = True
            setup_headless_mode(file_target)
            sys.exit(0)
        else:
            if not HAS_GUI:
                print("GUI dependencies not found. Falling back to CLI mode.")
                CLI_MODE = True
                setup_headless_mode(file_target)
                sys.exit(0)

            image = create_icon_image()
            menu = (
                item('Run Clipboard', lambda icon, item: on_hotkey(icon), default=True),
                item('Install && Run on Boot', toggle_startup, checked=lambda item: check_startup()),
                item('Force Stop All Runs', force_stop_all),
                item('Clear Image Cache', purge_cache),
                item('About', show_about),
                item('Quit', quit_app)
            )
            icon = pystray.Icon("Ephemeral", image, "Ephemeral", menu)
            icon.run(lambda icon: setup_oneshot_mode(icon, file_target))
    else:
        if not HAS_GUI:
            print("GUI dependencies not found. CLI mode requires a file argument.")
            sys.exit(1)

        image = create_icon_image()
        menu = (
            item('Run Clipboard', lambda icon, item: on_hotkey(icon), default=True),
            item('Install && Run on Boot', toggle_startup, checked=lambda item: check_startup()),
            item('Force Stop All Runs', force_stop_all),
            item('Clear Image Cache', purge_cache),
            item('About', show_about),
            item('Quit', quit_app)
        )
        icon = pystray.Icon("Ephemeral", image, "Ephemeral", menu)
        icon.run(setup_tray_mode)
