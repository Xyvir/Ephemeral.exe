"""
Shared platform plumbing for the unified Ephemeral tray front end.

Everything here is tier-agnostic: tray icon, clipboard, language prompt,
artifact routing, the clipboard->seed converter hotkey, login autostart,
and the headless ``DummyIcon``. Backends (``ephemeral_ui.backends.*``)
and the front end (``ephemeral_ui.tray``) import from this module, so the
local and distributed clients behave identically where behavior should be
identical.

``CLI_MODE`` is a module-level flag flipped by the front end before a
headless/CLI run; artifact routing and result output consult it.
"""
from __future__ import annotations

import base64
import ctypes
import os
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
import threading
import time

# GUI deps are optional (CLI/self-check work without them).
try:
    import pyperclip
    import keyboard
    import pystray
    from pystray import MenuItem as item
    from PIL import Image, ImageDraw, ImageGrab
    HAS_GUI = True
except Exception:
    # pystray's Xorg backend raises Xlib.error.DisplayNameError (not
    # ImportError) when no display is available — treat any GUI import
    # failure as headless so the app can fall back to CLI mode.
    HAS_GUI = False

# Windows-only — kept out of the GUI import chain so Linux builds stay
# GUI-capable (winreg does not exist on Linux).
try:
    import winreg
    HAS_WINREG = True
except ImportError:
    HAS_WINREG = False

CLI_MODE = False


# --- Tray Icon ----------------------------------------------------------

def create_icon_image(color=(0, 120, 215)):
    image = Image.new('RGB', (64, 64), (30, 30, 30))
    dc = ImageDraw.Draw(image)
    dc.rectangle((16, 16, 48, 48), fill=(255, 255, 255))
    dc.rectangle((20, 20, 44, 28), fill=color)
    return image


def set_icon_animation_state(icon, state):
    if not HAS_GUI or not icon:
        return
    if state:
        icon.icon = create_icon_image((255, 100, 0))  # Solid Orange
    else:
        icon.icon = create_icon_image((0, 120, 215))   # Solid Blue


class DummyIcon:
    """Headless stand-in for pystray icons (CLI / self-check runs)."""

    def notify(self, msg, title=""):
        print(f"[{title}] {msg}")

    def stop(self):
        pass


# --- Clipboard ----------------------------------------------------------

def get_clipboard():
    return pyperclip.paste()


def copy_image_to_clipboard(image_path):
    """Copy an image file to the platform clipboard (Win32 on Windows, xclip/wl-copy on Linux)."""
    if sys.platform == 'win32':
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
    return copy_image_to_clipboard_linux(image_path)


def copy_image_to_clipboard_linux(image_path):
    """Copy an image to the Wayland/X11 clipboard via wl-copy or xclip (as PNG)."""
    try:
        fd, tmp_path = tempfile.mkstemp(suffix='.png')
        os.close(fd)
        try:
            with Image.open(image_path) as img:
                img.convert("RGB").save(tmp_path, "PNG")
            if shutil.which("wl-copy"):
                subprocess.run(["wl-copy", "--type", "image/png", tmp_path], check=True)
                return True
            if shutil.which("xclip"):
                subprocess.run(
                    ["xclip", "-selection", "clipboard", "-t", "image/png", "-i", tmp_path],
                    check=True,
                )
                return True
        finally:
            try:
                os.unlink(tmp_path)
            except OSError:
                pass
    except Exception as e:
        print(f"Image copy failed: {e}")
    return False


# --- Windows-Specific Helpers -------------------------------------------

def get_startupinfo():
    if hasattr(subprocess, 'STARTUPINFO'):
        si = subprocess.STARTUPINFO()
        si.dwFlags |= subprocess.STARTF_USESHOWWINDOW
        return si
    return None


def prompt_user_for_language(default_lang, code_preview=""):
    """Ask the user to specify a runtime language (cmd.exe on Windows, a dialog on Linux)."""
    if sys.platform != 'win32':
        return _prompt_user_for_language_linux(default_lang, code_preview)
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


def _prompt_user_for_language_linux(default_lang, code_preview=""):
    """Linux language prompt: zenity -> kdialog -> tkinter -> stdin."""
    try:
        if shutil.which("zenity"):
            out = subprocess.run(
                ["zenity", "--entry", "--title=Ephemeral",
                 "--text=No language detected. Enter a runtime language:",
                 f"--entry-text={default_lang}"],
                capture_output=True, text=True, timeout=60,
            )
            if out.returncode == 0 and out.stdout.strip():
                return out.stdout.strip()
        if shutil.which("kdialog"):
            out = subprocess.run(
                ["kdialog", "--inputbox",
                 "No language detected. Enter a runtime language:", default_lang],
                capture_output=True, text=True, timeout=60,
            )
            if out.returncode == 0 and out.stdout.strip():
                return out.stdout.strip()
        try:
            import tkinter as _tk
            from tkinter import simpledialog
            root = _tk.Tk()
            root.withdraw()
            val = simpledialog.askstring(
                "Ephemeral", "No language detected. Enter a runtime language:",
                initialvalue=default_lang,
            )
            root.destroy()
            if val and val.strip():
                return val.strip()
        except Exception:
            pass
        val = input(f"Enter Language [Default: {default_lang}]: ").strip()
        return val or default_lang
    except Exception as e:
        print(f"Input error: {e}")
        return None


def _ps_quote(s: str) -> str:
    """Single-quote a string for PowerShell (escape embedded quotes)."""
    return s.replace("'", "''")


def open_terminal_emulator(script_path: str) -> bool:
    """Launch a visible terminal emulator running ``script_path`` (Linux).

    Returns True when a terminal was launched, False when none is
    available (callers fall back to printing).
    """
    bash = shutil.which("bash") or "/bin/bash"
    for term, flag in [
        ("gnome-terminal", "--"),
        ("konsole", "--hold -e"),
        ("xfce4-terminal", "--hold -e"),
        ("xterm", "-hold -e"),
    ]:
        if shutil.which(term):
            cmd = [term]
            if flag == "--":
                cmd += ["--", bash, script_path]
            else:
                # e.g. ["xterm", "-hold", "-e", "bash", script]
                cmd += flag.split() + [bash, script_path]
            subprocess.Popen(cmd)
            return True
    return False


def show_terminal_window(title: str, text: str, header: str | None = None) -> None:
    """Open a detached, non-blocking terminal window showing ``text``.

    The single entry point for feedback that needs longer review — errors,
    About/status, long-running output — using the same log-backed pattern as
    pre-hydration (minus the worker): the text is written to a temp log and a
    terminal window displays it, so the tray never blocks and the window stays
    open until the user dismisses it. Interactive prompts (language / seed /
    pre-hydration confirmation) intentionally stay on their own blocking
    consoles, since they need a reply.

    Windows: a PowerShell window reads the log and waits for Enter.
    Linux: a terminal emulator cats the log; falls back to printing.
    """
    if header:
        text = f"{header}\n\n{text}"
    try:
        if sys.platform != "win32":
            _show_terminal_linux(title, text)
            return
        token = f"ephmsg{time.time_ns()}{os.getpid()}"
        log = os.path.join(tempfile.gettempdir(), token + ".log")
        ps = os.path.join(tempfile.gettempdir(), token + ".ps1")
        with open(log, "w", encoding="utf-8") as f:
            f.write(text)
        with open(ps, "w", encoding="utf-8", newline="") as f:
            f.write("$Host.UI.RawUI.WindowTitle = '{title}'\n".format(title=_ps_quote(title)))
            f.write("Get-Content -LiteralPath '{log}'\n".format(log=_ps_quote(log)))
            f.write("Write-Host ''\n")
            f.write("Write-Host '[Ephemeral] Close this window to dismiss.'\n")
            f.write("Read-Host | Out-Null\n")
            f.write("Remove-Item -LiteralPath '{ps}' -Force -ErrorAction SilentlyContinue\n".format(ps=_ps_quote(ps)))
            f.write("Remove-Item -LiteralPath '{log}' -Force -ErrorAction SilentlyContinue\n".format(log=_ps_quote(log)))
        subprocess.Popen(
            ["powershell", "-NoProfile", "-ExecutionPolicy", "Bypass", "-File", ps],
            creationflags=getattr(subprocess, "CREATE_NEW_CONSOLE", 0),
        )
    except Exception as e:
        print(f"Failed to show terminal window: {e}")


def _show_terminal_linux(title: str, text: str) -> None:
    """Linux: terminal emulator running a temp script, else print."""
    try:
        fd, txt = tempfile.mkstemp(suffix=".txt")
        os.close(fd)
        with open(txt, "w", encoding="utf-8") as f:
            f.write(text)
        script = tempfile.NamedTemporaryFile(
            suffix=".sh", prefix="ephemeral_term_",
            mode="w", delete=False, encoding="utf-8", newline="",
        )
        script.write("#!/usr/bin/env bash\n")
        script.write('echo "========================================================="\n')
        script.write(f'echo "  {title}"\n')
        script.write('echo "========================================================="\n')
        script.write(f'cat "{txt}"\n')
        script.write('echo ""\n')
        script.write('echo "[Ephemeral] Press Enter to close."\n')
        script.write('read -p ""\n')
        script.close()
        if open_terminal_emulator(script.name):
            return
    except Exception as e:
        print(f"Failed to show terminal window: {e}")
    print(f"--- {title} ---\n{text}")


# --- Artifact Routing ---------------------------------------------------

def _save_artifact_to_downloads(downloads_dir, safe_lang, filepath, icon):
    """Move an artifact into Downloads with an Ephemeral_ prefix (deduped)."""
    target_name = f"Ephemeral_{safe_lang}_{os.path.basename(filepath)}"
    target_path = os.path.join(downloads_dir, target_name)
    base_name, ext = os.path.splitext(target_path)
    counter = 1
    while os.path.exists(target_path):
        target_path = f"{base_name}_{counter}{ext}"
        counter += 1
    shutil.move(filepath, target_path)
    icon.notify(f"File saved to Downloads:\n{os.path.basename(target_path)}", title="Ephemeral")


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
            elif sys.platform == 'win32':
                icon.notify("Failed to copy image. Check debug.", title="Ephemeral Error")
            else:
                # Linux without xclip/wl-copy: save like a regular file.
                _save_artifact_to_downloads(downloads_dir, safe_lang, filepath, icon)
        else:
            _save_artifact_to_downloads(downloads_dir, safe_lang, filepath, icon)

    elif len(files) > 1:
        timestamp = int(time.time())
        zip_base_name = f"Ephemeral_{safe_lang}_Artifacts_{timestamp}"
        zip_base_path = os.path.join(downloads_dir, zip_base_name)
        final_zip = shutil.make_archive(zip_base_path, 'zip', result.artifact_dir)
        icon.notify(f"Artifacts zipped to Downloads:\n{os.path.basename(final_zip)}", title="Ephemeral")

    return image_copied


# --- Convert-Hotkey (clipboard -> seed block) ---------------------------

def on_convert_hotkey(icon):
    """Convert whatever is on the clipboard into an Ephemeral seed block."""
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
            text = get_clipboard()
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


# --- Startup / Autostart (Windows Registry / Linux .desktop) ------------

class StartupManager:
    """Login-autostart for one app identity (``app_key``).

    Windows writes/removes a ``HKCU\\...\\Run`` value named ``app_key`` and
    keeps a permanent copy of the exe under ``%LOCALAPPDATA%\\<app_key>``;
    Linux manages ``~/.config/autostart/<key>.desktop``.
    """

    def __init__(self, app_key: str, display_name: str,
                 entry_path: str | None = None):
        self.app_key = app_key
        self.display_name = display_name
        # The real app file to copy for autostart. In the old per-client
        # files this was ``os.path.abspath(__file__)`` of the entry point;
        # backends pass their own module path so the right exe/script is
        # installed (not this shared module).
        self.entry_path = entry_path or os.path.abspath(sys.argv[0])

    def get_install_path(self):
        app_data = os.getenv('LOCALAPPDATA', os.path.expanduser('~'))
        install_dir = os.path.join(app_data, self.app_key)
        is_frozen = getattr(sys, 'frozen', False)
        ext = '.exe' if is_frozen else '.py'
        return os.path.join(install_dir, f'{self.app_key}{ext}')

    def _autostart_desktop_path(self):
        key = self.app_key.lower().replace(' ', '-')
        return os.path.join(os.path.expanduser("~"), ".config", "autostart", f"{key}.desktop")

    def set_startup(self, enable, icon=None):
        if sys.platform != 'win32':
            return self._set_startup_linux(enable, icon)
        app_path = sys.executable if getattr(sys, 'frozen', False) else self.entry_path
        install_path = self.get_install_path()

        try:
            key = winreg.OpenKey(winreg.HKEY_CURRENT_USER,
                                 r"Software\Microsoft\Windows\CurrentVersion\Run", 0, winreg.KEY_ALL_ACCESS)
            if enable:
                if os.path.abspath(app_path) != os.path.abspath(install_path):
                    os.makedirs(os.path.dirname(install_path), exist_ok=True)
                    shutil.copy2(app_path, install_path)

                winreg.SetValueEx(key, self.app_key, 0, winreg.REG_SZ, f'"{install_path}"')
                if icon:
                    icon.notify(f"Installed to and set to run on boot from:\n{install_path}", title="Ephemeral Setup")
            else:
                try:
                    winreg.DeleteValue(key, self.app_key)
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

    def _set_startup_linux(self, enable, icon=None):
        """Enable/disable login autostart via a freedesktop .desktop entry."""
        path = self._autostart_desktop_path()
        try:
            if enable:
                os.makedirs(os.path.dirname(path), exist_ok=True)
                exe = os.path.realpath(
                    sys.executable if getattr(sys, 'frozen', False) else self.entry_path
                )
                with open(path, "w", encoding="utf-8") as f:
                    f.write("[Desktop Entry]\n")
                    f.write("Type=Application\n")
                    f.write(f"Name={self.display_name}\n")
                    f.write(f"Exec={shlex.quote(exe)}\n")
                    f.write("X-GNOME-Autostart-enabled=true\n")
                if icon:
                    icon.notify(f"Set to run on login: {path}", title="Ephemeral Setup")
            else:
                if os.path.exists(path):
                    os.remove(path)
                if icon:
                    icon.notify("Disabled start on login.", title="Ephemeral Setup")
        except Exception as e:
            print(f"Failed to set startup: {e}")
            if icon:
                icon.notify(f"Failed to configure startup: {e}", title="Ephemeral Error")

    def check_startup(self):
        if sys.platform != 'win32':
            return os.path.exists(self._autostart_desktop_path())
        try:
            key = winreg.OpenKey(winreg.HKEY_CURRENT_USER,
                                 r"Software\Microsoft\Windows\CurrentVersion\Run", 0, winreg.KEY_READ)
            winreg.QueryValueEx(key, self.app_key)
            winreg.CloseKey(key)
            return True
        except FileNotFoundError:
            return False
