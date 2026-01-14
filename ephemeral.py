import pystray
from pystray import MenuItem as item
from PIL import Image, ImageDraw
import pyperclip
import subprocess
import threading
import keyboard
import sys
import re
import os
import tempfile
import time

# --- Configuration ---
HOTKEY = 'ctrl+alt+x'
APP_NAME = "Ephemeral"

# Map languages to the 'Clean Slate' Image on Docker Hub
LANG_MAP = {
    'python': {'image': 'python:3.10-slim', 'cmd': ['python', '-']},
    'node':   {'image': 'node:18-alpine',   'cmd': ['node', '-']},
    'bash':   {'image': 'alpine:latest',    'cmd': ['sh']},
    'ruby':   {'image': 'ruby:alpine',      'cmd': ['ruby']},
    
    # --- Science & Engineering ---
    'science': {'image': 'continuumio/anaconda3', 'cmd': ['python', '-']},
    'octave':  {'image': 'gnuoctave/octave:latest', 'cmd': ['octave', '--no-gui', '--quiet', '--eval', '-']},

    # --- Windows-like Shells (via Linux) ---
    'pwsh':    {'image': 'mcr.microsoft.com/powershell', 'cmd': ['pwsh', '-Command', '-']},
    
    # Aliases
    'py': 'python', 'js': 'node', 'sh': 'bash',
    'numpy': 'science', 'pandas': 'science',
    'matlab': 'octave',
    'powershell': 'pwsh', 'ps1': 'pwsh',
    'cmd': 'pwsh', 'batch': 'pwsh' 
}

def create_icon_image():
    image = Image.new('RGB', (64, 64), (30, 30, 30))
    dc = ImageDraw.Draw(image)
    dc.rectangle((16, 16, 48, 48), fill=(255, 255, 255)) 
    dc.rectangle((20, 20, 44, 28), fill=(0, 120, 215))   
    return image

def get_clipboard():
    return pyperclip.paste()

def parse_codeblock(content):
    if not content or not content.strip():
        return None, None

    # Strategy 1: Markdown
    pattern = r"```(\w+)?\s*\n(.*?)```"
    match = re.search(pattern, content, re.DOTALL)
    if match:
        lang = match.group(1).lower() if match.group(1) else 'auto'
        return lang, match.group(2)

    # Strategy 2: Shebang
    first_line = content.strip().splitlines()[0]
    if first_line.startswith("#!"):
        lower_line = first_line.lower()
        if 'python' in lower_line: return 'python', content
        if 'node' in lower_line:   return 'node', content
        if 'ruby' in lower_line:   return 'ruby', content
        if 'bash' in lower_line or 'sh' in lower_line: return 'bash', content
        if 'pwsh' in lower_line or 'powershell' in lower_line: return 'pwsh', content
        
    return None, None

# --- Podman Lifecycle Management ---

def check_podman_alive():
    """Returns True if 'podman info' succeeds (daemon is responding)."""
    try:
        startupinfo = subprocess.STARTUPINFO()
        startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
        subprocess.check_call(
            ['podman', 'info'], 
            stdout=subprocess.DEVNULL, 
            stderr=subprocess.DEVNULL, 
            startupinfo=startupinfo
        )
        return True
    except:
        return False

def ensure_podman_running(icon):
    """Checks status and runs 'machine start' or 'machine init' if needed."""
    if check_podman_alive():
        return # Already running

    icon.notify("Podman is not running. Attempting to start...", title="Ephemeral Init")
    
    startupinfo = subprocess.STARTUPINFO()
    startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
    
    try:
        # Try starting
        subprocess.check_call(
            ['podman', 'machine', 'start'], 
            startupinfo=startupinfo,
            stdout=subprocess.DEVNULL, # Keep it clean
            stderr=subprocess.DEVNULL 
        )
        icon.notify("Podman machine started successfully.", title="Ephemeral Init")
    except subprocess.CalledProcessError:
        # Start failed. Maybe it needs initialization?
        icon.notify("Start failed. Initializing new machine...", title="Ephemeral Init")
        try:
            # Init (this downloads WSL distro, might take a while)
            subprocess.check_call(['podman', 'machine', 'init'], startupinfo=startupinfo)
            # Try starting again
            subprocess.check_call(['podman', 'machine', 'start'], startupinfo=startupinfo)
            icon.notify("Podman machine initialized and started.", title="Ephemeral Init")
        except Exception as e:
            icon.notify(f"Could not start Podman: {e}", title="Ephemeral Fatal Error")

def stop_podman_machine(icon):
    """Stops the podman machine to save resources."""
    icon.notify("Stopping Podman machine...", title="Ephemeral Shutdown")
    startupinfo = subprocess.STARTUPINFO()
    startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
    try:
        subprocess.run(['podman', 'machine', 'stop'], startupinfo=startupinfo)
    except Exception as e:
        print(f"Error stopping podman: {e}")

# --- Execution Logic ---

def run_logic(icon):
    content = get_clipboard()
    lang, code = parse_codeblock(content)

    if not code:
        icon.notify("No valid codeblock or shebang found.", title="Ephemeral Error")
        return

    config = None
    if lang in LANG_MAP:
        val = LANG_MAP[lang]
        if isinstance(val, str):
            if val in LANG_MAP:
                config = LANG_MAP[val]
        else:
            config = val
    
    if not config:
        icon.notify(f"Language '{lang}' is not supported.", title="Ephemeral Error")
        return

    icon.notify(f"Launching {lang}...", title="Ephemeral Status")

    # Files for IPC
    fd_code, path_code = tempfile.mkstemp(suffix='.src')
    fd_out, path_out = tempfile.mkstemp(suffix='.res')
    os.close(fd_code)
    os.close(fd_out)

    try:
        # 1. Write Clipboard to Temp File
        with open(path_code, 'w', encoding='utf-8') as f:
            f.write(code)

        # 2. Construct the Native Command
        podman_base = [
            'podman', 'run', 
            '--rm', '-i', 
            '--network', 'none', 
            '--memory', '128m',
            config['image']
        ] + config['cmd']
        
        podman_str = " ".join(f'"{arg}"' if " " in arg else arg for arg in podman_base)
        cmd_line = f'cmd /C "(type "{path_code}" | {podman_str} > "{path_out}") || pause"'

        # 3. Launch Visible Console
        process = subprocess.Popen(cmd_line, creationflags=subprocess.CREATE_NEW_CONSOLE)
        process.wait()

        # 4. Read Result
        if os.path.exists(path_out):
            with open(path_out, 'r', encoding='utf-8', errors='replace') as f:
                result = f.read()
            
            if result.strip():
                pyperclip.copy(f"Result ({lang}):\n---\n{result}")
                icon.notify("Success", title="Ephemeral")

    except Exception as e:
        icon.notify(f"System Error: {str(e)}", title="Ephemeral Failed")
    
    finally:
        if os.path.exists(path_code): os.remove(path_code)
        if os.path.exists(path_out): os.remove(path_out)

def on_hotkey(icon):
    threading.Thread(target=run_logic, args=(icon,)).start()

def setup(icon):
    icon.visible = True
    
    # Run initialization in a background thread so UI doesn't freeze
    def init_sequence():
        ensure_podman_running(icon)
        
    threading.Thread(target=init_sequence).start()
    
    keyboard.add_hotkey(HOTKEY, lambda: on_hotkey(icon))

def quit_app(icon, item):
    # Run stop logic on the main thread (blocking exit is fine here)
    stop_podman_machine(icon)
    icon.stop()
    sys.exit()

if __name__ == '__main__':
    image = create_icon_image()
    menu = (
        item('Run Clipboard', lambda icon, item: on_hotkey(icon), default=True),
        item('Quit', quit_app)
    )
    icon = pystray.Icon("Ephemeral", image, "Ephemeral", menu)
    icon.run(setup)
