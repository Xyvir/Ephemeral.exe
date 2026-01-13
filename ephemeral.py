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
    
    # Aliases
    'py': 'python', 'js': 'node', 'sh': 'bash',
    'numpy': 'science', 'pandas': 'science',
    'matlab': 'octave'
}

def create_icon_image():
    # A distinct 'Clean Slate' Icon (White/Blue)
    image = Image.new('RGB', (64, 64), (30, 30, 30))
    dc = ImageDraw.Draw(image)
    dc.rectangle((16, 16, 48, 48), fill=(255, 255, 255)) # White Paper
    dc.rectangle((20, 20, 44, 28), fill=(0, 120, 215))   # Blue Header
    return image

def get_clipboard():
    return pyperclip.paste()

def parse_codeblock(content):
    pattern = r"```(\w+)?\n(.*?)```"
    match = re.search(pattern, content, re.DOTALL)
    if match:
        lang = match.group(1).lower() if match.group(1) else 'auto'
        return lang, match.group(2)
    return None, None

def show_error_window(error_text):
    """
    Writes the error to a temp file and opens a persistent cmd window to view it.
    This satisfies the 'persist on error' requirement.
    """
    try:
        # Write error to a temp file so cmd can read it cleanly
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as tmp:
            tmp.write("--- EPHEMERAL EXECUTION ERROR ---\n\n")
            tmp.write(error_text)
            tmp_path = tmp.name
            
        # Launch a visible CMD window that stays open (/K) and displays the file
        # 'start' is a shell command, so shell=True is needed here
        subprocess.Popen(
            f'start cmd /K "type "{tmp_path}" && echo. && echo. && echo [Ephemeral Debug] Window persisted due to error. Close to dismiss."', 
            shell=True
        )
    except Exception as e:
        print(f"Failed to show error window: {e}")

def run_logic(icon):
    content = get_clipboard()
    lang, code = parse_codeblock(content)

    if not code: return

    # Resolve alias (e.g., py -> python)
    config = None
    if lang in LANG_MAP:
        config = LANG_MAP[lang]
    elif lang in ['py', 'js', 'sh', 'numpy', 'pandas', 'matlab']: 
        if lang == 'py': config = LANG_MAP['python']
        elif lang == 'js': config = LANG_MAP['node']
        elif lang == 'sh': config = LANG_MAP['bash']
        elif lang in ['numpy', 'pandas']: config = LANG_MAP['science']
        elif lang == 'matlab': config = LANG_MAP['octave']
    
    if not config: return

    # --- 1. IMMEDIATE USER FEEDBACK ---
    icon.notify(f"Spinning up container for {lang}...", title="Ephemeral Status")

    try:
        podman_cmd = [
            'podman', 'run', 
            '--rm',              # Auto-delete
            '-i',                # Interactive
            '--network', 'none', # Sandbox
            '--memory', '128m',
            config['image']
        ] + config['cmd']

        # Suppress the main execution window (keep it clean)
        startupinfo = subprocess.STARTUPINFO()
        startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW

        process = subprocess.Popen(
            podman_cmd,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            startupinfo=startupinfo
        )
        
        stdout, stderr = process.communicate(input=code)
        
        if stderr:
            # --- 2. PERSIST ON ERROR ---
            # If there is an error, show the debug window and notify
            show_error_window(stderr)
            icon.notify(f"Execution failed for {lang}. See debug window.", title="Ephemeral Error")
            # Optionally still copy stdout if any? Usually on error we just want the debug info.
        else:
            # --- 3. AUTO-CLOSE ON SUCCESS ---
            # Update clipboard silently and notify completion
            result = stdout
            pyperclip.copy(f"Result ({lang}):\n---\n{result}")
            icon.notify(f"Success! Result copied to clipboard.", title="Ephemeral")

    except Exception as e:
        show_error_window(str(e))
        icon.notify("Critical failure. See debug window.", title="Ephemeral Failed")

def on_hotkey(icon):
    threading.Thread(target=run_logic, args=(icon,)).start()

def setup(icon):
    icon.visible = True
    keyboard.add_hotkey(HOTKEY, lambda: on_hotkey(icon))

def quit_app(icon, item):
    icon.stop()
    sys.exit()

if __name__ == '__main__':
    image = create_icon_image()
    # Updated menu to include manual trigger
    # default=True binds this action to the Left-Click event on the tray icon
    menu = (
        item('Run Clipboard', lambda icon, item: on_hotkey(icon), default=True),
        item('Quit', quit_app)
    )
    icon = pystray.Icon("Ephemeral", image, "Ephemeral", menu)
    icon.run(setup)
