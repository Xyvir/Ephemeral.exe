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
import ctypes # For native Windows Message Box
import shlex  # For parsing quoted strings in headers

# --- Configuration ---
HOTKEY = 'ctrl+alt+x'
APP_NAME = "Ephemeral"
LAST_DETECTED_LANG = "python" # Default starting language, updates dynamically

# Map languages to the 'Clean Slate' Image on Docker Hub
LANG_MAP = {
    # --- Standard Interpreted ---
    'python': {'image': 'python:3.10-slim', 'cmd': ['python', '-']},
    'node':   {'image': 'node:18-alpine',   'cmd': ['node', '-']},
    'bash':   {'image': 'alpine:latest',    'cmd': ['sh']},
    'ruby':   {'image': 'ruby:alpine',      'cmd': ['ruby']},
    
    # --- Science & Data ---
    'science': {'image': 'continuumio/anaconda3', 'cmd': ['python', '-']},
    'octave':  {'image': 'gnuoctave/octave:latest', 'cmd': ['octave', '--no-gui', '--quiet']},
    'r':       {'image': 'r-base:latest',          'cmd': ['R', '--vanilla', '--slave', '-f', '/dev/stdin']},
    'julia':   {'image': 'julia:alpine',           'cmd': ['julia']},

    # --- Systems & Compiled (Compile-and-Run Chains) ---
    'c':       {'image': 'gcc:latest', 'cmd': ['sh', '-c', 'gcc -x c - -o /tmp/run && /tmp/run']},
    'cpp':     {'image': 'gcc:latest', 'cmd': ['sh', '-c', 'g++ -x c++ - -o /tmp/run && /tmp/run']},
    'fortran': {'image': 'gcc:latest', 'cmd': ['sh', '-c', 'gfortran -x f95 - -o /tmp/run && /tmp/run']},
    'rust':    {'image': 'rust:alpine', 'cmd': ['sh', '-c', 'rustc - -o /tmp/run && /tmp/run']},
    'go':      {'image': 'golang:alpine', 'cmd': ['sh', '-c', 'cat > /tmp/main.go && go run /tmp/main.go']},
    
    # --- Expansion Pack (Systems) ---
    'java':    {'image': 'eclipse-temurin:21-jdk-alpine', 'cmd': ['sh', '-c', 'cat > /tmp/Main.java && java /tmp/Main.java']},

    # --- Golfing & Modern Compiled ---
    'crystal': {'image': 'crystallang/crystal:latest', 'cmd': ['sh', '-c', 'cat > /tmp/run.cr && crystal run /tmp/run.cr']},
    'nim':     {'image': 'nimlang/nim:alpine', 'cmd': ['sh', '-c', 'cat > /tmp/run.nim && nim c -r --verbosity:0 --hints:off /tmp/run.nim']},

    # --- Lisp & Functional ---
    'lisp':    {'image': 'clfoundation/sbcl:slim', 'cmd': ['sh', '-c', 'cat > /tmp/run.lisp && sbcl --script /tmp/run.lisp']},
    'clojure': {'image': 'clojure:temurin-17-alpine', 'cmd': ['sh', '-c', 'cat > /tmp/run.clj && clojure -M /tmp/run.clj']},
    'elixir':  {'image': 'elixir:alpine', 'cmd': ['sh', '-c', 'cat > /tmp/run.exs && elixir /tmp/run.exs']},
    'ocaml':   {'image': 'ocaml/opam', 'cmd': ['sh', '-c', 'cat > /tmp/run.ml && ocaml /tmp/run.ml']},

    # --- Logic ---
    'prolog':  {'image': 'swipl:latest', 'cmd': ['swipl', '-q', '-f', '/dev/stdin', '-t', 'halt']},

    # --- Esoteric ---
    'brainfuck': {'image': 'andregeddert/brainfuck', 'cmd': ['sh', '-c', 'cat > /tmp/run.bf && brainfuck /tmp/run.bf']},

    # --- Hardware Description (HDL) ---
    'verilog': {'image': 'hdlc/iverilog', 'cmd': ['sh', '-c', 'cat > /tmp/run.v && iverilog /tmp/run.v -o /tmp/out && vvp /tmp/out']},

    # --- Functional & Scripting ---
    'haskell': {'image': 'haskell:slim', 'cmd': ['runghc']},
    'lua':     {'image': 'nickblah/lua:5.4-alpine', 'cmd': ['lua', '-']},
    'perl':    {'image': 'perl:slim',      'cmd': ['perl', '-']},
    'php':     {'image': 'php:alpine',     'cmd': ['php']},

    # --- Windows-like Shells ---
    'pwsh':    {'image': 'mcr.microsoft.com/powershell', 'cmd': ['pwsh', '-NoProfile', '-NonInteractive', '-Command', '-']},
    
    # --- Aliases ---
    'py': 'python', 'js': 'node', 'sh': 'bash',
    'numpy': 'science', 'pandas': 'science',
    'matlab': 'octave',
    'powershell': 'pwsh', 'ps1': 'pwsh', 'cmd': 'pwsh', 'batch': 'pwsh',
    'R': 'r',
    'golang': 'go', 'cc': 'c', 'c++': 'cpp',
    'f90': 'fortran', 'f95': 'fortran',
    'sbcl': 'lisp', 'cl': 'lisp', 'common-lisp': 'lisp',
    'clj': 'clojure', 'ex': 'elixir', 'exs': 'elixir',
    'ml': 'ocaml',
    'swipl': 'prolog', 'pl': 'prolog',
    'cr': 'crystal', 'nimrod': 'nim',
    'bf': 'brainfuck'
}

def create_icon_image():
    image = Image.new('RGB', (64, 64), (30, 30, 30))
    dc = ImageDraw.Draw(image)
    dc.rectangle((16, 16, 48, 48), fill=(255, 255, 255)) 
    dc.rectangle((20, 20, 44, 28), fill=(0, 120, 215))   
    return image

def get_clipboard():
    return pyperclip.paste()

def strip_ansi_codes(text):
    ansi_escape = re.compile(r'\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])')
    return ansi_escape.sub('', text)

def strip_shebang(text):
    if not text: return text
    if text.lstrip().startswith("#!"):
        parts = text.split('\n', 1)
        if len(parts) > 1:
            return parts[1]
        return ""
    return text

def parse_codeblock(content):
    if not content or not content.strip():
        return None, None

    # Strategy 1: Markdown
    # Updated regex to capture the entire header line (everything after ``` until \n)
    pattern = r"```(.*?)\n(.*?)```"
    match = re.search(pattern, content, re.DOTALL)
    if match:
        header = match.group(1).strip() if match.group(1) else None
        # Ensure we strip shebangs even if they are inside markdown blocks
        return header, strip_shebang(match.group(2))

    # Strategy 2: Shebang
    first_line = content.strip().splitlines()[0]
    if first_line.startswith("#!"):
        lower_line = first_line.lower()
        # Sort keys by length descending to prevent substring collisions
        sorted_keys = sorted(LANG_MAP.keys(), key=len, reverse=True)
        for key in sorted_keys:
            if key in lower_line:
                return key, strip_shebang(content)
    
    return None, None

def prompt_user_for_language(default_lang):
    fd_out, path_out = tempfile.mkstemp(suffix='.txt')
    os.close(fd_out)
    fd_bat, path_bat = tempfile.mkstemp(suffix='.bat')
    os.close(fd_bat)
    
    detected_lang = None
    try:
        with open(path_bat, 'w') as f:
            f.write('@echo off\n')
            f.write('title Ephemeral: No language specified\n')
            f.write('cls\n')
            f.write('echo.\n')
            f.write('echo  --------------------------------------------------\n')
            f.write('echo   No language detected in clipboard.\n')
            f.write('echo  --------------------------------------------------\n')
            f.write('echo.\n')
            f.write(f'set /p "lang= Enter Language [Default: {default_lang}]: "\n')
            f.write(f'if "%lang%"=="" set lang={default_lang}\n')
            f.write(f'echo %lang%> "{path_out}"\n')
        
        subprocess.run(path_bat, creationflags=subprocess.CREATE_NEW_CONSOLE)
        
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
    return detected_lang

def resolve_runtime_config(header_line):
    """
    Resolves the header string to a docker config.
    Handles aliases, version overrides, and explicit image/cmd overrides.
    Header can be: 'python', 'python:3.8', 'custom image=alpine', 'node cmd="node -v"'
    """
    if not header_line: return None

    # Parse tokens handling quoted strings (e.g. cmd="bash -c 'echo hi'")
    try:
        tokens = shlex.split(header_line)
    except:
        tokens = header_line.split() # Fallback
    
    if not tokens: return None

    base_lang_input = tokens[0].lower()
    overrides = {}
    
    # Extract key=value pairs
    for token in tokens[1:]:
        if '=' in token:
            key, val = token.split('=', 1)
            overrides[key.lower()] = val

    # Resolve Base Language & Version
    base_lang = base_lang_input
    version = None
    # Regex allows +, # for c++, c#
    match = re.match(r"^([a-z0-9\+\#]+)(?:[:\-](\d+(?:\.\d+)*))?$", base_lang_input)
    if match:
        base_lang = match.group(1)
        version = match.group(2) 

    # Resolve Alias
    if base_lang in LANG_MAP:
        resolved = LANG_MAP[base_lang]
        if isinstance(resolved, str):
            base_lang = resolved
            if base_lang in LANG_MAP and isinstance(LANG_MAP[base_lang], str):
                 base_lang = LANG_MAP[base_lang]
        elif isinstance(resolved, dict):
            pass
    
    # Get Config
    config = None
    if base_lang in LANG_MAP and isinstance(LANG_MAP[base_lang], dict):
        config = LANG_MAP[base_lang].copy()
    
    # If no config found (unknown lang), check if 'image' override is provided
    # If so, treat as "custom" run
    if not config:
        if 'image' in overrides:
            config = {'image': '', 'cmd': []} # Will be filled by overrides
        else:
            # Wildcard Fallback (try lang:latest)
            image_tag = f"{base_lang_input}" if ':' in base_lang_input else f"{base_lang_input}:latest"
            config = {
                'image': image_tag,
                'cmd': [base_lang, '-'] # Best guess: cmd matches language name
            }

    # Apply Version Override (if not overridden by explicit image=)
    if version and config and 'image' not in overrides:
        # Keep repo, swap tag
        original_image = config.get('image', '')
        if ':' in original_image:
            repo = original_image.split(':')[0]
            config['image'] = f"{repo}:{version}"
        else:
            # If original had no tag, append version
            config['image'] = f"{original_image}:{version}"

    # Apply Explicit Overrides
    if 'image' in overrides:
        config['image'] = overrides['image']
    
    if 'cmd' in overrides:
        # Parse command string into list
        config['cmd'] = shlex.split(overrides['cmd'])
        
    if 'entrypoint' in overrides:
        config['entrypoint'] = overrides['entrypoint']

    return config

# --- Podman Lifecycle Management ---

def check_podman_alive():
    try:
        startupinfo = subprocess.STARTUPINFO()
        startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
        subprocess.check_call(['podman', 'info'], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, startupinfo=startupinfo)
        return True
    except:
        return False

def check_image_exists(image_name):
    try:
        startupinfo = subprocess.STARTUPINFO()
        startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
        subprocess.check_call(['podman', 'image', 'exists', image_name], startupinfo=startupinfo)
        return True
    except:
        return False

def ensure_podman_running(icon):
    if check_podman_alive(): return
    icon.notify("Podman is not running. Attempting to start...", title="Ephemeral Init")
    startupinfo = subprocess.STARTUPINFO()
    startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
    try:
        subprocess.check_call(['podman', 'machine', 'start'], startupinfo=startupinfo, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
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
    startupinfo = subprocess.STARTUPINFO()
    startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
    try:
        subprocess.run(['podman', 'machine', 'stop'], startupinfo=startupinfo)
    except Exception as e:
        print(f"Error stopping podman: {e}")

# --- Helper for Post-Mortem Debugging ---
def show_post_mortem_error(error_text):
    try:
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as tmp:
            tmp.write("--- EPHEMERAL EXECUTION ERROR ---\n\n")
            tmp.write(error_text)
            tmp_path = tmp.name
        subprocess.Popen(
            f'start cmd /K "type "{tmp_path}" && echo. && echo. && echo [Ephemeral Debug] Window persisted due to error. Close to dismiss."', 
            shell=True
        )
    except Exception as e:
        print(f"Failed to show error window: {e}")

# --- Cleanup ---

def purge_cache(icon, item):
    result = ctypes.windll.user32.MessageBoxW(0, 
        "This will remove ALL unused Podman images to free up disk space.\n\nAre you sure you want to proceed?", 
        "Clear Image Cache", 
        4 | 0x30) 

    if result != 6: return

    icon.notify("Pruning unused images... this may take a moment.", title="Ephemeral Maintenance")
    startupinfo = subprocess.STARTUPINFO()
    startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
    
    try:
        subprocess.run(['podman', 'image', 'prune', '--all', '--force'], startupinfo=startupinfo, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        icon.notify("Image cache cleared successfully.", title="Ephemeral")
    except Exception as e:
        icon.notify(f"Error clearing cache: {e}", title="Ephemeral Error")

# --- Execution Logic ---

def perform_visible_pull(image_name):
    cmd_line = (
        f'cmd /C "echo [Ephemeral] Image {image_name} not found. Downloading... '
        f'&& podman pull {image_name} || pause"'
    )
    process = subprocess.Popen(cmd_line, creationflags=subprocess.CREATE_NEW_CONSOLE)
    return process.wait()

def run_container_piped(icon, config, code, lang):
    try:
        startupinfo = subprocess.STARTUPINFO()
        startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
        
        if not code.endswith('\n'):
            code += '\n'
        code_bytes = code.replace('\r\n', '\n').encode('utf-8')

        # UPDATED: Memory limit bumped to 512m to prevent compiler OOM (Exit 137)
        podman_cmd = [
            'podman', 'run', '--rm', '-i', '--network', 'none', '--memory', '512m'
        ]
        
        # Add entrypoint override if specified (Needed for images like silkeh/gforth)
        if 'entrypoint' in config:
            podman_cmd.extend(['--entrypoint', config['entrypoint']])
            
        podman_cmd.append(config['image'])
        podman_cmd.extend(config['cmd'])

        process = subprocess.Popen(
            podman_cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            text=False, startupinfo=startupinfo
        )
        
        stdout_bytes, stderr_bytes = process.communicate(input=code_bytes)
        # Decode manually and STRIP ANSI
        stdout = strip_ansi_codes(stdout_bytes.decode('utf-8', errors='replace'))
        stderr = strip_ansi_codes(stderr_bytes.decode('utf-8', errors='replace'))
        
        if process.returncode == 0:
            result = stdout
            # Use lang name for notification title if available, else 'Custom'
            title_lang = lang.split()[0].capitalize() if lang else "Custom"
            pyperclip.copy(f"Result ({title_lang}):\n---\n```text\n{result.strip()}\n```")
            icon.notify(f"{title_lang} execution results copied to clipboard.", title="Ephemeral")
        else:
            full_error = f"Exit Code: {process.returncode}\n\nSTDERR:\n{stderr}\n\nSTDOUT:\n{stdout}"
            show_post_mortem_error(full_error)
            icon.notify("Execution Failed. Debug window opened.", title="Ephemeral Error")
    except Exception as e:
        show_post_mortem_error(f"System Exception:\n{str(e)}")
        icon.notify("Critical System Error", title="Ephemeral Failed")

def run_logic(icon):
    global LAST_DETECTED_LANG
    content = get_clipboard()
    
    # --- SAFETY CHECK: Prevent Recursion ---
    if re.search(r"^Result \(.*\):[\r\n]+---[\r\n]+", content.strip(), re.MULTILINE):
        icon.notify("Clipboard contains previous results. Execution halted.", title="Ephemeral Safety")
        return

    lang, code = parse_codeblock(content)

    if not lang:
        if content and content.strip():
            # Apply shebang stripping here too if manual entry assumes shebang exists but failed detection
            code = strip_shebang(content)
            user_input = prompt_user_for_language(LAST_DETECTED_LANG)
            if user_input:
                lang = user_input.strip() # Don't lower() here to preserve case for cmd args if any
            else:
                icon.notify("Execution cancelled.", title="Ephemeral")
                return
        else:
             icon.notify("Clipboard is empty.", title="Ephemeral Error")
             return

    # Update memory with the base language part only (first token)
    LAST_DETECTED_LANG = lang.split()[0]
    
    config = resolve_runtime_config(lang)
    
    if not config or not config.get('image'):
        icon.notify("Configuration failed. Could not resolve image.", title="Ephemeral Error")
        return

    icon.notify(f"Launching {LAST_DETECTED_LANG}...", title="Ephemeral Status")

    image_name = config['image']
    is_cached = check_image_exists(image_name)

    if not is_cached:
        exit_code = perform_visible_pull(image_name)
        if exit_code != 0:
            icon.notify("Image download failed.", title="Ephemeral Error")
            return

    run_container_piped(icon, config, code, lang)

def on_hotkey(icon):
    threading.Thread(target=run_logic, args=(icon,)).start()

def setup(icon):
    icon.visible = True
    def init_sequence():
        ensure_podman_running(icon)
    threading.Thread(target=init_sequence).start()
    keyboard.add_hotkey(HOTKEY, lambda: on_hotkey(icon))

def quit_app(icon, item):
    stop_podman_machine(icon)
    icon.stop()
    sys.exit()

if __name__ == '__main__':
    image = create_icon_image()
    menu = (
        item('Run Clipboard', lambda icon, item: on_hotkey(icon), default=True),
        item('Clear Image Cache', purge_cache),
        item('Quit', quit_app)
    )
    icon = pystray.Icon("Ephemeral", image, "Ephemeral", menu)
    icon.run(setup)
