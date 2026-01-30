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
import shlex  # For parsing quoted strings in headers
import ctypes
import shutil # For moving files and zipping

# --- Configuration ---
HOTKEY = 'ctrl+alt+x'
APP_NAME = "Ephemeral"
LAST_DETECTED_LANG = "python" # Default starting language

# Only this specific keyword enables network access
NETWORK_FLAGS = {'unsafe'} 

# Map languages to the 'Clean Slate' Image on Docker Hub
LANG_MAP = {
    # --- Standard Interpreted ---
    'python': {'image': 'python:3.10-slim', 'cmd': ['python', '-']},
    'node':   {'image': 'node:18-alpine',   'cmd': ['node', '-']},
    'bash':   {'image': 'alpine:latest',    'cmd': ['sh']},
    'ruby':   {'image': 'ruby:alpine',      'cmd': ['ruby']},
    
    # --- TiddlyWiki (Build Environment) ---
    'tiddlywiki': {
        'image': 'elasticdog/tiddlywiki', 
        'entrypoint': '/bin/sh', 
        'cmd': ['-c', 'cat > /tmp/build_script.sh && chmod +x /tmp/build_script.sh && /tmp/build_script.sh']
    },

    # --- Science & Data ---
    'science': {'image': 'continuumio/anaconda3', 'cmd': ['python', '-']},
    'octave':  {'image': 'gnuoctave/octave:latest', 'cmd': ['octave', '--no-gui', '--quiet']},
    'r':       {'image': 'r-base:latest',           'cmd': ['R', '--vanilla', '--slave', '-f', '/dev/stdin']},
    'julia':   {'image': 'julia:alpine',            'cmd': ['julia']},

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
    'brainfuck': {'image': 'esolang/brainfuck-esotope', 'cmd': ['sh', '-c', 'cat > /tmp/code && script /tmp/code']},

    # --- Hardware Description (HDL) ---
    'verilog': {'image': 'hdlc/iverilog', 'cmd': ['sh', '-c', 'cat > /tmp/run.v && iverilog /tmp/run.v -o /tmp/out && vvp /tmp/out']},

    # --- Functional & Scripting ---
    'haskell': {'image': 'haskell:slim', 'cmd': ['runghc']},
    'lua':     {'image': 'nickblah/lua:5.4-alpine', 'cmd': ['lua', '-']},
    'perl':    {'image': 'perl:slim',       'cmd': ['perl', '-']},
    'php':     {'image': 'php:alpine',      'cmd': ['php']},

    # --- Documents & Typesetting ---
    'latex':   {'image': 'pandoc/extra', 'entrypoint': '/bin/sh', 'cmd': ['-c', 'cat > /output/doc.tex && pdflatex -output-directory /output /output/doc.tex']},
    'pandoc':  {'image': 'pandoc/extra', 'entrypoint': '/bin/sh', 'cmd': ['-c', 'cat > /tmp/input.md && pandoc /tmp/input.md -o /output/converted.pdf']},
    'pandoc-pdf': {'image': 'pandoc/extra', 'entrypoint': '/bin/sh', 'cmd': ['-c', 'cat > /tmp/input.md && pandoc /tmp/input.md -o /output/converted.pdf']},
    'pandoc-docx': {'image': 'pandoc/extra', 'entrypoint': '/bin/sh', 'cmd': ['-c', 'cat > /tmp/input.md && pandoc /tmp/input.md -o /output/converted.docx']},

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
    'bf': 'brainfuck', 'spl': 'shakespeare', '><>': 'fish',
    'cob': 'cobol', 'gnucobol': 'cobol',
    'tw': 'tiddlywiki', 'tw5': 'tiddlywiki', 'wiki': 'tiddlywiki',
    'tex': 'latex', 'pdflatex': 'latex',
    'md': 'pandoc', 'markdown': 'pandoc'
}

# Add esolangs dynamically
ESOLANGS = [
    '05ab1e', 'golfscript', 'lolcode', 'piet', 'cjam', 'cobol'
]
for lang in ESOLANGS:
    if lang not in LANG_MAP:
        LANG_MAP[lang] = {'image': f'esolang/{lang}', 'cmd': ['sh', '-c', 'cat > /tmp/code && script /tmp/code']}

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
    pattern = r"```(.*?)\n(.*?)```"
    match = re.search(pattern, content, re.DOTALL)
    if match:
        header = match.group(1).strip() if match.group(1) else None
        return header, strip_shebang(match.group(2))
    first_line = content.strip().splitlines()[0]
    if first_line.startswith("#!"):
        lower_line = first_line.lower()
        sorted_keys = sorted(LANG_MAP.keys(), key=len, reverse=True)
        for key in sorted_keys:
            if key in lower_line:
                return key, strip_shebang(content)
    return None, None

def prompt_user_for_language(default_lang, code_preview=""):
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
        if os.path.exists(path_ctx): os.remove(path_ctx)
    return detected_lang

def resolve_runtime_config(header_line):
    if not header_line: return None
    try: tokens = shlex.split(header_line)
    except: tokens = header_line.split() 
    if not tokens: return None

    # 1. Detect Network Flags and isolate the language
    network_enabled = False
    cleaned_tokens = []
    
    for token in tokens:
        if token.lower() in NETWORK_FLAGS:
            network_enabled = True
        else:
            cleaned_tokens.append(token)
            
    if not cleaned_tokens: return None
    
    # 2. Parse Language and Overrides
    base_lang_input = cleaned_tokens[0].lower()
    overrides = {}
    
    for token in cleaned_tokens[1:]:
        if '=' in token:
            key, val = token.split('=', 1)
            overrides[key.lower()] = val

    base_lang = base_lang_input
    version = None
    match = re.match(r"^([a-z0-9\+\#]+)(?:[:\-](\d+(?:\.\d+)*))?$", base_lang_input)
    if match:
        base_lang = match.group(1)
        version = match.group(2) 
        
    if base_lang in LANG_MAP:
        resolved = LANG_MAP[base_lang]
        if isinstance(resolved, str):
            base_lang = resolved
            if base_lang in LANG_MAP and isinstance(LANG_MAP[base_lang], str):
                 base_lang = LANG_MAP[base_lang]
        elif isinstance(resolved, dict):
            pass

    config = None
    if base_lang in LANG_MAP and isinstance(LANG_MAP[base_lang], dict):
        config = LANG_MAP[base_lang].copy()
    
    if not config:
        if 'image' in overrides: config = {'image': '', 'cmd': []}
        else:
            image_tag = f"{base_lang_input}" if ':' in base_lang_input else f"{base_lang_input}:latest"
            config = {'image': image_tag, 'cmd': [base_lang, '-']}

    if version and config and 'image' not in overrides:
        original_image = config.get('image', '')
        if ':' in original_image:
            repo = original_image.split(':')[0]
            config['image'] = f"{repo}:{version}"
        else:
            config['image'] = f"{original_image}:{version}"

    if 'image' in overrides: config['image'] = overrides['image']
    if 'cmd' in overrides: config['cmd'] = shlex.split(overrides['cmd'])
    if 'entrypoint' in overrides: config['entrypoint'] = overrides['entrypoint']
    
    # 3. Inject Network Setting into Config
    config['allow_network'] = network_enabled
    return config

# --- Clipboard Images (Windows) ---
def copy_image_to_clipboard(image_path):
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

# --- Podman Lifecycle ---
def check_podman_alive():
    try:
        startupinfo = subprocess.STARTUPINFO()
        startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
        subprocess.check_call(['podman', 'info'], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, startupinfo=startupinfo)
        return True
    except: return False

def check_image_exists(image_name):
    try:
        startupinfo = subprocess.STARTUPINFO()
        startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
        subprocess.check_call(['podman', 'image', 'exists', image_name], startupinfo=startupinfo)
        return True
    except: return False

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

def show_post_mortem_error(error_text):
    try:
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as tmp:
            tmp.write("--- EPHEMERAL EXECUTION ERROR ---\n\n")
            tmp.write(error_text)
            tmp_path = tmp.name
        subprocess.Popen(f'start cmd /K "type "{tmp_path}" && echo. && echo. && echo [Ephemeral Debug] Window persisted due to error. Close to dismiss."', shell=True)
    except Exception as e: print(f"Failed to show error window: {e}")

def purge_cache(icon, item):
    icon.notify("Pruning unused images... this may take a moment.", title="Ephemeral Maintenance")
    startupinfo = subprocess.STARTUPINFO()
    startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
    try:
        subprocess.run(['podman', 'image', 'prune', '--all', '--force'], startupinfo=startupinfo, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        icon.notify("Image cache cleared successfully.", title="Ephemeral")
    except Exception as e: icon.notify(f"Error clearing cache: {e}", title="Ephemeral Error")

def perform_visible_pull(image_name):
    cmd_line = f'cmd /C "echo [Ephemeral] Image {image_name} not found. Downloading... && podman pull {image_name} || pause"'
    process = subprocess.Popen(cmd_line, creationflags=subprocess.CREATE_NEW_CONSOLE)
    return process.wait()

def run_container_piped(icon, config, code, lang):
    output_dir = tempfile.mkdtemp() # Create host temp dir for images
    try:
        startupinfo = subprocess.STARTUPINFO()
        startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
        
        if not code.endswith('\n'): code += '\n'
        code_bytes = code.replace('\r\n', '\n').encode('utf-8')

        # Base Command
        podman_cmd = ['podman', 'run', '--rm', '-i', '--memory', '512m']
        
        # --- UPDATED NETWORK LOGIC ---
        # If 'unsafe' is used, we force '--network host'. 
        # This bypasses WSL2/Bridge DNS issues by sharing the host's stack.
        if config.get('allow_network', False):
            podman_cmd.extend(['--network', 'host'])
        else:
            podman_cmd.extend(['--network', 'none'])
        # -----------------------------

        podman_cmd.extend(['-v', f'{output_dir}:/output'])
        
        if 'entrypoint' in config: podman_cmd.extend(['--entrypoint', config['entrypoint']])
        podman_cmd.append(config['image'])
        podman_cmd.extend(config['cmd'])

        process = subprocess.Popen(
            podman_cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            text=False, startupinfo=startupinfo
        )
        
        stdout_bytes, stderr_bytes = process.communicate(input=code_bytes)
        stdout = strip_ansi_codes(stdout_bytes.decode('utf-8', errors='replace'))
        stderr = strip_ansi_codes(stderr_bytes.decode('utf-8', errors='replace'))
        
        if process.returncode == 0:
            # CHECK FOR ARTIFACTS
            files = [f for f in os.listdir(output_dir) if os.path.isfile(os.path.join(output_dir, f))]
            downloads_dir = os.path.join(os.path.expanduser("~"), "Downloads")
            safe_lang = re.sub(r'[^a-zA-Z0-9]', '_', lang) if lang else "custom"

            if len(files) == 0:
                result = stdout
                title_lang = lang.split()[0].capitalize() if lang else "Custom"
                pyperclip.copy(f"Result ({title_lang}):\n---\n```text\n{result.strip()}\n```")
                icon.notify(f"{title_lang} execution results copied to clipboard.", title="Ephemeral")
            
            elif len(files) == 1:
                filename = files[0]
                filepath = os.path.join(output_dir, filename)
                
                if filename.lower().endswith(('.png', '.jpg', '.jpeg', '.bmp')):
                    if copy_image_to_clipboard(filepath):
                        icon.notify("Image generated and copied to clipboard!", title="Ephemeral")
                    else:
                        icon.notify("Failed to copy image. Check debug.", title="Ephemeral Error")
                else:
                    # Single non-image -> Move to Downloads
                    target_name = f"Ephemeral_{safe_lang}_{filename}"
                    target_path = os.path.join(downloads_dir, target_name)
                    
                    base, ext = os.path.splitext(target_path)
                    counter = 1
                    while os.path.exists(target_path):
                        target_path = f"{base}_{counter}{ext}"
                        counter += 1
                    
                    shutil.move(filepath, target_path)
                    icon.notify(f"File saved to Downloads:\n{os.path.basename(target_path)}", title="Ephemeral")

            else:
                # Multiple files -> Zip to Downloads
                timestamp = int(time.time())
                zip_base_name = f"Ephemeral_{safe_lang}_Artifacts_{timestamp}"
                zip_base_path = os.path.join(downloads_dir, zip_base_name)
                
                final_zip = shutil.make_archive(zip_base_path, 'zip', output_dir)
                icon.notify(f"Artifacts zipped to Downloads:\n{os.path.basename(final_zip)}", title="Ephemeral")
        else:
            full_error = f"Exit Code: {process.returncode}\n\nSTDERR:\n{stderr}\n\nSTDOUT:\n{stdout}"
            show_post_mortem_error(full_error)
            icon.notify("Execution Failed. Debug window opened.", title="Ephemeral Error")
    except Exception as e:
        show_post_mortem_error(f"System Exception:\n{str(e)}")
        icon.notify("Critical System Error", title="Ephemeral Failed")
    finally:
        try:
            for f in os.listdir(output_dir):
                os.remove(os.path.join(output_dir, f))
            os.rmdir(output_dir)
        except: pass

def run_logic(icon):
    global LAST_DETECTED_LANG
    content = get_clipboard()
    if re.search(r"^Result \(.*\):[\r\n]+---[\r\n]+", content.strip(), re.MULTILINE):
        icon.notify("Clipboard contains previous results. Execution halted.", title="Ephemeral Safety")
        return
    lang, code = parse_codeblock(content)
    if not lang:
        if content and content.strip():
            code = strip_shebang(content)
            code = re.sub(r"```+\s*$", "", code.rstrip())
            user_input = prompt_user_for_language(LAST_DETECTED_LANG, code)
            if user_input: lang = user_input.strip() 
            else:
                icon.notify("Execution cancelled.", title="Ephemeral")
                return
        else:
             icon.notify("Clipboard is empty.", title="Ephemeral Error")
             return
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
    def init_sequence(): ensure_podman_running(icon)
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
