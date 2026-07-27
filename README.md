# Ephemeral.exe

**Ephemeral** is a sandboxed code execution engine that parses Markdown for codeblocks, runs them in isolated Podman containers, and extracts generated artifacts. It can be used as a **Windows tray application** (clipboard-driven) or as a **FastAPI server** (for remote execution as a sidecar service).

![Ephemeral Demo](ephemeral.gif)


## The Problem

Windows is a fantastic OS, but it lacks the native "polyglot" flexibility of Linux. 
* Installing Python, Ruby, Node, Go, Rust, and Perl just to run a quick snippet is overkill.
* Managing multiple versions (Python 2.7 vs 3.10) is a nightmare of environment paths.
* Copying code from a textbook, StackOverflow, or your PKMS (Obsidian/Logseq) usually involves opening a heavy IDE, creating a file, saving it, and running it.



## The Ephemeral Solution

Ephemeral acts as a "Sidecar Notebook" for your entire operating system. It leverages **Podman** (via WSL2) to create instant, disposable execution environments.


### Why you want this:
1.  **Language Versatility:** Run Bash, Python, Ruby, R, Julia, Octave, C++, Rust, and more without installing them locally.
2.  **Clean System:** No more `npm_modules` or stray `.py` files cluttering your desktop. The container lives for milliseconds and vanishes.
3.  **Security:** Snippets run in a sandbox (`--network none`). A malicious `rm -rf /` only deletes a temporary container, not your hard drive.
4.  **Legacy Support:** Need to test a script in Python 2.7? Just type `python:2.7`. Ephemeral pulls the specific version for that run.
5.  **Context Agnostic:** It works anywhere you can copy text. 

## Architecture

Ephemeral uses a modular, dual-entry-point architecture. The core execution engine is platform-agnostic and can be driven by either a local Windows tray app or a remote FastAPI server.

```
ephemeral_core/          ← Platform-agnostic engine (parsing + Podman orchestration)
├── config.py            ← Language map (50+ languages), network & chaining flags
├── parser.py            ← Markdown codeblock extraction & runtime resolution
├── executor.py          ← Async container execution via parse_and_execute()
├── models.py            ← ExecutionResult, GroupResult, BlockResult dataclasses
└── __init__.py

main_local.py            ← Windows tray client (clipboard → Podman → clipboard)
main_api.py              ← FastAPI server (POST /api/v1/run, base64 payloads)
install.sh               ← One-shot sidecar deployment (systemd + rootless Podman)
```

### Running Modes

| Mode | Entry Point | Artifacts Route To |
|---|---|---|
| **Tray** (default) | `main_local.py` | Clipboard (images) or `~/Downloads` (files) |
| **One-shot** | `main_local.py script.md` | Same as tray, then exits |
| **Headless CLI** | `main_local.py --cli script.md` | Current working directory |
| **API Server** | `uvicorn main_api:app` | `/ephemeral/` (WebDAV mount) |
| **Sidecar Deploy** | `sudo ./install.sh` | systemd service on port 8787 |



## Prerequisites
Before running Ephemeral, you must ensure your Windows environment is ready to host Linux containers.



1.  **Enable Virtualization (BIOS/UEFI):**
    * Ensure **Virtualization Technology** (often labeled VT-x, AMD-V, or SVM) is enabled in your computer's BIOS/UEFI settings. This is strictly required for WSL2 to function.



2.  **Enable WSL2:**
    * Open PowerShell as Administrator and run: `wsl --install`
    * Restart your computer if prompted.



3.  **Install Podman for Windows:**
    * Download the installer from the [Podman Website](https://podman.io/docs/installation#windows).
    * Run the installer.
    * Open a terminal and initialize the machine:

        ```powershell
        podman machine init
        ```

    * *Note: Ephemeral will attempt to auto-start the machine if it's stopped, but the initial `init` setup usually requires manual intervention.*

## Usage


1.  **Highlight & Copy** any code block (or click the "Copy Code" button found on many documentation sites):
2.  **Press** `Ctrl+Alt+X` (or left click the Tray Icon, or right click and use the menu).
3.  **Wait** for the notification (or the status window if a download is required).
4.  **Paste** the result wherever you need it.



*Note: Ephemeral supports Markdown blocks with language tags, Shebang lines (`#!/bin/python`), and prompts for user-input if no language specified.*

A test suite file is provided in the repo to demonstration the usage of the various supported langauges.

## Features

### Manual Language Entry & History

If you copy raw text without a language tag (e.g., no ` ```python `), Ephemeral will pop up a terminal window asking you to specify the language. 

* It remembers your last used language for rapid iteration.
* Simply press **Enter** to use the default/last-used language.



### Smart Markdown Parsing

Ephemeral is built to forgive messy clipboard copies. You don't need to perfectly select only the codeblock!
* **Explanation Ignoring:** It gracefully ignores any explanatory text, formatting, or paragraphs outside of triple backticks. You can copy an entire tutorial with interstitial codeblocks and explanations, and Ephemeral will only extract and execute the code.
* **Wrapper Filtering:** It automatically detects and strips outer markdown wrappers (like ` ````text `) commonly used on documentation sites or chat apps to display nested blocks.
* **Orphaned Backticks:** Incomplete or trailing backticks are ignored; it exclusively extracts well-formed triple-backtick pairs.
* **Shebang Overrides:** If your code contains a shebang (e.g., `#!/usr/bin/node`), Ephemeral prioritizes it over the markdown language tag. For example, a ` ```python ` block containing `#!/bin/bash` will be correctly executed as Bash.

**Example: Copying a full tutorial document**
You can safely copy all the text below and Ephemeral will only execute the code block:
`````
````text
Welcome to my Python tutorial! First we will import the math module.
As you can see this text is not code, but Ephemeral will ignore it.

```python
import math
print("Pi is:", math.pi)
```

And that concludes the tutorial!
````
`````

**Example output:**
````
## Result (Python)

```text
Pi is: 3.141592653589793
```
````

### Legacy Versioning
You can override the default "Latest" version by appending a tag to the language name in your markdown block or shebang:



* `python:2.7` -> Runs in `python:2.7` container.
* `node:14` -> Runs in `node:14-alpine`.
* `ruby-2.6` -> Runs in `ruby:2.6`.



If no version is specified, it defaults to the stable/slim versions defined in the tool.



### Cache Management

Ephemeral downloads container images as needed. Over time, these can take up disk space.
* **Right-click** the tray icon and select **"Clear Image Cache"**.
* This runs a safe prune command (`podman image prune -a`) to delete all images not currently in use, freeing up space on your drive.

### Artifacts & File Exports

Ephemeral isn't just for text output. You can generate images, compile binaries, or create documents directly from your snippets.

**How it works:**
Any file your script saves to the **`/output`** directory inside the container is automatically captured:

1.  **Images (Plots/Graphs):** If a single image (PNG, JPG, BMP) is generated, it is automatically converted to a bitmap and copied to your **Clipboard**, ready to paste immediately.
2.  **Documents & Binaries:** If a non-image file is generated (e.g., PDF, DOCX, EXE), it is extracted and moved to your **Downloads** folder.
3.  **Multiple Files:** If your script generates multiple files, they are automatically zipped into a timestamped archive and saved to **Downloads**.

**Example (Python Plotting):**
```python
#!science
import matplotlib.pyplot as plt
import numpy as np

# Generate Data
x = np.linspace(0, 10, 100)
y = np.sin(x)

# Create Plot
plt.figure(figsize=(6, 4))
plt.plot(x, y, label='Sine Wave', color='blue')
plt.title("Generated via Ephemeral")
plt.legend()
plt.grid(True)

# Save to /output to trigger auto-clipboard copy
plt.savefig('/output/plot.png')
```

### Multi-Block Execution

Ephemeral supports running multiple sequential codeblocks from a single clipboard copy. If multiple codeblocks use the same language and configuration, Ephemeral automatically groups them into a single container run. Since they execute in the same container, filesystem state is preserved between the blocks.

**Example:**
````text
```python
# Block 1 (Python container starts)
with open("/tmp/shared.txt", "w") as f:
    f.write("42")
print("Saved 42 to /tmp/shared.txt")
```

```python
# Block 2 (Runs in the same Python container)
# Filesystem state is preserved between these blocks!
with open("/tmp/shared.txt", "r") as f:
    val = f.read()
print(f"Read shared value: {val}")
```

```node
// Block 3 (Switches to a new Node.js container)
console.log("This runs in a completely separate, fresh environment.");
```
````

**Example output:**
````text
## Run 1 (Python)

### Step 1 (Python)
```text
Saved 42 to /tmp/shared.txt
```

### Step 2 (Python)
```text
Read shared value: 42
```

## Run 2 (Node)

```text
This runs in a completely separate, fresh environment.
```
````

### Container Chaining (Piping)

By default, in multi-container executions (e.g., executing a Python block, followed by a Node.js block), any artifacts written to the `/output` directory in one container are automatically passed down to the root directory of the **following container**. This makes it incredibly easy to pipe data across entirely different language environments.

If you wish to prevent this behavior (for example, to isolate containers completely), you can append the `nopiping` or `nopipe` parameter to your block header.

**Example: Passing data from Python to Node.js**
````text
```python
# Container 1 (Python)
import json

data = {"user": "Alice", "score": 42}
with open("/output/data.json", "w") as f:
    json.dump(data, f)
print("Data written to /output/data.json")
```

```node
// Container 2 (Node.js)
// The data.json file from the previous container is automatically injected here!
const fs = require('fs');

const data = JSON.parse(fs.readFileSync('data.json', 'utf8'));
console.log(`Hello ${data.user}, your score is ${data.score}!`);
```
````

**Example output:**
````text
## Run 1 (Python)

```text
Data written to /output/data.json
```

## Run 2 (Node)

```text
Hello Alice, your score is 42!
```
````

### Seed Files & Binary Data

You can inject data files (like JSON, CSV, or text) into the container environment before your code executes. To do this, provide the filename as the language tag in the markdown header (e.g., `data.json` or `file data.json`). 

**Base64 Binary Seeding:** You can also seed binary files (like images or compiled data) by appending `b64` to the header. Ephemeral will automatically decode the base64 string back into the original binary file inside the container before your code runs.

**Conversion Hotkey:** To make seeding effortless, Ephemeral includes a conversion hotkey (`Ctrl+Win+X`). If you copy an image, a file in your file explorer, or raw text to your clipboard and press `Ctrl+Win+X`, Ephemeral will automatically convert it into the correct markdown seed block format (including base64 encoding if necessary) and replace your clipboard content, ready to be pasted!

*(Note: When using the hotkey on a copied file, Ephemeral will name the file `seed.<ext>` inside the container. It will also append `source=<original_name>` to the header just as informational metadata so you can remember where the data came from.)*

**Example (Text and Binary Seeds):**
````text
```data.json
{
  "message": "Hello from seed file!"
}
```

```seed.png b64 source=image.png
iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNk+A8AAQUBAScY42YAAAAASUVORK5CYII=
```

```python
import json
import os

with open('data.json', 'r') as f:
    print(json.load(f)['message'])

print(f"Image exists: {os.path.exists('seed.png')}")
```
````

**Example output:**
````text
## Result (Python)

```text
Hello from seed file!
Image exists: True
```
````

### No Local File Mounting

Ephemeral intentionally does **not** have a mechanism to 'mount' or bind local directories into the container (aside from the secure `/output` drop folder). This is an intentional design decision for two key reasons:
1. **Reproducibility**: Ephemeral codeblocks are envisioned to be entirely self-contained. By forcing data to be declared inside the markdown (via seed files), the snippet is guaranteed to run on any machine without relying on hidden external file structures.
2. **Security**: Preventing local mounts reduces the possibility of local data-harvesting or ransomware payloads scanning your host machine's hard drive when running untrusted code with the `unsafe` network flag enabled. (see below)


### Network Access (Unsafe Mode)

By default, Ephemeral runs all containers with network access disabled (`--network none`) to ensure a secure, sandboxed execution environment. If your snippet needs to download dependencies or interact with web APIs, you can append the `unsafe` keyword to your language tag to enable internet access.

> [!WARNING]
> **Security Risk:** Using `unsafe` removes the network sandbox, allowing the container to communicate externally. Be cautious when using this mode, especially with untrusted code, as malicious artifacts could be downloaded or your environment could be compromised.

**Example:**
```python
#!python unsafe
import urllib.request
response = urllib.request.urlopen('http://httpbin.org/get')
print("Successfully connected to the internet!")
```

## Supported Languages

05ab1e, Bash, Brainfuck, C, C++, CJam, Clojure, Common Lisp, Crystal, Elixir, Fortran, FreeBASIC, 
Go, GolfScript, Haskell, Java, Julia, Lolcode, Lua, Nim, Node.js, OCaml, Octave, Perl, PHP, Piet, 
PowerShell, Prolog, Python, R, Ruby, Rust, Science Python, Verilog.
## Declarative Image Mode
You are not limited to the built-in languages. You can run *any* Docker/Podman image by defining the `image` and `cmd` parameters directly in the markdown header.

**Example: Running COBOL via a custom declarative header:**

````text
```cobol image=esolang/cobol cmd="sh -c 'cat > /tmp/run.cob && cobc -x -free -o /tmp/run /tmp/run.cob && /tmp/run'"
IDENTIFICATION DIVISION.
PROGRAM-ID. DECLARATIVE-TEST.
PROCEDURE DIVISION.
    DISPLAY "Markdown: COBOL (Declarative) | Math Check: " 45 " - OK".
    STOP RUN.
```
````

> If you would like to have a built-in language added to the language map please open a pull request, preferrably containing a declarative example as above for me to test.

## Building from Source (with Ephemeral!)

Ephemeral can build itself! If you already have a working copy of Ephemeral, you can update to the latest version by copying and executing this snippet. It fetches the latest source from GitHub, cross-compiles a Windows `.exe` using PyWine, and drops it into your `\Downloads` folder:

````text
```pywine unsafe
# Fetch latest source from GitHub
wine python -c "import urllib.request, zipfile, io; zipfile.ZipFile(io.BytesIO(urllib.request.urlopen('https://github.com/Xyvir/Ephemeral.exe/archive/refs/heads/main.zip').read())).extractall()"
cd Ephemeral.exe-main

# Install all dependencies
wine python -m pip install -r requirements.txt pyinstaller Pillow

# Generate icon
wine python -c "from PIL import Image, ImageDraw; img=Image.new('RGB', (64, 64), (30, 30, 30)); dc=ImageDraw.Draw(img); dc.rectangle((16,16,48,48), fill=(255,255,255)); dc.rectangle((20,20,44,28), fill=(0,120,215)); img.save('ephemeral.ico')"

# Inject build timestamp as version
sed -i "s/Version number (injected from the github workflow)/LOCAL_$(date +%Y%m%d-%H%M%S)/g" main_local.py

# Build — main_local.py is the entry point, ephemeral_core is bundled as a hidden import
wine pyinstaller --noconsole --onefile --name Ephemeral --icon=ephemeral.ico --hidden-import=ephemeral_core main_local.py
cp dist/Ephemeral.exe /output/
```
````

> **Note:** This is not the primary build pipeline — it's a convenience for self-updating. The official builds use GitHub Actions (see below).

## CI/CD Pipeline

The official build pipeline uses GitHub Actions (`.github/workflows/build.yml`) with three stages:

1.  **Test** — Runs `test_core.py` + `test_api.py` on `ubuntu-latest` across Python 3.10 and 3.12.
2.  **Build** — Builds the Windows `.exe` via PyInstaller on `windows-latest`.
3.  **Release** — Creates a GitHub Release with the artifact (manual dispatch only).

## CLI Mode

Ephemeral features a `--cli` (or `parse`) headless mode that completely bypasses the GUI, tray icon, and clipboard integrations. This allows Ephemeral to run in automated pipelines or on headless machines.

```bash
python main_local.py --cli your_script.md
```

In CLI mode, Ephemeral will:
1. Parse your markdown file for codeblocks.
2. Spin up the isolated container environment.
3. Pipe stdout/stderr directly back to your terminal.
4. Export any `/output` artifacts to your **current working directory** (instead of Downloads).

## API Server

For remote or programmatic execution, Ephemeral can run as a FastAPI server:

```bash
pip install -r requirements-api.txt
uvicorn main_api:app --host 0.0.0.0 --port 8000
```

Send a base64-encoded Markdown document via `POST /api/v1/run`:

```bash
curl -X POST http://localhost:8000/api/v1/run \
  -H "Content-Type: application/json" \
  -d '{"document_blob": "'$(echo '```python\nprint("Hello!")\n```' | base64)'", "timeout": 10}'
```

For production deployment as a sidecar (e.g., alongside Caddy + WebDAV), use the included `install.sh`:

```bash
chmod +x install.sh && sudo ./install.sh
```

This creates a hardened systemd service at `127.0.0.1:8787`, ready to be reverse-proxied.
