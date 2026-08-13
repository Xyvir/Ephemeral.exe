# Ephemeral.exe

![Live swarm nodes](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2FXyvir%2FEphemeral.exe%2Fmain%2Fdocs%2Fswarm-status.json&label=live%20nodes)

**Ephemeral** is a **zero-friction, zero-barrier** way to run code — built for students, teachers, and analysts in math, engineering, and data science. It is a **one-shot** sandboxed code-execution engine that parses Markdown for codeblocks, runs them in isolated Podman containers, and extracts generated artifacts — a **literate-programming alternative to Jupyter notebooks** where the codeblocks live inside your **plaintext Markdown**, not a heavyweight notebook format. Rather than acting as a long-running daemon or task scheduler, it is a stateless, on-demand processing pipeline: highlight code in your notes, run it, paste the result back. It ships as a **Windows tray app**, a **Linux AppImage**, a **browser WebAssembly thin client**, and a **FastAPI server** (for remote sidecar execution), and it can run standalone or as a node in a peer-to-peer distributed compute network.

![Ephemeral Demo](ephemeral.gif)

---

## Introduction

### The Problem

Running code should be as easy as reading it — especially for students, teachers, and analysts whose focus is math, engineering, or data, not systems administration. But today there's a wall of friction:

* **Jupyter Notebooks** are the classic literate-programming tool, but they're heavyweight: a kernel to install and keep alive, a browser UI, hidden cell state, and `.ipynb` files that don't version-control cleanly and don't live where your notes live.
* **Languages are bolted to the machine, not the document.** Installing Python, Ruby, Node, Go, Rust, and Perl just to run a quick snippet is overkill, and managing multiple versions (Python 2.7 vs 3.10) is a nightmare of environment paths.
* **Copying code from a textbook, lecture notes, StackOverflow, or your PKMS (Obsidian/Logseq)** usually means opening a heavy IDE, creating a file, saving it, and running it — enough friction to kill the "let me just try this" moment.
* **In a classroom,** every student's machine is a support ticket: different OSes, missing dependencies, broken environments — before anyone has run a single line of code.

### The Ephemeral Solution

Ephemeral removes the barrier entirely. It is a **Jupyter-style literate-programming alternative that runs codeblocks directly inside plaintext Markdown** — a textbook, lecture notes, an Obsidian vault, a GitHub README, even a chat message. No kernel to install, no server to keep alive, no IDE, no installed languages: your notes *are* the notebook.

Technically, it acts as a **one-shot "Sidecar Notebook"** processing pipeline for your entire operating system. It leverages **Podman** (via WSL2 on Windows, rootless on Linux) to create instant, disposable execution environments that spin up, run your pipeline, return outputs/artifacts, and vanish.

**Why students, teachers, and analysts want this:**

1. **Zero friction:** highlight code in your notes, press a hotkey, paste the result back. No setup, no environment, no file juggling.
2. **Language Versatility:** run Bash, Python, Ruby, R, Julia, Octave, C++, Rust, and more without installing them locally — the language is a means, not the subject.
3. **Literate, not lock-in:** prose and code stay together in plaintext Markdown — readable, diffable, portable — with none of Jupyter's cell state or notebook format.
4. **Clean System:** no `npm_modules` or stray `.py` files cluttering your desktop. The container lives for milliseconds and vanishes.
5. **Security:** snippets run in a sandbox (`--network none`). A malicious `rm -rf /` only deletes a temporary container, not your hard drive.
6. **Legacy Support:** need to test a script in Python 2.7? Just type `python:2.7`. Ephemeral pulls the specific version for that run.
7. **Context Agnostic:** it works anywhere you can copy text.

### For Students & Teachers: The Zero-Setup Browser Client

The fastest way to experience Ephemeral needs no install at all — it's a web page. Open the [lite client](https://xyvir.github.io/Ephemeral.exe/) in any modern browser, paste or type a codeblock, and hit run:

* **Zero setup, zero sign-in, free.** No install, no account, no payment — the same engine as the desktop apps, with none of the prerequisites.
* **Runs anywhere.** A school Chromebook, a library computer, a phone — anything with a browser.
* **Responsive for everyday work.** Quick calculations, data analysis, plotting, and homework checks round-trip in seconds.
* **Backed by volunteers, not a datacenter.** Every run is executed by the community's donated compute — a robust, self-healing peer-to-peer network of nodes (see [Donating Compute](#donating-compute)) that keeps serving even as individual machines come and go.

The client speaks the same wire protocol as every other tier, so your Markdown codeblocks run in the same sandboxed containers on real cluster nodes, with the full language map and artifact support (technical details in [Web thin client](#web-thin-client-ephemeral-wasm-library)).

---

## Philosophy

A few design principles underpin every tier:

* **One-shot, stateless, on-demand.** Ephemeral is not a daemon or scheduler — it spins up, runs, returns, and vanishes. Every run starts from a clean environment, which keeps results reproducible.
* **Self-contained codeblocks.** Ephemeral deliberately has **no host-file mounting** (aside from the secure `/output` drop folder). Data must be declared inside the Markdown via seed files, which guarantees a snippet runs the same on any machine without hidden external file structures.
* **Sandbox-first.** Containers run with `--network none`, capped memory/CPU/PIDs, and no volume mounts. Untrusted code is contained by default; network access is an explicit, opt-in `unsafe` flag — and on the distributed network, a node operator's decision, never the requester's.
* **Privacy by default, locally.** In local-only mode, nothing ever leaves your machine.
* **Good-faith networking.** The public distributed network exists for teaching and shared clusters. It is not security-first or trust-first — treat anything submitted as public knowledge (see the [Trust Model](#distributed-tier--trust-model)).

---

## Donating Compute

![Live swarm nodes](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2FXyvir%2FEphemeral.exe%2Fmain%2Fdocs%2Fswarm-status.json&label=live%20nodes) — *this could be you.* 

**Join the horde:** the swarm only stays alive while people run nodes. If you have a machine that is on anyway (a home server, a spare VPS, an old laptop, or your daily desktop), please consider joining it. **The more nodes, the more resilient the network and the faster everyone's jobs run.**

Running a node takes minutes and requires nothing beyond the app itself:

* **Linux / DIY self-hosters:** one line installs the distributed gateway as a systemd service:

  ```bash
  curl -fsSL https://raw.githubusercontent.com/Xyvir/Ephemeral.exe/main/install_self_host.sh | SYSTEMD=1 bash -s -- distributed
  ```

  Or run it in Docker (see [Deployment](#deployment)).

  Optionally, turn your node into a **super-seed** that already has every language image warm: run `python scripts/hydrate_images.py` once. Budget roughly **15–25 GB** of disk for the full set — most of it is the big science and typesetting images (Anaconda and the TeX-enabled pandoc image are a few GB each). Pull just the common languages with `python scripts/hydrate_images.py --only python,node` if space is tight; nodes pull images lazily anyway, so hydrating is purely a performance boost.

* **Desktop / Windows users:** just run **`Ephemeral-Distributed.exe`** — it runs its own compute node when no background service is installed, or acts as a thin front-end to the installed **Background Service** (one node per machine). For a truly always-on node, right-click the tray icon and choose **Install Background Service**: the app first copies itself to a permanent location under your AppData, then registers that copy (`... --service`) as a boot-time scheduled task (running as SYSTEM) — so the node keeps serving jobs even while you're logged off, and moving or deleting the exe you launched can't break it.

That's it — no configuration. Your node fetches the live swarm list, joins the network, and the next scheduled refresh (within ~6 h) writes it into `docs/swarm.json`, where it starts carrying jobs and offloading for other nodes.

> [!WARNING]
> **Please read this before donating a node.** The distributed network is a peer-to-peer network: jobs submitted by strangers execute on your machine, and communication traverses public iroh relays. Ephemeral applies best-effort security — receiver-side sandboxing, an image allowlist, `--network none`, memory/CPU/PID caps, and removal of requester-supplied image/network overrides — but **there is no guarantee of safety**. P2P networks and their communication carry inherent risk, and no warranty is made against malicious payloads, data exfiltration, or compromise.
>
> **Do not run a node on a critical or irreplaceable workstation, on a machine holding sensitive or personal data, or on a network you consider sensitive** (an employer's or client's network, for example, or anywhere a sandbox escape would be unacceptable). Only donate compute you can afford to lose, and treat anything submitted to the public swarm as public knowledge. If you need a private cluster instead, self-host with explicit `EPHEMERAL_SEEDS` / `EPHEMERAL_RELAY` (see [Deployment](#deployment)) so only your own nodes participate.

---

## Usage

### Prerequisites

**Windows:**

1. **Enable Virtualization (BIOS/UEFI):** Ensure **Virtualization Technology** (often labeled VT-x, AMD-V, or SVM) is enabled in your computer's BIOS/UEFI settings. This is strictly required for WSL2 to function.
2. **Enable WSL2:** Open PowerShell as Administrator and run: `wsl --install` — restart your computer if prompted.
3. **Install Podman for Windows:** Download the installer from the [Podman Website](https://podman.io/docs/installation#windows), run it, then open a terminal and initialize the machine:
   ```powershell
   podman machine init
   ```
   *Note: Ephemeral will attempt to auto-start the machine if it's stopped, but the initial `init` setup usually requires manual intervention.*
   *Note: a stock WSL2 + `podman machine` setup doesn't delegate the cgroup controllers to rootless containers, so `--memory`/`--cpus`/`--pids-limit` cannot be enforced there. Ephemeral detects this once at startup and runs those jobs without cgroup limits (with a one-time notice in the output), while keeping `--network none` and the markdown-level sandbox intact. On hosts that can enforce limits (native Linux, non-WSL), they apply unconditionally.*

**Linux (AppImage or source):**

1. **Podman (rootless):** `sudo apt install podman`, then start the user socket: `systemctl --user start podman.socket` (enable it with `systemctl --user enable podman.socket`).
2. **A desktop tray host:** a StatusNotifier/AppIndicator implementation (most GNOME/KDE setups) or an X11 session for pystray's Xorg backend.
3. **FUSE** for AppImage mounting — or run with `APPIMAGE_EXTRACT_AND_RUN=1` on hosts without FUSE.

### Local vs distributed — what each flavor can and can't do

| | Local-only (`ephemeral-local`, `main_api.py`) | Distributed (`ephemeral-distributed`, `ephemeral-self-host-distributed`, wasm SPA) |
|---|---|---|
| **Execution** | Always on your machine/server | Local, or offloaded to the nearest neighbor with a warm image |
| **Multi-block requests** | Runs execute in parallel (up to 4) unless chaining is declared | Parallel locally *and* fanned out across idle cluster nodes unless chaining is declared |
| **Privacy** | **Nothing ever leaves the machine** | Public relays: treat submissions as public knowledge — no privacy guarantee |
| **Podman needed** | Yes (WSL2 on Windows, rootless on Linux) | Yes on compute nodes; **thin clients (browser) need none** |
| **Offloading** | None | Automatic nearest-neighbor offload + background image pull |
| **Node routing** | n/a | Idle-first: hello frames advertise each node's load (`active_jobs`/`max_jobs`); saturated nodes are skipped and the least-loaded warm node wins |
| **Network flag** | `unsafe` opt-in per block (local) | `unsafe` stripped receiver-side — network is a node-operator setting (`EPHEMERAL_ALLOW_NETWORK`), never the requester's |
| **Custom images** | Any Podman/Docker image via the `image=`/`cmd=` header (Declarative Image Mode) | **Disabled** — `image=`/`cmd=`/`entrypoint=` overrides are dropped receiver-side; only the node's allowlisted images (default: the built-in language map) run |
| **Config** | None | `EPHEMERAL_SEEDS` / `EPHEMERAL_RELAY` / `EPHEMERAL_SECRET` / `EPHEMERAL_ALLOW_NETWORK` |
| **Best for** | Private/sensitive work, offline, Lithic-UK sidecar | Teaching, shared clusters, browser access, heterogeneous images |

**In short:** if your code or data is sensitive, use a local-only flavor — the distributed network is good-faith, not private. If you want a browser-based classroom cluster where anyone's node can help run anyone's snippet, use the distributed tiers and assume everything you submit is public.

### Tray usage

1. **Highlight & Copy** any code block (or click the "Copy Code" button found on many documentation sites):
2. **Press** `Ctrl+Alt+X` (or left click the Tray Icon, or right click and use the menu).
3. **Wait** for the notification (or the status window if a download is required).
4. **Paste** the result wherever you need it.

*Note: Ephemeral supports Markdown blocks with language tags, Shebang lines (`#!/bin/python`), and prompts for user-input if no language specified.*

A test suite file is provided in the repo to demonstrate the usage of the various supported languages.

### CLI Mode

Ephemeral features a `--cli` (or `parse`) headless mode that completely bypasses the GUI, tray icon, and clipboard integrations. This allows Ephemeral to run in automated pipelines or on headless machines.

```bash
python main_local.py --cli your_script.md
```

In CLI mode, Ephemeral will:
1. Parse your markdown file for codeblocks.
2. Spin up the isolated container environment.
3. Pipe stdout/stderr directly back to your terminal.
4. Export any `/output` artifacts to your **current working directory** (instead of Downloads).

### API Server

For remote or programmatic execution, Ephemeral can run as a FastAPI server:

```bash
pip install -r requirements-api.txt
uvicorn main_api:app --host 0.0.0.0 --port 8787
```

Send a base64-encoded Markdown document via `POST /ephemeral/api/v1/run`:

```bash
curl -X POST http://localhost:8787/ephemeral/api/v1/run \
  -H "Content-Type: application/json" \
  -d '{"document_blob": "'$(echo '```python\nprint("Hello!")\n```' | base64)'", "timeout": 10}'
```

For production deployment as a sidecar (e.g., alongside Caddy + WebDAV), use the included `install.sh`:

```bash
chmod +x install.sh && sudo ./install.sh
```

This creates a hardened systemd service at `127.0.0.1:8787`, ready to be reverse-proxied.

### Supported Languages

05ab1e, Bash, Brainfuck, C, C++, CJam, Clojure, Common Lisp, Crystal, Elixir, Fortran, FreeBASIC,
Go, GolfScript, Haskell, Java, Julia, Lolcode, Lua, Nim, Node.js, OCaml, Octave, Perl, PHP, Piet,
PowerShell, Prolog, Python, R, Ruby, Rust, Science Python, Verilog.

### Features

#### Manual Language Entry & History

If you copy raw text without a language tag (e.g., no ` ```python `), Ephemeral will pop up a terminal window asking you to specify the language.

* It remembers your last used language for rapid iteration.
* Simply press **Enter** to use the default/last-used language.

#### Smart Markdown Parsing

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
## Python Result

```text
Pi is: 3.141592653589793
```
````

#### Legacy Versioning

You can override the default "Latest" version by appending a tag to the language name in your markdown block or shebang:

* `python:2.7` -> Runs in `python:2.7` container.
* `node:14` -> Runs in `node:14-alpine`.
* `ruby-2.6` -> Runs in `ruby:2.6`.

If no version is specified, it defaults to the stable/slim versions defined in the tool.

#### Cache Management

Ephemeral downloads container images as needed. Over time, these can take up disk space.

* **Right-click** the tray icon and select **"Clear Image Cache"**.
* This runs a safe prune command (`podman image prune -a`) to delete all images not currently in use, freeing up space on your drive.

#### Artifacts & File Exports

Ephemeral isn't just for text output. You can generate images, compile binaries, or create documents directly from your snippets.

**How it works:**
Any file your script saves to the **`/output`** directory inside the container is automatically captured:

1. **Images (Plots/Graphs):** If a single image (PNG, JPG, BMP) is generated, it is automatically converted to a bitmap and copied to your **Clipboard**, ready to paste immediately.
2. **Documents & Binaries:** If a non-image file is generated (e.g., PDF, DOCX, EXE), it is extracted and moved to your **Downloads** folder.
3. **Multiple Files:** If your script generates multiple files, they are automatically zipped into a timestamped archive and saved to **Downloads**.

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

#### Multi-Block Execution

Ephemeral supports running multiple codeblocks from a single clipboard copy. If multiple codeblocks use the same language and configuration, Ephemeral automatically groups them into a single container run. Since they execute in the same container, filesystem state is preserved between the blocks.

When a request spans **multiple languages/configs and no block declares chaining**, the runs are independent and execute **concurrently** (up to 4 parallel runs locally; distributed tiers additionally fan runs out across idle cluster nodes). Only a block that opts into chaining (see below) forces the whole request back onto the sequential, in-order path.

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
## Python Run 1

### Step 1 (Python)
```text
Saved 42 to /tmp/shared.txt
```

### Step 2 (Python)
```text
Read shared value: 42
```

## Node Run 2

```text
This runs in a completely separate, fresh environment.
```
````

#### Container Chaining (Piping)

Artifact chaining is **off by default**: in multi-container executions (e.g., a Python block followed by a Node.js block), each container runs in isolation — artifacts written to the previous container's `/output` directory do **not** leak into the next one, which is what makes parallel execution safe.

To opt in, append `chain` (or `piping` / `pipe`) to a block header: any artifacts that block writes to `/output` are passed down to the root directory of the **following container**. This makes it incredibly easy to pipe data across entirely different language environments — and because the runs depend on each other, declaring chaining anywhere in a request switches the whole request to sequential, in-order execution.

The legacy `nopiping` / `nopipe` tokens are still recognized (and still override `chain` if both appear), but they are redundant now that chaining is opt-in.

**Example: Passing data from Python to Node.js**
````text
```python chain
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
## Python Run 1

```text
Data written to /output/data.json
```

## Node Run 2

```text
Hello Alice, your score is 42!
```
````

#### Seed Files & Binary Data

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
## Python Result

```text
Hello from seed file!
Image exists: True
```
````

#### No Local File Mounting

Ephemeral intentionally does **not** have a mechanism to 'mount' or bind local directories into the container (aside from the secure `/output` drop folder). This is an intentional design decision for two key reasons:

1. **Reproducibility**: Ephemeral codeblocks are envisioned to be entirely self-contained. By forcing data to be declared inside the markdown (via seed files), the snippet is guaranteed to run on any machine without relying on hidden external file structures.
2. **Security**: Preventing local mounts reduces the possibility of local data-harvesting or ransomware payloads scanning your host machine's hard drive when running untrusted code with the `unsafe` network flag enabled. (see below)

#### Automatic Python Dependency Resolution

For Python blocks, Ephemeral resolves third-party packages automatically — no `unsafe` flag and no inline metadata required.

1. **Implicit dependency injection:** Ephemeral scans your Python code for `import` statements, filters out standard-library modules, and injects a [PEP 723](https://peps.python.org/pep-0723/) `# /// script` header declaring the inferred packages before the block is sent to the container.
2. **Two-stage sandboxed resolution:** When inferred dependencies exist and you did *not* opt into `unsafe`, the block runs in two container stages instead of one:
   - **Stage A** starts a container *with* network access and installs the dependencies into a virtual environment on a shared volume.
   - **Stage C** starts a fresh container *without* network access (`--network none`) and runs your payload using that environment's interpreter.

Your payload never sees the network — only the package-resolution step does. If you already wrote your own PEP 723 metadata, it is respected as-is (no re-injection), and if you do use `unsafe`, dependencies resolve in the normal single-stage mode.

**Example (no `unsafe`, no comments needed):**
```python
#!python
import numpy as np

arr = np.linspace(0, 10, 5)
print("numpy resolved and used offline:", arr.sum())
```

> Note the payload itself runs with `--network none` — the *resolution* stage has network, your code does not. A payload that makes a network request (e.g. `requests.get(...)`) still needs the `unsafe` flag.

#### Network Access (Unsafe Mode)

By default, Ephemeral runs all containers with network access disabled (`--network none`) to ensure a secure, sandboxed execution environment. If your snippet needs to download dependencies or interact with web APIs, you can append the `unsafe` keyword to your language tag to enable internet access.

> [!WARNING]
> **Security Risk:** Using `unsafe` removes the network sandbox, allowing the container to communicate externally. Be cautious when using this mode, especially with untrusted code, as malicious artifacts could be downloaded or your environment could be compromised. On the distributed network, `unsafe` is stripped by the receiver — network access is a node-operator setting, never a requester's flag.

**Example:**
```python
#!python unsafe
import urllib.request
response = urllib.request.urlopen('http://httpbin.org/get')
print("Successfully connected to the internet!")
```

#### Declarative Image Mode

You are not limited to the built-in languages. You can run *any* Docker/Podman image by defining the `image` and `cmd` parameters directly in the markdown header.

> **Distributed tiers only:** this is a **local-only** feature. Receiver-side sandboxing drops `image=`/`cmd=`/`entrypoint=` overrides from network jobs and enforces an image allowlist (default: the built-in language map), so a remote requester can never dictate what image runs — only the node operator's allowlist decides. Use the built-in languages on the distributed network, or extend the allowlist on your own nodes.

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

> If you would like to have a built-in language added to the language map please open a pull request, preferably containing a declarative example as above for me to test.

---

## Architecture

Ephemeral uses a modular, dual-entry-point architecture. The core execution engine is platform-agnostic and can be driven by a local tray app, a remote FastAPI server, or — in the distributed tiers — by peers on the network.

```
ephemeral_core/          ← Platform-agnostic engine (parsing + Podman orchestration)
├── config.py            ← Language map (50+ languages), network & chaining flags
├── parser.py            ← Markdown codeblock extraction & runtime resolution
├── executor.py          ← Async container execution via parse_and_execute()
├── models.py            ← ExecutionResult, GroupResult, BlockResult dataclasses
└── __init__.py

main_local.py            ← Windows tray client (clipboard → Podman → clipboard)
main_api.py              ← FastAPI server (POST /ephemeral/api/v1/run, base64 payloads)
main_distributed.py      ← Self-host distributed gateway (REST + cluster compute node)
main_distributed_client.py ← Distributed desktop tray client (local run + offloading)

ephemeral_net/           ← Distributed networking tier (iroh QUIC + peer discovery)
ephemeral_self_host/     ← Distributed gateway internals (sandboxed + offloading executor)

install.sh               ← One-shot sidecar deployment (systemd + rootless Podman)
install_self_host.sh     ← curl-able self-host installer (both flavors)
```

### Running Modes

| Mode | Entry Point | Artifacts Route To |
|---|---|---|
| **Tray** (default) | `main_local.py` | Clipboard (images) or `~/Downloads` (files) |
| **One-shot** | `main_local.py script.md` | Same as tray, then exits |
| **Headless CLI** | `ephemeral.exe --cli script.md` | Current working directory |
| **API Server** | `uvicorn main_api:app` | `/data/ephemeral/` (WebDAV mount) |
| **Sidecar Deploy** | `sudo ./install.sh` | systemd service on port 8787 |

### Distributed Tier & Trust Model

Ephemeral expands into a multi-tier distributed architecture built on the [iroh](https://www.iroh.computer) peer-to-peer networking library. The distributed tiers share the same `ephemeral_core` engine and a common networking core (`ephemeral_net`) that adds a peer-to-peer job network on top.

**Client/server thickness** (thinnest → thickest):

1. **Paper-thin clients** — the future static-URL REST API (curl-friendly, no WASM required).
2. **Thin clients** — the browser WASM SPA.
3. **Thick clients** — the desktop tray apps.
4. **Thick servers** — the self-hosted gateways.

*The phased implementation history (networking core, receiver-side sandboxing & offloading, parallel fan-out, desktop & browser tiers) lives in [misc.md](misc.md).*

| Package | Role | Runtime | Trust Model |
|---|---|---|---|
| `ephemeral-wasm-library` | Browser thin client (SPA) translating REST-style jobs into the distributed network | Browser (WebAssembly) | **Public** — good-faith |
| `ephemeral-distributed` | Desktop tray app: local execution, compute node, and nearest-neighbor offloading | Windows EXE + Linux AppImage | Public or private |
| `ephemeral-self-host-distributed` | Headless compute node + REST gateway for self-hosting (Docker/Coolify) | Linux container (Dockerfile) / source tarball | Public or private |
| `ephemeral-local` | Local-only desktop tray app (clipboard-driven, Podman) | Windows EXE + Linux AppImage | **Private** — nothing ever leaves the machine |

> **Trust Model & Privacy — please read before using the distributed tiers.**
> The public distributed network is a *good-faith* model designed for teaching (e.g., college students and professors running code snippets). **Anything you submit to the public ephemeral cloud should be treated as public knowledge — there is no privacy guarantee.** It is not security-first or trust-first: other network participants may be able to observe submitted code and outputs, and the shared public relays carry no uptime or performance guarantees.
> If you need privacy, **self-host instead** and use the non-distributed packages (`main_local.py` / `main_api.py`, or `ephemeral-self-host-distributed` on infrastructure you control).

### The default swarm (one big implicit network)

Every distributed binary joins the **same public swarm by default** — no configuration required. Run `Ephemeral-Distributed.exe`, the distributed AppImage, or `install_self_host.sh distributed` and your node is part of the network, discoverable by the web SPA and every other member. The mechanism:

- **The list *is* the bootstrap.** Nothing is hard-coded into the binaries: with `EPHEMERAL_SEED_NODES` / `EPHEMERAL_SEEDS` unset, every node fetches the live swarm list (`docs/swarm.json`) at startup and dials members by **stable node id + relay** (tickets only as a fallback for legacy entries), re-fetching every maintenance cycle so new members are learned without a restart.
- **Stable identity.** Every binary persists a 32-byte secret to `~/.ephemeral/secret_key.bin` (or `EPHEMERAL_STATE_DIR`) and reuses it across restarts, so its node id is permanent.
- **The list, not a box, is the lynchpin.** The list is refreshed by a scheduled Action (every 6 h) and regenerates from its own members as long as one is alive; a single **genesis anchor** (`SWARM_GENESIS` repo variable) exists only to bootstrap the first-ever empty list or rescue one whose every member went dark. **Adding an always-on node is fully automatic** — install any distributed flavor and the next refresh lists it, no code edits.
- **Mesh healing.** Nodes periodically re-dial known peers (by id + relay, ticket as fallback, with backoff), so the swarm repairs itself around a dead member.
- **Every listed node is probe-verified.** Each refresh sends every reachable entry a real job (a tiny Python payload with a fresh nonce) and only ranks it verified when it executes the payload and echoes the nonce back — a hello-only bot cannot fake that. Unreachable entries age out over a few runs; the genesis anchor is exempt from eviction only while it is the active bootstrap source.
- **Optional super-seeds & DNS.** `python scripts/hydrate_images.py` pre-pulls the whole language set so offloaded jobs land without a registry pull; the same Action can mirror the top two nodes into a DNS TXT record (`EPHEMERAL_DNS_TXT` + `EPHEMERAL_DNS_TOKEN`) as an independent first-contact path when GitHub is down.
- **Opt out.** Set `EPHEMERAL_SEED_NODES` / `EPHEMERAL_SEEDS` explicitly to bootstrap a private cluster instead; `EPHEMERAL_SECRET` pins an identity without touching disk.

> **The browser client is iroh-native too.** The wasm SPA dials by the same stable node id + relay — no asymmetry between tiers. Tickets remain only as a fallback for legacy peers that don't report a relay.

*The full mechanism — eviction thresholds, probe bookkeeping, the DNS mirror format, and manual refresh instructions — is in [misc.md](misc.md).*

### Web thin client (`ephemeral-wasm-library`)

The browser-side WebAssembly client speaks the **same wire protocol** as the Python tiers (`hello` handshake + `job_request` → `job_log`/`job_done`/`error` over iroh QUIC bi-streams), so it interoperates with Python compute nodes with zero translation. Browsers cannot hole-punch, so all browser↔cluster traffic traverses an iroh relay — n0's public relays by default, or a self-hosted one via the Relay URL field.

The SPA thin client lives in `ephemeral-wasm-library/web/` (vanilla JS — no framework). To run it:

```bash
cd ephemeral-wasm-library/web && python -m http.server 8787
# open http://localhost:8787 and run code — no ticket pasting needed.
```

**Discovery is automagic.** On load the client fetches the **live swarm list** (`docs/swarm.json`, refreshed every 6 h by a scheduled GitHub Action) and dials the current members by **stable node id + relay** (iroh-native — tickets only as a fallback for legacy peers). The public build ships with **no compiled-in seeds** (mirroring the Python tiers — nothing to keep in sync); if the list is unreachable the client falls back to the optional **DNS TXT mirror** (`BOOTSTRAP.dnsTxt` — the top two nodes in one 255-char TXT string, resolved via DNS-over-HTTPS as an independent, tiered path) and otherwise says so and offers the manual seed-ticket field as an operator override. This is configuration, not a job-routing dependency, so the execution path still runs entirely over the iroh network with no HTTP endpoint. Each dial completes the `hello` handshake and learns the whole cluster from the peer table (ids, relays, tickets and warm images); discovery runs concurrently so a few dead nodes can't stall it. Jobs then route automatically to the best available compute node — a peer whose warm images cover the document's languages first, then **idle-first** (nodes advertise their current load in hello frames; saturated nodes are skipped, the least-loaded warm node wins, RTT breaks ties) — submitted by node id + relay through the same `submit_job_to_node` path as the Python tiers. Multi-language documents without a `chain` flag are additionally fanned out across several idle nodes and the results merged. A manual seed-ticket field remains as an override for operators, and the *Cluster* panel lists discovered nodes with their images and latency.

To rebuild the wasm module: `cd ephemeral-wasm-library && bash build.sh` (see `build.sh` for the toolchain requirements — a stable Rust toolchain with the `wasm32-unknown-unknown` target, a wasm-capable clang for `ring`'s C files such as wasi-sdk, and the `wasm-bindgen` CLI pinned to 0.2.127). The built glue is committed under `web/wbg/` so the SPA works without a Rust toolchain.

### Desktop tiers & Linux AppImages

The desktop tray clients (`main_local.py` local-only, `main_distributed_client.py` distributed) are cross-platform: the same code builds a Windows EXE (PyInstaller) and a Linux **AppImage** (PyInstaller onedir + appimagetool, via `packaging/build_appimage.sh local|distributed`). Platform plumbing is guarded: the language prompt uses zenity/kdialog/tkinter on Linux, image-clipboard uses wl-copy/xclip, login autostart writes a `~/.config/autostart/ephemeral.desktop` entry, and Podman lifecycle uses the native rootless socket (`systemctl --user start podman.socket`) instead of `podman machine`. Both apps also expose `--cli script.md` (headless) and `--self-check` (install verification) modes. On Windows, the distributed tray's **Background Service** menu item (shown checked while installed) toggles a boot-time scheduled task that runs `Ephemeral-Distributed.exe --service` as SYSTEM from a permanent copy staged under `%LOCALAPPDATA%`, so the node stays in the swarm even while the user is logged off (the tray app keeps acting as the front end while logged in). Unchecking it removes the task, the staged copy, and the login-autostart entry in one go. The distributed tray is **hybrid**: when the background node is running it is a thin client over localhost (one identity per machine — any local user's tray can drive the same node), and when no service is reachable it falls back to running its own node exactly like the local client — no admin is required either way, and the merge to a single identity happens automatically the moment the service is installed.

---

## Deployment

### Release artifacts

The CI workflow (`/.github/workflows/build.yml`) builds and attaches **seven artifacts** to each release (triggered via the `workflow_dispatch` → *Create a new release* checkbox, or a push to `main`):

| Artifact | Tier | Notes |
|---|---|---|
| `Ephemeral.exe` | local (Windows) | one-file EXE |
| `Ephemeral-Distributed.exe` | distributed (Windows) | one-file EXE, bundles `iroh` |
| `ephemeral-local-x86_64.AppImage` | local (Linux) | portable tray app |
| `ephemeral-distributed-x86_64.AppImage` | distributed (Linux) | portable tray app, bundles `iroh` |
| `ephemeral-wasm-library.tar.gz` | web (browser) | SPA + wasm glue + crate source to rebuild |
| `ephemeral-self-host-distributed.tar.gz` | self-host (distributed server) | cluster gateway source + `Dockerfile` for Docker/Coolify |
| `ephemeral-self-host.tar.gz` | self-host (local API server) | plain REST gateway (`main_api.py`, no networking tier) + `Dockerfile.api` — the build bundled by Lithic-UK |

Run an AppImage like any executable: `chmod +x ephemeral-distributed-x86_64.AppImage && ./ephemeral-distributed-x86_64.AppImage` (configure the cluster via `EPHEMERAL_SEEDS`/`EPHEMERAL_RELAY`/`EPHEMERAL_SECRET`/`EPHEMERAL_ALLOW_NETWORK` environment variables, as with the Windows build). The AppImage needs a desktop with a StatusNotifier/AppIndicator host (most GNOME/KDE setups) or an X11 session (pystray's Xorg backend); on hosts without FUSE, run it with `APPIMAGE_EXTRACT_AND_RUN=1`.

### Install the self-host server in one line

Both self-host flavors install with a single curl (`install_self_host.sh` — installs into `~/ephemeral-self-host`, creates a venv, and either prints the `uvicorn` run command or installs a user systemd service with `SYSTEMD=1`):

```bash
# Non-distributed REST API — local-only execution (the Lithic-UK build)
curl -fsSL https://raw.githubusercontent.com/Xyvir/Ephemeral.exe/main/install_self_host.sh | bash -s -- local

# Distributed gateway — joins the ephemeral cluster as a compute node
curl -fsSL https://raw.githubusercontent.com/Xyvir/Ephemeral.exe/main/install_self_host.sh | bash -s -- distributed
```

Overrides: `INSTALL_DIR` (target directory), `PORT` (default **8787** — the Lithic-UK sidecar slot; see below), and for the distributed flavor `EPHEMERAL_RELAY` / `EPHEMERAL_SEEDS` / `EPHEMERAL_SECRET` / `EPHEMERAL_ALLOW_NETWORK`. Example: `curl -fsSL .../install_self_host.sh | EPHEMERAL_SEEDS="..." SYSTEMD=1 bash -s -- distributed`.

### Two installers, two slots

* **`install.sh`** — root, one-shot sidecar deployer (systemd service + rootless Podman, bound to `127.0.0.1:8787`). This is what Lithic-UK's `ENABLE_EPHEMERAL=true` flag invokes.
* **`install_self_host.sh`** — no root required; user-space install to `~/ephemeral-self-host`, both local and distributed flavors, optional `SYSTEMD=1` user service.

### Docker

```bash
# Distributed gateway (cluster compute node + REST)
docker build -f Dockerfile -t ephemeral-self-host-distributed .

# Local API server (no networking tier)
docker build -f Dockerfile.api -t ephemeral-self-host .
```

Mount the host Podman socket (`-v /run/podman/podman.sock:/run/podman/podman.sock`) so the node can execute jobs. Both images listen on port `8787`.

### Dropping into a Lithic-UK deployment

Lithic-UK can provision the Ephemeral backend itself (`ENABLE_EPHEMERAL=true` → `./install.sh` + a Caddy reverse proxy on `/ephemeral/api/v1`). The full integration contract lives in [misc.md](misc.md).

### Classroom deployment — three privacy dials

Ephemeral's privacy is a **deployment choice**, not a feature toggle: where the code runs is up to you, and every choice trades student setup against data movement.

| Model | Setup cost | Where student code runs | Privacy posture |
|---|---|---|---|
| **Desktop local** (`Ephemeral.exe`) | Each student (WSL2 + Podman) | The student's own machine | **Best** — nothing ever leaves the machine |
| **Professor gateway + browser** | The professor (one server) | The professor's server only | **Normal** — the same posture as an LMS/autograder |
| **Public swarm** (browser default) | None | Volunteers' machines | **Non-starter** for graded work — public by design |

"Zero setup" applies to the **browser client**, and only because it executes nothing locally — it ships the code somewhere. On Windows, private local execution costs a one-time WSL2 + Podman setup; no configuration is both zero-setup *and* offline. And that one-time setup buys **every language, forever**: once Podman is there, a language is just a container — the built-in map covers ~50 out of the box, and anything else that runs dockerized can be added on request. **One setup, all languages, forever.**

The **professor gateway** is the classroom sweet spot: students get the zero-setup browser client, and their code lands only on a server you control — encrypted end-to-end (iroh relays are blind forwarders), the same trust model as submitting homework to an LMS.

**Stand up a classroom node in four steps:**

1. **One Linux box** — any always-on server (a department machine, a cheap VPS) with Podman. Install the distributed gateway:

   ```bash
   curl -fsSL https://raw.githubusercontent.com/Xyvir/Ephemeral.exe/main/install_self_host.sh | SYSTEMD=1 bash -s -- distributed
   ```

2. **Keep it private** — run it with `EPHEMERAL_SEEDS` set to its own seed ticket (printed at startup) so it bootstraps privately and never appears in the public swarm list. Grab its identity from the log:

   ```bash
   journalctl --user -u ephemeral-self-host | grep SWARM
   # SWARM NODE_ID …  SWARM RELAY …  SWARM SEED TICKET …
   ```

3. **Pre-warm it once** so students never wait on an image pull: `python scripts/hydrate_images.py` (at minimum `--only python,octave` for a MATLAB-style course).

4. **Students connect from the browser** — open the SPA and paste the seed ticket into the manual seed field (or host the SPA yourself with the ticket baked in). From there it's the no-install loop: paste a codeblock, hit run, get the result.

Honest notes: the professor's box is the trust anchor — everything executes there, so treat it like any autograder server. First-run latency is image-pull time unless you hydrated (step 3). And school firewalls may need the iroh relay hostname allowlisted, since browser↔node traffic traverses n0's public relays by default.

### Building from Source (with Ephemeral!)

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

> **Note:** This is not the primary build pipeline — it's a convenience for self-updating. The official builds use GitHub Actions (see [misc.md](misc.md)).

### CI/CD Pipeline & GitHub Pages

The official build pipeline (test → build → release on every push or manual dispatch, with seven release artifacts) and the GitHub Pages hosting setup live in [misc.md](misc.md).

---

## Future Plans

- **Neighborhoods:** a short, human-friendly code that selects *which network* to join (a routing/partition field on top of the seed table — not authentication). With seed-mediated auto-discovery already handling node selection within a network, a neighborhood code becomes the user-facing way to choose the right cluster, e.g. per-class topics so a professor's students share an isolated neighborhood. If a neighborhood has no reachable seeds or peers, clients **fall back to the default iroh distributed peergroup** — joining a neighborhood never strands you, worst case you land on the shared default network.
- **Paper-Thin REST Clients:** a static-URL REST API that sends requests and responds over the ephemeral distributed network, for 'paper-thin' clients (curl-friendly, no WASM required) — with rate limiting, cached responses, and the like. This is a non-trivial service with a lot of implementation surface, so it is deliberately deferred.
- **Image-Layer Sync:** instead of pulling images from a registry after offloading, transfer warm image layers from the neighbor node over the iroh network (content-addressed, integrity-verified) so repeat jobs start instantly.

