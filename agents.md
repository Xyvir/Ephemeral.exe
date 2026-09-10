# Agent Knowledge Base

## Project Overview

Ephemeral is a sandboxed code execution engine that parses Markdown for codeblocks, runs them in isolated Podman containers, and extracts generated artifacts.

### Architecture

The codebase uses a modular, dual-entry-point design:

```
ephemeral_core/          ← Platform-agnostic engine (no GUI/HTTP)
├── config.py            ← LANG_MAP (50+ languages), NETWORK_FLAGS, NO_CHAIN_FLAGS
├── parser.py            ← parse_codeblocks(), resolve_runtime_config(), DEP_RESOLVERS (two-stage dep resolution)
├── executor.py          ← async parse_and_execute(), Podman orchestration
├── models.py            ← ExecutionResult, GroupResult, BlockResult dataclasses
└── __init__.py          ← Public API re-exports

main_api.py              ← FastAPI server (POST /ephemeral/api/v1/run, base64 Pydantic model)
ephemeral_ui/            ← Unified desktop front end (one tray, two backends)
├── tray.py              ← Shared tray UI (menu, hotkeys, modes) — drives generic backend functions
├── platform.py          ← Shared platform plumbing (clipboard, prompts, artifacts, autostart)
└── backends/            ← local.py (Podman) | distributed.py (iroh cluster)
main_local.py            ← Thin entry: LocalBackend
main_distributed_client.py ← Thin entry: DistributedBackend
install.sh               ← One-shot sidecar deployment (systemd + rootless Podman)
```

**Key rule:** `ephemeral_core/` must never import GUI, clipboard, HTTP, or platform-specific code. All platform logic lives in `ephemeral_ui/` (the unified front end + backends). The tray front end never imports `ephemeral_core`/`ephemeral_net` directly — backends own that, so the local build stays free of the networking tier.

### Core API

```python
from ephemeral_core import parse_and_execute, ExecutionResult

result: ExecutionResult = await parse_and_execute(markdown_text, timeout=60)
# result.stdout, result.stderr, result.exit_code, result.artifact_paths, result.artifact_dir
```

### Podman Security Invariants

These flags must remain in `executor.py` and must not be weakened:

- `podman run --rm -i --memory 2g -w /tmp --network none` — default for all containers (on hosts with ≤ 2.5 GiB RAM the memory/cpu/pids limits scale down to ~half of host RAM so a single job can't OOM a small VPS; `EPHEMERAL_MEMORY_LIMIT`/`EPHEMERAL_CPU_LIMIT`/`EPHEMERAL_PIDS_LIMIT` override)
- `--network none` is only removed when the user explicitly writes the `unsafe` keyword in a codeblock header **or** during the dependency-resolution stage of a two-stage run (see below)
- Containers have no host filesystem access except the ephemeral `/output` volume mount

### Dependency Resolution (Generic Two-Stage Runs)

Dependency resolution is **generic, resolver-driven, and keyed by image** — not Python-specific. `ephemeral_core/parser.py` holds `DEP_RESOLVERS`: a registry mapping image repositories (tag stripped) to a resolver dict with four hooks:

- `infer(block)` → `(deps, mutations)`: parse block content, return dependency specs plus block mutations (PEP 723 injection for Python; nothing for TeX).
- `stage_a(deps)` → POSIX sh snippet run **with network** in Stage A (receives `/deps` mount).
- `run_cmd`: Stage C payload command, or `None` to reuse the block's own `cmd`.
- `stage_c_env`: extra env vars Stage C needs to find the deps.

`executor.py`'s `_run_container_sync` looks the run's image up in the registry; when a resolver exists, deps were inferred, and the user did NOT write `unsafe`, `executor.py` runs the block in two stages:

1. **Stage A** — `podman run` with network (`--dns 8.8.8.8 --dns 1.1.1.1`, no `--network none`) runs the resolver's `stage_a` script into a host temp dir mounted at `/deps`. The payload is NOT executed here.
2. **Stage C** — a second `podman run` with `--network none` executes the payload (resolver `run_cmd` or the block's own cmd, plus `stage_c_env`) from the same `/deps` mount.

The `/deps` temp dir is removed after the run. With `unsafe`, deps resolve in the normal single-stage path. Registering a new language family = adding one `DEP_RESOLVERS` entry (its image must ship the Stage A tooling).

**Registered resolvers:**

- `docker.io/tymills620/ephemeral-python-uv` — scans `import`/`from`, filters stdlib, injects a PEP 723 `# /// script` header; Stage A runs `uv venv /deps/venv && uv pip install <deps>`; Stage C executes via `/deps/venv/bin/python -`. Existing user PEP 723 metadata is authoritative (never overwritten).
- `docker.io/pandoc/extra` (backs `latex`, `tex`, `pandoc`, `pandoc-pdf`, `pandoc-docx`) — scans `\usepackage`/`\RequirePackage` (TeX comments stripped, kernel packages like `fontenc` skipped) in both the block body and pandoc YAML `header-includes` metadata (block scalar, list item, inline, and flow-list forms — YAML-lite harvester in `_iter_header_include_lines`, no PyYAML); Stage A runs `tlmgr init-usermode && tlmgr --usermode install <pkgs>` with `TEXMFHOME=/deps/texmf` (the image ships full TeX Live; user mode installs into the shared mount and kpathsea finds it natively — no mktexlsr); Stage C reuses the block's own `pdflatex`/`pandoc` cmd with `TEXMFHOME` set. `\documentclass` is deliberately not scanned (class→tlmgr name mapping is unreliable; those classes ship in the image).

### Artifact Routing

Artifact routing is the caller's responsibility, not the core's:
- **API** (`main_api.py`): Zips to `/data/ephemeral/` (WebDAV mount)
- **Local** (`main_local.py`): Single image → clipboard, single file → Downloads, multiple → zipped to Downloads

### Tray Feedback (two mechanisms only)

The unified tray front end (`ephemeral_ui/`) has exactly two user-feedback surfaces:

1. **Toast notifications** (`icon.notify`) — transient / successful messages: "Launching bash...", "Results copied", validation ("Clipboard is empty").
2. **Terminal windows** (`ephemeral_ui.platform.show_terminal_window`) — anything needing longer review: errors (execution failures, cluster failures), About (with node status), long-running status. Non-blocking and log-backed (same pattern as pre-hydration); interactive prompts (language / seed / pre-hydration confirmation) stay on their own blocking consoles.

Do NOT introduce a third surface, and don't toast error output — route it through `show_terminal_window`.

### Dependencies

- `requirements.txt` — Windows tray client (pystray, Pillow, pyperclip, keyboard)
- `requirements-api.txt` — API server (fastapi, uvicorn, pydantic)

### Build Pipeline

GitHub Actions (`.github/workflows/build.yml`):
1. `test` job — runs `test_core.py`, `test_space.py`, `test_api.py`, `test_net.py`, and `test_self_host.py` on ubuntu-latest (Python 3.10 + 3.12)
2. `build-exe` job — builds Windows EXE via PyInstaller on windows-latest
3. `release` job — creates GitHub Release (manual dispatch)

---

## PyInstaller & Antivirus False Positives

**Context:** Ephemeral is compiled into a single executable using PyInstaller's `--onefile` flag via the GitHub Actions workflow.

**The Issue:** Antivirus software, particularly Windows Defender, relies heavily on heuristic scanning for self-extracting zip files containing Python bootloaders (which PyInstaller creates). In this case, the virus detection was NOT due to a random hash collision, but specifically due to the behavior of passing the local filename through the hotkey clipboard generation mechanism.

**Specific Example (June 2026):**
A change to the `on_convert_hotkey` function (now in `ephemeral_ui/platform.py`) caused Windows Defender to flag the output executable. The change involved taking a file copied to the clipboard, reading its contents, and injecting its local filename into the clipboard output as a "pass-thru" variable (`filename.replace(' ', '_')` or using `os.path.basename(file_path)` directly into the output string).

Because the executable was:
1. Grabbing clipboard data (`ImageGrab.grabclipboard()`)
2. Reading local files (`with open(file_path)`)
3. Extracting and embedding local filenames into an output

...the heuristic engine likely incorrectly categorized it as a "data stealer" or "spyware".

**Remediation & Best Practices:**
- If an executable build is suddenly flagged as a virus after modifying Python code, **assume it is a heuristic false-positive** unless proven otherwise.
- Avoid code that explicitly handles local user filenames alongside clipboard manipulation if it is not strictly necessary, as this pattern mimics malware behavior.
- To resolve false positives without removing necessary features, you can attempt to:
  1. Add a benign dummy comment (e.g., `# Hash Shifter: v1`) to shift the resulting executable hash.
  2. Pin PyInstaller to an older, established version (e.g., `pyinstaller==6.6.0`), as bleeding-edge bootloaders are frequently blanket-flagged by Microsoft until whitelisted.
  3. Simplify the code to avoid triggering heuristic rules (e.g., fallback to generic filenames like `seed.png` rather than extracting the user's actual local filename).
