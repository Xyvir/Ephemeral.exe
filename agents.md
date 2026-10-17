# Agent Knowledge Base

## Project Overview

Ephemeral is a sandboxed code execution engine that parses Markdown for codeblocks, runs them in isolated Podman containers, and extracts generated artifacts.

### Architecture

The codebase uses a modular, dual-entry-point design:

```
ephemeral_core/          ← Platform-agnostic engine (no GUI/HTTP)
├── config.py            ← LANG_MAP (50+ languages), NETWORK_FLAGS, NO_CHAIN_FLAGS
├── parser.py            ← parse_codeblocks(), resolve_runtime_config()
├── executor.py          ← async parse_and_execute(), Podman orchestration
├── models.py            ← ExecutionResult, GroupResult, BlockResult dataclasses
└── __init__.py          ← Public API re-exports

main_api.py              ← FastAPI server (POST /ephemeral/api/v1/run, base64 Pydantic model)
ephemeral_ui/            ← Unified desktop front end (one tray, two backends)
├── tray.py              ← Shared tray UI (menu, hotkeys, modes) — drives generic backend functions
├── platform.py          ← Shared platform plumbing (clipboard, prompts, artifacts, autostart)
└── backends/            ← local.py (Podman) | distributed.py (iroh cluster)
main_local.py            ← Thin explicit `local` mode entry
main_distributed_client.py ← Thin explicit `distributed` mode entry
ephemeral_ui/app.py       ← Shared mode dispatcher; never infers mode from filename
install.sh               ← One-shot sidecar deployment (systemd + rootless Podman)
```

**Key rule:** `ephemeral_core/` must never import GUI, clipboard, HTTP, or platform-specific code. All platform logic lives in `ephemeral_ui/` (the unified front end + backends). The tray front end never imports `ephemeral_core`/`ephemeral_net` directly — backends own that. Both desktop builds may bundle the same distributed modules for parity; only the explicit immutable mode selects whether the distributed backend is initialized.

### Core API

```python
from ephemeral_core import parse_and_execute, ExecutionResult

result: ExecutionResult = await parse_and_execute(markdown_text, timeout=60)
# result.stdout, result.stderr, result.exit_code, result.artifact_paths, result.artifact_dir
```

### Podman Security Invariants

These flags must remain in `executor.py` and must not be weakened:

- `podman run --rm -i --memory 2g -w /tmp --network none` — default for all containers (on hosts with ≤ 2.5 GiB RAM the memory/cpu/pids limits scale down to ~half of host RAM so a single job can't OOM a small VPS; `EPHEMERAL_MEMORY_LIMIT`/`EPHEMERAL_CPU_LIMIT`/`EPHEMERAL_PIDS_LIMIT` override)
- `--network none` is only removed when the user explicitly writes the `unsafe` keyword in a codeblock header **or** during the dependency-resolution stage of a two-stage Python run (see below)
- Containers have no host filesystem access except the ephemeral `/output` volume mount

### Python Dependency Resolution (Two-Stage Runs)

Python blocks backed by `uv run` (the `docker.io/tymills620/ephemeral-python-uv` image) get implicit PEP 723 injection: `ephemeral_core/parser.py` scans `import`/`from` statements, filters stdlib, and prepends a `# /// script` header. When inferred/declared deps exist and the user did NOT write `unsafe`, `executor.py` runs the block in two stages:

1. **Stage A** — `podman run` with network (`--dns 8.8.8.8 --dns 1.1.1.1`, no `--network none`) runs `uv venv /deps/venv && uv pip install <deps>` into a host temp dir mounted at `/deps`. The payload is NOT executed here.
2. **Stage C** — a second `podman run` with `--network none` executes the payload via `/deps/venv/bin/python -` from the same `/deps` mount.

The `/deps` temp dir is removed after the run. Existing user PEP 723 metadata is authoritative (never overwritten). With `unsafe`, python deps resolve in the normal single-stage `uv run -` path using the injected header.

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
1. `test` job — runs `test_core.py`, `test_space.py`, `test_api.py`, `test_net.py`, `test_self_host.py` and `test_ui.py` on ubuntu-latest (Python 3.10 + 3.12)
2. `build-exe` job — builds Windows EXE via PyInstaller on windows-latest (plus `build-exe-distributed`, `build-exe-distributed-beta`, `build-linux-appimage`)
   - `build-exe-distributed-beta` also wraps the onedir bundle in a **machine-wide MSI** (`packaging/msi/`: WiX v3, preinstalled on the runner) and installs/uninstalls it silently as a smoke test. The MSI installs to `%ProgramFiles%`, adds an all-users Start Menu shortcut, and writes a machine-wide `Run` entry so the node starts at any user's login. It contains **no custom actions, no service, no scheduled task and no `SYSTEM` component** — only declarative Windows Installer tables. A pre-login *service* is deliberately not offered: Podman on Windows is a per-user WSL2 machine and WSL cannot run from session 0, so such a service could not execute jobs. `test_ui.py` guards the WXS invariants (`packaging/msi/Ephemeral.Distributed.Beta.wxs`).
3. `scan-artifacts` job — best-effort ClamAV canary over the Windows executables (`scripts/scan_artifacts.sh`). Warns only: it must never block a release, and it is deliberately not in the `release` job's `needs`
4. `release` job — creates GitHub Release (manual dispatch)

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
  4. Prefer an **onedir** bundle over `--onefile` for anything antivirus-sensitive: the onefile bootloader unpacks a `_MEI…` temp directory and runs Python from it at every launch, which is the single most-flagged packaging trait. Disable UPX (`upx=False`) and embed a version resource — see `packaging/ephemeral-distributed-beta.spec` and `packaging/version_info.txt`.

**Never ship a privilege-escalating or service-installing feature from an unsigned build.** The first "always-on node" implementation re-launched the exe elevated (`ShellExecuteW` with `runas`) and registered `schtasks /SC ONSTART /RU SYSTEM`; the build was flagged. Its replacement, `ephemeral_ui.platform.NodeAutostart`, writes a **per-user login entry only** — no elevation, no service, no scheduled task, no copy of the exe — and is login-scoped by design (pre-login always-on is the Linux gateway's job, via `install_self_host.sh`). `test_ui.py` guards both invariants: the class's code must not contain `runas`, `ShellExecuteW`, `schtasks`, `SYSTEM`, `sc create`, `shutil.copy`, or `HKEY_LOCAL_MACHINE`, and the beta spec must stay onedir without UPX.
