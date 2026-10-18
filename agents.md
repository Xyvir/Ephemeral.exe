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
main_distributed_instructor.py ← Thin explicit `instructor` mode entry (classroom build: no public swarm)
ephemeral_ui/app.py       ← Shared mode dispatcher; never infers mode from filename
install.sh               ← One-shot sidecar deployment (systemd + rootless Podman)
```

**Key rule:** `ephemeral_core/` must never import GUI, clipboard, HTTP, or platform-specific code. All platform logic lives in `ephemeral_ui/` (the unified front end + backends). The tray front end never imports `ephemeral_core`/`ephemeral_net` directly — backends own that. Both desktop builds may bundle the same distributed modules for parity; only the explicit immutable mode selects whether the distributed backend is initialized.

The `instructor` mode is the classroom build. It selects `InstructorBackend`, whose node **never** bootstraps from the public swarm list, so "no public swarm" is a property of the build rather than a setting a user can flip or an environment variable that can re-enable it. Keep it that way: the live-list bootstrap stays behind the per-build `public_swarm` flag, exactly one call site reaches `bootstrap_from_list()`, and `test_ui.py` fails if either changes. With the public swarm gone, private mode OFF means local-only — the node dials nothing and runs every job on that machine.

The first interactive launch offers create-or-join exactly once, so a teacher can put two instructor machines in one classroom swarm for redundancy: the prompt accepts the other machine's student link, a bare ticket, or `node_id@relay`, through `ephemeral_net.swarm.parse_seed_input` (a link carrying no `#seed=` is refused rather than stored as a seed nothing can parse). Two invariants keep that honest and `test_ui.py` pins both: applying `default_private` records nothing, so the prompt still gets its chance on a headless-vs-interactive start, and the recorded choice — written by the prompt or by the toggle, never by the default — is what stops the default from reimposing itself over an explicit OFF.

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

#### Prerequisites (WSL2 + Podman) and the Setup Bundle

A fresh Windows PC can run nothing until two machine-level things exist: WSL2 (an optional Windows feature needing elevation and, on first enablement, a reboot) and Podman (whose Windows installer is per-user). The missing piece used to be a README instruction; now setup owns it.

The privilege split is the design:

* `ephemeral_ui/prereq.py` answers "can this PC run a block?" with unprivileged probes (`wsl --status`, `podman machine list --format json`, `podman info`), names every state a teacher can land in, and explains the fix. It never elevates itself — `test_prereq.py` unparses the module and fails if `runas`, `ShellExecuteW`, `Start-Process`, `schtasks` or an HKLM key ever appear in executable code.
* `--setup-prereqs` on the instructor entry point (`main_distributed_instructor.py` → `prereq.cli`) is the elevated half: DISM `/norestart` for `Microsoft-Windows-Subsystem-Linux` + `VirtualMachinePlatform`, then `wsl --install --no-distribution`. It exits 3010 for "restart required" and 740 for "elevation refused", and is idempotent, so a healthy machine returns immediately on repair.
* The per-user half — `podman machine init --now --update-connection=false`, then a real `podman run` canary — happens in the tray *before* any classroom link is announced (`DistributedBackend._announce_first_run`). A link is only shown once a container has actually exited 0; otherwise the missing piece is named instead. `Distributed → Repair runtime` repeats the unprivileged half on demand.
* `packaging/bootstrapper/` holds the Burn bundle (`build_setup.sh` + the `.wxs`) that chains WSL2 → Podman → the MSI, and is what teachers download. It needs `burn.exe` and `WixBalExtension.dll` (the full WiX v3 toolset — the binaries-only zip lacks `burn.exe`). Both prerequisite packages are `Permanent="yes"`: uninstalling Ephemeral never disables WSL2 or removes Podman. The bundle passes `LAUNCHFIRSTRUN=1` because Burn installs MSIs quietly and the MSI's silent guard would otherwise suppress the post-install launch. `test_prereq.py` pins the chain order, those properties, and that the WSL package has no `DetectCondition` (the idempotent executable is the detector, on purpose — Burn has no probe for an optional Windows feature).
* Detection detail worth keeping: `wsl.exe` writes **UTF-16LE with no BOM**, so a UTF-8 decode interleaves NULs and every keyword check silently misses — a healthy machine then looks like it has no WSL. `prereq.decode_console` handles it and a test pins the case.

The Podman payload is pinned by version in CI (`PODMAN_VERSION`) and embedded, so setup works offline and a moved release fails the build instead of shipping a bundle without its runtime.

## Dependencies

- `requirements.txt` — Windows tray client (pystray, Pillow, pyperclip, keyboard)
- `requirements-api.txt` — API server (fastapi, uvicorn, pydantic)

### Build Pipeline

GitHub Actions (`.github/workflows/build.yml`):
1. `test` job — runs `test_core.py`, `test_space.py`, `test_api.py`, `test_net.py`, `test_self_host.py` and `test_ui.py` on ubuntu-latest (Python 3.10 + 3.12)
2. `build-exe` job — builds Windows EXE via PyInstaller on windows-latest (plus `build-exe-distributed`, `build-exe-distributed-instructor`, `build-linux-appimage`)
   - `build-exe-distributed-instructor` also wraps the onedir bundle in a **machine-wide MSI** (`packaging/msi/`: WiX v3, preinstalled on the runner) and installs/uninstalls it silently as a smoke test. The MSI installs to `%ProgramFiles%`, adds an all-users Start Menu shortcut, and writes a machine-wide `Run` entry so the node starts at any user's login. It contains **no service, no scheduled task and no `SYSTEM` component**, and exactly **one custom action**: the post-install launch that starts the app once, as the installing user, so the classroom link is on screen when setup ends. That action is type 34 (the installed exe, no script or shell), impersonated, `asyncNoWait`, sequenced after `InstallFinalize`, and skipped when `UILevel <> 2` is false, i.e. for a silent `msiexec /qn` deployment. `test_ui.py` pins that shape and fails if a second action appears or any of those guards is dropped. A pre-login *service* is deliberately not offered: Podman on Windows is a per-user WSL2 machine and WSL cannot run from session 0, so such a service could not execute jobs. `test_ui.py` guards the WXS invariants (`packaging/msi/Ephemeral.Distributed.Instructor.wxs`).
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
  4. Prefer an **onedir** bundle over `--onefile` for anything antivirus-sensitive: the onefile bootloader unpacks a `_MEI…` temp directory and runs Python from it at every launch, which is the single most-flagged packaging trait. Disable UPX (`upx=False`) and embed a version resource — see `packaging/ephemeral-distributed-instructor.spec` and `packaging/version_info.txt`.

**Never ship a privilege-escalating or service-installing feature from an unsigned build.** The first "always-on node" implementation re-launched the exe elevated (`ShellExecuteW` with `runas`) and registered `schtasks /SC ONSTART /RU SYSTEM`; the build was flagged. Its replacement, `ephemeral_ui.platform.NodeAutostart`, writes a **per-user login entry only** — no elevation, no service, no scheduled task, no copy of the exe — and is login-scoped by design (pre-login always-on is the Linux gateway's job, via `install_self_host.sh`). `test_ui.py` guards both invariants: the class's code must not contain `runas`, `ShellExecuteW`, `schtasks`, `SYSTEM`, `sc create`, `shutil.copy`, or `HKEY_LOCAL_MACHINE`, and the instructor spec must stay onedir without UPX.
