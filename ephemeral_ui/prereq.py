"""Can this PC actually run a block — and if not, what is missing?

The question matters because of what a classroom build promises: setup ends
with a student link on screen. A link is only honest if a container will start
behind it, and on a fresh Windows machine nothing will: Podman needs WSL2, WSL2
is an optional Windows feature that needs elevation and usually a reboot, and a
Podman machine has to be created before ``podman info`` answers at all.

So the work is split by privilege, and that split is the design:

* **Enabling WSL2** is a machine-level change needing elevation and a reboot.
  It happens in the installer (``--setup-prereqs``, run by the setup bundle,
  which the user has already consented to elevate) and never from the tray.
  Nothing in this module elevates itself, and there is deliberately no
  ``runas``/``ShellExecuteW``/``Start-Process -Verb`` path to copy.
* **Creating and proving the Podman machine** is per-user and unprivileged, so
  it happens here on first run — before any classroom link is announced.

Detection is deliberately done with unprivileged commands (``wsl --status``,
``podman machine list``), because the query must work in a tray that never
elevates; DISM is used only to *change* feature state, in the install path.

The bundle is the default route, not the only one. A PC where it cannot run —
the MSI or the zip installed directly, or a machine whose policy refuses an
unsigned installer — gets the vendor's own front end as a second route
(:func:`desktop_guide`). It installs the engine and, per Podman Desktop's own
docs, does **not** enable WSL2, so it is offered after the elevated half and
never instead of it; ``test_prereq`` pins that ordering.
"""
from __future__ import annotations

import json
import shutil
import subprocess
import sys

from ephemeral_net.swarm import default_state_dir
from ephemeral_ui import platform

#: Tiny image used to prove the runtime works end to end. Kept in step with
#: ``ephemeral_ui.backends.distributed.PREHYDRATE_IMAGE`` (a test pins that), so
#: the first-run canary pulls an image the node wants anyway.
CANARY_IMAGE = "docker.io/library/alpine:latest"

#: Exit codes worth telling apart.
OK = 0
FAILED = 1
REBOOT_REQUIRED = 3010        # what DISM/MSI return for "restart to finish"
ELEVATION_REQUIRED = 740      # DISM's "the requested operation requires elevation"

#: The optional Windows features WSL2 needs, in Microsoft's manual-install order.
WSL_FEATURES = ("Microsoft-Windows-Subsystem-Linux", "VirtualMachinePlatform")

#: The vendor's own Windows front end, offered as a *second route* for the PCs
#: our setup cannot cover: a teacher who installed the MSI or the zip directly,
#: or one whose machine policy blocks an unsigned installer. Pinned here rather
#: than in prose so the URL a teacher retypes is the one a test checked.
PODMAN_DESKTOP_URL = "https://podman-desktop.io/downloads"

#: Vendor install guide for the engine itself, used by the non-Windows guide.
PODMAN_INSTALL_URL = "https://podman.io/docs/installation"


def guide_url() -> str:
    """The page the vendor-route guide points at on this platform."""
    return PODMAN_DESKTOP_URL if sys.platform == "win32" else PODMAN_INSTALL_URL

#: Package ids, verified with ``winget search`` on Windows 11 (1.30.5 / 6.1.4):
#: the GUI, and the engine alone for a managed PC where winget is allowed but
#: our installer is not. Both install per-user, like the MSI we embed.
PODMAN_DESKTOP_WINGET = "RedHat.Podman-Desktop"
PODMAN_CLI_WINGET = "Podman.CLI"

_BACKSLASH = 92


class RuntimeState:
    """What, if anything, stands between this PC and running a block."""

    def __init__(self, stage: str, detail: str = "") -> None:
        self.stage = stage
        self.detail = detail

    @property
    def ok(self) -> bool:
        return self.stage == "ready"

    def __repr__(self) -> str:  # keeps test failures readable
        return f"RuntimeState({self.stage!r}, {self.detail!r})"


# --- parsing (pure, so it is testable without WSL or Podman) -----------------

def classify_wsl_status(rc: int, output: str) -> RuntimeState:
    """Read ``wsl --status`` output.

    WSL's messages are not a documented machine-readable contract, so this
    matches the *meanings* Microsoft documents (not enabled, needs a restart,
    no virtualization) and treats anything it does not recognise as ready only
    when the command itself succeeded. A wrong "ready" is caught later by the
    canary; a wrong "broken" would block a working machine, which is worse.
    """
    text = (output or "").lower()
    if "virtualization" in text and ("not supported" in text or "disable" in text):
        return RuntimeState("no-virtualization",
                            "CPU virtualization is disabled (check BIOS/UEFI)")
    if "restart" in text or "reboot" in text:
        return RuntimeState("needs-reboot",
                            "a restart is required to finish WSL setup")
    if ("not enabled" in text or "not installed" in text
            or "enable it by running" in text):
        return RuntimeState("no-wsl", "the WSL optional component is not enabled")
    if rc != 0:
        detail = (output or "").strip()[:400]
        return RuntimeState("no-wsl", detail or f"wsl --status exited {rc}")
    return RuntimeState("ready", (output or "").strip()[:400])


def classify_machine_list(rc: int, output: str) -> RuntimeState:
    """Read ``podman machine list --format json``.

    ``--format json`` is stable enough to parse (unlike the table layout), and
    "a machine exists" and "a machine is running" are different answers: the
    first still needs a ``start``, the second is what ``podman info`` needs.
    """
    if rc != 0:
        return RuntimeState("no-machine", (output or "").strip()[:400])
    try:
        machines = json.loads(output or "[]")
    except ValueError:
        return RuntimeState("no-machine", "unreadable machine list")
    if not isinstance(machines, list) or not machines:
        return RuntimeState("no-machine", "no Podman machine has been created")
    for m in machines:
        if isinstance(m, dict) and str(m.get("Running", "")).lower() in ("true", "1"):
            return RuntimeState("ready", "")
    return RuntimeState("machine-stopped", "the Podman machine is not running")


# --- probing (unprivileged; the tray's questions) ----------------------------

def say(line: str) -> None:
    """Record a setup step somewhere it can still be read afterwards.

    The shipped build is windowed (PyInstaller ``console=False``), so
    ``sys.stdout`` and ``sys.stderr`` are ``None`` and ``print`` becomes a
    silent no-op: Burn would capture nothing, and a prerequisite that failed
    inside setup would leave no trace anywhere. Verified, not assumed — a
    windowed exe does not raise on ``print``, it simply drops the line.

    So each line also goes to ``<state dir>/setup.log``, which is what the CI
    smoke test and a support conversation actually read. Logging is
    best-effort: never let bookkeeping break setup.
    """
    for stream in (sys.stdout, sys.stderr):
        if stream is not None:
            try:
                print(line, file=stream)
            except Exception:
                pass
            break
    try:
        log = default_state_dir() / "setup.log"
        log.parent.mkdir(parents=True, exist_ok=True)
        with open(log, "a", encoding="utf-8") as fh:
            fh.write(line + "\n")
    except OSError:
        pass


def decode_console(raw: bytes) -> str:
    """Decode what a Windows tool wrote, which is often not UTF-8.

    ``wsl.exe`` writes **UTF-16LE with no BOM** — verified on Windows 11, where
    every character is followed by a NUL. Decoding that as UTF-8 yields
    interleaved NULs, every keyword in :func:`classify_wsl_status` silently
    misses, and a perfectly healthy machine gets reported as missing WSL.
    BOM-less UTF-16 is recognised by NUL density rather than guessed.
    """
    if not raw:
        return ""
    if raw[:2] in (b"\xff\xfe", b"\xfe\xff"):
        return raw.decode("utf-16", errors="replace")
    if raw.count(b"\x00") > max(1, len(raw) // 4):
        return raw.decode("utf-16-le", errors="replace")
    try:
        return raw.decode("utf-8")
    except UnicodeDecodeError:
        return raw.decode("cp1252", errors="replace")


def run(cmd: list[str], timeout: int = 60) -> tuple[int, str]:
    """Run a command without a window, returning ``(exit code, combined output)``.

    Never raises: a missing binary and a non-zero exit are both answers here,
    and both callers (the tray and the installer) have to render them.
    """
    try:
        done = subprocess.run(
            cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
            timeout=timeout, startupinfo=platform.get_startupinfo(),
        )
        return done.returncode, decode_console(done.stdout or b"")
    except FileNotFoundError:
        return 127, f"{cmd[0]} not found"
    except subprocess.TimeoutExpired:
        return 124, f"{cmd[0]} timed out after {timeout}s"
    except Exception as e:  # pragma: no cover - defensive
        return 1, str(e)


def wsl_status() -> RuntimeState:
    """Whether WSL2 is installed, ready, or needs a restart."""
    if sys.platform != "win32":
        return RuntimeState("ready", "")  # native Linux/darwin: no WSL layer
    if not shutil.which("wsl"):
        return RuntimeState("no-wsl", "wsl.exe is not available on this PC")
    rc, out = run(["wsl", "--status"], timeout=30)
    return classify_wsl_status(rc, out)


def machine_status() -> RuntimeState:
    """Whether a Podman machine exists, and whether it is running.

    Only Windows and macOS have machines; on native Linux Podman has no VM and
    readiness is the rootless socket, so the same question is answered there
    instead of reporting a "machine" that can never exist.
    """
    if not shutil.which("podman"):
        return RuntimeState("no-podman", "the podman command is not installed")
    if sys.platform not in ("win32", "darwin"):
        if _podman_answers():
            return RuntimeState("ready", "")
        return RuntimeState("machine-stopped",
                            "the rootless Podman socket is not answering "
                            "(systemctl --user start podman.socket)")
    rc, out = run(["podman", "machine", "list", "--format", "json"], timeout=60)
    return classify_machine_list(rc, out)


def status() -> RuntimeState:
    """The single question the tray asks before promising anything."""
    if sys.platform == "win32":
        wsl = wsl_status()
        if not wsl.ok:
            return wsl
    podman = machine_status()
    if podman.stage == "ready" and not _podman_answers():
        # A machine can be listed as running while the connection is stale.
        return RuntimeState("machine-stopped", "Podman is not answering")
    return podman


def _podman_answers() -> bool:
    """Whether ``podman info`` succeeds — the actual readiness signal."""
    from ephemeral_core import check_podman_alive

    return check_podman_alive()


# --- repair (per-user, unprivileged) -----------------------------------------

def ensure_machine() -> tuple[bool, str]:
    """Create or start the per-user Podman machine.

    Unattended on purpose: ``--now`` creates and starts in one step, and
    ``--update-connection=false`` keeps the command from prompting to become
    the default connection (documented podman-machine-init behaviour), which
    would otherwise hang with no console to answer it.

    Returns ``(ok, message)``; the message is what to show the teacher.
    """
    state = machine_status()
    if state.stage == "no-podman":
        return False, (
            "The podman command is not installed. Run the Ephemeral setup "
            "(Ephemeral-Distributed-Instructor-Setup.exe) to install it, or "
            f"use the vendor route in Distributed -> Install Podman Desktop "
            f"({PODMAN_DESKTOP_URL})."
        )
    if state.stage == "ready":
        return True, "Podman machine already running"
    if state.stage == "machine-stopped":
        rc, out = run(["podman", "machine", "start"], timeout=600)
        return (rc == 0), (out.strip() or "podman machine start failed")
    if state.stage == "no-machine":
        rc, out = run(
            ["podman", "machine", "init", "--now", "--update-connection=false"],
            timeout=1800,
        )
        return (rc == 0), (out.strip() or "podman machine init failed")
    return False, state.detail or f"cannot start Podman ({state.stage})"


def canary(timeout: int = 600) -> tuple[bool, str]:
    """Prove the runtime can run a container — the only claim worth making.

    Cheaper than it looks: the image is the same tiny alpine the node
    prehydrates, so on a used machine it is already local, and on a fresh one it
    is a pull the node wants anyway.
    """
    rc, out = run(
        ["podman", "run", "--rm", "--network", "none", CANARY_IMAGE, "true"],
        timeout=timeout,
    )
    return rc == 0, out.strip()


def _is_cgroup_delegation_error(detail: str) -> bool:
    """Whether a failed canary is the WSL2 'controllers not delegated' fault."""
    d = detail.lower()
    return "cgroup.controllers" in d or (
        "controller" in d and "is not available" in d
    )


def repair_cgroup_delegation() -> tuple[bool, str]:
    """Point the Podman machine at the cgroupfs fallback, idempotently.

    Unprivileged from the tray's point of view: ``podman machine ssh`` runs as
    the machine's own user, and the sudo here is inside the WSL2 VM, not on the
    Windows host, so no elevation happens. Affects every container on this
    machine, which is why it is only run from Repair runtime / first run.
    """
    if sys.platform != "win32":
        return False, "not a WSL2 machine"
    script = (
        "printf '[engine]\\ncgroup_manager = \"cgroupfs\"\\n"
        "cgroup_parent = \"/\"\\n' "
        "| sudo tee /etc/containers/containers.conf.d/99-ephemeral-cgroups.conf >/dev/null "
        "&& sudo systemctl restart podman.service podman.socket"
    )
    rc, out = run(["podman", "machine", "ssh", script], timeout=120)
    return rc == 0, out.strip()


def ensure_canary(timeout: int = 600) -> tuple[bool, str]:
    """:func:`canary`, with one self-repair for the WSL2 cgroup delegation fault."""
    ok, detail = canary(timeout)
    if ok or sys.platform != "win32" or not _is_cgroup_delegation_error(detail):
        return ok, detail
    repaired, msg = repair_cgroup_delegation()
    if not repaired:
        return False, f"{detail}\n\ncgroup repair failed: {msg}"
    return canary(timeout)


# --- the installer's elevated half -------------------------------------------

def enable_wsl(features: tuple[str, ...] = WSL_FEATURES) -> int:
    """Enable the optional features WSL2 needs. **Requires elevation.**

    ``/norestart`` defers the reboot instead of starting one: the setup bundle
    owns that decision, and an installer that reboots underneath a teacher who
    is still reading the summary is how work gets lost.

    Returns ``OK``, ``REBOOT_REQUIRED`` or ``FAILED`` (see :func:`describe_exit`).
    """
    if sys.platform != "win32":
        return OK
    need_reboot = False
    for feature in features:
        rc, out = run(
            ["dism", "/online", "/enable-feature", f"/featurename:{feature}",
             "/all", "/norestart"],
            timeout=1800,
        )
        if rc == REBOOT_REQUIRED:
            need_reboot = True
        elif rc != OK:
            say(f"[setup] enabling {feature} failed ({rc}):\n{out}")
            return rc if rc == ELEVATION_REQUIRED else FAILED
    # The features alone are not the whole story on Windows 10, or on a machine
    # whose WSL runtime predates them.
    rc, out = run(["wsl", "--install", "--no-distribution"], timeout=1800)
    if rc == REBOOT_REQUIRED:
        need_reboot = True
    elif rc != OK:
        rc2, out2 = run(["wsl", "--update", "--web-download"], timeout=1800)
        if rc2 not in (OK, REBOOT_REQUIRED):
            say(f"[setup] wsl install failed ({rc}):\n{out}\n{out2}")
            return FAILED
        need_reboot = need_reboot or rc2 == REBOOT_REQUIRED
    return REBOOT_REQUIRED if need_reboot else OK


def describe_exit(code: int) -> str:
    """One line for the setup summary, so a failed prerequisite is legible."""
    if code == OK:
        return "WSL2 is enabled."
    if code == REBOOT_REQUIRED:
        return ("WSL2 is enabled. Restart Windows to finish it — Ephemeral "
                "finishes its own setup on the next launch.")
    if code == ELEVATION_REQUIRED:
        return "Enabling WSL2 needs administrator rights."
    return "WSL2 could not be enabled automatically; see the setup log."


def desktop_guide() -> tuple[str, str]:
    """Steps for the vendor route, in the order that actually works.

    Offered because setup cannot be every PC's answer: the MSI and the zip can
    be installed without the bundle, and a managed machine may refuse an
    unsigned installer outright. What this route does **not** do is replace the
    elevated half — Podman Desktop's own docs are explicit that "you must
    enable the WSL feature", and its onboarding cannot create a machine
    without it — so WSL2 is step zero here, not an afterthought. A teacher who
    installs the GUI first hits a dead end inside someone else's wizard, and
    that is the failure this ordering exists to prevent.

    Nothing here is required when the bundle ran; the readiness gate and
    ``Repair runtime`` accept whatever this route produces, because both ask
    ``podman machine list`` rather than caring which provider created it.
    """
    if sys.platform != "win32":
        return (
            "Install Podman",
            "Install Podman with your package manager (for example "
            "'sudo apt install podman'), start the user socket with "
            "'systemctl --user start podman.socket', then start the tray "
            "again.\n\n"
            "Distributed -> Repair runtime re-checks it and proves it with a "
            "real container.\n\n"
            f"Installation guide: {PODMAN_INSTALL_URL}",
        )
    return (
        "Install Podman Desktop",
        "Podman Desktop is the official front end for Podman, and its setup "
        "flow installs the engine and creates a machine on Windows. Use it "
        "when Ephemeral's own setup cannot run on this PC.\n\n"
        "WSL2 comes first, whichever route you take: Podman Desktop installs "
        "the engine but does not enable WSL2, and cannot create a machine "
        "without it.\n"
        "  * run Ephemeral-Distributed-Instructor-Setup.exe, or\n"
        "  * in an administrator PowerShell: wsl --install --no-distribution\n"
        "  then restart Windows if it asks.\n\n"
        "Then:\n"
        f"  1. Install Podman Desktop: {PODMAN_DESKTOP_URL}\n"
        f"     (or, in a terminal: winget install {PODMAN_DESKTOP_WINGET})\n"
        "  2. In its onboarding (or Settings -> Resources) choose Install for "
        "Podman, then Create for the machine. The defaults are fine: a WSL or "
        "a Hyper-V machine both work, and Hyper-V needs Windows Pro or "
        "Enterprise.\n"
        "  3. Start Ephemeral again. Distributed -> Repair runtime starts the "
        "machine if it was left stopped, then proves it with a real "
        "container.\n\n"
        "Engine only, no GUI: winget install " + PODMAN_CLI_WINGET + ".",
    )


def explain(state: RuntimeState) -> tuple[str, str]:
    """A terminal-window explanation for a PC that cannot run a block.

    The tray has exactly two feedback surfaces; this needs review (it is a dead
    end for the teacher until it is fixed), so it is text for the terminal
    window, never a toast.
    """
    fixes = {
        "no-wsl": (
            "Nothing here can run a block yet: Podman on Windows runs inside "
            "WSL2, and WSL2 is not enabled.\n\n"
            "Run Ephemeral-Distributed-Instructor-Setup.exe (it enables WSL2 "
            "and installs Podman), or in an administrator PowerShell run:\n\n"
            "    wsl --install --no-distribution\n\n"
            "then restart Windows and start the tray again.\n\n"
            "Podman Desktop does not enable WSL2 either, so install it after "
            "this step, not instead of it: Distributed -> Install Podman "
            "Desktop has the rest of its steps."
        ),
        "needs-reboot": (
            "WSL2 is installed but needs a restart to finish enabling.\n\n"
            "Restart Windows, then start the tray again — the classroom link "
            "appears once a container can actually run."
        ),
        "no-virtualization": (
            "This PC cannot run WSL2 because CPU virtualization is disabled in "
            "firmware.\n\n"
            "Enable Intel VT-x / AMD-V (sometimes called SVM) in BIOS/UEFI, "
            "save, and start the tray again. No installer can do this part."
        ),
        "no-podman": (
            "Podman is not installed, so there is no container runtime.\n\n"
            "Run Ephemeral-Distributed-Instructor-Setup.exe (offline, and it "
            "also enables WSL2), or use the vendor route: Distributed -> "
            "Install Podman Desktop.\n\n"
            f"The vendor route starts at {PODMAN_DESKTOP_URL} "
            "(winget install " + PODMAN_DESKTOP_WINGET + " works too), but "
            "WSL2 has to be enabled first or Podman Desktop cannot create "
            "its machine."
        ),
        "no-machine": (
            "Podman is installed but has no machine yet.\n\n"
            "Distributed -> Repair runtime creates one. It downloads the Podman "
            "WSL image (a few hundred MB) the first time."
        ),
        "machine-stopped": (
            "The Podman machine is not running.\n\n"
            "Distributed -> Repair runtime starts it."
        ),
    }
    body = fixes.get(state.stage, (
        "The container runtime is not usable: " + (state.detail or state.stage)
    ))
    if state.detail and state.stage in fixes:
        body += f"\n\nDetail: {state.detail}"
    return "Ephemeral needs a container runtime", body


# --- the installer's entry point ---------------------------------------------

def cli(argv: list[str] | None = None) -> int:
    """``--setup-prereqs``: the half of setup that needs elevation.

    Called by the setup bundle, which is already elevated under the user's
    consent to install, so this never asks for elevation itself — it reports
    :data:`ELEVATION_REQUIRED` and lets the bundle fail loudly instead.
    ``--check`` answers the same question without changing anything, which is
    what CI and support use.
    """
    args = list(sys.argv[1:] if argv is None else argv)
    if "--check" in args:
        state = status()
        say(f"[setup] check: {state.stage}: {state.detail or 'ok'}")
        if state.ok:
            return OK
        return REBOOT_REQUIRED if state.stage == "needs-reboot" else FAILED
    if sys.platform != "win32":
        say("[setup] no Windows prerequisites on this platform")
        return OK

    # Fast, non-destructive path: Burn runs this package on repair too, and a
    # machine that is already fine should not pay for a DISM pass.
    state = wsl_status()
    if state.ok:
        say("[setup] WSL2 is already enabled")
        return OK
    if state.stage == "needs-reboot":
        say("[setup] " + describe_exit(REBOOT_REQUIRED))
        return REBOOT_REQUIRED
    if state.stage == "no-virtualization":
        say("[setup] " + state.detail)
        return FAILED

    code = enable_wsl()
    say(f"[setup] {describe_exit(code)}")
    return code
