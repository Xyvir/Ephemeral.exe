"""
Tests for the tray front end's always-on-node plumbing.

Covers the parts that must not regress:

* the login entry the "Run Node on Login" menu item writes — deliberately
  per-user and unprivileged. The first version of this feature re-launched
  itself elevated and registered a ``/SC ONSTART /RU SYSTEM`` scheduled task
  from an unsigned onefile exe, and Windows Defender flagged the build;
* ``--node`` dispatch: the headless node that login entry starts;
* packaging invariants for the distributed-beta artifact (onedir bundle, no
  UPX, version resource), which exists so the antivirus-sensitive path has a
  bundle that is not a self-extractor;
* the ClamAV canary, which must stay a soft signal: it may never fail a build,
  and it must never report "not scanned" as "clean".

No GUI, Podman, network, or elevated privilege is needed: the tray menu is
built through injected fakes, and the Windows checks run against a temporary
key under HKCU (Software) that they delete afterwards.

Run:  python test_ui.py
"""
from __future__ import annotations

import ast
import inspect
import os
import pathlib
import re
import subprocess
import sys
import tempfile
import textwrap
from types import SimpleNamespace

from ephemeral_ui import platform, tray
from ephemeral_ui.backends.base import Backend

try:
    import winreg
except ImportError:  # pragma: no cover - POSIX CI
    winreg = None

ROOT = pathlib.Path(__file__).resolve().parent

#: HKCU key the Windows tests use instead of the real Run key.
TEST_RUN_KEY = r"Software\EphemeralUiTests"


# --- fakes ---------------------------------------------------------------

class _FakeMenu(list):
    """Stand-in for ``pystray.Menu`` (iterable, callable like the real one)."""

    def __init__(self, *items):
        super().__init__(items)

    def __call__(self):
        return self


class _FakeMenuItem:
    """Stand-in for ``pystray.MenuItem`` that records text/kwargs."""

    def __init__(self, text, action=None, **kwargs):
        self.text = text
        self.action = action
        self.kwargs = kwargs
        self.menu = action if isinstance(action, _FakeMenu) else None


class _StubBackend:
    """Records the calls ``tray.run`` makes, so dispatch is observable."""

    def __init__(self):
        self.calls = []

    def self_check(self):
        self.calls.append("self-check")
        return 0

    def start_background(self):
        self.calls.append("start_background")

    def run_node_forever(self):
        self.calls.append("run_node_forever")
        return 7


def _collect_texts(items, found=None):
    """Every menu label reachable from ``items`` (recursing into submenus)."""
    found = [] if found is None else found
    for it in items:
        found.append(it.text)
        if it.menu is not None:
            _collect_texts(it.menu, found)
    return found


# --- node login entry ----------------------------------------------------

def test_node_command_runs_the_app_in_place():
    autostart = platform.NodeAutostart("Ephemeral-Distributed", "Ephemeral-Distributed")
    cmd = autostart.node_command()

    assert cmd.endswith(" --node"), cmd
    # The app is registered where it lives: an onedir bundle cannot be copied
    # out of its folder, and a copy into AppData reads as dropper behavior.
    install_path = platform.StartupManager("Ephemeral-Distributed", "x").get_install_path()
    assert install_path not in cmd, cmd
    print("PASS: the node login entry runs the app in place with --node")


def test_node_command_prefers_the_appimage_path():
    old = os.environ.get("APPIMAGE")
    os.environ["APPIMAGE"] = os.path.join(tempfile.gettempdir(), "Ephemeral.AppImage")
    try:
        cmd = platform.NodeAutostart("ephemeral-distributed", "d").node_command()
    finally:
        if old is None:
            os.environ.pop("APPIMAGE", None)
        else:
            os.environ["APPIMAGE"] = old
    # sys.executable inside an AppImage is a per-launch temp mount.
    assert cmd.startswith('"/') or cmd.startswith('"C:'), cmd
    assert "Ephemeral.AppImage" in cmd and sys.executable not in cmd, cmd
    print("PASS: the node login entry uses the stable AppImage path")


def test_node_value_name_does_not_collide_with_the_tray_autostart():
    node = platform.NodeAutostart("Ephemeral-Distributed", "Ephemeral-Distributed")
    tray_autostart = platform.StartupManager("Ephemeral-Distributed", "Ephemeral-Distributed")

    assert node.value_name == "Ephemeral-Distributed-Node"
    assert node.value_name != tray_autostart.app_key
    print("PASS: node and tray autostart use distinct login entries")


def test_onedir_bundles_are_never_copied_out_of_their_folder():
    sm = platform.StartupManager("Ephemeral-Distributed", "Ephemeral-Distributed")
    bundle = os.path.join(tempfile.gettempdir(), "Ephemeral-Distributed-Beta")
    exe = os.path.join(bundle, "Ephemeral-Distributed-Beta.exe")
    old = (getattr(sys, "frozen", False), getattr(sys, "_MEIPASS", None), sys.executable)
    try:
        sys.frozen = True
        sys.executable = exe
        # onedir layout: _MEIPASS is <bundle>/_internal, next to the exe.
        sys._MEIPASS = os.path.join(bundle, "_internal")
        assert sm._copyable_for_autostart() is False
        # onefile layout: _MEIPASS is a temp extraction dir, unrelated to the exe.
        sys._MEIPASS = os.path.join(tempfile.gettempdir(), "_MEI123456")
        assert sm._copyable_for_autostart() is True
    finally:
        sys.frozen, sys._MEIPASS, sys.executable = old
    print("PASS: onedir bundles register in place; onefile copies still work")


def _cleanup_test_run_key():
    if winreg is None:
        return
    try:
        with winreg.OpenKey(winreg.HKEY_CURRENT_USER, TEST_RUN_KEY, 0,
                            winreg.KEY_ALL_ACCESS) as key:
            for name in ("Ephemeral-Distributed-Node",):
                try:
                    winreg.DeleteValue(key, name)
                except FileNotFoundError:
                    pass
        winreg.DeleteKey(winreg.HKEY_CURRENT_USER, TEST_RUN_KEY)
    except OSError:
        pass


def test_windows_login_entry_is_per_user_and_never_elevated():
    autostart = platform.NodeAutostart("Ephemeral-Distributed", "Ephemeral-Distributed",
                                       run_key=TEST_RUN_KEY)
    try:
        autostart.set_node_autostart(False)  # start from a clean key
        assert autostart.check_node_autostart() is False

        autostart.set_node_autostart(True)
        assert autostart.check_node_autostart() is True
        with winreg.OpenKey(winreg.HKEY_CURRENT_USER, TEST_RUN_KEY, 0,
                            winreg.KEY_QUERY_VALUE) as key:
            stored, _ = winreg.QueryValueEx(key, autostart.value_name)
        assert stored.endswith(" --node"), stored
        for needle in ("SYSTEM", "runas", "schtasks", "/RL HIGHEST"):
            assert needle not in stored, stored

        autostart.set_node_autostart(False)
        assert autostart.check_node_autostart() is False
    finally:
        _cleanup_test_run_key()
    print("PASS: Windows login entry is a per-user Run value ending in --node")


def test_linux_login_entry_is_an_in_place_desktop_file():
    home = tempfile.mkdtemp(prefix="ephemeral-home-")
    old_home = os.environ.get("HOME")
    os.environ["HOME"] = home
    try:
        autostart = platform.NodeAutostart("ephemeral-distributed", "Ephemeral-Distributed")
        autostart.set_node_autostart(True)
        path = autostart._desktop_path()
        assert path.startswith(home), path
        body = pathlib.Path(path).read_text(encoding="utf-8")
        assert body.startswith("[Desktop Entry]"), body
        assert "--node" in body, body
        assert autostart.check_node_autostart() is True

        autostart.set_node_autostart(False)
        assert not os.path.exists(path)
        assert autostart.check_node_autostart() is False
    finally:
        if old_home is None:
            os.environ.pop("HOME", None)
        else:
            os.environ["HOME"] = old_home
    print("PASS: Linux login entry is an in-place autostart .desktop file")


def _strip_docstrings(source: str) -> str:
    """Python source re-emitted without docstrings and without comments."""
    tree = ast.parse(textwrap.dedent(source))
    for node in ast.walk(tree):
        if isinstance(node, (ast.Module, ast.ClassDef, ast.FunctionDef, ast.AsyncFunctionDef)):
            body = node.body
            if (body and isinstance(body[0], ast.Expr)
                    and isinstance(body[0].value, ast.Constant)
                    and isinstance(body[0].value.value, str)):
                node.body = body[1:] or [ast.Pass()]
    return ast.unparse(tree)


def _code_without_docstrings(obj) -> str:
    """``obj``'s code, with docstrings stripped and string arguments kept.

    Docstrings go so the guard can inspect the *implementation* while the
    class stays free to explain the flagged history in prose; the string
    arguments that make up a dangerous command line (``"runas"``, ``"/RU"``)
    survive stripping and stay detectable.
    """
    return _strip_docstrings(inspect.getsource(obj))


def test_node_autostart_contains_no_elevation_or_service_registration():
    code = _code_without_docstrings(platform.NodeAutostart)
    for needle in ("runas", "ShellExecuteW", "schtasks", "SYSTEM", "sc create",
                   "shutil.copy", "HKEY_LOCAL_MACHINE"):
        assert needle not in code, (
            f"{needle!r} must not appear in NodeAutostart: that combination is "
            "what got the unsigned build flagged by Windows Defender"
        )
    # Positive checks, so an emptied class can't pass the guard vacuously.
    assert "HKEY_CURRENT_USER" in code, "the login entry must be per-user (HKCU)"
    assert "CreateKeyEx" in code and "SetValueEx" in code, code
    print("PASS: NodeAutostart neither elevates nor registers a service")


# --- --node dispatch -----------------------------------------------------

def test_node_flag_runs_the_headless_node_without_a_gui():
    backend = _StubBackend()
    argv = sys.argv
    sys.argv = ["Ephemeral-Distributed-Beta.exe", "--node"]
    try:
        try:
            tray.run(backend)
            raise AssertionError("tray.run must exit on --node")
        except SystemExit as e:
            assert e.code == 7, e.code
    finally:
        sys.argv = argv
    # Node mode owns its own warmup: the tray must not start first.
    assert backend.calls == ["run_node_forever"], backend.calls
    print("PASS: --node runs the headless node and exits without the tray")


def test_node_hooks_are_inert_without_a_node():
    class _Minimal(Backend):
        app_key = "minimal"
        display_name = "Minimal"

        def about(self):
            return ""

        def startup_message(self):
            return ""

        def run_logic(self, icon, content=None):
            pass

        def quit(self, icon, item_unused=None):
            pass

        def force_stop_all(self, icon, item_unused=None):
            pass

        def purge_cache(self, icon, item_unused=None):
            pass

        def self_check(self):
            return 0

    backend = _Minimal()
    assert backend.check_node_autostart() is False
    backend.toggle_node_autostart(None)  # no-op: never raises
    assert backend.run_node_forever() == 2  # --node is a usage error here
    print("PASS: a backend without a node offers no autostart and rejects --node")


# --- distributed menu ---------------------------------------------------

def test_distributed_menu_offers_a_login_scoped_node():
    from ephemeral_ui.backends.distributed import DistributedBackend

    # Without GUI deps platform.item/platform.pystray do not exist at all, so
    # the fakes are installed (and removed) rather than swapped.
    real_item = getattr(platform, "item", None)
    real_pystray = getattr(platform, "pystray", None)
    platform.item = _FakeMenuItem
    platform.pystray = SimpleNamespace(Menu=_FakeMenu)
    try:
        items = DistributedBackend().extra_menu_items(None)
        texts = _collect_texts(items)
    finally:
        for name, value in (("item", real_item), ("pystray", real_pystray)):
            if value is None:
                try:
                    delattr(platform, name)
                except AttributeError:
                    pass
            else:
                setattr(platform, name, value)

    assert "Distributed" in texts, texts
    assert "Run Node on Login" in texts, texts
    assert "Private Mode" in texts, texts
    joined = " ".join(texts).lower()
    for needle in ("service", "system", "boot"):
        assert needle not in joined, (
            f"the Distributed menu must not promise {needle!r}: the node starts "
            "at login for one user, and a boot-time service is what was flagged"
        )
    print("PASS: the Distributed menu offers a login-scoped node, not a service")


# --- packaging invariants -----------------------------------------------

def test_beta_bundle_is_not_a_self_extractor():
    spec = (ROOT / "packaging" / "ephemeral-distributed-beta.spec").read_text(encoding="utf-8")
    code = _strip_docstrings(spec)  # prose may name what the build avoids
    assert "COLLECT(" in code, "the beta bundle must be a onedir COLLECT"
    assert "upx=False" in code, "UPX packing must stay off in the beta bundle"
    assert "onefile" not in code, "the beta bundle must not request a onefile build"
    assert "version=VERSION_FILE" in code, "the beta exe must carry a version resource"
    version_info = (ROOT / "packaging" / "version_info.txt").read_text(encoding="utf-8")
    assert "Ephemeral-Distributed-Beta.exe" in version_info
    assert "Version number (injected from the github workflow)" in version_info
    print("PASS: the distributed-beta bundle is onedir, un-UPXed, with metadata")


def test_ci_builds_and_ships_the_beta_bundle():
    workflow = (ROOT / ".github" / "workflows" / "build.yml").read_text(encoding="utf-8")
    assert "build-exe-distributed-beta:" in workflow
    assert "packaging/ephemeral-distributed-beta.spec" in workflow
    assert "Ephemeral-Distributed-Beta-win64.zip" in workflow

    beta_job = workflow.split("build-exe-distributed-beta:", 1)[1]
    beta_job = beta_job.split("build-linux-appimage:", 1)[0]
    assert "--onefile" not in beta_job, "the beta job must not build a onefile exe"
    assert "--self-check" in beta_job, "the beta job must smoke-test the bundle"

    release = workflow.split("release:", 1)[1]
    assert "dist/Ephemeral-Distributed-Beta-win64.zip" in release
    assert "build-exe-distributed-beta" in release
    print("PASS: CI builds, smoke-tests and releases the beta bundle")


def _write(path: pathlib.Path, text: str) -> None:
    """Write exactly ``text`` — never CRLF: bash chokes on a trailing \\r."""
    with open(path, "w", encoding="utf-8", newline="\n") as fh:
        fh.write(text)


def _clamscan_stub(exit_code: int) -> str:
    """Stand-in for clamscan: writes a report through --log= and exits as told.

    Deliberately bash, so the child needs no Python on PATH and behaves the
    same under Git Bash and on the Linux CI runner.
    """
    verdict = ("Thing.exe: Win.Test.FAKE FOUND" if exit_code == 1
               else "Thing.exe: OK")
    return (
        "#!/usr/bin/env bash\n"
        'if [ "${1:-}" = "--version" ]; then\n'
        '  echo "ClamAV 0.0 test stub"\n'
        "  exit 0\n"
        "fi\n"
        'for arg in "$@"; do\n'
        "  case \"$arg\" in\n"
        '    --log=*) log="${arg#--log=}" ;;\n'
        "  esac\n"
        "done\n"
        'if [ -n "${log:-}" ]; then\n'
        f"  printf '{verdict}\\n' > \"$log\"\n"
        "fi\n"
        f"exit {exit_code}\n"
    )


def _run_canary(tmp: str, exit_code: int | None = None, stub: bool = True):
    """Run the canary with a stubbed scanner; return (proc, step-summary text).

    Everything is passed as relative paths with the temp directory as cwd, so
    no Windows/POSIX path conversion or child-environment propagation is
    needed — the same invocation works on Git Bash and on the Linux runner.
    """
    tmp_path = pathlib.Path(tmp)
    artifacts = tmp_path / "artifacts"
    artifacts.mkdir(exist_ok=True)
    (artifacts / "Thing.exe").write_bytes(b"MZ not really a PE")
    scanner = "./clamscan" if stub else "./missing-clamscan"
    if stub:
        executable = tmp_path / "clamscan"
        _write(executable, _clamscan_stub(exit_code or 0))
        os.chmod(executable, 0o755)
    _write(tmp_path / "scan_artifacts.sh",
           (ROOT / "scripts" / "scan_artifacts.sh").read_text(encoding="utf-8"))
    proc = subprocess.run(
        ["bash", "./scan_artifacts.sh", "artifacts",
         "--scanner", scanner, "--report", "report.txt", "--summary", "summary.md"],
        cwd=tmp, capture_output=True, text=True, encoding="utf-8", errors="replace",
    )
    summary = tmp_path / "summary.md"
    return proc, (summary.read_text(encoding="utf-8") if summary.exists() else "")


def test_clamav_canary_is_soft_and_reports_honestly():
    """Every outcome exits 0, and "not scanned" is never reported as "clean"."""
    for exit_code, in_output, in_summary in (
        (0, "clean", "**clean**"),
        (1, "DETECTIONS FOUND", "**detection(s) found**"),
        (2, "did NOT complete", "**did not complete**"),
    ):
        # ignore_cleanup_errors: these tests spawn child processes in the temp
        # dir, and Windows can still hold a handle when cleanup runs.
        with tempfile.TemporaryDirectory(ignore_cleanup_errors=True) as tmp:
            proc, summary = _run_canary(tmp, exit_code=exit_code)
            assert proc.returncode == 0, (exit_code, proc.returncode, proc.stdout, proc.stderr)
            assert in_output in proc.stdout, (exit_code, proc.stdout, proc.stderr)
            assert in_summary in summary, (exit_code, summary)
            if exit_code == 1:
                # A detection must be surfaced, and still not block.
                assert "Win.Test.FAKE FOUND" in proc.stdout, proc.stdout
                assert "NOT blocked" in proc.stdout, proc.stdout

    with tempfile.TemporaryDirectory(ignore_cleanup_errors=True) as tmp:
        proc, summary = _run_canary(tmp, stub=False)
        assert proc.returncode == 0, (proc.stdout, proc.stderr)
        assert "NOT SCANNED" in proc.stdout, proc.stdout
        assert "did not run" in summary, summary
        assert "clean" not in summary, summary
    print("PASS: the ClamAV canary is soft in every outcome and never fakes a clean scan")


def test_ci_clamav_canary_cannot_gate_a_release():
    workflow = (ROOT / ".github" / "workflows" / "build.yml").read_text(encoding="utf-8")
    assert "scan-artifacts:" in workflow
    job = workflow.split("scan-artifacts:", 1)[1].split("release:", 1)[0]
    assert "continue-on-error: true" in job, "the canary must never fail the job"
    assert "scripts/scan_artifacts.sh" in job, job[:400]
    assert "freshclam" in job, "the signature database must be refreshed"

    release_deps = workflow.split("release:", 1)[1].split("steps:", 1)[0]
    assert "scan-artifacts" not in release_deps, (
        "a soft gate must not be a release dependency"
    )

    script = (ROOT / "scripts" / "scan_artifacts.sh").read_text(encoding="utf-8")
    # clamscan's 25M default would silently skip the ~28 MB one-file builds and
    # report them clean; the caps must stay explicit and raised.
    assert '--max-filesize="$MAX_FILE"' in script, script
    assert '--max-scansize="$MAX_SCAN"' in script, script
    assert "MAX_FILE:-300M" in script and "MAX_SCAN:-600M" in script, script
    assert "exit 0" in script, "the canary must always exit 0"
    print("PASS: the ClamAV canary is CI-wired, non-blocking, and cannot silently skip files")


# --- MSI: machine-wide install of the distributed beta -------------------

MSI_DIR = ROOT / "packaging" / "msi"


def _msi_text(name):
    return (MSI_DIR / name).read_text(encoding="utf-8")


def test_msi_is_machine_wide_and_declarative():
    """The installer must serve every user, and change nothing it cannot undo."""
    wxs = _msi_text("Ephemeral.Distributed.Beta.wxs")

    assert 'InstallScope="perMachine"' in wxs, "the MSI must install machine-wide"
    assert 'InstallPrivileges="elevated"' in wxs, (
        "machine-wide needs the privileges up front; without them the install "
        "fails halfway through a write to %ProgramFiles%")
    assert "ProgramFiles64Folder" in wxs, "the app belongs in %ProgramFiles%"
    assert "LocalAppDataFolder" not in wxs, (
        "a per-user install dir is what made this per-user in the first place")

    # One login entry for the whole machine, so the node starts for ANY user.
    assert "HKEY_CURRENT_USER" not in wxs and 'Root="HKCU"' not in wxs, (
        "every registry value must be HKLM: HKCU would only cover whoever ran "
        "the installer")
    marker = wxs.index('Name="Ephemeral-Distributed-Node"')
    entry = wxs[marker:marker + 400]
    assert "--node" in entry, "the machine-wide login entry must start the node"
    assert "[INSTALLDIR]Ephemeral-Distributed-Beta.exe" in entry, (
        "the entry must point at the installed exe, not a copy")

    # Declarative only: no custom actions, no service, no scheduled task.
    # Checked against markup, not prose: the comments above explain what is
    # deliberately absent and must not be able to fail (or satisfy) this.
    markup = re.sub(r"<!--.*?-->", "", wxs, flags=re.S)
    assert "<CustomAction" not in markup, (
        "custom actions that write the registry or launch processes are the "
        "shape installer heuristics hunt for")
    for forbidden in ("ServiceInstall", "ServiceControl", "schtasks", "sc create"):
        assert forbidden not in markup, f"{forbidden} must not appear in the installer"
    assert "UpgradeCode=\"00B73DB1-B66B-4A71-9AE7-3C3E9E1C6971\"" in wxs, (
        "the UpgradeCode must stay stable or upgrades duplicate the product")
    # CI must actually build it, prove it installs, and ship it with the release.
    workflow = (ROOT / ".github" / "workflows" / "build.yml").read_text(encoding="utf-8")
    assert "packaging/msi/build_msi.sh" in workflow, "CI must build the MSI"
    # msiexec needs a Windows path: it was handed the POSIX-relative
    # `dist/...msi` and answered 1619, "This installation package could not be
    # opened", in the time it takes to look at the argument. cygpath -w is the
    # fix, so assert the resolved variable rather than the literal path.
    assert 'MSI="$(cygpath -w "$PKG")"' in workflow, (
        "the smoke test must hand msiexec an absolute Windows path")
    assert 'msiexec /i "$MSI" /qn' in workflow, (
        "CI must prove the MSI installs, not just that it links")
    assert 'msiexec /x "$MSI" /qn' in workflow, (
        "...and that it uninstalls again, or the smoke test leaks an install")
    for relative in ("msiexec /i dist/", "msiexec /x dist/"):
        assert relative not in workflow, (
            "a POSIX-relative package path is the 1619 'could not be opened' "
            "case, not shorthand for the same thing")
    assert 'if [ ! -f "$PKG" ]' in workflow, (
        "the smoke test must say the MSI is missing instead of letting msiexec "
        "report it as a package it could not open")
    assert "Ephemeral-Distributed-Node" in workflow, (
        "the machine-wide Run value is the claim this package makes, so CI has "
        "to check it and not only that files landed")
    assert "--artifact dist/Ephemeral-Distributed-Beta.msi" in workflow, (
        "the MSI's hash must reach the build manifest consumers verify against")
    assert "dist/Ephemeral-Distributed-Beta.msi" in workflow, (
        "the MSI must be among the release's uploaded files")
    print("PASS: the MSI installs machine-wide, per-machine resources only, declaratively")


def test_msi_suppresses_only_the_per_user_shortcut_checks():
    """Exactly two ICE checks are waived, and they must not silently grow."""
    script = _msi_text("build_msi.sh")
    # Comments explain the suppressions; only real command lines count.
    code = "\n".join(
        line for line in script.splitlines() if not line.lstrip().startswith("#"))

    suppressed = set()
    for line in code.splitlines():
        for flag in line.replace("\\", " ").split():
            if flag.startswith("-sice:"):
                suppressed.add(flag.split(":", 1)[1])
    assert suppressed == {"ICE43", "ICE57"}, (
        f"suppressions changed: {sorted(suppressed)} - ICE43/ICE57 are the two "
        "that assume a per-user install; anything else needs its own reason")
    assert "-sval" not in code, "validation must never be switched off wholesale"

    # The version rules Windows Installer enforces, checked in the build script.
    assert "major<256" in script and "build<65536" in script, (
        "Windows Installer rejects out-of-range versions; the script must say so")
    assert "finalize_harvest.py" in script, "the harvest post-processor must run"
    print("PASS: only ICE43/ICE57 are waived, with validation otherwise on")


def test_msi_harvest_postprocessor_covers_every_directory():
    """Heat emits no RemoveFolder rows; the post-processor must add them all."""
    harvest = """<?xml version="1.0" encoding="utf-8"?>
<Wix xmlns="http://schemas.microsoft.com/wix/2006/wi">
  <Fragment>
    <DirectoryRef Id="INSTALLDIR">
      <Directory Id="dirInternal" Name="_internal">
        <Directory Id="dirEmptyParent" Name="tcl8">
          <Directory Id="dirChild" Name="8.6" />
        </Directory>
      </Directory>
    </DirectoryRef>
  </Fragment>
  <Fragment>
    <ComponentGroup Id="HarvestedFiles">
      <Component Id="cmpExe" Directory="INSTALLDIR" Guid="*">
        <File Id="filExe" KeyPath="yes" Source="$(var.SourceDir)\\app.exe" />
      </Component>
      <Component Id="cmpDll" Directory="dirInternal" Guid="*">
        <File Id="filDll" KeyPath="yes" Source="$(var.SourceDir)\\_internal\\a.dll" />
      </Component>
      <Component Id="cmpLeaf" Directory="dirChild" Guid="*">
        <File Id="filLeaf" KeyPath="yes" Source="$(var.SourceDir)\\_internal\\tcl8\\8.6\\init.tcl" />
      </Component>
    </ComponentGroup>
  </Fragment>
</Wix>
"""
    with tempfile.TemporaryDirectory(ignore_cleanup_errors=True) as tmp:
        path = pathlib.Path(tmp) / "Harvested.wxs"
        path.write_text(harvest, encoding="utf-8")
        script = MSI_DIR / "finalize_harvest.py"

        def run():
            return subprocess.run(
                [sys.executable, str(script), str(path)],
                capture_output=True, text=True,
            )

        proc = run()
        assert proc.returncode == 0, proc.stderr
        out = path.read_text(encoding="utf-8")

        for directory in ("INSTALLDIR", "dirInternal", "dirEmptyParent", "dirChild"):
            assert f'Id="rmf_{directory}"' in out, f"{directory} would survive uninstall"
        # A parent that owns no component keeps its entry on the root component
        # and must name itself, or uninstall would remove the install root twice.
        orphan = out.split('Id="rmf_dirEmptyParent"')[1].split("/>")[0]
        assert 'Directory="dirEmptyParent"' in orphan, (
            "a borrowed RemoveFolder must name its directory explicitly")

        # Idempotent: running twice must not duplicate rows.
        assert run().returncode == 0
        again = path.read_text(encoding="utf-8")
        assert again.count('Id="rmf_dirInternal"') == 1
    print("PASS: every harvested directory gets a RemoveFolder, idempotently")


def main():
    test_node_command_runs_the_app_in_place()
    test_node_command_prefers_the_appimage_path()
    test_node_value_name_does_not_collide_with_the_tray_autostart()
    test_onedir_bundles_are_never_copied_out_of_their_folder()
    if sys.platform == "win32" and winreg is not None:
        test_windows_login_entry_is_per_user_and_never_elevated()
    else:
        print("SKIP: Windows Run-key test - not a Windows host")
    if sys.platform != "win32":
        test_linux_login_entry_is_an_in_place_desktop_file()
    else:
        print("SKIP: Linux autostart test - not a POSIX host")
    test_node_autostart_contains_no_elevation_or_service_registration()
    test_node_flag_runs_the_headless_node_without_a_gui()
    test_node_hooks_are_inert_without_a_node()
    test_distributed_menu_offers_a_login_scoped_node()
    test_beta_bundle_is_not_a_self_extractor()
    test_ci_builds_and_ships_the_beta_bundle()
    test_clamav_canary_is_soft_and_reports_honestly()
    test_ci_clamav_canary_cannot_gate_a_release()
    test_msi_is_machine_wide_and_declarative()
    test_msi_suppresses_only_the_per_user_shortcut_checks()
    test_msi_harvest_postprocessor_covers_every_directory()
    print("\n=== ALL TRAY / NODE TESTS PASSED ===")


if __name__ == "__main__":
    main()
