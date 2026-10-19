"""Tests for the container-runtime prerequisite and the setup bundle.

These exist because the interesting failure is not a crash — it is a classroom
setup that *looks* finished. The instructor build ends setup by showing a
student link; if a fresh PC cannot run a container behind that link, the link is
a promise the machine cannot keep. So two things are pinned here:

* the gate: no link until the runtime is proven with a real container, and a
  precise explanation when it is not;
* the bundle: WSL2, then Podman, then the app, with nothing removable and no
  elevation outside the single prompt the user already accepted.

The one bug this file was written around is real and was found by running the
code on Windows: ``wsl.exe`` writes UTF-16LE with no BOM, so a UTF-8 decode
interleaved NULs, every keyword missed, and a healthy machine was reported as
having no WSL. ``decode_console`` is pinned first for that reason.

No WSL, Podman, network, or elevation is needed: detection parsing is pure, and
the gate is driven through the same functions the tray calls.
"""
from __future__ import annotations

import re
import sys
import xml.etree.ElementTree as ET
from pathlib import Path

from ephemeral_ui import platform, prereq

ROOT = Path(__file__).resolve().parent
WIX_NS = {"w": "http://schemas.microsoft.com/wix/2006/wi"}


class _FakeIcon:
    """Just enough tray icon for a backend to call ``notify`` on."""

    def __init__(self) -> None:
        self.messages: list[str] = []

    def notify(self, message, title=None):
        self.messages.append(message)


def test_console_output_is_decoded_the_way_windows_writes_it():
    """``wsl.exe`` speaks UTF-16LE with no BOM; UTF-8 decoding hides everything.

    Found by running it: the decoded text was 'D\\x00e\\x00f\\x00a...', every
    keyword check missed, and a Windows 11 machine with WSL 2 installed was
    reported as ``no-wsl`` — which would have sent a teacher to install what
    they already had.
    """
    text = "Default Distribution: Ubuntu-24.04\r\nDefault Version: 2"
    raw = text.encode("utf-16-le")
    assert b"\x00" in raw, "the fixture must really be NUL-interleaved"
    assert prereq.decode_console(raw) == text
    assert prereq.decode_console(b"\xff\xfe" + raw) == text, "a BOM must be handled too"
    assert prereq.decode_console(text.encode("utf-8")) == text, "UTF-8 must still work"
    assert prereq.decode_console(b"") == ""
    # ...and the classification above it must then see real words.
    assert prereq.classify_wsl_status(0, prereq.decode_console(raw)).ok


def test_cgroup_delegation_failure_is_recognised_and_nothing_else():
    """Only the WSL2 'controllers not delegated' canary failure triggers repair."""
    fault = (
        'Error: crun: the requested cgroup controller `memory` is not available: '
        'OCI runtime error'
    )
    assert prereq._is_cgroup_delegation_error(fault)
    assert prereq._is_cgroup_delegation_error("open cgroup.controllers: no such file")
    assert not prereq._is_cgroup_delegation_error("Error: image not known")
    assert not prereq._is_cgroup_delegation_error("")


def test_ensure_canary_repairs_cgroup_delegation_once_then_retries():
    """A delegation fault is repaired on the machine and the canary re-proven.

    The repair is idempotent and unprivileged (``podman machine ssh``), so the
    tray may call it on every first run; a second failure must be reported with
    the repair's own output rather than looping.
    """
    real = (prereq.canary, prereq.repair_cgroup_delegation, sys.platform)
    results = iter([(False, "crun: controller `memory` is not available"), (True, "")])
    repairs = []
    try:
        sys.platform = "win32"
        prereq.canary = lambda timeout=600: next(results)
        prereq.repair_cgroup_delegation = lambda: (repairs.append(1) or True, "ok")
        assert prereq.ensure_canary() == (True, "")
        assert repairs == [1]

        # A repair that fails is reported as such, with the original detail.
        prereq.canary = lambda timeout=600: (False, "controller `cpu` is not available")
        prereq.repair_cgroup_delegation = lambda: (False, "ssh refused")
        ok, detail = prereq.ensure_canary()
        assert not ok
        assert "cgroup repair failed: ssh refused" in detail
        assert "controller `cpu` is not available" in detail

        # Unrelated failures are not 'repaired' at all.
        repairs.clear()
        prereq.canary = lambda timeout=600: (False, "Error: image not known")
        prereq.repair_cgroup_delegation = lambda: (repairs.append(1) or True, "")
        assert prereq.ensure_canary() == (False, "Error: image not known")
        assert repairs == []
    finally:
        prereq.canary, prereq.repair_cgroup_delegation, sys.platform = real


def test_setup_survives_a_windowed_build_with_no_stdout():
    """The shipped exe has no console, so a print is a silent no-op.

    PyInstaller builds the instructor exe with ``console=False``. Verified on
    this machine rather than assumed: ``print()`` to a ``None`` stream neither
    raises nor records anything, so a prerequisite that failed inside setup
    would leave *no* trace — and CI, which greps output, would pass while
    asserting nothing. ``prereq.say`` writes to ``<state dir>/setup.log`` for
    exactly this reason, and this pins it.
    """
    import io
    import os
    import tempfile

    saved_out, saved_err = sys.stdout, sys.stderr
    saved_dir = os.environ.get("EPHEMERAL_STATE_DIR")
    noisy = io.StringIO()
    try:
        with tempfile.TemporaryDirectory() as tmp:
            os.environ["EPHEMERAL_STATE_DIR"] = tmp
            log = Path(tmp) / "setup.log"

            # With a stream, the line goes to both the stream and the log.
            sys.stdout, sys.stderr = noisy, noisy
            prereq.say("visible")
            assert "visible" in noisy.getvalue()
            assert log.read_text(encoding="utf-8").strip() == "visible"

            # Windowed: no streams at all. Nothing raises, and the log is the
            # only place the decision survives.
            sys.stdout = None
            sys.stderr = None
            code = prereq.cli(["--check"])
            assert isinstance(code, int), code
            body = log.read_text(encoding="utf-8")
            assert "[setup] check:" in body, body
            assert any(
                f"check: {stage}" in body
                for stage in ("ready", "no-wsl", "needs-reboot",
                              "no-virtualization", "no-podman", "no-machine",
                              "machine-stopped")
            ), f"the state must be named, not just numbered: {body}"
    finally:
        sys.stdout, sys.stderr = saved_out, saved_err
        if saved_dir is None:
            os.environ.pop("EPHEMERAL_STATE_DIR", None)
        else:
            os.environ["EPHEMERAL_STATE_DIR"] = saved_dir


def test_wsl_and_machine_states_are_classified():
    """Every state a teacher can land in has a name, so the fix can be named."""
    assert prereq.classify_wsl_status(0, "Default Version: 2").stage == "ready"
    # Microsoft's wording for "you never enabled it", verbatim in shape.
    assert prereq.classify_wsl_status(
        1,
        "The Windows Subsystem for Linux has not been enabled. Please enable "
        "it by running 'wsl.exe --install'",
    ).stage == "no-wsl"
    assert prereq.classify_wsl_status(
        1, "Please restart your machine to complete the installation"
    ).stage == "needs-reboot"
    assert prereq.classify_wsl_status(
        1, "WSL2 is not supported on this machine: virtualization is not supported"
    ).stage == "no-virtualization"
    # An unrecognised failure must not be read as healthy.
    assert prereq.classify_wsl_status(1, "something new and unexplained").stage == "no-wsl"
    # ...but a success with unrecognised output must not be read as broken.
    assert prereq.classify_wsl_status(0, "some future wording").stage == "ready"

    assert prereq.classify_machine_list(0, "[]").stage == "no-machine"
    assert prereq.classify_machine_list(1, "error").stage == "no-machine"
    assert prereq.classify_machine_list(0, "not json").stage == "no-machine"
    assert prereq.classify_machine_list(
        0, '[{"Name": "podman-machine-default", "Running": true}]'
    ).ok
    assert prereq.classify_machine_list(
        0, '[{"Name": "podman-machine-default", "Running": false}]'
    ).stage == "machine-stopped"


def test_canary_uses_the_image_the_node_wants_anyway():
    """The proof must not cost an extra download on a fresh classroom PC."""
    from ephemeral_ui.backends.distributed import PREHYDRATE_IMAGE

    assert prereq.CANARY_IMAGE == PREHYDRATE_IMAGE, (
        "the first-run canary must pull the image prehydration already wants, "
        "or every fresh install pays for two pulls")


def test_no_classroom_link_before_a_container_has_run():
    """The gate: explain, or prove — never promise.

    Three shapes, all driven through the method the tray actually starts:
    a missing runtime explains and starts nothing; a runtime that cannot run a
    container explains and withholds the link; only a passing container
    announces a URL students could dial.
    """
    from ephemeral_ui.backends.distributed import InstructorBackend

    events: list[str] = []
    shown: list[tuple[str, str]] = []
    real_status, real_canary = prereq.status, prereq.canary
    real_window = platform.show_terminal_window
    platform.show_terminal_window = lambda title, body: shown.append((title, body))

    def announce(state, canary):
        backend = InstructorBackend()
        backend._ensure_cluster = lambda: events.append("cluster")
        backend._announce_private = lambda *a, **k: events.append("announce")
        backend.current_student_url = lambda: "https://x/#seed=endpointA"
        prereq.status = lambda: state
        prereq.canary = lambda *a, **k: canary
        backend._announce_first_run(None)

    try:
        # 1. Nothing can run yet: say so, do not even start the node.
        announce(prereq.RuntimeState("no-wsl", "not enabled"), (True, ""))
        assert events == [], f"nothing should start on a broken PC: {events}"
        assert shown and "WSL2" in shown[0][1], shown

        # 2. Runtime nominally ready, but the container failed: no link.
        events.clear(); shown.clear()
        announce(prereq.RuntimeState("ready"), (False, "Error: image pull failed"))
        assert events == ["cluster"], events
        assert shown and "classroom link is shown yet" in shown[0][1], shown

        # 3. Proven: announce the link students join.
        events.clear(); shown.clear()
        announce(prereq.RuntimeState("ready"), (True, ""))
        assert events == ["cluster", "announce"], events
        assert not shown, f"no explanation is due on the happy path: {shown}"
    finally:
        prereq.status, prereq.canary = real_status, real_canary
        platform.show_terminal_window = real_window


def test_repair_runtime_creates_the_machine_then_proves_it():
    """The menu path exists so a dead end has a next step, not a support ticket."""
    from ephemeral_ui.backends.distributed import InstructorBackend

    shown: list[tuple[str, str]] = []
    calls: list[str] = []
    real = (prereq.status, prereq.ensure_machine, prereq.canary)
    real_window = platform.show_terminal_window
    platform.show_terminal_window = lambda title, body: shown.append((title, body))
    try:
        prereq.status = lambda: prereq.RuntimeState("no-machine", "no machine")
        prereq.ensure_machine = lambda: (calls.append("ensure") or (True, "created"))
        prereq.canary = lambda *a, **k: (calls.append("canary") or (True, ""))
        InstructorBackend().repair_runtime(_FakeIcon())
        assert calls == ["ensure", "canary"], calls
        assert shown and "works" in shown[0][1], shown

        # A failing machine must be reported, and the canary must not run.
        calls.clear(); shown.clear()
        prereq.ensure_machine = lambda: (calls.append("ensure") or (False, "init failed"))
        InstructorBackend().repair_runtime(_FakeIcon())
        assert calls == ["ensure"], calls
        assert shown and "could not be prepared" in shown[0][1], shown

        # WSL problems are not repaired here: that needs elevation.
        calls.clear(); shown.clear()
        prereq.status = lambda: prereq.RuntimeState("no-wsl", "not enabled")
        InstructorBackend().repair_runtime(_FakeIcon())
        assert calls == [], f"the tray must not try to enable WSL: {calls}"
        assert shown and "administrator" in shown[0][1].lower(), shown
    finally:
        prereq.status, prereq.ensure_machine, prereq.canary = real
        platform.show_terminal_window = real_window


def test_prerequisite_module_never_elevates_itself():
    """Elevation is the installer's, with the user's consent, or nobody's.

    AGENTS.md is explicit that an unsigned build must not ship a
    privilege-escalating path, and the first-run flow runs unprivileged in the
    user's session. Enabling WSL2 belongs to ``--setup-prereqs``, invoked by the
    bundle that is already elevated.
    """
    import ast

    src = (ROOT / "ephemeral_ui" / "prereq.py").read_text(encoding="utf-8")
    tree = ast.parse(src)
    # Docstrings explain *why* these are absent, so check executable code only:
    # drop docstrings, then unparse (comments never enter the AST).
    for node in ast.walk(tree):
        if isinstance(node, (ast.Module, ast.ClassDef, ast.FunctionDef, ast.AsyncFunctionDef)):
            node.body = [
                child for child in node.body
                if not (isinstance(child, ast.Expr)
                        and isinstance(child.value, ast.Constant)
                        and isinstance(child.value.value, str))
            ]
    code = ast.unparse(tree)
    for forbidden in ("runas", "ShellExecuteW", "Start-Process", "schtasks",
                      "sc create", "HKEY_LOCAL_MACHINE", "IsUserAnAdmin"):
        assert forbidden not in code, (
            f"{forbidden!r} would make the tray escalate privileges; "
            "the installer owns that prompt")
    # ...and the elevated half is reachable only through the install entry point.
    assert "def cli(" in src and "enable_wsl()" in src


def test_the_vendor_route_comes_after_wsl_and_never_replaces_it():
    """Podman Desktop is an escape hatch, and it cannot enable WSL2.

    The tempting simplification — "point teachers at the official installer" —
    breaks on one documented fact: Podman Desktop's Windows guide says "you
    must enable the WSL feature", and Podman's own guide says the installer
    "no longer automatically installs WSL". So a GUI-first teacher reaches a
    wizard that cannot create a machine, which is worse than the dead end we
    started from. Every surface that offers both routes must therefore put WSL2
    first and say plainly that the GUI does not do it.
    """
    assert prereq.PODMAN_DESKTOP_URL.startswith("https://podman-desktop.io/"), (
        prereq.PODMAN_DESKTOP_URL)
    assert prereq.PODMAN_DESKTOP_WINGET != prereq.PODMAN_CLI_WINGET
    assert prereq.PODMAN_DESKTOP_WINGET in ("RedHat.Podman-Desktop",)
    assert prereq.PODMAN_CLI_WINGET in ("Podman.CLI",)

    saved = sys.platform
    try:
        sys.platform = "win32"  # the branch that matters for the WSL wording
        title, body = prereq.desktop_guide()
    finally:
        sys.platform = saved
    assert "WSL2" in title or "Podman" in title, title
    assert prereq.PODMAN_DESKTOP_URL in body, body
    assert "does not enable WSL2" in body, body
    # Step order, not just presence: the enable-WSL command precedes the link.
    assert body.index("wsl --install") < body.index(prereq.PODMAN_DESKTOP_URL), body
    assert "Repair runtime" in body, "the guide must end where the tray can finish"

    # The in-state messages carry the same ordering guarantee.
    _, no_wsl = prereq.explain(prereq.RuntimeState("no-wsl", "not enabled"))
    assert "wsl --install" in no_wsl, no_wsl
    assert no_wsl.index("wsl --install") < no_wsl.index("Podman Desktop"), no_wsl
    assert "does not enable WSL2" in no_wsl, no_wsl

    _, no_podman = prereq.explain(prereq.RuntimeState("no-podman", "missing"))
    assert prereq.PODMAN_DESKTOP_URL in no_podman, no_podman
    assert "first" in no_podman.lower(), (
        "the engine route must say WSL2 comes first: " + no_podman)


def test_the_gui_escape_hatch_opens_the_page_and_keeps_the_url_visible():
    """The menu path hands over the vendor page without ever installing.

    Two shapes: a browser opens (and the steps still open in a terminal window,
    because the guide is longer than a toast), and no browser answers — a
    locked-down machine — in which case the URL has to remain readable in text.
    """
    from ephemeral_ui.backends.distributed import InstructorBackend

    opened: list[str] = []
    shown: list[tuple[str, str]] = []
    real_open, real_window = platform.open_url, platform.show_terminal_window
    platform.show_terminal_window = lambda title, body: shown.append((title, body))
    try:
        platform.open_url = lambda url: (opened.append(url) or True)
        InstructorBackend().show_podman_guide(_FakeIcon())
        assert opened == [prereq.guide_url()], opened
        assert shown and prereq.guide_url() in shown[0][1], shown
        assert "browser" not in shown[0][1].lower(), shown

        opened.clear(); shown.clear()
        platform.open_url = lambda url: False
        InstructorBackend().show_podman_guide(_FakeIcon())
        assert shown and prereq.guide_url() in shown[0][1], shown
        assert "browser" in shown[0][1].lower(), shown
    finally:
        platform.open_url, platform.show_terminal_window = real_open, real_window


def test_setup_bundle_chains_wsl_then_podman_then_the_app():
    """One download, in order, with nothing an uninstall would gut."""
    wxs = (ROOT / "packaging" / "bootstrapper"
           / "Ephemeral.Distributed.Instructor.Setup.wxs").read_text(encoding="utf-8")
    try:
        root = ET.fromstring(wxs)
    except ET.ParseError as e:
        # Two real authoring mistakes have landed here, and `--` is the trap: an
        # XML comment may not contain it (WiX reports CNDL0104), which is easy
        # to hit while explaining a command-line flag in prose.
        raise AssertionError(
            f"the bundle authoring is not well-formed XML: {e}. If the message "
            "points at a comment, look for a double hyphen in it"
        ) from e
    bundle = root.find("w:Bundle", WIX_NS)
    assert bundle is not None, "the authoring must have a Bundle root"
    assert bundle.get("UpgradeCode"), "a bundle without an UpgradeCode cannot upgrade"
    chain = bundle.find("w:Chain", WIX_NS)
    kinds = [c.tag.split("}")[1] for c in chain]
    assert kinds == ["ExePackage", "MsiPackage", "MsiPackage"], (
        f"prerequisites first, the app last: {kinds}")
    wsl, podman, app = list(chain)

    # WSL2, through the app's own headless entry point (there is no
    # redistributable WSL installer to chain).
    assert wsl.get("Id") == "Wsl2"
    assert wsl.get("InstallCommand") == "--setup-prereqs"
    assert wsl.get("Permanent") == "yes", (
        "uninstalling Ephemeral must never disable a feature the rest of the "
        "machine may depend on")
    assert wsl.get("DetectCondition") is None, (
        "Burn has no registry probe for an optional Windows feature; the "
        "idempotent executable is the detector, by design")
    exits = {e.get("Value"): e.get("Behavior") for e in wsl.findall("w:ExitCode", WIX_NS)}
    assert exits.get("3010") == "forceReboot", (
        "DISM's documented 'restart to finish' code must become a real reboot, "
        "not a quietly broken install")
    assert exits.get("740") == "error", (
        "elevation refused must fail loudly instead of pretending to succeed")

    # Podman, per-user, and survivable when a different build is already there.
    assert podman.get("Permanent") == "yes"
    assert podman.get("Vital") == "no", (
        "a machine with a different Podman version returns 1638; rolling the "
        "whole setup back over that leaves the teacher with nothing")
    props = {p.get("Name"): p.get("Value") for p in podman.findall("w:MsiProperty", WIX_NS)}
    assert props.get("MSIINSTALLPERUSER") == "1"
    assert props.get("MACHINE_PROVIDER") == "wsl"
    assert any(e.get("Value") == "1638" for e in podman.findall("w:ExitCode", WIX_NS))

    # The app opts the post-install launch back in: Burn installs quietly, and
    # the MSI's silent guard would otherwise suppress exactly the classroom
    # link that setup exists to show.
    app_props = {p.get("Name"): p.get("Value") for p in app.findall("w:MsiProperty", WIX_NS)}
    assert app_props.get("LAUNCHFIRSTRUN") == "1", (
        "Burn runs MSIs with /qn; without this the first-run launch is skipped")

    # Every variable the authoring reads has to be passed by the build script,
    # or the first real compile happens in CI and fails there instead of here.
    script = (ROOT / "packaging" / "bootstrapper" / "build_setup.sh").read_text(
        encoding="utf-8")
    used = set(re.findall(r"\$\(var\.(\w+)\)", wxs))
    assert used, "the authoring should be parameterised"
    assert "burn.exe" in script and "WixBalExtension.dll" in script, (
        "a bundle needs burn.exe and the Bal extension, unlike the MSI")
    for var in sorted(used):
        assert f'-d "{var}=' in script, f"{var} is read by the bundle but never passed"


def test_msi_launch_is_opt_in_for_the_bundle_but_still_silent_by_default():
    """The one custom action keeps its guards, plus an explicit bundle opt-in."""
    markup = (ROOT / "packaging" / "msi"
              / "Ephemeral.Distributed.Instructor.wxs").read_text(encoding="utf-8")
    publish = re.search(r"<InstallExecuteSequence>(.*?)</InstallExecuteSequence>",
                        markup, re.S).group(1)
    for guard in ("NOT Installed", "NOT REMOVE", "UILevel <> 2"):
        assert guard in publish, (
            f"{guard!r} keeps the launch off repair, off uninstall, and out of a "
            "silent deployment")
    assert "(UILevel <> 2 OR LAUNCHFIRSTRUN = 1)" in publish, (
        "the bundle installs quietly and must be able to opt the launch in")
    assert '<Property Id="LAUNCHFIRSTRUN" Value="0" />' in markup, (
        "the default has to be off, so a plain msiexec /qn stays silent")


def test_setup_bundle_is_built_and_shipped_by_ci():
    """A bundle nobody builds is a design document, not an artefact."""
    workflow = (ROOT / ".github" / "workflows" / "build.yml").read_text(encoding="utf-8")
    assert "packaging/bootstrapper/build_setup.sh" in workflow, (
        "CI must build the setup bundle, not just the MSI inside it")
    assert "Ephemeral-Distributed-Instructor-Setup.exe" in workflow, (
        "the artifact teachers download must be the bundle")
    assert "PODMAN_VERSION" in workflow, (
        "the Podman payload is pinned by version, so a release is reproducible")
    # ...and the checkout that installs and removes the MSI must still run,
    # because that is the half that can be proven on a runner.
    assert "msiexec /i \"$MSI\" /qn" in workflow


def main():
    test_console_output_is_decoded_the_way_windows_writes_it()
    test_cgroup_delegation_failure_is_recognised_and_nothing_else()
    test_ensure_canary_repairs_cgroup_delegation_once_then_retries()
    test_setup_survives_a_windowed_build_with_no_stdout()
    test_wsl_and_machine_states_are_classified()
    test_canary_uses_the_image_the_node_wants_anyway()
    test_no_classroom_link_before_a_container_has_run()
    test_repair_runtime_creates_the_machine_then_proves_it()
    test_prerequisite_module_never_elevates_itself()
    test_the_vendor_route_comes_after_wsl_and_never_replaces_it()
    test_the_gui_escape_hatch_opens_the_page_and_keeps_the_url_visible()
    test_setup_bundle_chains_wsl_then_podman_then_the_app()
    test_msi_launch_is_opt_in_for_the_bundle_but_still_silent_by_default()
    test_setup_bundle_is_built_and_shipped_by_ci()
    print("\n=== ALL PREREQUISITE / SETUP TESTS PASSED ===")


if __name__ == "__main__":
    sys.exit(main())
