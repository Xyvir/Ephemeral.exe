"""
Tests for the tray front end's packaging and startup plumbing.

Covers the parts that must not regress:

* the tray has no headless "node" mode: start-on-boot launches the same tray
  app a user sees, and nothing runs invisibly in the background;
* packaging invariants for the distributed-instructor artifact (onedir bundle, no
  UPX, version resource), which exists so the antivirus-sensitive path has a
  bundle that is not a self-extractor;
* the ClamAV canary, which must stay a soft signal: it may never fail a build,
  and it must never report "not scanned" as "clean".

No GUI, Podman, network, or elevated privilege is needed: the tray menu is
built through injected fakes.

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

ROOT = pathlib.Path(__file__).resolve().parent


# --- fakes ---------------------------------------------------------------

class _FakeIcon:
    """Just enough tray icon for a backend to call ``notify`` on."""

    def __init__(self) -> None:
        self.messages: list[str] = []

    def notify(self, message, title=None):
        self.messages.append(message)


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


def _collect_texts(items, found=None):
    """Every menu label reachable from ``items`` (recursing into submenus)."""
    found = [] if found is None else found
    for it in items:
        found.append(it.text)
        if it.menu is not None:
            _collect_texts(it.menu, found)
    return found


def test_onedir_bundles_are_never_copied_out_of_their_folder():
    sm = platform.StartupManager("Ephemeral-Distributed", "Ephemeral-Distributed")
    bundle = os.path.join(tempfile.gettempdir(), "Ephemeral-Distributed-Instructor")
    exe = os.path.join(bundle, "Ephemeral-Distributed-Instructor.exe")
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


# --- distributed menu ---------------------------------------------------

def test_distributed_menu_has_no_background_node():
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
    assert "Run Node on Login" not in texts, texts
    assert "Private Mode" in texts, texts
    assert "Install Podman Desktop" in texts, (
        "the vendor route has to be reachable from the tray, not only from a "
        "machine that already failed: " + repr(texts))
    joined = " ".join(texts).lower()
    for needle in ("service", "system", "boot"):
        assert needle not in joined, (
            f"the Distributed menu must not promise {needle!r}: the node starts "
            "at login for one user, and a boot-time service is what was flagged"
        )
    print("PASS: the Distributed menu has no background node or service item")


# --- packaging invariants -----------------------------------------------

def test_instructor_bundle_is_not_a_self_extractor():
    spec = (ROOT / "packaging" / "ephemeral-distributed-instructor.spec").read_text(encoding="utf-8")
    code = _strip_docstrings(spec)  # prose may name what the build avoids
    assert "COLLECT(" in code, "the instructor bundle must be a onedir COLLECT"
    assert "upx=False" in code, "UPX packing must stay off in the instructor bundle"
    assert "onefile" not in code, "the instructor bundle must not request a onefile build"
    assert "version=VERSION_FILE" in code, "the instructor exe must carry a version resource"
    version_info = (ROOT / "packaging" / "version_info.txt").read_text(encoding="utf-8")
    assert "Ephemeral-Distributed-Instructor.exe" in version_info
    assert "Version number (injected from the github workflow)" in version_info
    print("PASS: the distributed-instructor bundle is onedir, un-UPXed, with metadata")


def test_ci_builds_and_ships_the_instructor_bundle():
    workflow = (ROOT / ".github" / "workflows" / "build.yml").read_text(encoding="utf-8")
    assert "build-exe-distributed-instructor:" in workflow
    assert "packaging/ephemeral-distributed-instructor.spec" in workflow
    assert "Ephemeral-Distributed-Instructor-win64.zip" not in workflow, (
        "the instructor build ships only the MSI")

    beta_job = workflow.split("build-exe-distributed-instructor:", 1)[1]
    beta_job = beta_job.split("build-linux-appimage:", 1)[0]
    assert "--onefile" not in beta_job, "the instructor job must not build a onefile exe"
    assert "--self-check" in beta_job, "the instructor job must smoke-test the bundle"

    release = workflow.split("release:", 1)[1]
    assert "dist/Ephemeral-Distributed-Instructor.msi" in release
    assert "dist/Ephemeral-Distributed-Instructor-win64.zip" not in release
    assert "Ephemeral-Distributed-Instructor-Setup.exe" not in release
    assert "build-exe-distributed-instructor" in release
    print("PASS: CI builds, smoke-tests and releases the instructor bundle")


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


def test_instructor_build_cannot_join_the_public_swarm():
    """The classroom build is a different build, not a user setting.

    Removing the public swarm has to hold for the *shipped exe*, so four things
    have to line up: the spec builds the instructor entry point, that entry
    point selects the instructor mode, ``app.run`` maps the mode to the
    instructor backend, and the live-list bootstrap sits behind the per-build
    flag with no second call site to slip through.
    """
    spec = (ROOT / "packaging" / "ephemeral-distributed-instructor.spec").read_text(encoding="utf-8")
    assert "main_distributed_instructor.py" in spec, (
        "the instructor exe must be built from the instructor entry point, or "
        "the public swarm ships inside it")
    assert "main_distributed_client.py" not in spec.split("Analysis(")[1], (
        "the public client's entry point must not be what this spec builds")

    entry = (ROOT / "main_distributed_instructor.py").read_text(encoding="utf-8")
    assert 'run("instructor")' in entry, "the entry point must select the instructor mode"

    app_src = (ROOT / "ephemeral_ui" / "app.py").read_text(encoding="utf-8")
    assert '"instructor"' in app_src and "InstructorBackend" in app_src, (
        "app.run must map the instructor mode to the instructor backend")

    backend_src = (ROOT / "ephemeral_ui" / "backends" / "distributed.py").read_text(
        encoding="utf-8")
    assert "elif self.public_swarm:" in backend_src, (
        "the live-list bootstrap must sit behind the per-build flag")
    assert backend_src.count("bootstrap_from_list()") == 1, (
        "one call site only, or the flag can be bypassed")
    assert "public_swarm = False" in backend_src and "default_private = True" in backend_src, (
        "the instructor backend must refuse the public swarm and start private")
    assert '"--first-run" in sys.argv' in backend_src, (
        "the flag the installer launches with must be honoured")
    wxs = _msi_text("Ephemeral.Distributed.Instructor.wxs")
    assert "--first-run" in wxs, (
        "the install-time launch is what asks for the classroom link")

    from ephemeral_ui.backends.distributed import DistributedBackend, InstructorBackend
    assert InstructorBackend.public_swarm is False, "instructor must not reach the public swarm"
    assert DistributedBackend.public_swarm is True, (
        "the public client keeps the swarm this build is about")
    assert InstructorBackend.default_private is True
    assert DistributedBackend.default_private is False, (
        "the public client's starting mode must not change")
    assert InstructorBackend.app_key != DistributedBackend.app_key, (
        "the two builds must not share a tray identity, or a login entry")
    assert InstructorBackend.display_name.startswith("Ephemeral Distributed")

    # Private ON is a starting point, not a policy: applying it records
    # nothing, the interactive first launch is what records the choice, and an
    # explicit OFF then survives every restart.
    saved = os.environ.get("EPHEMERAL_STATE_DIR")
    try:
        with tempfile.TemporaryDirectory() as tmp:
            os.environ["EPHEMERAL_STATE_DIR"] = tmp
            from ephemeral_net.swarm import PRIVATE_DEFAULT_MARKER, PRIVATE_MODE_MARKER
            from ephemeral_ui.backends.distributed import default_state_dir

            fresh = InstructorBackend()
            assert fresh.private_checked() is False, "a fresh state dir starts switched off"
            fresh._apply_default_mode()
            assert fresh.private_checked() is True, (
                "the instructor build must create its private swarm on first run")
            assert not (default_state_dir() / PRIVATE_DEFAULT_MARKER).exists(), (
                "applying the default must record nothing: the first interactive "
                "launch still has to offer create-or-join")

            # The teacher switches it off in the tray, then starts again.
            fresh.toggle_private(_FakeIcon())
            assert InstructorBackend().private_checked() is False, (
                "the first-run default must not overrule an explicit OFF")
            assert (default_state_dir() / PRIVATE_DEFAULT_MARKER).exists(), (
                "the toggle is what records the choice")
            InstructorBackend()._apply_default_mode()
            assert InstructorBackend().private_checked() is False, (
                "a recorded OFF must survive every later start")

            # ...and the public client never switches itself into private mode.
            DistributedBackend()._apply_default_mode()
            assert not (default_state_dir() / PRIVATE_MODE_MARKER).exists(), (
                "the public client must not be dragged into private mode by the "
                "instructor build's default")
    finally:
        if saved is None:
            os.environ.pop("EPHEMERAL_STATE_DIR", None)
        else:
            os.environ["EPHEMERAL_STATE_DIR"] = saved

    print("PASS: the instructor build cannot reach the public swarm")


def test_instructor_first_launch_offers_create_or_join():
    """Two instructor machines can anchor one classroom swarm.

    A professor's second machine should be redundancy, not a second classroom,
    and the only thing the first machine hands them is the student link — which
    carries the ticket in its ``#seed=`` fragment. So the first launch has to
    accept that link and persist the ticket inside it, refuse a paste that is
    not a seed, and ask exactly once.
    """
    from ephemeral_net.swarm import (
        PRIVATE_DEFAULT_MARKER,
        PRIVATE_MODE_MARKER,
        PRIVATE_SEED_MARKER,
        parse_seed_input,
    )
    from ephemeral_ui.backends.distributed import DistributedBackend, InstructorBackend

    ticket = "endpoint" + "A1b2C3d4" * 18
    link = f"https://xyvir.github.io/Ephemeral.exe/#seed={ticket}"
    assert parse_seed_input(link) == ticket, "the student link must yield its ticket"
    assert parse_seed_input(f"  {ticket}  ") == ticket
    assert parse_seed_input(f"?seed={ticket}&x=1") == ticket
    assert parse_seed_input("https://xyvir.github.io/Ephemeral.exe/") is None, (
        "a link with no #seed= must never be stored as a seed")
    assert parse_seed_input("endpoint with spaces in it") is None
    assert parse_seed_input("not a seed at all, thanks") is None
    assert parse_seed_input("") == "", "empty input means create-new"
    compact = "iroh1:" + "a" * 64 + ";relay.example.org"
    assert parse_seed_input(compact) == "a" * 64 + "@relay.example.org", (
        "the compact form the swarm list publishes must be usable too")

    # The tickets this repo actually publishes have to survive the validator,
    # or a teacher pasting one from the live list gets refused. Checked against
    # the checked-in list rather than a made-up string, so an iroh format change
    # shows up here instead of in a classroom.
    published = re.findall(r'"(endpoint[^"]+)"', (ROOT / "docs" / "swarm.json").read_text(
        encoding="utf-8"))
    assert published, "docs/swarm.json should publish at least one ticket"
    for real in published:
        assert parse_seed_input(real) == real, (
            "a real published ticket must not be refused as unparseable")

    saved = os.environ.get("EPHEMERAL_STATE_DIR")
    shown: list[tuple[str, str]] = []
    real_window = platform.show_terminal_window
    platform.show_terminal_window = lambda title, body: shown.append((title, body))
    try:
        with tempfile.TemporaryDirectory() as tmp:
            os.environ["EPHEMERAL_STATE_DIR"] = tmp
            state = pathlib.Path(tmp)

            # The runtime gate is exercised in test_prereq.py; here it is
            # stubbed ready so a unit test never starts a container (or waits
            # for a Podman machine that this host may not have).
            from ephemeral_ui import prereq

            real_status, real_canary = prereq.status, prereq.canary
            prereq.status = lambda: prereq.RuntimeState("ready")
            prereq.canary = lambda *a, **k: (True, "")

            def launch(answer):
                backend = InstructorBackend()
                backend._ensure_cluster = lambda: None      # no node in a unit test
                backend.current_student_url = lambda: link
                backend._announce_private = lambda *a, **k: None
                backend._prompt_user_for_seed = (
                    lambda current="", prompt=None: answer
                )
                backend._first_launch_setup(None)
                assert (state / PRIVATE_DEFAULT_MARKER).exists(), (
                    "the launch must record the choice, or it will ask again")
                return backend

            def reset():
                (state / PRIVATE_SEED_MARKER).unlink(missing_ok=True)
                (state / PRIVATE_MODE_MARKER).unlink(missing_ok=True)
                (state / PRIVATE_DEFAULT_MARKER).unlink(missing_ok=True)

            # JOIN: the pasted link becomes the seed the node dials, which is
            # what puts both machines in one swarm.
            joined = launch(link)
            assert joined.private_checked() is True
            assert (state / PRIVATE_SEED_MARKER).read_text(encoding="utf-8").strip() == ticket, (
                "the link's ticket must be what joins the second machine")

            # CREATE: empty input, so this machine anchors its own swarm.
            reset()
            created = launch("")
            assert created.private_checked() is True
            assert not (state / PRIVATE_SEED_MARKER).exists()

            # A paste that is not a seed is refused rather than written, and
            # the machine still ends up reachable on its own swarm.
            reset()
            shown.clear()
            rejected = launch("https://xyvir.github.io/Ephemeral.exe/")
            assert rejected.private_checked() is True
            assert not (state / PRIVATE_SEED_MARKER).exists(), (
                "a link without #seed= must never be persisted as a seed")
            assert shown, "the refusal has to be explained, not swallowed"

            # No console to ask in: take the default rather than leave a
            # classroom with no reachable node.
            reset()
            fallback = launch(None)
            assert fallback.private_checked() is True
            assert not (state / PRIVATE_SEED_MARKER).exists()

            # The menu path takes the same forms, for a teacher who skipped it.
            reset()
            later = InstructorBackend()
            later._announce_private = lambda *a, **k: None
            later._prompt_user_for_seed = lambda current="", prompt=None: link
            later.toggle_private(_FakeIcon())
            assert (state / PRIVATE_SEED_MARKER).read_text(encoding="utf-8").strip() == ticket

            # A join that lands while the node is still bootstrapping must
            # still re-dial: the old swarm would otherwise stay in place,
            # because Cluster.stop() has no node to close yet.
            class _Cluster:
                def __init__(self, node, starting):
                    self.node = node
                    self._thread = SimpleNamespace(is_alive=lambda: starting)
                    self.calls: list[str] = []

                def start(self):
                    self.calls.append("start")
                    self.node = object()

                def stop(self):
                    self.calls.append("stop")
                    self.node = None

            idle = InstructorBackend()
            idle.cluster = _Cluster(None, False)
            idle._redial_with_seed()
            assert idle.cluster.calls == [], (
                "before anything has dialled there is nothing to re-dial")

            mid = InstructorBackend()
            mid.cluster = _Cluster(None, True)  # a bootstrap is still running
            mid._redial_with_seed()
            assert mid.cluster.calls == ["start", "stop", "start"], mid.cluster.calls

            # Asked once, and only by the build that asks: the public client
            # has no classroom swarm to anchor or join.
            asked: list[int] = []
            again = InstructorBackend()
            again._ensure_cluster = lambda: None
            again._announce_private = lambda *a, **k: None
            again._prompt_user_for_seed = (
                lambda current="", prompt=None: asked.append(1) or ""
            )
            again._first_launch_setup(None)
            assert not asked, "the create-or-join question must be offered once"

            def never(*a, **k):
                raise AssertionError("the public client must not ask where to join")

            # No recorded choice here, so this proves the *build* keeps quiet,
            # not that an old marker short-circuited the question.
            (state / PRIVATE_DEFAULT_MARKER).unlink(missing_ok=True)
            public = DistributedBackend()
            public._ensure_cluster = lambda: None
            public._announce_private = lambda *a, **k: None
            public._prompt_user_for_seed = never
            public._first_launch_setup(None)
    finally:
        prereq.status, prereq.canary = real_status, real_canary
        platform.show_terminal_window = real_window
        if saved is None:
            os.environ.pop("EPHEMERAL_STATE_DIR", None)
        else:
            os.environ["EPHEMERAL_STATE_DIR"] = saved

    print("PASS: the instructor first launch asks create-or-join")


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


# --- MSI: machine-wide install of the distributed instructor build -------------------

MSI_DIR = ROOT / "packaging" / "msi"


def _msi_text(name):
    return (MSI_DIR / name).read_text(encoding="utf-8")


def test_msi_is_machine_wide_and_declarative():
    """The installer must serve every user, and change nothing it cannot undo."""
    wxs = _msi_text("Ephemeral.Distributed.Instructor.wxs")

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
    marker = wxs.index('Name="Ephemeral-Distributed-Instructor" Type="string"')
    entry = wxs[marker:marker + 400]
    assert "--node" not in entry, (
        "the machine-wide login entry starts the tray app, not a hidden node: "
        "a teacher must see the same app the Install & Run on Boot item starts")
    assert 'Name="Ephemeral-Distributed-Instructor" Type="string"' in wxs
    assert "[INSTALLDIR]Ephemeral-Distributed-Instructor.exe" in entry, (
        "the entry must point at the installed exe, not a copy")

    # Declarative apart from exactly one action: the launch that hands a
    # teacher their classroom link. Windows Installer cannot start a program
    # without a custom action, so the invariant is no longer "none" -- it is
    # "one, and this one, in this shape". Checked against markup, not prose:
    # the comments above explain what is deliberately absent and must not be
    # able to satisfy this.
    markup = re.sub(r"<!--.*?-->", "", wxs, flags=re.S)
    actions = re.findall(r"<CustomAction\b[^>]*>", markup)
    assert len(actions) == 2, (
        "this source gets exactly two custom actions: the machine setup that the "
        "wizard waits for, and the post-install launch; "
        f"found {len(actions)}: {actions}")
    prepare = next(a for a in actions if 'Id="PrepareInstructorRuntime"' in a)
    assert ('ExeCommand="&quot;[INSTALLDIR]Ephemeral-Distributed-Instructor.exe&quot;'
            ' --setup-machine"') in prepare, prepare
    assert 'Impersonate="yes"' in prepare, (
        "the Podman machine must belong to the installing user, not SYSTEM")
    assert 'Return="ignore"' in prepare, (
        "the wizard waits for the machine; a failure must not roll the app back")
    assert 'Directory="INSTALLDIR"' in prepare, prepare
    # ...and the built package has exactly one more, WixUIPrintEula, which the
    # WixUI extension contributes because the feature tree shows a licence. It
    # prints the RTF and launches nothing; anything beyond that pair is new.
    launch = next(a for a in actions if 'Id="LaunchInstructorFirstRun"' in a)
    assert 'Id="LaunchInstructorFirstRun"' in launch, launch
    assert 'Directory="INSTALLDIR"' in launch, (
        "a type 34 action needs the install directory as its working directory")
    assert ('ExeCommand="&quot;[INSTALLDIR]Ephemeral-Distributed-Instructor.exe&quot;'
            ' --first-run"') in launch, (
        "type 34 requires the full executable path in ExeCommand, and --first-run "
        "is what makes the app hand over the classroom link")
    assert 'Impersonate="yes"' in launch, (
        "the tray has to appear in the installing user's session, not SYSTEM's")
    assert 'Return="asyncNoWait"' in launch, (
        "a tray that fails to start must not wedge the installer")
    for dangerous in ("ShellExecute", "cmd.exe", "powershell", "runas",
                      "wscript", "cscript", "RunAs"):
        assert dangerous not in markup, (
            f"{dangerous!r} is the shape installer heuristics hunt for; the one "
            "action launches the installed exe and nothing else")
    # ...and it may only fire when a person is watching setup finish.
    sequence = re.search(r"<InstallExecuteSequence>(.*?)</InstallExecuteSequence>",
                         markup, re.S)
    assert sequence, "the launch must be sequenced explicitly"
    publish = sequence.group(1)
    assert 'Action="LaunchInstructorFirstRun"' in publish, publish
    assert 'Action="PrepareInstructorRuntime"' in publish, (
        "the machine setup must be sequenced into the wizard, before the launch")
    assert 'After="PrepareInstructorRuntime"' in publish, publish
    assert "InstallFinalize" in publish, (
        "the launch runs after the files are on disk, not while they are")
    for guard in ("NOT Installed", "NOT REMOVE", "UILevel <> 2"):
        assert guard in publish, (
            f"{guard!r} must guard the launch: it must not run on uninstall, on "
            "repair, or during a silent /qn deployment")
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
    assert "/v Ephemeral-Distributed-Instructor /reg:64" in workflow, (
        "the machine-wide Run value is the claim this package makes, so CI has "
        "to check it and not only that files landed")
    assert "dist/Ephemeral-Distributed-Instructor.msi" in workflow, (
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
    test_onedir_bundles_are_never_copied_out_of_their_folder()
    test_distributed_menu_has_no_background_node()
    test_instructor_bundle_is_not_a_self_extractor()
    test_instructor_build_cannot_join_the_public_swarm()
    test_instructor_first_launch_offers_create_or_join()
    test_ci_builds_and_ships_the_instructor_bundle()
    test_clamav_canary_is_soft_and_reports_honestly()
    test_ci_clamav_canary_cannot_gate_a_release()
    test_msi_is_machine_wide_and_declarative()
    test_msi_suppresses_only_the_per_user_shortcut_checks()
    test_msi_harvest_postprocessor_covers_every_directory()
    print("\n=== ALL TRAY / NODE TESTS PASSED ===")


if __name__ == "__main__":
    main()
