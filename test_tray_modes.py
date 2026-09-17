"""
Tests for the CLI/pipe entry points (no Podman needed).

Covers the pure mode-detection table, the ``-``/``--stdin`` document reader
against a real OS pipe, and the CLI stderr gate in platform. CI can run it
directly:

    python test_tray_modes.py
"""
import contextlib
import io
import os
import subprocess
import sys
import tempfile

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from ephemeral_ui import platform, tray  # noqa: E402

ok = 0


def check(name, cond):
    global ok
    assert cond, name
    ok += 1
    print(f"PASS: {name}")


# --- mode detection table -------------------------------------------------

check("bare argv + GUI -> tray", tray._detect_mode(["exe"], gui=True) == ("tray", None))
# Bare argv with no GUI stays "tray" so run()'s legacy branch keeps its
# "CLI mode requires a file argument" error (an argless no-GUI launch must
# not silently block reading stdin).
check("bare argv, no GUI -> tray (legacy error path)", tray._detect_mode(["exe"], gui=False) == ("tray", None))
check("- -> cli-stdin", tray._detect_mode(["exe", "-"], gui=True) == ("cli-stdin", None))
check("--stdin -> cli-stdin", tray._detect_mode(["exe", "--stdin"], gui=False) == ("cli-stdin", None))
check("--cli - -> cli-stdin", tray._detect_mode(["exe", "--cli", "-"], gui=True) == ("cli-stdin", None))
check("--cli alone -> tray (legacy behavior)", tray._detect_mode(["exe", "--cli"], gui=False) == ("tray", None))
with tempfile.NamedTemporaryFile(suffix=".md", delete=False) as f:
    f.write(b"temporary")
    real = f.name
try:
    check("existing file + GUI -> oneshot", tray._detect_mode(["exe", real], gui=True) == ("oneshot", real))
    check("existing file, no GUI -> headless", tray._detect_mode(["exe", real], gui=False) == ("headless", real))
    check("--cli existing file -> headless", tray._detect_mode(["exe", "--cli", real], gui=True) == ("headless", real))
    check("--cli missing file -> cli-missing-file", tray._detect_mode(["exe", "--cli", "gone.md"], gui=True) == ("cli-missing-file", "gone.md"))
finally:
    os.unlink(real)

# --- stdin document reader (real OS pipe) ----------------------------------

doc = "# Piped doc\n\n```python\nprint('through the pipe')\n```\n"
probe = subprocess.run(
    [sys.executable, "-c",
     "import sys; sys.path.insert(0, '.');"
     "from ephemeral_ui.tray import _read_document;"
     "sys.stdout.write(_read_document('-'))"],
    input=doc.encode("utf-8"), capture_output=True,
)
check("stdin reader round-trips a real pipe",
      probe.returncode == 0
      and probe.stdout.replace(b"\r\n", b"\n").decode("utf-8") == doc)

with tempfile.NamedTemporaryFile(suffix=".md", delete=False, mode="w", encoding="utf-8") as f:
    f.write(doc)
    path = f.name
try:
    check("file reader still reads files", tray._read_document(path) == doc)
finally:
    os.unlink(path)

# --- CLI stderr gate --------------------------------------------------------

orig_cli = platform.CLI_MODE
try:
    platform.CLI_MODE = True
    err = io.StringIO()
    with contextlib.redirect_stderr(err):
        platform.show_terminal_window("Ephemeral Error", "boom", header="H")
    check("CLI mode errors land on stderr",
          "H" in err.getvalue() and "boom" in err.getvalue())

    platform.CLI_MODE = False
    # GUI mode on Windows successfully spawns a real console; force the
    # failure branch to observe the stdout fallback deterministically.
    real_popen = platform.subprocess.Popen
    def broken_popen(*a, **k):
        raise OSError("no console in tests")
    platform.subprocess.Popen = broken_popen
    out = io.StringIO()
    try:
        with contextlib.redirect_stdout(out):
            platform.show_terminal_window("Ephemeral Error", "boom", header="H")
    finally:
        platform.subprocess.Popen = real_popen
    # Windows: a failed console spawn surfaces only the failure notice on
    # stdout (the full-text fallback is the Linux path). Assert the sink,
    # not the payload.
    check("GUI-mode failures report on stdout, not stderr",
          "Failed to show terminal window" in out.getvalue())
finally:
    platform.CLI_MODE = orig_cli

# --- headless run end-to-end with a fake backend ----------------------------

class FakeBackend:
    app_key = "Fake"
    display_name = "Fake"

    def __init__(self):
        self.ran = []

    def prepare_run(self, icon):
        return None

    def run_logic(self, icon, content=None):
        self.ran.append(content)

    def cleanup_run(self, icon, token):
        pass

    def shutdown(self):
        pass


fake = FakeBackend()
buf = io.StringIO(doc)
orig_stdin = sys.stdin
try:
    sys.stdin = buf
    code = None
    try:
        tray.setup_headless_mode(fake, "-")
    except SystemExit as e:
        code = e.code
    check("headless stdin run executes and exits 0", code in (0, None) and fake.ran == [doc])
finally:
    sys.stdin = orig_stdin

print(f"\n=== ALL TRAY MODE TESTS PASSED ({ok}) ===")
