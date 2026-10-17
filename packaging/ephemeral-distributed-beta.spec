# -*- mode: python ; coding: utf-8 -*-
"""PyInstaller spec for the Ephemeral DISTRIBUTED tray client — **beta** bundle.

Same application as the stable distributed build, but packaged as an
**onedir folder** instead of a ``--onefile`` self-extractor:

* ``upx=False`` — no executable packing. Both UPX and the ``--onefile``
  bootloader (which unpacks a ``_MEI…`` temp dir and runs Python from it at
  every launch) are the two packaging traits most commonly cited in antivirus
  false positives against PyInstaller apps, and neither is needed here.
* a Win32 version resource (``packaging/version_info.txt``) so the exe carries
  an accurate, inspectable identity.

The result ships as a *folder*; nothing copies the exe out of it (an onedir
exe is useless without its sibling ``_internal`` directory), which also avoids
the "copy into AppData + run key" pattern.

Run from the repo root::

    pyinstaller packaging/ephemeral-distributed-beta.spec

Output: ``dist/Ephemeral-Distributed-Beta/Ephemeral-Distributed-Beta.exe``
"""
import os

from PyInstaller.utils.hooks import collect_all

datas, binaries, hiddenimports = [], [], []
for _pkg in ("PIL", "pystray", "pyperclip", "keyboard", "Xlib", "iroh"):
    _d, _b, _h = collect_all(_pkg)
    datas += _d
    binaries += _b
    hiddenimports += _h

ROOT = os.path.join(SPECPATH, "..")

# The CI job generates ephemeral.ico before invoking this spec; keep local
# builds working when it is absent.
_icon = os.path.join(ROOT, "ephemeral.ico")
ICON = _icon if os.path.exists(_icon) else None

# Win32 version resource (ignored on non-Windows toolchains).
VERSION_FILE = os.path.join(SPECPATH, "version_info.txt")

a = Analysis(
    [os.path.join(ROOT, "main_distributed_client.py")],
    pathex=[ROOT],
    binaries=binaries,
    datas=datas,
    hiddenimports=hiddenimports + ["ephemeral_core", "ephemeral_net"],
    hookspath=[],
    hooksconfig={},
    runtime_hooks=[],
    excludes=[],
    noarchive=False,
)
pyz = PYZ(a.pure)

exe = EXE(
    pyz,
    a.scripts,
    [],
    exclude_binaries=True,
    name="Ephemeral-Distributed-Beta",
    debug=False,
    bootloader_ignore_signals=False,
    strip=False,
    upx=False,
    console=False,
    icon=ICON,
    version=VERSION_FILE,
)

coll = COLLECT(
    exe,
    a.binaries,
    a.datas,
    strip=False,
    upx=False,
    name="Ephemeral-Distributed-Beta",
)
