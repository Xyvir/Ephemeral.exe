# -*- mode: python ; coding: utf-8 -*-
"""PyInstaller spec for the Ephemeral DISTRIBUTED tray client.

Builds an onedir bundle named ``ephemeral-distributed`` (used by
``build_appimage.sh`` to assemble a Linux AppImage). Bundles the ``iroh``
Python extension and the ``ephemeral_net`` tier. Run from the repo root:
``pyinstaller packaging/ephemeral-distributed.spec``.
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
    name="ephemeral-distributed",
    debug=False,
    bootloader_ignore_signals=False,
    strip=False,
    upx=True,
    console=False,
)

coll = COLLECT(
    exe,
    a.binaries,
    a.datas,
    strip=False,
    upx=True,
    name="ephemeral-distributed",
)
