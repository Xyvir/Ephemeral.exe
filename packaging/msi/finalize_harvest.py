#!/usr/bin/env python3
"""Post-process `heat dir` output for the Ephemeral MSI.

**RemoveFolder entries.** heat never tells Windows Installer to remove the
directories it creates, so an uninstall leaves an empty skeleton behind that
reads as "still installed".

Usage:
    python finalize_harvest.py <harvested.wxs> [<fixed.wxs>]

Rewrites in place when no output path is given.
"""

from __future__ import annotations

import sys
import xml.etree.ElementTree as ET
from pathlib import Path

WIX_NS = "http://schemas.microsoft.com/wix/2006/wi"
ET.register_namespace("", WIX_NS)

INSTALL_ROOT = "INSTALLDIR"


def q(tag: str) -> str:
    return f"{{{WIX_NS}}}{tag}"


def add_remove_folders(root: ET.Element) -> int:
    """Add one RemoveFolder per directory the installer creates."""
    directories: list[str] = []
    for ref in root.findall(f".//{q('DirectoryRef')}"):
        ref_id = ref.get("Id")
        if not ref_id:
            continue
        directories.append(ref_id)
        for child in ref.iter(q("Directory")):
            child_id = child.get("Id")
            if child_id:
                directories.append(child_id)

    by_directory: dict[str, ET.Element] = {}
    for component in root.findall(f".//{q('Component')}"):
        directory = component.get("Directory")
        if directory and directory not in by_directory:
            by_directory[directory] = component

    root_component = by_directory.get(INSTALL_ROOT)
    if root_component is None:
        raise SystemExit(f"no component installs into {INSTALL_ROOT}")

    added = 0
    for directory_id in dict.fromkeys(directories):
        owner = by_directory.get(directory_id) or root_component
        if owner.find(f"{q('RemoveFolder')}[@Id='rmf_{directory_id}']") is not None:
            continue
        remove = ET.SubElement(owner, q("RemoveFolder"))
        remove.set("Id", f"rmf_{directory_id}")
        if by_directory.get(directory_id) is None:
            # Borrowed the root component, so name the directory explicitly;
            # otherwise the entry would remove the install root instead.
            remove.set("Directory", directory_id)
        remove.set("On", "uninstall")
        added += 1
    return added


def main(argv: list[str]) -> int:
    if len(argv) not in (2, 3):
        print("usage: finalize_harvest.py <harvested.wxs> [fixed.wxs]", file=sys.stderr)
        return 2

    source = Path(argv[1])
    target = Path(argv[2]) if len(argv) == 3 else source
    if not source.is_file():
        print(f"harvest file not found: {source}", file=sys.stderr)
        return 2

    tree = ET.parse(source)
    root = tree.getroot()

    directories = add_remove_folders(root)

    tree.write(target, encoding="utf-8", xml_declaration=True)
    print(f"==> RemoveFolder entries for {directories} directories")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
