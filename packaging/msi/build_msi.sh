#!/usr/bin/env bash
# ============================================================================
# Build the Ephemeral Distributed (beta) MSI from a onedir bundle.
# ============================================================================
#
# packaging/ephemeral-distributed-beta.spec produces a folder (the tray exe
# plus its _internal payload). This wraps that folder in a machine-wide MSI, so
# a professor installs the app instead of unzipping it — and so a lab can deploy
# or remove it unattended (elevation is required: it writes %ProgramFiles% and
# the machine-wide Run key; run these from an elevated prompt or a deployment
# tool):
#
#   msiexec /i Ephemeral-Distributed-Beta.msi /qn
#   msiexec /i Ephemeral-Distributed-Beta.msi /qn ADDLOCAL=ApplicationFiles
#   msiexec /i Ephemeral-Distributed-Beta.msi /qn REMOVE=NodeAtLogin
#   msiexec /x Ephemeral-Distributed-Beta.msi /qn
#
# Usage:
#   bash packaging/msi/build_msi.sh <bundle-dir> <output.msi> [product-version]
#
# Environment:
#   WIX_BIN   directory holding heat.exe/candle.exe/light.exe. Defaults to the
#             preinstalled WiX v3 toolset (GitHub's windows runners ship
#             v3.14.1), then to PATH. WiX v3 deliberately: it is already on the
#             runner (no network install) and is what the CI job uses.
#
# Toolchain notes, each verified against `heat -?` / `light -?`:
#   * `-ag` (not `-gg`): component GUIDs are generated at compile time from the
#     harvested paths, so they stay stable across rebuilds. Random GUIDs per
#     build would break ComponentRef identity on upgrade.
#   * `-var var.SourceDir` keeps the harvest machine-independent: the .wxs never
#     hard-codes this checkout's path, so CI and a local build agree.
#   * `-sreg` because heat must not invent registry entries, and `-srd` so heat
#     does not redeclare the root directory we author by hand.
# Toolchain notes (continued):
#   * After harvesting, finalize_harvest.py adds one RemoveFolder per created
#     directory (so the uninstall takes the whole tree with it) and hangs the
#     machine-wide Start Menu shortcut off the exe's own component.
#   * Exactly two ICE checks are suppressed, and they are the two that assume a
#     per-user install: ICE43 ("non-advertised shortcut must key on HKCU") and
#     ICE57 ("component has per-user and per-machine data"). This package is
#     ALLUSERS=1 and installs the shortcut into the all-users Start Menu, so
#     the HKCU keypath they demand would put the marker in whichever admin ran
#     the installer and orphan it on uninstall by another admin. Everything
#     else — including every structural ICE — still runs; test_ui.py fails if
#     this list grows or if -sval is ever added.
set -euo pipefail

BUNDLE="${1:?usage: build_msi.sh <bundle-dir> <output.msi> [product-version]}"
OUT="${2:?usage: build_msi.sh <bundle-dir> <output.msi> [product-version]}"
VERSION="${3:-0.0.0}"
HERE="$(cd "$(dirname "$0")" && pwd)"

# Windows Installer version fields are numeric AND range-limited: candle fails
# with CNDL0242 unless major < 256, minor < 256 and build < 65536. The
# date+SHA version the other artifacts carry therefore cannot be used here, and
# callers pass a date-derived x.y.z instead (CI computes `date +'%y.%-m.%-d'`,
# e.g. 26.10.17 — readable, and increasing, for the life of this installer).
printf '%s' "$VERSION" | grep -Eq '^[0-9]+\.[0-9]+\.[0-9]+$' || {
    echo "product version must be numeric x.y.z (Windows Installer), got: $VERSION" >&2
    exit 2
}
v_major="${VERSION%%.*}"
v_rest="${VERSION#*.}"
v_minor="${v_rest%%.*}"
v_build="${v_rest#*.}"
if [ "$v_major" -ge 256 ] || [ "$v_minor" -ge 256 ] || [ "$v_build" -ge 65536 ]; then
    echo "product version out of range (major<256, minor<256, build<65536): $VERSION" >&2
    exit 2
fi

[ -f "$BUNDLE/Ephemeral-Distributed-Beta.exe" ] || {
    echo "not a distributed-beta bundle (missing Ephemeral-Distributed-Beta.exe): $BUNDLE" >&2
    exit 2
}

WIX_BIN="${WIX_BIN:-}"
if [ -z "$WIX_BIN" ]; then
    for candidate in "/c/Program Files (x86)/WiX Toolset"*/bin "/c/Program Files/WiX Toolset"*/bin; do
        if [ -x "$candidate/candle.exe" ]; then
            WIX_BIN="$candidate"
            break
        fi
    done
fi
if [ -z "$WIX_BIN" ] && command -v candle >/dev/null 2>&1; then
    WIX_BIN="$(dirname "$(command -v candle)")"
fi
if [ -z "$WIX_BIN" ]; then
    echo "WiX v3 toolchain not found: set WIX_BIN to the directory holding heat.exe/candle.exe/light.exe" >&2
    exit 2
fi
for tool in heat candle light; do
    [ -x "$WIX_BIN/$tool.exe" ] || { echo "missing $tool.exe in $WIX_BIN" >&2; exit 2; }
done

# The WiX tools are native Windows binaries: hand them Windows paths so an
# MSYS/Git Bash checkout and a runner behave identically.
win() {
    if command -v cygpath >/dev/null 2>&1; then
        cygpath -w "$1"
    else
        printf '%s' "$1"
    fi
}

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

echo "==> harvesting $(find "$BUNDLE" -type f | wc -l) files from $BUNDLE"
"$WIX_BIN/heat.exe" dir "$(win "$BUNDLE")" \
    -cg HarvestedFiles -dr INSTALLDIR -ag -srd -sreg -nologo \
    -var var.SourceDir -out "$(win "$WORK/Harvested.wxs")"

echo "==> finalizing harvest (shortcut + clean uninstall)"
PYTHON="${PYTHON:-python3}"
command -v "$PYTHON" >/dev/null 2>&1 || PYTHON=python
"$PYTHON" "$(win "$HERE/finalize_harvest.py")" "$(win "$WORK/Harvested.wxs")"

echo "==> compiling (version $VERSION)"
"$WIX_BIN/candle.exe" -nologo -arch x64 -ext WixUIExtension \
    -dSourceDir="$(win "$BUNDLE")" \
    -dProductVersion="$VERSION" \
    -dLicenseRtf="$(win "$HERE/license.rtf")" \
    -out "$(win "$WORK/Product.wixobj")" "$(win "$HERE/Ephemeral.Distributed.Beta.wxs")"
# The harvested fragment resolves every File/@Source through $(var.SourceDir),
# so it needs the same binding as the product source above.
"$WIX_BIN/candle.exe" -nologo -arch x64 \
    -dSourceDir="$(win "$BUNDLE")" \
    -out "$(win "$WORK/Harvested.wixobj")" "$(win "$WORK/Harvested.wxs")"

echo "==> linking and validating"
mkdir -p "$(dirname "$OUT")"
"$WIX_BIN/light.exe" -nologo -ext WixUIExtension -cultures:en-us \
    -sice:ICE43 -sice:ICE57 \
    -out "$(win "$OUT")" \
    "$(win "$WORK/Product.wixobj")" "$(win "$WORK/Harvested.wixobj")"

echo "==> built $OUT ($(du -h "$OUT" | cut -f1))"
