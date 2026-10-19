#!/usr/bin/env bash
# ============================================================================
# Build Ephemeral-Distributed-Instructor-Setup.exe (a WiX Burn bundle)
# ============================================================================
#
# Usage:
#   bash packaging/bootstrapper/build_setup.sh <bundle-dir> <podman-msi> \
#       <app-msi> <output.exe> [product-version]
#
#   <bundle-dir>  the PyInstaller onedir folder (holds the instructor exe, which
#                 is both the app and the `--setup-prereqs` prerequisite step)
#   <podman-msi>  Podman's Windows MSI, downloaded by CI (see the workflow's
#                 PODMAN_VERSION) — never fetched at install time on a teacher's
#                 machine, so setup works on an offline-by-policy network
#   <app-msi>     the MSI produced by packaging/msi/build_msi.sh
#
# Environment:
#   WIX_BIN  directory with burn.exe and the extension DLLs (WixBalExtension).
#            Defaults to the preinstalled WiX v3.14 toolset on the GitHub
#            runner, then PATH.
#
# Notes:
#   * `burn.exe` is in the full WiX v3 toolset (the `wix314-binaries.zip`
#     download does NOT contain it) — CI uses the preinstalled copy, exactly
#     like build_msi.sh uses its candle/light.
#   * The bundle is per-machine, so Windows shows a single UAC prompt at launch
#     and the WSL2 step inherits it. There is no second elevation anywhere.
#   * Every payload is embedded (Compressed="yes"), so setup needs no network.
set -euo pipefail

BUNDLE_DIR="${1:?usage: build_setup.sh <bundle-dir> <podman-msi> <app-msi> <output.exe> [version]}"
PODMAN_MSI="${2:?usage: build_setup.sh <bundle-dir> <podman-msi> <app-msi> <output.exe> [version]}"
APP_MSI="${3:?usage: build_setup.sh <bundle-dir> <podman-msi> <app-msi> <output.exe> [version]}"
OUT="${4:?usage: build_setup.sh <bundle-dir> <podman-msi> <app-msi> <output.exe> [version]}"
VERSION="${5:-0.0.0}"
HERE="$(cd "$(dirname "$0")" && pwd)"

printf '%s' "$VERSION" | grep -Eq '^[0-9]+\.[0-9]+\.[0-9]+$' || {
    echo "bundle version must be numeric x.y.z, got: $VERSION" >&2
    exit 2
}
for f in "$PODMAN_MSI" "$APP_MSI" "$BUNDLE_DIR/Ephemeral-Distributed-Instructor.exe"; do
    [ -f "$f" ] || { echo "missing required input: $f" >&2; exit 2; }
done

if [ -z "${WIX_BIN:-}" ]; then
    # Same lookup as build_msi.sh: the runner's WiX v3 patch version varies,
    # so match the install directory by glob rather than one hardcoded path.
    for candidate in "/c/Program Files (x86)/WiX Toolset"*/bin "/c/Program Files/WiX Toolset"*/bin; do
        if [ -x "$candidate/burn.exe" ]; then
            WIX_BIN="$candidate"
            break
        fi
    done
fi
if [ -z "${WIX_BIN:-}" ] && command -v burn.exe >/dev/null 2>&1; then
    WIX_BIN="$(dirname "$(command -v burn.exe)")"
fi
# build_msi.sh finds the toolset through candle on PATH; the runner's WiX may
# only be reachable that way, so accept the directory that holds candle.exe.
if [ -z "${WIX_BIN:-}" ] && command -v candle.exe >/dev/null 2>&1; then
    WIX_BIN="$(dirname "$(command -v candle.exe)")"
fi
if [ -z "${WIX_BIN:-}" ]; then
    echo "burn.exe not found; set WIX_BIN to a WiX v3 bin directory" >&2
    ls -d "/c/Program Files (x86)/WiX Toolset"* "/c/Program Files/WiX Toolset"* 2>/dev/null >&2 || echo "no WiX Toolset install directory found" >&2
    exit 2
fi

mkdir -p "$(dirname "$OUT")"
# A WIX_BIN that exists but holds the wrong toolset is the confusing case: the
# WiX binaries-only zip has no burn.exe at all (it ships candle/light/heat), so
# say which file is missing instead of failing at exec time.
[ -f "$WIX_BIN/burn.exe" ] || {
    echo "no burn.exe in WIX_BIN=$WIX_BIN — bundles need the full WiX v3 toolset" >&2
    exit 2
}
[ -f "$WIX_BIN/WixBalExtension.dll" ] || {
    echo "no WixBalExtension.dll in WIX_BIN=$WIX_BIN — the bundle needs the Bal extension" >&2
    exit 2
}

OUT_DIR="$(cd "$(dirname "$OUT")" && pwd)"
OUT_NAME="$(basename "$OUT")"

# -d passes the authoring variables the .wxs reads; paths are absolute so the
# build is independent of the working directory.
"$WIX_BIN/burn.exe" \
    -arch x64 \
    -ext "$WIX_BIN/WixBalExtension.dll" \
    -d "ProductVersion=$VERSION" \
    -d "BundleDir=$(cd "$BUNDLE_DIR" && pwd)" \
    -d "PodmanMsi=$(cd "$(dirname "$PODMAN_MSI")" && pwd)/$(basename "$PODMAN_MSI")" \
    -d "AppMsi=$(cd "$(dirname "$APP_MSI")" && pwd)/$(basename "$APP_MSI")" \
    -out "$OUT_DIR/$OUT_NAME" \
    "$HERE/Ephemeral.Distributed.Instructor.Setup.wxs"

[ -f "$OUT_DIR/$OUT_NAME" ] || { echo "burn.exe produced no output" >&2; exit 1; }
echo "wrote $OUT_DIR/$OUT_NAME"
