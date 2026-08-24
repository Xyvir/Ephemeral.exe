#!/usr/bin/env bash
# Build an Ephemeral Linux tray client as a portable AppImage.
#
#   ./packaging/build_appimage.sh <local|distributed>
#
# Produces dist/ephemeral-<target>-x86_64.AppImage.
#
# Build-machine requirements:
#   * python3 with venv support
#   * wget (fetches appimagetool on first use)
#   * internet access (PyPI, appimagetool GitHub release)
#
# The AppImage itself runs on any modern x86_64 Linux desktop. The tray
# needs a StatusNotifier/AppIndicator host (most GNOME/KDE desktops) or
# an X11 session (pystray's Xorg backend). If the host lacks FUSE, run
# with APPIMAGE_EXTRACT_AND_RUN=1.
set -euo pipefail

TARGET="${1:?usage: build_appimage.sh <local|distributed>}"
case "$TARGET" in
  local|distributed) ;;
  *) echo "unknown target: $TARGET (expected local|distributed)" >&2; exit 1 ;;
esac

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

VENV="$ROOT/.appimage-venv"
NAME="ephemeral-${TARGET}"
VERSION="${VERSION:-$(date +%Y%m%d)-dev}"

echo "==> Building ${NAME} (version ${VERSION})"

# 1) Reusable build venv -------------------------------------------------
if [ ! -d "$VENV" ]; then
  python3 -m venv "$VENV"
fi
"$VENV/bin/pip" install -q --upgrade pip
"$VENV/bin/pip" install -q -r requirements.txt pyinstaller
if [ "$TARGET" = "distributed" ]; then
  "$VENV/bin/pip" install -q -r requirements-net.txt
fi

# 2) Version stamp (same placeholder the Windows job injects) ------------
sed -i "s/Version number (injected from the github workflow)/${VERSION}/g" \
  ephemeral_ui/backends/local.py ephemeral_ui/backends/distributed.py

# 3) PyInstaller onedir bundle -------------------------------------------
"$VENV/bin/pyinstaller" --noconfirm --clean "packaging/${NAME}.spec"

# 4) Assemble the AppDir -------------------------------------------------
APPDIR="$ROOT/dist/appimage-${NAME}"
rm -rf "$APPDIR"
mkdir -p "$APPDIR"
cp -r "$ROOT/dist/${NAME}" "$APPDIR/${NAME}"

cat > "$APPDIR/AppRun" <<EOF
#!/bin/sh
SELF=\$(readlink -f "\$0")
HERE=\${SELF%/*}
exec "\$HERE/${NAME}/${NAME}" "\$@"
EOF
chmod +x "$APPDIR/AppRun"

if [ "$TARGET" = "distributed" ]; then
  DISPLAY_NAME="Ephemeral Distributed"
  COMMENT="Clipboard-driven code runner on the ephemeral distributed cluster"
else
  DISPLAY_NAME="Ephemeral Local"
  COMMENT="Clipboard-driven sandboxed code runner (local Podman)"
fi

cat > "$APPDIR/${NAME}.desktop" <<EOF
[Desktop Entry]
Name=${DISPLAY_NAME}
Comment=${COMMENT}
Exec=${NAME}
Icon=${NAME}
Terminal=false
Type=Application
Categories=Utility;Development;
StartupNotify=false
EOF

# 5) Icon (256x256, same design as the tray glyph) -----------------------
"$VENV/bin/python" - "$APPDIR/${NAME}.png" <<'PY'
import sys
from PIL import Image, ImageDraw

img = Image.new("RGB", (256, 256), (30, 30, 30))
dc = ImageDraw.Draw(img)
dc.rectangle((64, 64, 192, 192), fill=(255, 255, 255))
dc.rectangle((80, 80, 176, 112), fill=(0, 120, 215))
img.save(sys.argv[1])
PY

# 6) appimagetool (cached under .appimage-tool/) -------------------------
TOOL_DIR="$ROOT/.appimage-tool"
TOOL="$TOOL_DIR/appimagetool-x86_64.AppImage"
if [ ! -f "$TOOL" ]; then
  mkdir -p "$TOOL_DIR"
  wget -q -O "$TOOL" \
    https://github.com/AppImage/AppImageKit/releases/download/continuous/appimagetool-x86_64.AppImage
  chmod +x "$TOOL"
fi

"$TOOL" --appimage-extract-and-run "$APPDIR" "$ROOT/dist/${NAME}-x86_64.AppImage"

echo "==> Built $ROOT/dist/${NAME}-x86_64.AppImage"
