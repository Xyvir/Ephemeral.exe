#!/usr/bin/env bash
# ============================================================================
# Ephemeral API — One-Time Setup Script
# ============================================================================
#
# Deploys Ephemeral as a sidecar API service alongside an existing stack
# (e.g., Lithic with Caddy/Lighttpd + WebDAV).
#
# Prerequisites:
#   - Linux host with systemd
#   - Podman installed and accessible by the service user
#   - Python 3.10+ with pip
#   - /ephemeral/ directory exists (WebDAV mount point)
#
# Usage:
#   chmod +x install.sh && sudo ./install.sh
#
# What this does:
#   1. Creates a dedicated system user (ephemeral)
#   2. Installs the Python package + dependencies into a venv
#   3. Ensures /ephemeral/ artifact directory exists
#   4. Creates and enables a systemd service
#   5. Initializes Podman for the service user (rootless)
# ============================================================================

set -euo pipefail

# --- Configuration ---
APP_NAME="ephemeral-api"
APP_USER="ephemeral"
INSTALL_DIR="/opt/ephemeral"
VENV_DIR="${INSTALL_DIR}/venv"
WEBDAV_DIR="/ephemeral"
SERVICE_PORT=8787
BIND_HOST="127.0.0.1"    # Bind to localhost; reverse-proxy (Caddy) handles external

# --- Colors ---
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

info()  { echo -e "${GREEN}[✓]${NC} $*"; }
warn()  { echo -e "${YELLOW}[!]${NC} $*"; }
error() { echo -e "${RED}[✗]${NC} $*" >&2; exit 1; }

# --- Pre-flight Checks ---
echo ""
echo "══════════════════════════════════════════════"
echo "  Ephemeral API — Sidecar Setup"
echo "══════════════════════════════════════════════"
echo ""

[[ $EUID -eq 0 ]] || error "This script must be run as root (sudo)."

command -v python3 >/dev/null 2>&1 || error "python3 is required but not found."
command -v podman  >/dev/null 2>&1 || error "podman is required but not found."
command -v systemctl >/dev/null 2>&1 || error "systemd is required but not found."

PYTHON_VERSION=$(python3 -c "import sys; print(f'{sys.version_info.major}.{sys.version_info.minor}')")
PYTHON_MAJOR=$(echo "$PYTHON_VERSION" | cut -d. -f1)
PYTHON_MINOR=$(echo "$PYTHON_VERSION" | cut -d. -f2)
if [[ "$PYTHON_MAJOR" -lt 3 ]] || { [[ "$PYTHON_MAJOR" -eq 3 ]] && [[ "$PYTHON_MINOR" -lt 10 ]]; }; then
    error "Python 3.10+ required, found $PYTHON_VERSION"
fi
info "Python $PYTHON_VERSION detected"

PODMAN_VERSION=$(podman --version | awk '{print $NF}')
info "Podman $PODMAN_VERSION detected"

# --- Step 1: Create Service User ---
if id "$APP_USER" &>/dev/null; then
    info "User '$APP_USER' already exists"
else
    useradd --system --shell /usr/sbin/nologin --home-dir "$INSTALL_DIR" --create-home "$APP_USER"
    info "Created system user '$APP_USER'"
fi

# Enable lingering so rootless Podman can run without an active login session
loginctl enable-linger "$APP_USER" 2>/dev/null || true
info "Enabled loginctl linger for '$APP_USER'"

# --- Step 2: Install Application ---
mkdir -p "$INSTALL_DIR"

# Copy application files
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cp -r "$SCRIPT_DIR/ephemeral_core" "$INSTALL_DIR/"
cp "$SCRIPT_DIR/main_api.py" "$INSTALL_DIR/"
info "Installed application to $INSTALL_DIR"

# Create virtual environment and install dependencies
if command -v apt-get >/dev/null 2>&1; then
    apt-get install -y -qq python3 python${PYTHON_VERSION}-venv 2>/dev/null || true
fi
if [[ ! -d "$VENV_DIR" ]]; then
    python3 -m venv "$VENV_DIR"
    info "Created virtual environment at $VENV_DIR"
else
    info "Virtual environment already exists"
fi

"$VENV_DIR/bin/pip" install --quiet --upgrade pip
"$VENV_DIR/bin/pip" install --quiet fastapi uvicorn[standard] pydantic
info "Installed Python dependencies (fastapi, uvicorn, pydantic)"

# --- Step 3: Create Artifact Directory ---
mkdir -p "$WEBDAV_DIR"
chown "$APP_USER":"$APP_USER" "$WEBDAV_DIR"
chmod 755 "$WEBDAV_DIR"
info "Ensured WebDAV artifact directory at $WEBDAV_DIR"

# --- Step 4: Fix Ownership ---
chown -R "$APP_USER":"$APP_USER" "$INSTALL_DIR"

# --- Step 5: Initialize Podman (Rootless) ---
warn "Initializing Podman for user '$APP_USER' (this may take a moment)..."

APP_UID=$(id -u "$APP_USER")
if [ ! -d "/run/user/$APP_UID" ]; then
    warn "Runtime directory /run/user/$APP_UID missing (common in CI). Creating it..."
    mkdir -p "/run/user/$APP_UID"
    chown "$APP_UID":"$APP_UID" "/run/user/$APP_UID"
    chmod 700 "/run/user/$APP_UID"
    export XDG_RUNTIME_DIR="/run/user/$APP_UID"
fi

if sudo -E -u "$APP_USER" XDG_RUNTIME_DIR="/run/user/$APP_UID" podman info &>/dev/null; then
    info "Podman already initialized for '$APP_USER'"
else
    # Give the system user a dedicated pool of Sub-UIDs/Sub-GIDs for rootless Podman
    usermod --add-subuids 1000000-1065535 --add-subgids 1000000-1065535 "$APP_USER" 2>/dev/null || true
    # Then migrate Podman
    sudo -E -u "$APP_USER" XDG_RUNTIME_DIR="/run/user/$APP_UID" podman system migrate 2>/dev/null || true
    info "Podman initialized for '$APP_USER'"
fi

# --- Step 6: Create systemd Service ---
cat > "/etc/systemd/system/${APP_NAME}.service" << EOF
[Unit]
Description=Ephemeral API — Sandboxed Code Execution Sidecar
After=network.target
Wants=network.target

[Service]
Type=exec
User=${APP_USER}
Group=${APP_USER}
WorkingDirectory=${INSTALL_DIR}
Environment=PATH=${VENV_DIR}/bin:/usr/local/bin:/usr/bin:/bin
Environment=XDG_RUNTIME_DIR=/run/user/$(id -u "$APP_USER")
ExecStart=${VENV_DIR}/bin/uvicorn main_api:app \\
    --host ${BIND_HOST} \\
    --port ${SERVICE_PORT} \\
    --workers 1 \\
    --log-level info
Restart=on-failure
RestartSec=5
StandardOutput=journal
StandardError=journal

# Hardening
NoNewPrivileges=yes
ProtectSystem=strict
ProtectHome=yes
ReadWritePaths=/run/user/$(id -u "$APP_USER")
ReadWritePaths=${WEBDAV_DIR}
ReadWritePaths=/tmp
ReadWritePaths=${INSTALL_DIR}
# PrivateTmp=yes

[Install]
WantedBy=multi-user.target
EOF
info "Created systemd service: ${APP_NAME}.service"

systemctl daemon-reload
systemctl enable "$APP_NAME"
info "Enabled $APP_NAME to start on boot"

# --- Step 7: Start the Service ---
systemctl restart "$APP_NAME"
sleep 2

if systemctl is-active --quiet "$APP_NAME"; then
    info "Service is running!"
else
    warn "Service may not have started cleanly. Check: journalctl -u $APP_NAME -f"
fi

# --- Done ---
echo ""
echo "══════════════════════════════════════════════"
echo "  Setup Complete!"
echo "══════════════════════════════════════════════"
echo ""
echo "  Service:    ${APP_NAME}.service"
echo "  Endpoint:   http://${BIND_HOST}:${SERVICE_PORT}/api/v1/run"
echo "  Health:     http://${BIND_HOST}:${SERVICE_PORT}/health"
echo "  Artifacts:  ${WEBDAV_DIR}/"
echo "  Logs:       journalctl -u ${APP_NAME} -f"
echo ""
echo "  Caddy reverse-proxy example:"
echo "    route /api/v1/* {"
echo "        reverse_proxy localhost:${SERVICE_PORT}"
echo "    }"
echo ""
