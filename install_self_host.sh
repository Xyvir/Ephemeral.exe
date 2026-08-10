#!/usr/bin/env bash
# Install the Ephemeral self-host server in one line.
#
#   # Non-distributed REST API (local-only execution — the Lithic-UK build)
#   curl -fsSL https://raw.githubusercontent.com/Xyvir/Ephemeral.exe/main/install_self_host.sh | bash -s -- local
#
#   # Distributed gateway (joins the ephemeral cluster as a compute node)
#   curl -fsSL https://raw.githubusercontent.com/Xyvir/Ephemeral.exe/main/install_self_host.sh | bash -s -- distributed
#
# Environment overrides:
#   INSTALL_DIR   target directory (default: ~/ephemeral-self-host)
#   PORT          uvicorn port (default: 8787 — Lithic-UK's Caddyfile
#                 proxies /ephemeral/api/v1/* to this port, so the REST
#                 API drops straight into the existing sidecar slot)
#   SYSTEMD=1     install a user systemd unit instead of printing the run command
#   EPHEMERAL_FROM_MAIN=1
#                 skip the released tarball and install from the main branch
#                 (used by CI to test the installer deterministically)
#   EPHEMERAL_RELAY / EPHEMERAL_SEEDS / EPHEMERAL_SECRET / EPHEMERAL_ALLOW_NETWORK
#                 distributed-tier configuration (passed to the service/command)
set -euo pipefail

FLAVOR="${1:-local}"
case "$FLAVOR" in
  local|distributed) ;;
  *) echo "unknown flavor: $FLAVOR (expected local|distributed)" >&2; exit 1 ;;
esac

INSTALL_DIR="${INSTALL_DIR:-$HOME/ephemeral-self-host}"
# 8787 matches Lithic-UK's generated Caddyfile sidecar slot
# (handle /ephemeral/api/v1/* { reverse_proxy 127.0.0.1:8787 }).
PORT="${PORT:-8787}"
REPO="Xyvir/Ephemeral.exe"

echo "==> Installing ephemeral-self-host ($FLAVOR) into $INSTALL_DIR"

mkdir -p "$INSTALL_DIR"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

# Prefer the released tarball; fall back to the main branch before the first
# release (or when EPHEMERAL_FROM_MAIN=1, which CI uses for a deterministic test).
if [ "$FLAVOR" = "distributed" ]; then
  ASSET="ephemeral-self-host-distributed.tar.gz"
else
  ASSET="ephemeral-self-host.tar.gz"
fi
if [ "${EPHEMERAL_FROM_MAIN:-0}" != "1" ] \
  && curl -fsSL -o "$TMP/tarball.tar.gz" "https://github.com/$REPO/releases/latest/download/$ASSET"; then
  echo "==> Using release asset $ASSET"
  tar -xzf "$TMP/tarball.tar.gz" -C "$INSTALL_DIR"
else
  echo "==> Installing from the main branch"
  curl -fsSL -o "$TMP/main.tar.gz" "https://github.com/$REPO/archive/refs/heads/main.tar.gz"
  tar -xzf "$TMP/main.tar.gz" -C "$TMP"
  SRC="$TMP/Ephemeral.exe-main"
  if [ "$FLAVOR" = "distributed" ]; then
    # main_distributed.py imports main_api (wire contract) -> ephemeral_core,
    # and the gateway pulls in ephemeral_net — all four modules are required.
    cp -r "$SRC/ephemeral_self_host" "$SRC/ephemeral_net" "$SRC/ephemeral_core" "$INSTALL_DIR/"
    cp "$SRC/main_distributed.py" "$SRC/main_api.py" "$INSTALL_DIR/"
    cp "$SRC/requirements-net.txt" "$SRC/requirements-api.txt" "$INSTALL_DIR/"
    cp "$SRC/Dockerfile" "$SRC/.dockerignore" "$INSTALL_DIR/" 2>/dev/null || true
  else
    cp "$SRC/main_api.py" "$INSTALL_DIR/"
    cp -r "$SRC/ephemeral_core" "$INSTALL_DIR/"
    cp "$SRC/requirements-api.txt" "$INSTALL_DIR/"
    cp "$SRC/Dockerfile.api" "$SRC/.dockerignore" "$INSTALL_DIR/" 2>/dev/null || true
  fi
fi

# Virtualenv + dependencies.
if [ ! -d "$INSTALL_DIR/.venv" ]; then
  if ! python3 -m venv "$INSTALL_DIR/.venv" 2>"$TMP/venv.err"; then
    echo "Could not create a virtualenv. On Debian/Ubuntu, install python3-venv first:" >&2
    echo "  sudo apt-get install -y python3-venv" >&2
    cat "$TMP/venv.err" >&2
    exit 1
  fi
fi
"$INSTALL_DIR/.venv/bin/pip" install -q --upgrade pip
"$INSTALL_DIR/.venv/bin/pip" install -q -r "$INSTALL_DIR/requirements-api.txt"
if [ "$FLAVOR" = "distributed" ]; then
  "$INSTALL_DIR/.venv/bin/pip" install -q -r "$INSTALL_DIR/requirements-net.txt"
fi

# Systemd unit (optional) or a plain run command.
APP="main_api"
[ "$FLAVOR" = "distributed" ] && APP="main_distributed"
if [ "${SYSTEMD:-0}" = "1" ]; then
  UNIT_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/systemd/user"
  mkdir -p "$UNIT_DIR"
  UNIT="$UNIT_DIR/ephemeral-self-host.service"
  {
    echo "[Unit]"
    echo "Description=Ephemeral self-host server ($FLAVOR)"
    echo "After=network-online.target"
    echo ""
    echo "[Service]"
    echo "WorkingDirectory=$INSTALL_DIR"
    echo "ExecStart=$INSTALL_DIR/.venv/bin/uvicorn $APP:app --host 0.0.0.0 --port $PORT"
    if [ "$FLAVOR" = "distributed" ]; then
      [ -n "${EPHEMERAL_RELAY:-}" ]       && echo "Environment=EPHEMERAL_RELAY=$EPHEMERAL_RELAY"
      [ -n "${EPHEMERAL_SEEDS:-}" ]       && echo "Environment=EPHEMERAL_SEEDS=$EPHEMERAL_SEEDS"
      [ -n "${EPHEMERAL_SECRET:-}" ]      && echo "Environment=EPHEMERAL_SECRET=$EPHEMERAL_SECRET"
      [ -n "${EPHEMERAL_ALLOW_NETWORK:-}" ] && echo "Environment=EPHEMERAL_ALLOW_NETWORK=$EPHEMERAL_ALLOW_NETWORK"
    fi
    echo "Restart=on-failure"
    echo ""
    echo "[Install]"
    echo "WantedBy=default.target"
  } > "$UNIT"
  systemctl --user daemon-reload
  echo "==> Installed user service: $UNIT"
  echo "    Start:   systemctl --user enable --now ephemeral-self-host"
  echo "    Follow:  journalctl --user -u ephemeral-self-host -f"
else
  echo "==> Installed. Run it with:"
  echo "    $INSTALL_DIR/.venv/bin/uvicorn $APP:app --host 0.0.0.0 --port $PORT"
  if [ "$FLAVOR" = "distributed" ]; then
    echo "    (configure the cluster via EPHEMERAL_SEEDS / EPHEMERAL_RELAY / EPHEMERAL_SECRET / EPHEMERAL_ALLOW_NETWORK)"
  fi
fi
