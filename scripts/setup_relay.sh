#!/usr/bin/env bash
# Deploy a self-hosted iroh relay server in one line.
#
# The relay is the "blind forwarder" every Ephemeral endpoint uses when
# direct p2p isn't possible. By default the swarm uses n0's public relays
# (https://*.relay.n0.iroh.link.); an org that wants to own the
# infrastructure end-to-end runs this on a server with a public IP + DNS
# name, then points every node at it with EPHEMERAL_RELAY=<url>.
#
# The relay server is the upstream `iroh-relay` BINARY (the protocol is
# only implemented in Rust — the Python/wasm tiers are clients). It runs
# as a bare systemd service — podman stays reserved for job execution.
# `RELAY_RUNTIME=podman` uses the container image instead, for hosts that
# prefer containers.
#
#   sudo RELAY_DOMAIN=relay.myorg.com ./scripts/setup_relay.sh
#   sudo RELAY_DOMAIN=relay.myorg.com RELAY_ACCESS=allowlist \
#        RELAY_ALLOW="<node-id>,<node-id>" ./scripts/setup_relay.sh
#
# Or piped (one line, no checkout needed):
#   curl -fsSL https://raw.githubusercontent.com/Xyvir/Ephemeral.exe/main/scripts/setup_relay.sh \
#     | sudo RELAY_DOMAIN=relay.myorg.com bash
#
# Environment overrides:
#   RELAY_DOMAIN    required — public DNS name pointing at this host (ACME
#                   issues the TLS cert for it, so it must resolve to here)
#   RELAY_RUNTIME   "binary" (default) | "podman"
#   RELAY_VERSION   iroh release tag to fetch (default: latest)
#   RELAY_ACCESS    "everyone" (default) | "allowlist" | "denylist" | "shared_token"
#   RELAY_ALLOW     comma-separated endpoint ids when RELAY_ACCESS=allowlist
#   RELAY_DENY      comma-separated endpoint ids when RELAY_ACCESS=denylist
#   RELAY_TOKEN     comma-separated bearer tokens when RELAY_ACCESS=shared_token
#                   (NOTE: the current Ephemeral Python/wasm clients cannot
#                   present a relay token yet — use allowlist/denylist for
#                   org-only control today)
#   RELAY_EMAIL     ACME contact email (optional; recommended for expiry notices)
#   RELAY_IMAGE     container image when RELAY_RUNTIME=podman
#                   (default: n0computer/iroh-relay:latest)
#   RELAY_NO_SYSTEMD 1 — run the relay directly in the foreground instead
#                   of installing a systemd unit (debug/foreground use)
#
# The relay binds ports 80 (HTTP / ACME challenge), 443 (HTTPS relay) and
# 7842 (QUIC address discovery) — privileged ports, hence root/sudo. After
# install, point nodes at it:
#   EPHEMERAL_RELAY="https://$RELAY_DOMAIN"            # every node
#   EPHEMERAL_RELAY_FALLBACK=1                         # + public n0 fallback
# and the wasm client's Relay URL field (or BOOTSTRAP.relayUrl).
set -euo pipefail

RELAY_DOMAIN="${RELAY_DOMAIN:-${1:-}}"
if [ -z "$RELAY_DOMAIN" ]; then
  echo "ERROR: RELAY_DOMAIN is required (public DNS name of this host)." >&2
  echo "       e.g.  sudo RELAY_DOMAIN=relay.myorg.com $0" >&2
  exit 1
fi

RELAY_RUNTIME="${RELAY_RUNTIME:-binary}"
RELAY_ACCESS="${RELAY_ACCESS:-everyone}"
CONF_DIR="${RELAY_CONF_DIR:-/etc/iroh-relay}"
CONF="$CONF_DIR/config.toml"
BIN_DIR="${RELAY_BIN_DIR:-/usr/local/bin}"
RELAY_BIN="$BIN_DIR/iroh-relay"
SUDO=""
if [ "$(id -u)" -ne 0 ]; then
  if command -v sudo >/dev/null 2>&1; then
    SUDO="sudo"
  else
    echo "ERROR: ports 80/443/7842 are privileged — run as root (or via sudo)." >&2
    exit 1
  fi
fi

echo "==> iroh relay: $RELAY_DOMAIN (runtime: $RELAY_RUNTIME, access: $RELAY_ACCESS)"
if [ "$RELAY_ACCESS" = "shared_token" ]; then
  echo "    ! warning: Ephemeral clients can't present a relay token yet (iroh"
  echo "      FFI lacks with_auth_token) — a shared_token relay would reject"
  echo "      every Ephemeral node. Prefer allowlist/denylist for org control."
fi

# --- relay binary --------------------------------------------------------

if [ "$RELAY_RUNTIME" = "binary" ]; then
  if ! command -v "$RELAY_BIN" >/dev/null 2>&1; then
    # Map the host to n0's release asset triple.
    ARCH="$(uname -m)"
    case "$ARCH" in
      x86_64) TRIPLE="x86_64-unknown-linux-gnu" ;;
      aarch64|arm64) TRIPLE="aarch64-unknown-linux-gnu" ;;
      *) echo "ERROR: unsupported arch '$ARCH' for the iroh-relay binary (use RELAY_RUNTIME=podman)." >&2; exit 1 ;;
    esac
    RELAY_VERSION="${RELAY_VERSION:-latest}"
    if [ "$RELAY_VERSION" = "latest" ]; then
      TAG="$(curl -fsSL https://api.github.com/repos/n0-computer/iroh/releases/latest \
        | sed -n 's/.*"tag_name": *"\([^"]*\)".*/\1/p' | head -1)"
    else
      TAG="v${RELAY_VERSION#v}"
    fi
    echo "==> downloading iroh-relay $TAG ($TRIPLE)..."
    TMP="$(mktemp -d)"
    trap 'rm -rf "$TMP"' EXIT
    curl -fsSL "https://github.com/n0-computer/iroh/releases/download/$TAG/iroh-relay-$TAG-$TRIPLE.tar.gz" \
      -o "$TMP/iroh-relay.tar.gz"
    tar -xzf "$TMP/iroh-relay.tar.gz" -C "$TMP"
    # The tarball contains the binary (and possibly docs); install just the
    # executable, wherever it landed. Note: n0's release tarballs strip the
    # exec bit (mode 0644), so match by name and let install -m fix perms.
    BIN="$(find "$TMP" -type f -name iroh-relay | head -1)"
    [ -n "$BIN" ] || { echo "ERROR: no iroh-relay executable in the tarball" >&2; exit 1; }
    $SUDO install -m 0755 "$BIN" "$RELAY_BIN"
    echo "==> installed $RELAY_BIN"
  fi
  RUN_CMD="$RELAY_BIN --config-path $CONF"
elif [ "$RELAY_RUNTIME" = "podman" ]; then
  if ! command -v podman >/dev/null 2>&1; then
    echo "==> podman not found — installing it (sudo required)..."
    $SUDO apt-get update -y >/dev/null 2>&1 || true
    $SUDO apt-get install -y --no-install-recommends podman >/dev/null
  fi
  PODMAN="$(command -v podman)"
  RELAY_IMAGE="${RELAY_IMAGE:-n0computer/iroh-relay:latest}"
  RUN_CMD="$PODMAN run --name iroh-relay --rm -p 80:80 -p 443:443 -p 7842:7842 -v $CONF_DIR:/config:Z -e RUST_LOG=info $RELAY_IMAGE --config-path /config/config.toml"
else
  echo "ERROR: unknown RELAY_RUNTIME '$RELAY_RUNTIME' (binary|podman)" >&2
  exit 1
fi

# --- config --------------------------------------------------------------

$SUDO mkdir -p "$CONF_DIR"
$SUDO tee "$CONF" >/dev/null <<EOF
# iroh relay server config — generated by scripts/setup_relay.sh
# Schema: https://github.com/n0-computer/iroh (iroh-relay/src/main.rs)

enable_relay = true
enable_quic_addr_discovery = true
enable_metrics = false
http_bind_addr = "[::]:80"

[tls]
hostname = "$RELAY_DOMAIN"
cert_mode = "LetsEncrypt"
prod_tls = true
cert_dir = "$CONF_DIR/certs"
EOF
if [ -n "${RELAY_EMAIL:-}" ]; then
  $SUDO bash -c "echo 'contact = \"$RELAY_EMAIL\"' >> '$CONF'"
fi

_csv_quoted() {
  # "a,b,c" -> "a", "b", "c" (TOML array values)
  local out="" item
  local IFS=','
  for item in $1; do
    [ -n "$out" ] && out="$out, "
    out="$out\"$item\""
  done
  printf '%s' "$out"
}

case "$RELAY_ACCESS" in
  everyone) ;;
  shared_token)
    [ -n "${RELAY_TOKEN:-}" ] || { echo "ERROR: RELAY_ACCESS=shared_token needs RELAY_TOKEN" >&2; exit 1; }
    printf '\naccess.shared_token = [%s]\n' "$(_csv_quoted "$RELAY_TOKEN")" | $SUDO tee -a "$CONF" >/dev/null
    ;;
  allowlist)
    [ -n "${RELAY_ALLOW:-}" ] || { echo "ERROR: RELAY_ACCESS=allowlist needs RELAY_ALLOW" >&2; exit 1; }
    printf '\naccess.allowlist = [%s]\n' "$(_csv_quoted "$RELAY_ALLOW")" | $SUDO tee -a "$CONF" >/dev/null
    ;;
  denylist)
    [ -n "${RELAY_DENY:-}" ] || { echo "ERROR: RELAY_ACCESS=denylist needs RELAY_DENY" >&2; exit 1; }
    printf '\naccess.denylist = [%s]\n' "$(_csv_quoted "$RELAY_DENY")" | $SUDO tee -a "$CONF" >/dev/null
    ;;
  *) echo "ERROR: unknown RELAY_ACCESS '$RELAY_ACCESS'" >&2; exit 1 ;;
esac

echo "==> wrote $CONF"

# --- firewall (best-effort) ---------------------------------------------

if command -v ufw >/dev/null 2>&1; then
  $SUDO ufw allow 80/tcp >/dev/null 2>&1 || true
  $SUDO ufw allow 443/tcp >/dev/null 2>&1 || true
  $SUDO ufw allow 7842/tcp >/dev/null 2>&1 || true
  echo "==> ufw: allowed 80/443/7842"
fi

# --- run -----------------------------------------------------------------

if [ "${RELAY_NO_SYSTEMD:-0}" = "1" ]; then
  echo "==> starting in foreground (Ctrl+C to stop)..."
  exec $RUN_CMD
fi

UNIT="/etc/systemd/system/iroh-relay.service"
$SUDO tee "$UNIT" >/dev/null <<EOF
[Unit]
Description=iroh relay server ($RELAY_DOMAIN)
After=network-online.target
Wants=network-online.target

[Service]
Restart=always
ExecStart=$RUN_CMD

[Install]
WantedBy=multi-user.target
EOF
$SUDO systemctl daemon-reload
$SUDO systemctl enable --now iroh-relay >/dev/null 2>&1 || true
echo "==> installed systemd unit: $UNIT"

echo "==> relay started — verify:  curl -sI https://$RELAY_DOMAIN/generate_204"
echo ""
echo "Point your Ephemeral nodes at it:"
echo "    EPHEMERAL_RELAY=\"https://$RELAY_DOMAIN\""
echo "    EPHEMERAL_RELAY_FALLBACK=1    # optional: keep public n0 relays as backup"
echo "Wasm client: use the Relay URL field / BOOTSTRAP.relayUrl = \"https://$RELAY_DOMAIN\""
if [ "$RELAY_ACCESS" = "shared_token" ]; then
  echo "Shared-token access is set — BUT see the warning above: Ephemeral"
  echo "clients cannot present the token with the current iroh FFI, so this"
  echo "relay will reject them. Other iroh apps can connect via"
  echo "RelayMap::with_auth_token (or '?token=' URL query)."
fi
echo "Logs: journalctl -u iroh-relay -f"
