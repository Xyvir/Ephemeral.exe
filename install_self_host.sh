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
#   SYSTEMD=1     install a user systemd unit AND start it, instead of
#                 printing the run command
#   EPHEMERAL_FROM_MAIN=1
#                 skip the released tarball and install from the main branch
#                 (used by CI to test the installer deterministically)
#   EPHEMERAL_RELAY / EPHEMERAL_SEEDS / EPHEMERAL_SECRET / EPHEMERAL_ALLOW_NETWORK
#                 distributed-tier configuration (passed to the service/command)
#   EPHEMERAL_PRIVATE=1
#                 skip the public swarm list — run a private classroom node
#                 (distributed flavor only; prints a student-ready #seed= URL)
#
# Podman is part of the install: the script installs the binary if missing
# (sudo), configures rootless storage (subuid/subgid ranges, linger, the
# user socket), and can pre-hydrate the language image map. It also
# self-heals a missing python3-venv on minimal images and validates
# EPHEMERAL_SECRET before installing.
#   EPHEMERAL_STORAGE_ROOT  relocate rootless Podman's image cache to this
#                 host path (e.g. a big attached block volume) — written to
#                 ~/.config/containers/storage.conf before first use, and
#                 passed to the service as the runtime's space-check root
#   EPHEMERAL_PREHYDRATE    "1" | "0" (default "0" — off). The installer
#                 always pulls the bash canary image as an end-to-end
#                 podman check; "1" additionally hydrates the full language
#                 map (~15-25 GB) so the node is warm from the first hello
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

# --- Podman (rootless) — required for job execution -----------------------
# The one-line installer owns the whole Podman story: installs the binary if
# missing (sudo), wires up rootless storage (subuid/subgid ranges, linger,
# the user socket), and can point the image cache at a big attached volume.

SUDO=""
[ "$(id -u)" -ne 0 ] && command -v sudo >/dev/null 2>&1 && SUDO="sudo"

require_podman() {
    if command -v podman >/dev/null 2>&1; then
        echo "==> podman $(podman --version | awk '{print $NF}') detected"
        return 0
    fi
    echo "==> podman not found — installing it (${SUDO:-root} required)..."
    if [ -n "$SUDO" ] || [ "$(id -u)" -eq 0 ]; then
        if command -v apt-get >/dev/null 2>&1; then
            $SUDO apt-get update -qq || true
            $SUDO DEBIAN_FRONTEND=noninteractive apt-get install -y -qq podman
        elif command -v dnf >/dev/null 2>&1; then
            $SUDO dnf install -y podman
        elif command -v yum >/dev/null 2>&1; then
            $SUDO yum install -y podman
        elif command -v zypper >/dev/null 2>&1; then
            $SUDO zypper --non-interactive install podman
        elif command -v pacman >/dev/null 2>&1; then
            $SUDO pacman -Sy --noconfirm podman
        elif command -v apk >/dev/null 2>&1; then
            $SUDO apk add --no-cache podman
        else
            echo "    ! no supported package manager found" >&2
        fi
    fi
    command -v podman >/dev/null 2>&1 || {
        echo "ERROR: podman is required to execute code but could not be installed." >&2
        echo "       Install it manually (e.g. 'sudo apt-get install -y podman') and re-run." >&2
        exit 1
    }
}

configure_storage_root() {
    # EPHEMERAL_STORAGE_ROOT moves the image cache off the boot volume — e.g.
    # onto a big attached block device. Written BEFORE the first podman run
    # so the storage graph is created there from the start. The path must
    # already be a mounted filesystem: the installer formats nothing and
    # mounts nothing (mkfs + mount + fstab are the operator's job).
    if [ -z "${EPHEMERAL_STORAGE_ROOT:-}" ]; then
        return 0
    fi
    case "$EPHEMERAL_STORAGE_ROOT" in
        /*) ;;
        *)  echo "ERROR: EPHEMERAL_STORAGE_ROOT must be an absolute path (got '$EPHEMERAL_STORAGE_ROOT')." >&2
            exit 1 ;;
    esac
    if ! mkdir -p "$EPHEMERAL_STORAGE_ROOT" 2>/dev/null; then
        echo "ERROR: EPHEMERAL_STORAGE_ROOT ($EPHEMERAL_STORAGE_ROOT) is not writable." >&2
        echo "       Format and mount the block volume there first, e.g.:" >&2
        echo "         sudo mkfs.ext4 /dev/sdb && sudo mkdir -p $EPHEMERAL_STORAGE_ROOT" >&2
        echo "         sudo mount /dev/sdb $EPHEMERAL_STORAGE_ROOT   # + /etc/fstab entry" >&2
        echo "       then re-run the installer." >&2
        exit 1
    fi
    BOOT_DEV="$(df -P / 2>/dev/null | awk 'NR==2 {print $1}')"
    STORE_DEV="$(df -P "$EPHEMERAL_STORAGE_ROOT" 2>/dev/null | awk 'NR==2 {print $1}')"
    if [ -n "$BOOT_DEV" ] && [ "$BOOT_DEV" = "$STORE_DEV" ]; then
        echo "    ! warning: EPHEMERAL_STORAGE_ROOT is on the same filesystem as / —" >&2
        echo "      the image cache will live on the boot volume, not a dedicated one" >&2
    fi
    { [ -n "$SUDO" ] && $SUDO chown -R "$(id -u)":"$(id -g)" "$EPHEMERAL_STORAGE_ROOT"; } 2>/dev/null || true
    mkdir -p "$HOME/.config/containers"
    cat > "$HOME/.config/containers/storage.conf" <<EOF
[storage]
driver = "overlay"
graphroot = "$EPHEMERAL_STORAGE_ROOT"
EOF
    echo "==> Podman image cache -> $EPHEMERAL_STORAGE_ROOT"
}

configure_podman_dns() {
    # Rootless systemd-resolved loopback (127.0.0.53) breaks container DNS on
    # many Ubuntu hosts — pin public resolvers (mirrors install.sh).
    mkdir -p "$HOME/.config/containers"
    if [ ! -f "$HOME/.config/containers/containers.conf" ]; then
        cat > "$HOME/.config/containers/containers.conf" <<'EOF'
[containers]
dns_servers = [
  "8.8.8.8",
  "1.1.1.1"
]
EOF
    fi
}

setup_rootless_podman() {
    # Rootful (running as root) needs none of this.
    [ "$(id -u)" -eq 0 ] && return 0

    MY_UID=$(id -u)
    # Over SSH the XDG runtime dir is often missing — create it (sudo) and
    # export it so the user systemd manager and rootless podman can start.
    if [ -z "${XDG_RUNTIME_DIR:-}" ]; then
        export XDG_RUNTIME_DIR="/run/user/$MY_UID"
    fi
    if [ ! -d "$XDG_RUNTIME_DIR" ] && [ -n "$SUDO" ]; then
        $SUDO mkdir -p "$XDG_RUNTIME_DIR"
        $SUDO chown "$MY_UID":"$(id -g)" "$XDG_RUNTIME_DIR"
        $SUDO chmod 700 "$XDG_RUNTIME_DIR"
    fi

    if [ -n "$SUDO" ]; then
        # One dedicated pool of sub-UIDs/sub-GIDs for rootless containers
        # (the same range install.sh uses for its service user).
        if ! grep -q "^$USER:" /etc/subuid 2>/dev/null; then
            $SUDO usermod --add-subuids 1000000-1065535 --add-subgids 1000000-1065535 "$USER" \
                || echo "    ! could not add subuid/subgid ranges — rootless podman will fail" >&2
        fi
        # Survive logout: keep the user systemd manager + socket alive.
        $SUDO loginctl enable-linger "$USER" 2>/dev/null || true
    fi
    # The engine shells out to the podman CLI; the user socket keeps the
    # rootless service addressable (and is what `systemctl --user` units use).
    if command -v systemctl >/dev/null 2>&1; then
        systemctl --user enable --now podman.socket 2>/dev/null || true
    fi
    # Initialize the (possibly redirected) storage graph once.
    podman system migrate >/dev/null 2>&1 || podman info >/dev/null 2>&1 \
        || echo "    ! podman self-check failed — inspect with 'podman info'" >&2
    echo "==> rootless Podman configured"
}

prehydrate_images() {
    # End-to-end podman check: pull the bash canary image. If the rootless
    # stack works (storage graph, DNS, subuids), this pull succeeds — the
    # same one-shot verification install.sh performs. Best-effort, never fatal.
    if podman pull docker.io/library/alpine:latest >/dev/null 2>&1; then
        echo "==> podman end-to-end OK — bash image warm"
    else
        echo "    ! could not pull the bash image — inspect 'podman info' (and containers.conf DNS)" >&2
    fi

    # Full language-map hydration is opt-in (EPHEMERAL_PREHYDRATE=1): it pulls
    # ~15-25 GB and can take a while, so it is OFF by default. "1" turns a
    # thick node into a super-seed — every image the allowlist may request,
    # warm from the first hello frame.
    if [ "${EPHEMERAL_PREHYDRATE:-0}" != "1" ]; then
        return 0
    fi
    echo "==> Pre-hydrating every language-map image (set EPHEMERAL_PREHYDRATE=0 to skip)..."
    # hydrate_images.py isn't shipped in the self-host tarball — fetch the
    # canonical copy from the repo (pulls mapped_images(), skips cached
    # images, retries with backoff, never aborts on a single image).
    if curl --retry 3 --retry-delay 2 -fsSL \
        "https://raw.githubusercontent.com/$REPO/main/scripts/hydrate_images.py" \
        -o "$INSTALL_DIR/.hydrate_images.py"; then
        PYTHONPATH="$INSTALL_DIR" "$INSTALL_DIR/.venv/bin/python" "$INSTALL_DIR/.hydrate_images.py" \
            || echo "    ! pre-hydration finished with failures (see above)" >&2
        rm -f "$INSTALL_DIR/.hydrate_images.py"
    else
        echo "    ! could not fetch hydrate_images.py — images will pull on demand" >&2
    fi
}

ensure_venv() {
    # A fresh minimal Ubuntu image ships python3 without ensurepip, so
    # `python3 -m venv` fails outright. Detect that, self-heal by installing
    # the distro's venv package (sudo), then retry; other venv failures still
    # abort with the original error and instructions.
    if [ -d "$INSTALL_DIR/.venv" ]; then
        return 0
    fi
    if ! python3 -c "import ensurepip" >/dev/null 2>&1; then
        echo "==> python3-venv missing — installing it (${SUDO:-root} required)..."
        if [ -n "$SUDO" ] || [ "$(id -u)" -eq 0 ]; then
            if command -v apt-get >/dev/null 2>&1; then
                $SUDO apt-get update -qq || true
                $SUDO DEBIAN_FRONTEND=noninteractive apt-get install -y -qq python3-venv python3-pip
            elif command -v dnf >/dev/null 2>&1; then
                $SUDO dnf install -y python3-virtualenv python3-pip 2>/dev/null || $SUDO dnf install -y python3-venv
            elif command -v yum >/dev/null 2>&1; then
                $SUDO yum install -y python3-virtualenv python3-pip
            elif command -v zypper >/dev/null 2>&1; then
                $SUDO zypper --non-interactive install python3-virtualenv python3-pip
            elif command -v pacman >/dev/null 2>&1; then
                $SUDO pacman -Sy --noconfirm python-virtualenv
            elif command -v apk >/dev/null 2>&1; then
                $SUDO apk add --no-cache py3-virtualenv
            fi
        fi
    fi
    if ! python3 -m venv "$INSTALL_DIR/.venv" 2>"$TMP/venv.err"; then
        echo "Could not create a virtualenv. On Debian/Ubuntu, install python3-venv first:" >&2
        echo "  sudo apt-get install -y python3-venv" >&2
        cat "$TMP/venv.err" >&2
        exit 1
    fi
}

require_podman
configure_storage_root
configure_podman_dns
setup_rootless_podman

# Guardrail: a non-hex EPHEMERAL_SECRET crashes the gateway at import time
# (bytes.fromhex at module load) — fail here with an actionable message.
if [ -n "${EPHEMERAL_SECRET:-}" ] && ! printf '%s' "$EPHEMERAL_SECRET" | grep -Eq '^[0-9a-fA-F]{64}$'; then
    echo "ERROR: EPHEMERAL_SECRET must be exactly 64 hex characters (32 bytes)." >&2
    echo "       Generate one with:  openssl rand -hex 32" >&2
    exit 1
fi

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
  && curl --retry 3 --retry-delay 2 -fsSL -o "$TMP/tarball.tar.gz" "https://github.com/$REPO/releases/latest/download/$ASSET"; then
  echo "==> Using release asset $ASSET"
  tar -xzf "$TMP/tarball.tar.gz" -C "$INSTALL_DIR"
else
  echo "==> Installing from the main branch"
  # Prefer a shallow clone over the codeload tarball endpoint: github.com's
  # on-demand archive generation throttles anonymous downloads (HTTP 503)
  # from CI/datacenter IPs. Smart-HTTP clone is far more reliable.
  if command -v git >/dev/null 2>&1; then
    git clone --depth 1 --quiet "https://github.com/$REPO.git" "$TMP/repo"
    SRC="$TMP/repo"
  else
    curl --retry 3 --retry-delay 2 -fsSL -o "$TMP/main.tar.gz" "https://github.com/$REPO/archive/refs/heads/main.tar.gz"
    tar -xzf "$TMP/main.tar.gz" -C "$TMP"
    SRC="$TMP/Ephemeral.exe-main"
  fi
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
ensure_venv
"$INSTALL_DIR/.venv/bin/pip" install -q --upgrade pip
"$INSTALL_DIR/.venv/bin/pip" install -q -r "$INSTALL_DIR/requirements-api.txt"
if [ "$FLAVOR" = "distributed" ]; then
  "$INSTALL_DIR/.venv/bin/pip" install -q -r "$INSTALL_DIR/requirements-net.txt"
fi

# Systemd unit (optional) or a plain run command.
APP="main_api"
[ "$FLAVOR" = "distributed" ] && APP="main_distributed"
# Verify podman end-to-end (bash canary pull); full-map hydration is opt-in
# via EPHEMERAL_PREHYDRATE=1 and runs here too, before the service starts.
prehydrate_images

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
      [ -n "${EPHEMERAL_PRIVATE:-}" ]      && echo "Environment=EPHEMERAL_PRIVATE=$EPHEMERAL_PRIVATE"
    fi
    [ -n "${EPHEMERAL_STORAGE_ROOT:-}" ]  && echo "Environment=EPHEMERAL_STORAGE_ROOT=$EPHEMERAL_STORAGE_ROOT"
    echo "Restart=on-failure"
    echo ""
    echo "[Install]"
    echo "WantedBy=default.target"
  } > "$UNIT"
  systemctl --user daemon-reload
  echo "==> Installed user service: $UNIT"
  systemctl --user enable --now ephemeral-self-host
  echo "    Follow:  journalctl --user -u ephemeral-self-host -f"
else
  echo "==> Installed. Run it with:"
  echo "    $INSTALL_DIR/.venv/bin/uvicorn $APP:app --host 0.0.0.0 --port $PORT"
  if [ "$FLAVOR" = "distributed" ]; then
    echo "    (configure the cluster via EPHEMERAL_SEEDS / EPHEMERAL_RELAY / EPHEMERAL_SECRET / EPHEMERAL_ALLOW_NETWORK / EPHEMERAL_PRIVATE)"
  fi
fi
