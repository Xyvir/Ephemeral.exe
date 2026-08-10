#!/usr/bin/env bash
# Build ephemeral-wasm-library for wasm32-unknown-unknown and generate the
# browser glue into web/wbg/.
#
# Requirements:
#   * a stable Rust toolchain with the wasm32-unknown-unknown target
#   * a clang with the wasm backend for ring's C files — on Windows the
#     stock LLVM and llvm-mingw binaries LACK the wasm backend, so use
#     wasi-sdk's clang (set WASI_CLANG to its clang.exe)
#   * the wasm-bindgen CLI matching the pinned version (0.2.127) —
#     set WASM_BINDGEN to the executable, or install
#     `cargo install wasm-bindgen-cli --version 0.2.127`
#
# Environment:
#   RUSTUP_HOME / CARGO_HOME   toolchain locations (defaults to ~)
#   WASI_CLANG                 path to a wasm-capable clang (adds it to PATH)
#   WASM_BINDGEN               path to the wasm-bindgen CLI (default: on PATH)
set -euo pipefail
cd "$(dirname "$0")"

export RUSTUP_HOME="${RUSTUP_HOME:-$HOME/.rustup}"
export CARGO_HOME="${CARGO_HOME:-$HOME/.cargo}"
export PATH="$RUSTUP_HOME/bin:$CARGO_HOME/bin:$PATH"

if [ -n "${WASI_CLANG:-}" ]; then
  export PATH="$(dirname "$WASI_CLANG"):$PATH"
fi

rustup target list --installed --toolchain stable | grep -q wasm32-unknown-unknown \
  || rustup target add wasm32-unknown-unknown --toolchain stable

cargo build --release --target wasm32-unknown-unknown

WBG="${WASM_BINDGEN:-wasm-bindgen}"
"$WBG" --target web --out-dir web/wbg \
  target/wasm32-unknown-unknown/release/ephemeral_wasm_library.wasm

echo "Done. Serve the SPA from web/ (e.g. python -m http.server)."
