# ephemeral-wasm-library

Browser-side WebAssembly client for the Ephemeral distributed cluster. It
handshakes with the iroh network and speaks the **same wire protocol** as the
Python `ephemeral_net` tier (`hello` handshake + `job_request` →
`job_log`/`job_done`/`error` over iroh QUIC bi-streams), so it interoperates
with Python compute nodes with zero translation. Browsers cannot hole-punch,
so all browser↔cluster traffic traverses an iroh relay — n0's public relays
by default, or a self-hosted one.

## Layout

- `src/lib.rs` — the wasm crate: `EphemeralClient::create()` (endpoint),
  `discover(seed_ticket)` (learn the cluster from a seed's hello reply),
  `submit_job(ticket, doc, timeout, on_event)` (streamed job events).
- `web/` — the SPA thin client (vanilla JS): auto-connects using the seed
  tickets in `web/config.js`, auto-routes jobs to the best node, and renders
  streamed output. The built glue is committed under `web/wbg/` so the SPA
  runs without a Rust toolchain.
- `build.sh` — rebuilds the wasm module (see its header for the toolchain
  requirements: stable Rust with `wasm32-unknown-unknown`, a wasm-capable
  clang for `ring`'s C files such as wasi-sdk, and the `wasm-bindgen` CLI
  pinned to 0.2.127).

## Run the SPA

```bash
cd web && python -m http.server 8787
# open http://localhost:8787 — no ticket pasting needed
```

## Security note

The public network is a good-faith model for teaching. Anything submitted to
the public ephemeral cloud is public knowledge — no privacy guarantee. For
private use, self-host and point `web/config.js` at your own relay + seeds.
