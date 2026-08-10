//! ephemeral-wasm-library — browser-side thin client for the ephemeral cluster.
//!
//! A WebAssembly client that handshakes with the iroh network and
//! translates REST-style job requests into the distributed job network.
//! It speaks the *same* wire protocol as the Python `ephemeral_net` tier:
//!
//! * one length-prefixed JSON frame per exchange (`u32be len || json`),
//! * a `hello` handshake on the first QUIC bi-stream of a connection,
//! * a `job_request` on a second bi-stream, streaming `job_log` /
//!   `job_done` / `error` frames back.
//!
//! Browsers cannot hole-punch, so every connection traverses an iroh
//! relay (n0's public relays by default; a custom relay URL can be
//! passed to [`EphemeralClient::create`]). The bootstrap config — relay
//! and the seed node's `EndpointTicket` — is supplied from JavaScript.
//!
//! ```js
//! import init, { EphemeralClient } from "./wbg/ephemeral_wasm_library.js";
//! await init();
//! const client = await EphemeralClient.create(null, null); // public n0 relay
//! const promise = client.submit_job(ticket, btoa(markdown), 300, (json) => {
//!     const evt = JSON.parse(json);
//!     // evt.type === "job_log" | "job_done" | "error"
//! });
//! ```

use std::str::FromStr;

use base64::engine::general_purpose::STANDARD as B64;
use base64::Engine as _;
use iroh::{Endpoint, RelayMode, endpoint::presets, SecretKey};
use iroh_tickets::endpoint::EndpointTicket;
use js_sys::Function;
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::future_to_promise;

/// ALPN advertised by ephemeral nodes (must match `ephemeral_net.ALPN`).
const ALPN: &[u8] = b"ephemeral/1";

/// Protocol version reported in handshakes (matches `ephemeral_net`).
const PROTOCOL_VERSION: u32 = 1;

/// Maximum frame payload size, matching `ephemeral_net`'s 16 MiB guard.
const MAX_FRAME_SIZE: usize = 16 * 1024 * 1024;

fn to_js_err(e: impl std::fmt::Display) -> JsValue {
    JsValue::from_str(&e.to_string())
}

// --- Framing (mirrors ephemeral_net.protocol) ---------------------------

fn encode_frame(msg: &serde_json::Value) -> Result<Vec<u8>, JsValue> {
    let payload = serde_json::to_vec(msg).map_err(to_js_err)?;
    if payload.len() > MAX_FRAME_SIZE {
        return Err(JsValue::from_str("frame payload too large"));
    }
    let mut out = Vec::with_capacity(4 + payload.len());
    out.extend_from_slice(&(payload.len() as u32).to_be_bytes());
    out.extend_from_slice(&payload);
    Ok(out)
}

/// Read one length-prefixed JSON frame from a stream.
async fn read_frame(recv: &mut noq::RecvStream) -> Result<serde_json::Value, JsValue> {
    let mut header = [0u8; 4];
    recv.read_exact(&mut header).await.map_err(to_js_err)?;
    let size = u32::from_be_bytes(header) as usize;
    if size > MAX_FRAME_SIZE {
        return Err(JsValue::from_str(&format!("frame payload {size} exceeds limit")));
    }
    let mut payload = vec![0u8; size];
    recv.read_exact(&mut payload).await.map_err(to_js_err)?;
    serde_json::from_slice(&payload).map_err(to_js_err)
}

/// Deliver one event object to the JS callback as a JSON string.
fn emit_event(on_event: &Function, event: &serde_json::Value) -> Result<(), JsValue> {
    let json = event.to_string();
    on_event.call1(&JsValue::NULL, &JsValue::from_str(&json))?;
    Ok(())
}

/// Run the ``hello`` handshake on a fresh connection and return the reply.
async fn hello_exchange(
    endpoint: &Endpoint,
    conn: &iroh::endpoint::Connection,
) -> Result<serde_json::Value, JsValue> {
    let (mut hs_send, mut hs_recv) = conn.open_bi().await.map_err(to_js_err)?;
    let hello = serde_json::json!({
        "type": "hello",
        "v": PROTOCOL_VERSION,
        "node_id": endpoint.id().to_string(),
        "ticket": EndpointTicket::new(endpoint.addr()).to_string(),
        "peers": [],
        "images": [],
    });
    hs_send
        .write_all(&encode_frame(&hello)?)
        .await
        .map_err(to_js_err)?;
    hs_send.finish().map_err(to_js_err)?;
    read_frame(&mut hs_recv).await
}

// --- Client -------------------------------------------------------------

/// A browser-side ephemeral client: owns an iroh endpoint and submits
/// jobs to cluster compute nodes by their EndpointTicket.
#[wasm_bindgen]
pub struct EphemeralClient {
    endpoint: Endpoint,
}

#[wasm_bindgen]
impl EphemeralClient {
    /// Create a client endpoint.
    ///
    /// `secret_key_hex` (optional) pins a persistent node identity;
    /// `relay_url` (optional) overrides the public n0 relays with a
    /// self-hosted relay (e.g. `"https://relay.example.com."`).
    pub async fn create(
        secret_key_hex: Option<String>,
        relay_url: Option<String>,
    ) -> Result<EphemeralClient, JsValue> {
        let mut builder = match relay_url {
            Some(url) => {
                let relay = RelayMode::custom(vec![url.parse().map_err(to_js_err)?]);
                Endpoint::builder(presets::Minimal).relay_mode(relay)
            }
            None => Endpoint::builder(presets::N0),
        };
        builder = builder.alpns(vec![ALPN.to_vec()]);
        if let Some(hex) = secret_key_hex {
            builder = builder.secret_key(SecretKey::from_str(&hex).map_err(to_js_err)?);
        }
        let endpoint = builder.bind().await.map_err(to_js_err)?;
        Ok(EphemeralClient { endpoint })
    }

    /// This client's node id (hex).
    pub fn node_id(&self) -> String {
        self.endpoint.id().to_string()
    }

    /// A serialized EndpointTicket others can dial this client with.
    pub fn make_ticket(&self) -> Result<String, JsValue> {
        let ticket = EndpointTicket::new(self.endpoint.addr());
        Ok(ticket.to_string())
    }

    /// Discover the cluster around a seed node — no user-supplied ticket
    /// needed beyond the bootstrap config.
    ///
    /// Dial `seed_ticket`, complete the ``hello`` handshake, and resolve
    /// with a JSON string describing the seed itself plus any peers its
    /// hello carried:
    ///
    /// ```json
    /// {"seed":{"node_id":"…","ticket":"…","images":["…"],"rtt_ms":42},
    ///  "peers":[{"node_id":"…","ticket":"…","images":[],"rtt_ms":42}]}
    /// ```
    ///
    /// Peers carry dialable EndpointTickets, so one seed is enough to
    /// learn and reach the whole cluster.
    pub fn discover(&self, seed_ticket: String) -> js_sys::Promise {
        let endpoint = self.endpoint.clone();
        future_to_promise(async move {
            let ticket = EndpointTicket::from_str(&seed_ticket).map_err(to_js_err)?;
            let addr = ticket.endpoint_addr().clone();
            let conn = endpoint.connect(addr, ALPN).await.map_err(to_js_err)?;
            // std::time::Instant panics on wasm32-unknown-unknown — use the JS clock.
            let started = js_sys::Date::now();
            let reply = hello_exchange(&endpoint, &conn).await?;
            let rtt_ms = (js_sys::Date::now() - started) as u64;

            let entry = |v: &serde_json::Value| {
                serde_json::json!({
                    "node_id": v.get("node_id").cloned().unwrap_or(serde_json::Value::Null),
                    "ticket": v.get("ticket").cloned().unwrap_or(serde_json::Value::Null),
                    "images": v.get("images").cloned().unwrap_or(serde_json::Value::Array(vec![])),
                    "rtt_ms": rtt_ms,
                })
            };

            let seed = entry(&reply);
            let mut peers: Vec<serde_json::Value> = Vec::new();
            if let Some(list) = reply.get("peers").and_then(|p| p.as_array()) {
                for peer in list {
                    let has_id = peer
                        .get("node_id")
                        .and_then(|n| n.as_str())
                        .map_or(false, |n| !n.is_empty());
                    if has_id {
                        peers.push(entry(peer));
                    }
                }
            }
            let result = serde_json::json!({ "seed": seed, "peers": peers });
            let _ = conn.close(0u32.into(), b"done");
            Ok(JsValue::from_str(&result.to_string()))
        })
    }

    /// Submit a job to the compute node described by `ticket`.
    ///
    /// `document_blob` is a base64-encoded UTF-8 Markdown document (same
    /// contract as the REST `RunRequest`). `on_event` is called once per
    /// wire frame with a JSON string: `{"type":"job_log","channel":...,
    /// "data":"<base64>"}`, `{"type":"job_done",...}`, or
    /// `{"type":"error","message":...}`. The returned promise resolves
    /// when the job terminates and rejects if the exchange fails.
    pub fn submit_job(
        &self,
        ticket: String,
        document_blob: String,
        timeout: u32,
        on_event: Function,
    ) -> js_sys::Promise {
        let endpoint = self.endpoint.clone();
        future_to_promise(async move {
            run_job(&endpoint, &ticket, &document_blob, timeout, &on_event).await
        })
    }
}

async fn run_job(
    endpoint: &Endpoint,
    ticket_str: &str,
    document_blob: &str,
    timeout: u32,
    on_event: &Function,
) -> Result<JsValue, JsValue> {
    let ticket = EndpointTicket::from_str(ticket_str).map_err(to_js_err)?;
    let addr = ticket.endpoint_addr().clone();
    let conn = endpoint.connect(addr, ALPN).await.map_err(to_js_err)?;

    // 1) hello handshake (same as ephemeral_net's dial path)
    hello_exchange(endpoint, &conn).await?;

    // 2) job request on a second bi-stream
    let (mut send, mut recv) = conn.open_bi().await.map_err(to_js_err)?;
    let request = serde_json::json!({
        "type": "job_request",
        "job_id": format!("wasm-{}", &endpoint.id().to_string()[..8]),
        "document_blob": document_blob,
        "timeout": timeout,
    });
    send.write_all(&encode_frame(&request)?)
        .await
        .map_err(to_js_err)?;
    send.finish().map_err(to_js_err)?;

    // 3) stream events back to JS until done/error
    loop {
        let frame = read_frame(&mut recv).await?;
        let kind = frame
            .get("type")
            .and_then(|v| v.as_str())
            .unwrap_or("");
        emit_event(on_event, &frame)?;
        if kind == "job_done" || kind == "error" {
            break;
        }
    }
    let _ = conn.close(0u32.into(), b"done");
    Ok(JsValue::NULL)
}

/// Decode a base64 string (used by the SPA for job_log data).
#[wasm_bindgen]
pub fn base64_decode(data: &str) -> Result<Vec<u8>, JsValue> {
    B64.decode(data).map_err(to_js_err)
}
