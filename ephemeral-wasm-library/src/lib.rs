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
//! and the seed node's stable node id + relay — is supplied from
//! JavaScript (``web/config.js``, mirrored from ``ephemeral_net/swarm.py``).
//! Dialing is iroh-native: nodes are addressed by their stable node id
//! through a relay, no EndpointTicket required (tickets remain supported
//! as a fallback for legacy peers).
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
use iroh::{Endpoint, EndpointAddr, EndpointId, RelayMode, RelayUrl, endpoint::presets, SecretKey};
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

/// Build a relay-routed address from a stable node id + relay URL.
///
/// This is the iroh-native dial (same mechanism `ephemeral_net.dial_node`
/// uses): the relay routes by node id, so a compiled-in id never goes
/// stale across seed restarts — no EndpointTicket required.
fn node_addr(node_id: &str, relay_url: &str) -> Result<EndpointAddr, JsValue> {
    let id = EndpointId::from_str(node_id).map_err(to_js_err)?;
    let relay = RelayUrl::from_str(relay_url).map_err(to_js_err)?;
    Ok(EndpointAddr::new(id).with_relay_url(relay))
}

/// Run the ``hello`` handshake on a fresh connection and return the reply.
/// `relay` is this client's relay URL (or null) — it lets peers dial us
/// back by node id, mirroring the Python tier's hello frames.
async fn hello_exchange(
    endpoint: &Endpoint,
    conn: &iroh::endpoint::Connection,
    relay: Option<&str>,
) -> Result<serde_json::Value, JsValue> {
    let (mut hs_send, mut hs_recv) = conn.open_bi().await.map_err(to_js_err)?;
    let hello = serde_json::json!({
        "type": "hello",
        "v": PROTOCOL_VERSION,
        "node_id": endpoint.id().to_string(),
        "relay": relay,
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
/// jobs to cluster compute nodes by stable node id + relay (or by
/// EndpointTicket as a fallback).
#[wasm_bindgen]
pub struct EphemeralClient {
    endpoint: Endpoint,
    // This client's configured relay (None = n0 public relays) — sent in
    // hello frames so peers can dial us back by node id.
    relay: Option<String>,
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
        let mut builder = match relay_url.as_deref() {
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
        Ok(EphemeralClient {
            endpoint,
            relay: relay_url,
        })
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
    /// Discover the cluster around a seed node — no user-supplied ticket
    /// needed beyond the bootstrap config.
    ///
    /// Dial `seed_ticket`, complete the ``hello`` handshake, and resolve
    /// with a JSON string describing the seed itself plus any peers its
    /// hello carried:
    ///
    /// ```json
    /// {"seed":{"node_id":"…","relay":"…","ticket":"…","images":["…"],"rtt_ms":42},
    ///  "peers":[{"node_id":"…","relay":"…","ticket":"…","images":[],"rtt_ms":42}]}
    /// ```
    ///
    /// Peers carry dialable EndpointTickets (and relays when known), so
    /// one seed is enough to learn and reach the whole cluster.
    pub fn discover(&self, seed_ticket: String) -> js_sys::Promise {
        let endpoint = self.endpoint.clone();
        let relay = self.relay.clone();
        future_to_promise(async move {
            let ticket = EndpointTicket::from_str(&seed_ticket).map_err(to_js_err)?;
            let addr = ticket.endpoint_addr().clone();
            discover_from_addr(&endpoint, addr, relay.as_deref()).await
        })
    }

    /// Discover the cluster around a seed node by its STABLE NODE ID +
    /// relay URL — the iroh-native dial (no ticket). Same result shape
    /// as [`EphemeralClient::discover`].
    pub fn discover_node(&self, node_id: String, relay_url: String) -> js_sys::Promise {
        let endpoint = self.endpoint.clone();
        let relay = self.relay.clone();
        future_to_promise(async move {
            let addr = node_addr(&node_id, &relay_url)?;
            discover_from_addr(&endpoint, addr, relay.as_deref()).await
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
        let relay = self.relay.clone();
        future_to_promise(async move {
            let ticket = EndpointTicket::from_str(&ticket).map_err(to_js_err)?;
            let addr = ticket.endpoint_addr().clone();
            run_job(&endpoint, addr, &document_blob, timeout, &on_event, relay.as_deref()).await
        })
    }

    /// Submit a job to a compute node by its STABLE NODE ID + relay URL
    /// — the iroh-native dial (no ticket). Same event contract as
    /// [`EphemeralClient::submit_job`].
    pub fn submit_job_to_node(
        &self,
        node_id: String,
        relay_url: String,
        document_blob: String,
        timeout: u32,
        on_event: Function,
    ) -> js_sys::Promise {
        let endpoint = self.endpoint.clone();
        let relay = self.relay.clone();
        future_to_promise(async move {
            let addr = node_addr(&node_id, &relay_url)?;
            run_job(&endpoint, addr, &document_blob, timeout, &on_event, relay.as_deref()).await
        })
    }
}

/// Shared discovery body: dial `addr`, hello, and summarize seed + peers.
async fn discover_from_addr(
    endpoint: &Endpoint,
    addr: EndpointAddr,
    relay: Option<&str>,
) -> Result<JsValue, JsValue> {
    let conn = endpoint.connect(addr, ALPN).await.map_err(to_js_err)?;
    // std::time::Instant panics on wasm32-unknown-unknown — use the JS clock.
    let started = js_sys::Date::now();
    let reply = hello_exchange(endpoint, &conn, relay).await?;
    let rtt_ms = (js_sys::Date::now() - started) as u64;

    let entry = |v: &serde_json::Value| {
        serde_json::json!({
            "node_id": v.get("node_id").cloned().unwrap_or(serde_json::Value::Null),
            "relay": v.get("relay").cloned().unwrap_or(serde_json::Value::Null),
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
}

async fn run_job(
    endpoint: &Endpoint,
    addr: EndpointAddr,
    document_blob: &str,
    timeout: u32,
    on_event: &Function,
    relay: Option<&str>,
) -> Result<JsValue, JsValue> {
    let conn = endpoint.connect(addr, ALPN).await.map_err(to_js_err)?;

    // 1) hello handshake (same as ephemeral_net's dial path)
    hello_exchange(endpoint, &conn, relay).await?;

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
