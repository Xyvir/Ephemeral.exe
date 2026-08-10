// Ephemeral Web — SPA thin client on top of ephemeral_wasm_library.
// Auto-connects to the cluster: fetches the live bootstrap list
// (docs/swarm.json, refreshed every 6 h by a GitHub Action), dials the
// current members by STABLE NODE ID + relay (iroh-native; tickets only
// as a fallback for legacy nodes), learns the cluster via hello
// handshakes, and routes jobs to the best available compute node (warm
// image first, latency).
import init, { EphemeralClient, base64_decode } from "./wbg/ephemeral_wasm_library.js";
import { BOOTSTRAP } from "./config.js";

const $ = (id) => document.getElementById(id);

let client = null;
// node_id -> { node_id, relay, ticket, images: [..], rtt_ms, seed: bool }
let peers = new Map();

function setStatus(text, cls) {
  const el = $("status");
  el.textContent = text;
  el.className = "status" + (cls ? " " + cls : "");
}

function appendOut(text, cls) {
  const box = $("output");
  const pre = document.createElement("pre");
  pre.className = "line " + (cls || "");
  pre.textContent = text === "" ? " " : text;
  box.appendChild(pre);
  box.scrollTop = box.scrollHeight;
}

// btoa on a UTF-8 string (document_blob is base64-encoded UTF-8 Markdown).
function b64encode(str) {
  const bytes = new TextEncoder().encode(str);
  let bin = "";
  for (const b of bytes) bin += String.fromCharCode(b);
  return btoa(bin);
}

function shortId(id) {
  return id ? id.slice(0, 8) + "…" : "—";
}

// Fetch the live bootstrap list (docs/swarm.json) — first URL that
// parses wins; [] when none are reachable.
async function fetchSwarmNodes() {
  for (const url of BOOTSTRAP.swarmJson || []) {
    try {
      const res = await fetch(url);
      if (!res.ok) continue;
      const data = await res.json();
      if (data && Array.isArray(data.nodes)) {
        setStatus(`swarm list: ${data.nodes.length} node(s) (${data.updated || "?"})`);
        return data.nodes;
      }
    } catch (e) {
      // unreachable / not JSON — try the next candidate URL
    }
  }
  return [];
}

// Bound a wasm call so a dead/stale node can't wedge discovery.
function withTimeout(promise, ms) {
  return Promise.race([
    promise,
    new Promise((_, rej) => setTimeout(() => rej(new Error("timed out")), ms)),
  ]);
}

// Languages used in a Markdown doc (code fence info strings).
function languagesIn(markdown) {
  const langs = new Set();
  const re = /```([A-Za-z0-9_+.-]+)/g;
  let m;
  while ((m = re.exec(markdown)) !== null) langs.add(m[1].toLowerCase());
  return [...langs];
}

function imageMatches(image, lang) {
  const l = lang.toLowerCase();
  return image.toLowerCase().includes("/" + l + ":") || image.toLowerCase().includes(l + "-");
}

// Pick the best compute node for a doc: a peer whose warm images cover
// the doc's languages (lowest RTT wins), else the lowest-RTT peer.
function pickTarget(doc) {
  const all = [...peers.values()].sort((a, b) => (a.rtt_ms ?? 1e9) - (b.rtt_ms ?? 1e9));
  if (!all.length) return null;
  for (const lang of languagesIn(doc)) {
    const warm = all.find((p) => (p.images || []).some((img) => imageMatches(img, lang)));
    if (warm) return warm;
  }
  return all[0];
}

function renderCluster() {
  const list = $("clusterList");
  list.textContent = "";
  if (!peers.size) {
    const li = document.createElement("li");
    li.className = "empty";
    li.textContent = "no nodes discovered — paste a seed ticket to connect manually";
    list.appendChild(li);
    $("clusterCount").textContent = "0 nodes";
    return;
  }
  const sorted = [...peers.values()].sort((a, b) => (a.rtt_ms ?? 1e9) - (b.rtt_ms ?? 1e9));
  for (const p of sorted) {
    const li = document.createElement("li");
    const id = document.createElement("code");
    id.textContent = shortId(p.node_id);
    li.appendChild(id);
    if (p.seed) {
      const tag = document.createElement("span");
      tag.className = "tag seed";
      tag.textContent = "seed";
      li.appendChild(tag);
    }
    const imgs = document.createElement("span");
    imgs.className = "images";
    imgs.textContent = p.images && p.images.length ? p.images.join(", ") : "no warm images";
    li.appendChild(imgs);
    const rtt = document.createElement("span");
    rtt.className = "rtt";
    rtt.textContent = p.rtt_ms == null ? "" : `${p.rtt_ms} ms`;
    li.appendChild(rtt);
    list.appendChild(li);
  }
  $("clusterCount").textContent = `${peers.size} node${peers.size === 1 ? "" : "s"}`;
}

// A candidate to dial: { node_id, relay, ticket } — node-id + relay is
// the iroh-native path, ticket the fallback (legacy nodes).
function dialCandidate(client, c) {
  if (c.node_id && c.relay) return client.discover_node(c.node_id, c.relay);
  return client.discover(c.ticket);
}

async function refreshPeers() {
  setStatus("discovering cluster…");

  // Live bootstrap list first (current members), compiled seeds as
  // fallback — dedupe by node_id (falling back to ticket).
  const swarmNodes = await fetchSwarmNodes();
  const candidates = [];
  const seen = new Set();
  const push = (c) => {
    const key = c.node_id || c.ticket;
    if (!key || seen.has(key)) return;
    seen.add(key);
    candidates.push(c);
  };
  for (const n of swarmNodes) {
    if (n && (n.node_id || n.ticket)) {
      push({ node_id: n.node_id || null, relay: n.relay || null, ticket: n.ticket || null });
    }
  }
  for (const n of BOOTSTRAP.nodes || []) {
    if (n && n.node_id) push({ node_id: n.node_id, relay: n.relay || null, ticket: null });
  }
  for (const t of BOOTSTRAP.seedTickets || []) {
    push({ node_id: null, relay: null, ticket: t });
  }
  if (!candidates.length) {
    renderCluster();
    setStatus("no bootstrap sources", "err");
    return;
  }

  // Dial every candidate concurrently; render as peers appear so the
  // panel isn't held hostage by dead nodes' timeouts.
  const tasks = candidates.map(async (c) => {
    try {
      const res = JSON.parse(await withTimeout(dialCandidate(client, c), 15000));
      let added = false;
      if (res.seed && res.seed.node_id) {
        peers.set(res.seed.node_id, { ...res.seed, seed: true });
        added = true;
      }
      for (const p of res.peers || []) {
        if (p.node_id === client.node_id()) continue; // don't list ourselves
        const known = peers.get(p.node_id);
        if (known && known.seed) continue; // keep the seed's own rtt
        peers.set(p.node_id, { ...p, seed: known ? known.seed : false });
        added = true;
      }
      if (added) renderCluster();
    } catch (e) {
      // node unreachable — the others continue
    }
  });
  await Promise.allSettled(tasks);
  renderCluster();
  setStatus(peers.size ? "ready" : "no cluster discovered", peers.size ? "" : "err");
}

async function start() {
  $("ticket").value = localStorage.getItem("ephemeral.ticket") || "";
  $("relay").value = localStorage.getItem("ephemeral.relay") || BOOTSTRAP.relay || "";
  setStatus("loading wasm…");
  await init();
  const relayUrl = $("relay").value.trim() || BOOTSTRAP.relay || null;
  try {
    client = await EphemeralClient.create(null, relayUrl);
    $("nodeId").textContent = client.node_id();
    await refreshPeers();
  } catch (e) {
    setStatus("failed: " + e, "err");
  }
}

async function run() {
  const markdown = $("input").value;
  if (!markdown.trim()) {
    setStatus("paste some Markdown with code blocks", "err");
    return;
  }

  const manual = $("ticket").value.trim();
  let target;
  if (manual) {
    target = { node_id: "(manual)", ticket: manual, relay: null };
  } else {
    target = pickTarget(markdown);
    if (!target) {
      setStatus("no compute nodes discovered — paste a seed ticket", "err");
      return;
    }
  }
  localStorage.setItem("ephemeral.ticket", manual);
  localStorage.setItem("ephemeral.relay", $("relay").value.trim());

  $("output").textContent = "";
  setStatus(`running on ${shortId(target.node_id)}…`);

  const onEvent = (jsonStr) => {
    const evt = JSON.parse(jsonStr);
    if (evt.type === "job_log") {
      const data = new TextDecoder().decode(base64_decode(evt.data));
      appendOut(data, "log-" + evt.channel);
    } else if (evt.type === "job_done") {
      if (evt.stdout) appendOut(evt.stdout, "done");
      if (evt.stderr) appendOut(evt.stderr, "err");
      if (evt.artifact_file) {
        appendOut(`[artifact: ${evt.artifact_file}${evt.artifact_ext || ""}]`, "done");
      }
      setStatus(evt.exit_code === 0 ? "done (exit 0)" : `failed (exit ${evt.exit_code})`,
                evt.exit_code === 0 ? "" : "err");
    } else if (evt.type === "error") {
      appendOut(evt.message, "err");
      setStatus("rejected", "err");
    }
  };
  // iroh-native dial by stable node id + relay; ticket only as a
  // fallback for legacy nodes that don't report a relay.
  const submit = target.node_id && target.relay
    ? () => client.submit_job_to_node(target.node_id, target.relay, b64encode(markdown), 300, onEvent)
    : () => client.submit_job(target.ticket, b64encode(markdown), 300, onEvent);

  try {
    await submit();
    refreshPeers(); // re-sync the peer table after each run (non-blocking)
  } catch (e) {
    setStatus("error", "err");
    appendOut(String(e), "err");
  }
}

$("run").addEventListener("click", run);
$("refresh").addEventListener("click", () => { peers.clear(); refreshPeers(); });
$("sample").addEventListener("click", () => {
  $("input").value =
    "```python\n" +
    "import sys\n" +
    "print('hello from the ephemeral cluster')\n" +
    "print('python', sys.version.split()[0])\n" +
    "```";
});

start();
