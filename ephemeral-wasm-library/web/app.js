// Ephemeral Web — SPA thin client on top of ephemeral_wasm_library.
// Auto-connects to the cluster: fetches the live bootstrap list
// (docs/swarm.json, refreshed every 6 h by a GitHub Action), dials the
// current members by STABLE NODE ID + relay (iroh-native; tickets only
// as a fallback for legacy nodes), learns the cluster via hello
// handshakes, and routes jobs to the best available compute node (warm
// image first, latency).
import init, { EphemeralClient, base64_decode } from "./wbg/ephemeral_wasm_library.js";
import { BOOTSTRAP } from "./config.js";
import { SUPPORTED_LANGUAGES, CANONICAL_LANGUAGES, ALIAS_MAP } from "./languages.js";

const $ = (id) => document.getElementById(id);

let client = null;
// node_id -> { node_id, relay, ticket, images: [..], rtt_ms, seed: bool }
let peers = new Map();
// Operator-supplied bootstrap from the URL fragment (#seed=…&relay=…) —
// set in start() before discovery. Lets a professor hand out a link to a
// private swarm on this hosted site without editing config.js.
const urlBootstrap = { seed: null, relay: null };

// Artifacts streamed by the current run's node (one "artifact" frame per
// file, before job_done). Cleared at the start of each run.
let runArtifacts = [];
// Raw (unformatted) markdown of everything appended to the Output box —
// results keep their `## <Lang> Result` headers and fenced code blocks
// exactly as the node emitted them, so Copy output pastes the markdown
// source rather than the flattened rendered text.
let outputRaw = "";

// Interleaved output view: shows the original document with each code
// block's result slotted in after it (best-effort, in-order matching).
// Captured per run; lastOutputRaw lets the results-only view be rebuilt
// when the toggle is switched back off.
let interleaved = false;
let lastMarkdown = "";
let lastResultText = "";
let lastOutputRaw = "";

// Image extensions -> mime (inline render + clipboard copy).
const IMAGE_MIMES = {
  ".png": "image/png",
  ".jpg": "image/jpeg",
  ".jpeg": "image/jpeg",
  ".gif": "image/gif",
  ".webp": "image/webp",
  ".bmp": "image/bmp",
};

// The Run Code editor: OverType (invisible-textarea WYSIWYG markdown) with
// highlight.js per-fence highlighting — monospace, code fences rendered as
// distinct highlighted blocks. Falls back to a plain textarea if the CDN
// is unreachable (editor = { getValue, setValue } either way).
let editor = null;

function initEditor() {
  const el = $("editor");
  if (window.OverType) {
    if (window.hljs) {
      // Real-time, per-language: use the fence's language when hljs knows
      // it, otherwise auto-detect. Shebang lines are pulled out first and
      // rendered as a bright marker line (header-ish, not code). Must
      // never throw (OverType calls this on every keystroke) and must
      // preserve every character (the invisible textarea aligns with it).
      const escHtml = (s) =>
        s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
      OverType.setCodeHighlighter((code, language) => {
        try {
          const nl = code.indexOf("\n");
          const firstLine = nl === -1 ? code : code.slice(0, nl);
          const hasShebang = firstLine.startsWith("#!");
          const body = hasShebang ? (nl === -1 ? "" : code.slice(nl + 1)) : code;
          let html = "";
          if (body) {
            const known = language && hljs.getLanguage(language);
            const res = known
              ? hljs.highlight(body, { language })
              : hljs.highlightAuto(body);
            html = res.value;
          }
          return (hasShebang
            ? `<span class="shebang">${escHtml(firstLine)}</span>${body ? "\n" : ""}`
            : "") + html;
        } catch (e) {
          return hljs.util.escapeHtml(code);
        }
      });
    }
    [editor] = new OverType(el, {
      value: "",
      placeholder: "```python\nprint(\"hello from the cluster\")\n```",
      // OverType copies the placeholder onto the overlay textarea too —
      // blank the native one or the hint renders doubled (styled preview
      // + browser textarea placeholder).
      textareaProps: { placeholder: "" },
      // Grow the editor with its content instead of scrolling internally.
      autoResize: true,
      theme: "cave", // dark — matches the SPA
      fontFamily: "ui-monospace, Consolas, monospace",
      spellcheck: false,
      onChange: () => {
        updateLangStatus();
      },
      // Re-apply the fence-header/shebang highlights after every preview
      // render. OverType replaces the preview DOM wholesale on each
      // keystroke (preview.innerHTML = ...), so a setTimeout from onChange
      // races the next render and the highlights flicker away. onRender
      // fires synchronously after every replacement — no race.
      onRender: () => {
        highlightCodeHeaders();
      },
    });
  } else {
    const ta = document.createElement("textarea");
    ta.id = "input";
    ta.placeholder = "```python\nprint(\"hello from the cluster\")\n```";
    el.appendChild(ta);
    ta.addEventListener("input", updateLangStatus);
    editor = { getValue: () => ta.value, setValue: (v) => { ta.value = v; } };
  }
}

// Live language badges under the editor: every declared fence language
// gets a chip — ✓ when it's in the language map, ✗ when it isn't (and the
// block will be rejected by the node). Dot-bearing tokens are file/seed
// blocks, shown neutrally.
// Ephemeral header semantics mirrored from ephemeral_core.config: unsafe
// (network) and image/cmd/entrypoint overrides are stripped on remote
// nodes; chain/piping flags are honored.
const NETWORK_FLAGS = new Set(["unsafe"]);
const CHAIN_FLAGS = new Set(["chain", "pipe", "piping"]);
const NO_CHAIN_FLAGS = new Set(["nopipe", "nopiping"]);
const DROPPED_OVERRIDES = ["image", "cmd", "entrypoint"];

// Every fence's (language, params) from a markdown doc — the first token
// of each fence header is the language, the rest are ephemeral params
// (flags like `unsafe chain`, overrides like `image=...`).
function fenceInfo(markdown) {
  const out = [];
  // `[ \t]*` (not `\s*`) matters: a fence header lives on the same line
  // as its opening fence. With `\s*` a closing ``` fence swallows the
  // following newlines and the next Markdown heading (e.g. `## Node
  // Example`) gets misread as a fence header — literate-programming prose
  // would then show up as bogus `##`/`Node`/`Example` language chips.
  const re = /```[ \t]*([^\n`]*)/g;
  let m;
  while ((m = re.exec(markdown)) !== null) {
    const tokens = m[1].trim().split(/\s+/).filter(Boolean);
    if (!tokens.length) continue; // bare fence, no header
    out.push({ lang: tokens[0].toLowerCase(), params: tokens.slice(1) });
  }
  return out;
}

function updateLangStatus() {
  if (!editor) return; // OverType fires onChange during its own _init,
  // before the constructor's destructuring assigns `editor`.
  const el = $("langStatus");
  el.textContent = "";
  const fences = fenceInfo(editor.getValue());
  if (!fences.length) {
    el.hidden = true;
    return;
  }
  for (const f of fences) {
    // Language chip (as before).
    const ok = SUPPORTED_LANGUAGES.has(f.lang);
    const isFile = !ok && f.lang.includes(".");
    const chip = document.createElement("span");
    chip.className = "lang-chip " + (ok ? "ok" : isFile ? "file" : "bad");
    chip.textContent = (ok ? "✓ " : isFile ? "" : "✗ ") + f.lang;
    chip.title = ok
      ? "supported"
      : isFile
        ? "file/seed block — not a language"
        : "not in the language map — this block will be rejected";
    el.appendChild(chip);
    // Ephemeral parameter chips: ✗ for what the distributed network
    // strips, ✓ for what it honors, neutral for unknown tokens.
    for (const raw of f.params) {
      const p = raw.toLowerCase();
      const pchip = document.createElement("span");
      if (NETWORK_FLAGS.has(p)) {
        pchip.className = "lang-chip bad";
        pchip.textContent = "✗ " + raw;
        pchip.title =
          "'unsafe' is not supported on the distributed network — jobs always run sandboxed";
      } else if (DROPPED_OVERRIDES.some((o) => p.startsWith(o + "="))) {
        pchip.className = "lang-chip bad";
        pchip.textContent = "✗ " + raw;
        pchip.title = "overrides are dropped on remote jobs — the node operator decides the image";
      } else if (CHAIN_FLAGS.has(p) || NO_CHAIN_FLAGS.has(p)) {
        pchip.className = "lang-chip ok";
        pchip.textContent = "✓ " + raw;
        pchip.title = "supported execution flag";
      } else {
        pchip.className = "lang-chip file";
        pchip.textContent = raw;
        pchip.title = "unknown header token — ignored by the parser";
      }
      el.appendChild(pchip);
    }
  }
  el.hidden = false;
}

// Bright-highlight fence header info-strings (the language declaration +
// ephemeral parameters, e.g. `python unsafe chain`) in the OverType
// preview. OverType re-renders the preview on every keystroke, wiping any
// spans we add — so this re-runs after each render (onChange, sample,
// clear). Whitespace is preserved exactly so the invisible textarea stays
// aligned.
function highlightCodeHeaders() {
  if (!editor || !editor.getRenderedHTML) return; // not OverType
  const preview = document.querySelector("#editor .overtype-preview");
  if (!preview) return;
  for (const span of preview.querySelectorAll("span.code-fence")) {
    if (span.querySelector(".code-header")) continue; // already processed
    const m = /^(`{3,}|~{3,})([\s\S]*)$/.exec(span.textContent);
    if (!m || !m[2]) continue; // closing fence — nothing to highlight
    span.textContent = "";
    span.appendChild(document.createTextNode(m[1]));
    for (const part of m[2].split(/(\s+)/)) {
      if (/^\s+$/.test(part)) {
        span.appendChild(document.createTextNode(part));
      } else {
        const t = document.createElement("span");
        t.className = "code-header";
        t.textContent = part;
        span.appendChild(t);
      }
    }
  }
}

// One status pill in the header: it communicates both the network mode
// (public/private) and the connection lifecycle (joining… → Public/Private),
// with errors shown in red. Transient detail (node counts, job progress)
// lives in the pill's hover tooltip, keeping the header to one element.
let modePrivate = false;
let connState = "connecting";   // "connecting" | "ready" | "error"
let pillErr = "";

function renderMode() {
  const el = $("mode");
  let label, cls;
  if (connState === "error") {
    label = pillErr;
    cls = "mode mode-err";
  } else if (connState === "ready") {
    label = modePrivate ? "Private" : "Public";
    cls = modePrivate ? "mode mode-private" : "mode mode-public";
  } else {
    label = modePrivate ? "joining private…" : "joining public…";
    cls = (modePrivate ? "mode mode-private" : "mode mode-public") + " mode-joining";
  }
  el.textContent = label;
  el.className = cls;
}

// Connection/validation status: updates the tooltip and drives the pill.
// Errors are sticky-red; "ready" resolves to Public/Private; other
// transient text keeps the pill in its joining state (but never demotes
// an already-ready pill back to "joining…" during a post-run re-sync).
function setStatus(text, cls) {
  $("mode").title = text;
  if (cls === "err") {
    connState = "error";
    pillErr = text;
  } else if (text === "ready") {
    connState = "ready";
  } else if (connState !== "ready") {
    connState = "connecting";
  }
  renderMode();
}

// Tooltip-only detail (job progress / outcome): the pill keeps showing
// mode + connection state; the hover title carries the specifics.
function setDetail(text) {
  $("mode").title = text;
}

// A `#seed=` link puts the SPA in private mode (public swarm skipped):
// it flips the pill's mode and the disclaimer under the Run button.
function setMode(privateMode) {
  modePrivate = privateMode;
  const noteEl = $("runNote");
  if (privateMode) {
    noteEl.className = "run-note private";
    noteEl.innerHTML = "<strong class=\"caps\">Private network</strong> — jobs run only on the node(s) you connected to, not the public swarm.";
  } else {
    noteEl.className = "run-note";
    noteEl.innerHTML = "<strong class=\"caps\">Public network</strong> — anything you submit is <strong>public knowledge</strong>. No privacy guarantee. For private use, self-host.";
  }
  renderMode();
}

// Spin the icon inside the Run button (and disable the button) while a
// job is in flight — cleared when the whole job finishes, not per result
// block. Disabling doubles as debounce: a disabled button swallows clicks,
// so spamming Run (intentional or not) can't queue multiple jobs.
function setBusy(busy) {
  $("run").classList.toggle("busy", busy);
  $("run").disabled = busy;
  $("run").setAttribute("aria-busy", String(busy));
}

function appendOut(text, cls) {
  const box = $("output");
  const pre = document.createElement("pre");
  pre.className = "line " + (cls || "");
  pre.textContent = text === "" ? " " : text;
  box.appendChild(pre);
  outputRaw += text + "\n";
  box.scrollTop = box.scrollHeight;
}

// Render a job result envelope ("## <Lang> Result\n```lang\n...```") as
// highlighted HTML: result headers, step sub-headers, and code fences as
// distinct blocks. Everything is HTML-escaped; highlight.js is applied
// after insertion (gracefully skipped when the CDN is unreachable).
function renderResult(text) {
  const esc = (s) => s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
  const html = [];
  let inFence = false;
  let fenceLang = "";
  let fenceBuf = [];
  const fence = /^```([\w+-]*)\s*$/;
  for (const line of text.split("\n")) {
    const m = fence.exec(line);
    if (m) {
      if (!inFence) {
        inFence = true;
        fenceLang = m[1] || "text";
        fenceBuf = [];
      } else {
        inFence = false;
        html.push(`<pre class="code-block"><code class="hljs language-${esc(fenceLang)}">${esc(fenceBuf.join("\n"))}</code></pre>`);
      }
      continue;
    }
    if (inFence) {
      fenceBuf.push(line);
      continue;
    }
    if (/^### /.test(line)) html.push(`<div class="result-step">${esc(line.slice(4))}</div>`);
    else if (/^## /.test(line)) html.push(`<div class="result-title">${esc(line.slice(3))}</div>`);
    else if (line.trim()) html.push(`<div class="result-line">${esc(line)}</div>`);
  }
  if (inFence) {
    html.push(`<pre class="code-block"><code class="hljs language-${esc(fenceLang)}">${esc(fenceBuf.join("\n"))}</code></pre>`);
  }
  return html.join("\n");
}

// Build a `.block.result` element for a result envelope without appending,
// so the streaming view and the interleaved view share one renderer.
function resultElement(text) {
  const div = document.createElement("div");
  div.className = "block result";
  div.innerHTML = renderResult(text);
  if (window.hljs) {
    div.querySelectorAll("pre code").forEach((el) => hljs.highlightElement(el));
  }
  return div;
}

function appendResult(text) {
  $("output").appendChild(resultElement(text));
  // Keep the raw markdown source so Copy output pastes the unformatted
  // version (headers + fences), not the flattened rendered text.
  outputRaw += (text.endsWith("\n") ? text : text + "\n") + "\n";
  $("output").scrollTop = $("output").scrollHeight;
}

// --- interleaved view ------------------------------------------------------
// Split a Markdown doc into prose / fenced-code segments (in order). Seed
// blocks (a `.`-bearing, non-language info string — e.g. ```data.csv) are
// flagged so results are NOT paired with them.
function splitMarkdown(md) {
  const segs = [];
  const lines = md.split("\n");
  let prose = [];
  const flushProse = () => {
    if (prose.length) { segs.push({ type: "prose", text: prose.join("\n") }); prose = []; }
  };
  let i = 0;
  while (i < lines.length) {
    const open = /^(`{3,}|~{3,})[ \t]*(.*)$/.exec(lines[i]);
    if (!open) { prose.push(lines[i]); i++; continue; }
    flushProse();
    const fence = open[1];
    const closeRe = new RegExp("^" + (fence[0] === "~" ? "~" : "`") + "{" + fence.length + ",}\\s*$");
    const header = open[2].trim();
    const lang = (header.split(/\s+/)[0] || "").toLowerCase();
    const body = [];
    i++;
    while (i < lines.length && !closeRe.test(lines[i])) { body.push(lines[i]); i++; }
    if (i < lines.length) i++; // consume the closing fence
    segs.push({
      type: "code",
      header,
      lang,
      code: body.join("\n"),
      isSeed: !!lang && !SUPPORTED_LANGUAGES.has(lang) && lang.includes("."),
    });
  }
  flushProse();
  return segs;
}

// Split the node's result envelope into per-block chunks on their
// `## <Lang> Result` / `## <Lang> Run N` headers.
function splitResults(stdout) {
  const out = [];
  let cur = null;
  const header = /^## .*?\s(Result|Run \d+)\s*$/;
  for (const line of stdout.split("\n")) {
    if (header.test(line)) {
      if (cur !== null) out.push(cur.join("\n"));
      cur = [line];
    } else if (cur !== null) {
      cur.push(line);
    }
  }
  if (cur !== null) out.push(cur.join("\n"));
  return out;
}

function renderCodeSeg(seg) {
  const div = document.createElement("div");
  div.className = "block source";
  const head = document.createElement("div");
  head.className = "source-head";
  head.textContent = "```" + (seg.header ? seg.header : "");
  div.appendChild(head);
  const pre = document.createElement("pre");
  pre.className = "code-block";
  const code = document.createElement("code");
  code.className = "hljs language-" + (seg.lang || "text");
  code.textContent = seg.code;
  pre.appendChild(code);
  div.appendChild(pre);
  if (window.hljs) hljs.highlightElement(code);
  return div;
}

function renderProseSeg(seg) {
  const div = document.createElement("div");
  div.className = "block prose";
  for (const raw of seg.text.split("\n")) {
    const line = raw.trim();
    if (!line) continue;
    let el, m;
    if ((m = /^###\s+/.exec(line))) { el = document.createElement("div"); el.className = "interleave-h3"; el.textContent = line.slice(m[0].length); }
    else if ((m = /^##\s+/.exec(line))) { el = document.createElement("div"); el.className = "interleave-h2"; el.textContent = line.slice(m[0].length); }
    else if ((m = /^#\s+/.exec(line))) { el = document.createElement("div"); el.className = "interleave-h1"; el.textContent = line.slice(m[0].length); }
    else { el = document.createElement("div"); el.className = "result-line"; el.textContent = line; }
    div.appendChild(el);
  }
  return div;
}

// Interleaved view: the original document with each executable code
// block's result slotted in right after it. Best-effort: results are
// matched positionally (in order) to non-seed code blocks.
function renderInterleaved() {
  const box = $("output");
  box.textContent = "";
  if (!lastMarkdown) return;
  const results = splitResults(lastResultText);
  let ri = 0;
  for (const seg of splitMarkdown(lastMarkdown)) {
    if (seg.type === "prose") {
      box.appendChild(renderProseSeg(seg));
    } else {
      box.appendChild(renderCodeSeg(seg));
      if (!seg.isSeed && ri < results.length) {
        box.appendChild(resultElement(results[ri++]));
      }
    }
  }
  box.scrollTop = box.scrollHeight;
}

// Normal (results-only) view, rebuilt from the raw markdown we captured.
function renderNormal() {
  const box = $("output");
  box.textContent = "";
  if (lastOutputRaw) box.appendChild(resultElement(lastOutputRaw));
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

// DNS TXT fallback (DoH): when every swarm.json URL is unreachable,
// resolve the operator's TXT mirror of the list (one `iroh1:<node_id>;<relay>`
// per node, comma-separated, split across 255-char strings) via
// DNS-over-HTTPS — DNS is tiered/cached infrastructure, so this is an
// independent path to first contact.
async function fetchDnsMirror(hostname) {
  const urls = [
    `https://cloudflare-dns.com/dns-query?name=${encodeURIComponent(hostname)}&type=TXT`,
    `https://dns.google/resolve?name=${encodeURIComponent(hostname)}&type=TXT`,
  ];
  for (const url of urls) {
    try {
      const res = await fetch(url, { headers: { Accept: "application/dns-json" } });
      if (!res.ok) continue;
      const data = await res.json();
      const entries = [];
      for (const a of data.Answer || []) {
        if (a.type !== 16) continue; // TXT
        for (const part of String(a.data).replace(/"/g, "").split(",")) {
          const m = /^iroh1:([0-9a-f]{64})(?:;([^,\s"]+))?/.exec(part.trim());
          if (m) entries.push({ node_id: m[1], relay: m[2] || null, ticket: null });
        }
      }
      if (entries.length) {
        setStatus(`dns mirror: ${entries.length} node(s) (${hostname})`);
        return entries;
      }
    } catch (e) {
      // unreachable — try the next resolver
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

  const candidates = [];
  const seen = new Set();
  const push = (c) => {
    const key = c.node_id || c.ticket;
    if (!key || seen.has(key)) return;
    seen.add(key);
    candidates.push(c);
  };

  // Private-mode link (#seed=…): dial ONLY the operator's node (and the
  // peers it reports via hello) — the public swarm list is skipped so a
  // classroom's graded work never lands on a volunteer's machine.
  const urlSeed = urlSeedCandidate(urlBootstrap.seed);
  if (urlSeed) {
    push(urlSeed);
  } else {
    // Live bootstrap list first (current members) — no compiled seeds in
    // the public build; operator-configured ids/tickets (private swarms)
    // are dialed too. Dedupe by node_id (falling back to ticket).
    const swarmNodes = await fetchSwarmNodes();
    // DNS TXT fallback when the list is unreachable (independent, tiered
    // path — see fetchDnsMirror).
    const dnsNodes = !swarmNodes.length && BOOTSTRAP.dnsTxt
      ? await fetchDnsMirror(BOOTSTRAP.dnsTxt)
      : [];
    for (const n of [...swarmNodes, ...dnsNodes]) {
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
  }
  if (!candidates.length) {
    renderCluster();
    setStatus(
      "bootstrap list unreachable — check connectivity or paste a seed ticket",
      "err"
    );
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

// --- URL bootstrap (private-swarm links) ----------------------------
// A professor can hand out one link to this hosted SPA with their node's
// bootstrap baked into the URL fragment:
//
//   https://…/web/#seed=<EndpointTicket>
//   https://…/web/#seed=<node_id>@<relay_url>&relay=<relay_url>
//
// The fragment (not `?query`) keeps the ticket out of server/CDN logs and
// Referer headers. These are *bootstrap* credentials (a seed ticket, or a
// stable node id + relay) — never an identity secret. A `#seed=` link puts
// the SPA into private mode: the public swarm list is skipped.

// Split `node_id@relay_url` into { node_id, relay }; null when the value
// isn't that shape (e.g. a bare EndpointTicket).
function splitNodeAtRelay(seed) {
  if (!seed) return null;
  const at = seed.indexOf("@");
  if (at <= 0) return null;
  const node_id = seed.slice(0, at).trim();
  const relay = seed.slice(at + 1).trim();
  return /^[0-9a-f]{64}$/.test(node_id) && relay ? { node_id, relay } : null;
}

// Percent-decode a fragment param without throwing on a hand-typed `%`.
function decodeParam(s) {
  try { return decodeURIComponent(s); } catch (e) { return s; }
}

// Parse the location fragment for `seed` and `relay` params.
function parseUrlBootstrap() {
  const out = { seed: null, relay: null };
  const raw = (location.hash || "").replace(/^#/, "");
  if (!raw) return out;
  for (const part of raw.split("&")) {
    const eq = part.indexOf("=");
    if (eq < 0) continue;
    const k = decodeParam(part.slice(0, eq)).trim();
    const v = decodeParam(part.slice(eq + 1)).trim();
    if (k === "seed" && v) out.seed = v;
    else if (k === "relay" && v) out.relay = v;
  }
  return out;
}

// Convert a #seed value into a discovery candidate (dialCandidate shape).
function urlSeedCandidate(seed) {
  const nr = splitNodeAtRelay(seed);
  if (nr) return { node_id: nr.node_id, relay: nr.relay, ticket: null };
  if (seed) return { node_id: null, relay: null, ticket: seed };
  return null;
}

async function start() {
  const url = parseUrlBootstrap();
  if (url.seed) {
    urlBootstrap.seed = url.seed;
    // A #seed= link is a full private-mode bootstrap: drop any stale
    // remembered ticket/relay so a student's earlier public session can't
    // override the operator's node.
    localStorage.removeItem("ephemeral.ticket");
    localStorage.removeItem("ephemeral.relay");
    // A bare EndpointTicket also pre-fills the manual field (so run() can
    // submit straight to it); a `node_id@relay` value is handled purely by
    // discovery and never belongs in the ticket box.
    if (!splitNodeAtRelay(url.seed)) localStorage.setItem("ephemeral.ticket", url.seed);
  }
  if (url.relay) {
    urlBootstrap.relay = url.relay;
    localStorage.setItem("ephemeral.relay", url.relay);
  }
  setMode(!!url.seed);
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
  if ($("run").disabled) return; // already running — swallow re-entry
  const markdown = editor.getValue();
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

  runArtifacts = [];
  outputRaw = "";
  lastMarkdown = markdown;
  lastResultText = "";
  $("output").textContent = "";
  setDetail(`running on ${shortId(target.node_id)}…`);
  setBusy(true);

  const onEvent = (jsonStr) => {
    const evt = JSON.parse(jsonStr);
    if (evt.type === "job_log") {
      const data = new TextDecoder().decode(base64_decode(evt.data));
      appendOut(data, "log-" + evt.channel);
    } else if (evt.type === "artifact") {
      const ext = evt.ext || "";
      runArtifacts.push({
        name: String(evt.name || "artifact" + ext),
        ext: ext,
        size: evt.size || 0,
        b64: evt.data,
        mime: IMAGE_MIMES[ext] || "application/octet-stream",
      });
    } else if (evt.type === "job_done") {
      if (evt.stdout) { lastResultText = evt.stdout; appendResult(evt.stdout); }
      if (evt.stderr) appendOut(evt.stderr, "err");
      if (runArtifacts.length) {
        renderArtifacts(runArtifacts, markdown);
      } else if (evt.artifact_file) {
        // Legacy nodes that only report metadata, not bytes.
        appendOut(`[artifact: ${evt.artifact_file}${evt.artifact_ext || ""}]`, "done");
      }
      setDetail(evt.exit_code === 0 ? "done (exit 0)" : `failed (exit ${evt.exit_code})`);
    } else if (evt.type === "error") {
      appendOut(evt.message, "err");
      setDetail("rejected");
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
    setDetail("error");
    appendOut(String(e), "err");
  } finally {
    setBusy(false);
  }

  // A fence that declared an unknown language gets rejected by the node's
  // image allowlist — remind the user what the cluster actually supports.
  const unsupported = languagesIn(markdown).filter(
    (l) => !SUPPORTED_LANGUAGES.has(l) && !l.includes(".")
  );
  if (unsupported.length) appendLangReminder(unsupported);

  // If the run actually attempted the `unsafe` flag, note that the
  // distributed network strips it — red text outside any reminder box,
  // only shown when unsafe was declared.
  const triedUnsafe = fenceInfo(markdown).some((f) =>
    f.params.some((p) => p.toLowerCase() === "unsafe")
  );
  if (triedUnsafe) {
    const note = document.createElement("div");
    note.className = "unsafe-note";
    note.textContent =
      "Note: the `unsafe` network flag is not supported on the distributed " +
      "network — jobs always run sandboxed with no network access.";
    const box = $("output");
    box.appendChild(note);
    box.scrollTop = box.scrollHeight;
  }
  lastOutputRaw = outputRaw;
  if (interleaved) renderInterleaved();
}

// Render the "unsupported language" reminder: which fences were unknown,
// plus the full supported-language list (canonical + aliases).
function appendLangReminder(unsupported) {
  const esc = (s) => s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
  const div = document.createElement("div");
  div.className = "block reminder";
  // Each canonical language with its aliases inline in parentheses:
  // python (py), node (js, javascript, npm, npx, …)
  const supported = CANONICAL_LANGUAGES.map((l) => {
    const als = ALIAS_MAP[l] || [];
    const aliases = als.length
      ? ` <span class="reminder-als">(${als.map(esc).join(", ")})</span>`
      : "";
    return `<code>${esc(l)}</code>${aliases}`;
  }).join(" ");
  div.innerHTML =
    `<div class="reminder-title">Unsupported language${unsupported.length > 1 ? "s" : ""}: ` +
    `<code>${unsupported.map(esc).join("</code>, <code>")}</code></div>` +
    `<div class="reminder-body">This cluster only runs code declared with a supported ` +
    `language. Supported: ${supported}</div>` +
    `<div class="reminder-hint">Edit the fence info string (e.g. \`\`\`python) or pick one of the above.</div>`;
  const box = $("output");
  box.appendChild(div);
  box.scrollTop = box.scrollHeight;
}

// --- artifacts ------------------------------------------------------------

function b64ToBlob(b64, mime) {
  return new Blob([base64_decode(b64)], { type: mime });
}

function artifactSafeName(name) {
  return String(name).replace(/[\\/]/g, "_");
}

// Ephemeral's artifact naming (mirrors main_local.py's Downloads routing):
// single file -> Ephemeral_{lang}_{filename}, zip ->
// Ephemeral_{lang}_Artifacts_{epoch}.zip, with the block's language
// sanitized to [^a-zA-Z0-9] -> _.
function artifactLang(markdown) {
  return (languagesIn(markdown)[0] || "custom").replace(/[^a-zA-Z0-9]/g, "_");
}

function artifactFileName(lang, name) {
  return `Ephemeral_${lang}_${artifactSafeName(name)}`;
}

function artifactZipName(lang) {
  return `Ephemeral_${lang}_Artifacts_${Math.floor(Date.now() / 1000)}.zip`;
}

function triggerDownload(blob, filename) {
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;
  a.download = filename;
  document.body.appendChild(a);
  a.click();
  a.remove();
  setTimeout(() => URL.revokeObjectURL(url), 10000);
}

// Download artifacts: a single file downloads directly; multiple files
// are zipped client-side (JSZip, CDN) into one ball so the user gets a
// single download — no multi-file permission prompts. Falls back to
// sequential single downloads if the JSZip CDN is unreachable.
async function downloadArtifacts(artifacts, lang) {
  if (artifacts.length === 1) {
    const a = artifacts[0];
    triggerDownload(b64ToBlob(a.b64, a.mime), artifactFileName(lang, a.name));
    return;
  }
  if (window.JSZip) {
    const zip = new JSZip();
    for (const a of artifacts) {
      zip.file(artifactSafeName(a.name), b64ToBlob(a.b64, a.mime));
    }
    const blob = await zip.generateAsync({ type: "blob" });
    triggerDownload(blob, artifactZipName(lang));
  } else {
    for (const a of artifacts) {
      triggerDownload(b64ToBlob(a.b64, a.mime), artifactFileName(lang, a.name));
    }
  }
}

// Copy a single image artifact to the clipboard as an image (ClipboardItem
// write — Chrome/Edge; Firefox requires a permission).
async function copyArtifactImage(a, btn) {
  const label = btn.textContent;
  try {
    await navigator.clipboard.write([new ClipboardItem({ [a.mime]: b64ToBlob(a.b64, a.mime) })]);
    btn.classList.add("ok");
    btn.textContent = "Copied image";
  } catch (e) {
    btn.textContent = "Copy failed";
  }
  setTimeout(() => {
    btn.classList.remove("ok");
    btn.textContent = label;
  }, 1200);
}

// Render a run's artifacts in the output: inline image previews, then an
// action bar — Download (single file) / Download all (.zip) (multi),
// plus Copy image for a single image artifact.
function renderArtifacts(artifacts, markdown) {
  const lang = artifactLang(markdown);
  const box = $("output");
  const div = document.createElement("div");
  div.className = "block artifacts";
  for (const a of artifacts) {
    if (!IMAGE_MIMES[a.ext]) continue;
    const img = document.createElement("img");
    img.className = "artifact-img";
    img.src = `data:${a.mime};base64,${a.b64}`;
    img.alt = a.name;
    img.title = a.name;
    div.appendChild(img);
  }
  const bar = document.createElement("div");
  bar.className = "artifact-bar";
  const label = document.createElement("span");
  label.className = "artifact-count";
  label.textContent = `${artifacts.length} artifact${artifacts.length === 1 ? "" : "s"}`;
  bar.appendChild(label);
  if (artifacts.length === 1 && IMAGE_MIMES[artifacts[0].ext]) {
    const copy = document.createElement("button");
    copy.className = "secondary artifact-btn";
    copy.textContent = "Copy image";
    copy.addEventListener("click", () => copyArtifactImage(artifacts[0], copy));
    bar.appendChild(copy);
  }
  const dl = document.createElement("button");
  dl.className = "secondary artifact-btn";
  dl.textContent = artifacts.length === 1 ? "Download" : "Download all (.zip)";
  dl.addEventListener("click", () => downloadArtifacts(artifacts, lang));
  bar.appendChild(dl);
  div.appendChild(bar);
  box.appendChild(div);
  box.scrollTop = box.scrollHeight;
}

$("run").addEventListener("click", run);
$("refresh").addEventListener("click", () => { peers.clear(); refreshPeers(); });
$("clearOutput").addEventListener("click", () => {
  $("output").textContent = "";
  outputRaw = "";
  lastMarkdown = lastResultText = lastOutputRaw = "";
});

$("interleave").addEventListener("click", () => {
  interleaved = !interleaved;
  const btn = $("interleave");
  btn.classList.toggle("active", interleaved);
  btn.setAttribute("aria-pressed", String(interleaved));
  btn.title = interleaved
    ? "Interleave: on — results after each code block"
    : "Interleave: off — results only";
  if (interleaved && lastMarkdown) renderInterleaved();
  else if (!interleaved && lastOutputRaw) renderNormal();
});

// Copy text to the clipboard, then flash the button green. Races the
// async clipboard API against a timeout: in embedded/iframe contexts
// writeText() can hang on a permission prompt that never resolves, so it
// falls back to the synchronous execCommand path.
async function copyText(text, btn, restoreTitle) {
  if (!text.trim()) return;
  const copied = await Promise.race([
    navigator.clipboard.writeText(text).then(() => true).catch(() => false),
    new Promise((resolve) => setTimeout(() => resolve(false), 1000)),
  ]);
  if (!copied) {
    try {
      const ta = document.createElement("textarea");
      ta.value = text;
      document.body.appendChild(ta);
      ta.select();
      document.execCommand("copy");
      ta.remove();
    } catch (e) {
      // give up — still flash so the click feels responsive
    }
  }
  btn.classList.add("ok");
  btn.title = "Copied";
  setTimeout(() => {
    btn.classList.remove("ok");
    btn.title = restoreTitle;
  }, 1200);
}

$("copyOutput").addEventListener("click", () => {
  copyText(outputRaw.replace(/\n+$/, ""), $("copyOutput"), "Copy output");
});
$("copyCode").addEventListener("click", () => {
  copyText(editor.getValue(), $("copyCode"), "Copy code");
});
$("clearCode").addEventListener("click", () => {
  editor.setValue("");
  updateLangStatus();
  highlightCodeHeaders();
});
$("sample").addEventListener("click", () => {
  editor.setValue(
    "## Python Example\n\n" +
    "```python\n" +
    "import sys\n" +
    "print('hello from the ephemeral cluster')\n" +
    "print('python', sys.version.split()[0])\n" +
    "```\n\n" +
    "## Node Example\n\n" +
    "```node\n" +
    "console.log('node', process.version)\n" +
    "```\n\n" +
    "## Bash Example\n\n" +
    "```bash\n" +
    "echo \"bash $BASH_VERSION\"\n" +
    "```"
  );
  updateLangStatus(); // setValue doesn't fire OverType's onChange
  highlightCodeHeaders();
});

initEditor();
updateLangStatus();
highlightCodeHeaders();
start();
