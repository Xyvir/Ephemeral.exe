// Ephemeral Web — SPA thin client on top of ephemeral_wasm_library.
// Auto-connects to the cluster: fetches the live bootstrap list
// (docs/swarm.json, refreshed every 6 h by a GitHub Action), dials the
// current members by STABLE NODE ID + relay (iroh-native; tickets only
// as a fallback for legacy nodes), learns the cluster via hello
// handshakes, and routes jobs to the best available compute node (warm
// image first, latency).
import init, { EphemeralClient, base64_decode } from "./wbg/ephemeral_wasm_library.js";
import { BOOTSTRAP } from "./config.js";
import { SUPPORTED_LANGUAGES, CANONICAL_LANGUAGES, ALIAS_MAP, IMAGE_LANGUAGES } from "./languages.js";
import { LANG_SNIPPETS } from "./snippets.js";

const $ = (id) => document.getElementById(id);

let client = null;
// node_id -> { node_id, relay, ticket, images: [..], rtt_ms, seed: bool }
let peers = new Map();
// Operator-supplied bootstrap from the URL fragment (#seed=…&relay=…) —
// set in start() before discovery. Lets a professor hand out a link to a
// private swarm on this hosted site without editing config.js.
const urlBootstrap = { seed: null, relay: null };

// "Good neighbor" metric: a public node that is alive (has a measured
// RTT) and advertises at least this many warm container images.
// Used as a quick swarm-health shorthand in the status pill tooltip.
const GOOD_NEIGHBOR_THRESHOLD = 5;
let goodNeighborCount = 0;

// Artifacts streamed by the current run's node (one "artifact" frame per
// file, before job_done). Cleared at the start of each run.
let runArtifacts = [];
// Artifacts NOT placed inline under a run result (pure-positional 1:1
// matching) — these still get the rolled-up artifact block at the end.
let leftoverArtifacts = [];
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
// stderr from the final job_done frame + its exit code, so the interleaved
// and rebuilt views can re-append it (the streaming error element would
// otherwise vanish on toggle). Kept out of outputRaw so the copy stays
// clean markdown — failure envelopes already embed the error text.
let lastStderr = "";
let lastExitCode = 0;
// Warnings toggle: stderr on an exit-0 run is advisory, so it stays
// collapsed behind a yellow "!" button that only appears when there is
// something to reveal (no badge — the button itself is the signal).
// warningsOn persists within a run so view rebuilds re-append the element
// honoring it; both reset on every run.
let warningsOn = false;
let lastStderrEl = null;

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

// Shared fence-body highlighter used by BOTH the OverType Run Code editor
// and the interleaved output view, so the rendered source blocks mirror
// the editor exactly: shebang first lines become a bright marker line,
// declared fence languages highlight per-language, and unknown ones fall
// back to highlight.js auto-detect. Returns the highlighted HTML (already
// escaped) — or null when highlight.js is unavailable (the caller falls
// back to plain text). Must never throw: OverType calls it on every
// keystroke and must preserve every character (the invisible textarea
// aligns with the preview).
function highlightFenceBody(code, language) {
  if (!window.hljs) return null;
  try {
    const escHtml = (s) =>
      s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
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
}

function initEditor() {
  const el = $("editor");
  if (window.OverType) {
    if (window.hljs) {
      // Real-time, per-language highlighting through the shared
      // highlightFenceBody() — the SAME highlighter the interleaved
      // output view uses, so rendered source blocks mirror the editor.
      // Must never throw (OverType calls this on every keystroke) and
      // must preserve every character (the invisible textarea aligns
      // with the preview).
      OverType.setCodeHighlighter((code, language) =>
        highlightFenceBody(code, language) || hljs.util.escapeHtml(code)
      );
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
        syncCodeToUrl();
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

// Pull literal artifact filenames from common script references such as
// `/output/plot.png` or `png("/output/chart.png")`. Dynamic paths cannot be
// known before execution, so callers keep the generic artifact label when
// this returns no names.
function artifactNamesFromScript(body) {
  const names = [];
  const add = (name) => {
    const clean = name.replace(/[.?!:]+$/, "").split("/").pop();
    if (clean && !names.includes(clean)) names.push(clean);
  };
  const direct = /\/output\/([^\"'`\\\s,;)}\]]+)/g;
  let match;
  while ((match = direct.exec(body || "")) !== null) add(match[1]);

  // Also recognize Python/R-style path joins with a literal filename.
  const joined = /(?:path\.join|os\.path\.join)\(\s*["']\/output["']\s*,\s*["']([^"']+)["']\s*\)/g;
  while ((match = joined.exec(body || "")) !== null) add(match[1]);
  return names;
}

// Every fence's (language, params, body) from a markdown doc — the first
// token of each fence header is the language, the rest are ephemeral params
// (flags like `unsafe chain`, overrides like `image=...`); `body` is the
// code between the opening and closing fences (or the rest of the doc when
// it is still unclosed, so chips keep showing while typing).
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
    // Body: everything after this fence header up to the next closing
    // fence (or end of input if unclosed — chips must still show while
    // typing). Used to flag blocks that write artifacts to /output.
    const bodyStart = re.lastIndex;
    const closeIdx = markdown.indexOf("```", bodyStart);
    const body = closeIdx === -1
      ? markdown.slice(bodyStart)
      : markdown.slice(bodyStart, closeIdx);
    out.push({ lang: tokens[0].toLowerCase(), params: tokens.slice(1), body });
  }
  return out;
}

function updateLangStatus() {
  if (!editor) return; // OverType fires onChange during its own _init,
  // before the constructor's destructuring assigns `editor`.
  const el = $("langStatus");
  el.textContent = "";
  const fences = fenceInfo(editor.getValue());
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
    // Artifact chip: the block references /output, so the run is expected
    // to return a downloadable artifact (single images preview inline).
    const artifactNames = artifactNamesFromScript(f.body);
    if (/(?<![\w/])\/output(?=[/'"\s]|$)/.test(f.body || "")) {
      const achip = document.createElement("span");
      achip.className = "lang-chip artifact";
      achip.textContent = artifactNames.length
        ? "📦 " + artifactNames.join(", ")
        : "📦 artifact";
      achip.title = artifactNames.length
        ? `writes ${artifactNames.join(", ")} to /output — the result is returned as a downloadable artifact (images preview inline)`
        : "writes to /output — the result is returned as a downloadable artifact (images preview inline)";
      el.appendChild(achip);
    }
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
  // The (i) help pill is always the trailing chip — even with no fences,
  // so it's a static affordance, not tied to whatever the doc declares.
  el.appendChild(langHelpPillEl());
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
  let label, cls, title;
  if (connState === "error") {
    label = pillErr;
    cls = "mode mode-err";
    title = pillErr;
  } else if (connState === "ready") {
    label = modePrivate ? "Private" : "Public";
    cls = modePrivate ? "mode mode-private" : "mode mode-public";
    const nn = goodNeighborCount;
    title = modePrivate
      ? "Private cluster"
      : `Public swarm — ${nn} live neighbor${nn !== 1 ? "s" : ""}`;
  } else {
    label = modePrivate ? "joining private…" : "joining public…";
    cls = (modePrivate ? "mode mode-private" : "mode mode-public") + " mode-joining";
    title = "Connecting to cluster…";
  }
  el.textContent = label;
  el.className = cls;
  if (title !== undefined) el.title = title;
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
// it flips the pill's mode and the disclaimer under the Run button. The
// private note names the joined swarm (the seed node's id when it's a
// `node_id@relay` link; a bare EndpointTicket carries no readable name)
// and states plainly that jobs run on its members' machines unencrypted.
function setMode(privateMode, swarmName) {
  modePrivate = privateMode;
  const noteEl = $("runNote");
  if (privateMode) {
    noteEl.className = "run-note private";
    const joined = swarmName
      ? `private swarm <strong>"${swarmName}"</strong>`
      : "a private swarm";
    noteEl.innerHTML =
      `<strong class="caps">Private swarm</strong> — you have joined ${joined}; ` +
      `jobs run remotely and <strong>unencrypted</strong> on its members' machines.`;
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

// Append the final job_done stderr WITHOUT polluting outputRaw (so the
// Copy-output markdown stays clean; failure envelopes embed the error
// text themselves). Colored by exit code: stderr on a successful run is
// advisory (interpreters legitimately warn on stderr with exit 0 — e.g.
// osabie's Enum.slice notice), so it renders muted and collapsed behind
// the warnings toggle; only a non-zero exit renders it as a visible error.
function appendStderr(text, exitCode) {
  lastStderr = text;
  lastExitCode = exitCode;
  if (exitCode === 0) showWarningsButton();
  else $("warnings").hidden = true;
  lastStderrEl = stderrElement();
  if (!lastStderrEl) return;
  $("output").appendChild(lastStderrEl);
  $("output").scrollTop = $("output").scrollHeight;
}

// The stderr element, honoring the warnings toggle: collapsed (hidden)
// by default on exit-0 runs, visible once the user reveals it. Null when
// the run had no stderr. Used for both the streaming append and rebuilds.
function stderrElement() {
  if (!lastStderr) return null;
  const pre = document.createElement("pre");
  pre.className = "line " + (lastExitCode === 0 ? "stderr-muted" : "err");
  pre.textContent = lastStderr;
  if (lastExitCode === 0 && !warningsOn) pre.hidden = true;
  return pre;
}

// Show the yellow warnings toggle and sync its tooltip/state to the
// current warning line count. Called when a run lands exit-0 stderr.
function showWarningsButton() {
  const btn = $("warnings");
  btn.hidden = false;
  syncWarningsButton(btn);
}

// Keep the button's pressed-state, tooltip, and line count in sync.
function syncWarningsButton(btn) {
  btn.classList.toggle("on", warningsOn);
  btn.setAttribute("aria-pressed", String(warningsOn));
  const n = lastStderr ? lastStderr.split("\n").filter((l) => l.trim()).length : 0;
  btn.title =
    `Warnings (${n} line${n === 1 ? "" : "s"}) — click to ` +
    (warningsOn ? "hide" : "show");
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
  addCopyButtons($("output"));
  // Keep the raw markdown source so Copy output pastes the unformatted
  // version (headers + fences), not the flattened rendered text.
  outputRaw += (text.endsWith("\n") ? text : text + "\n") + "\n";
  pruneEmptyStdoutForImages($("output"));
  $("output").scrollTop = $("output").scrollHeight;
}

// Hide the empty stdout container for runs whose real output is an image:
// when a block writes a picture and prints nothing, the blank code box is
// just noise in a printed report — the image IS the result. Prunes only
// when an image artifact can be attributed 1:1 in document order to that
// envelope (the same positional trust the inline view uses); any other
// shape — no results, extra/missing artifacts, non-image files — keeps
// the empty-box placeholder for genuinely silent runs.
function pruneEmptyStdoutForImages(root) {
  if (!root || !lastResultText) return;
  const results = splitResults(lastResultText);
  if (!results.length || results.length !== runArtifacts.length) return;
  // One .result-title per envelope; bail if the DOM can't be mapped back
  // to the parsed envelopes (a stray ## line would shift the pairing).
  const titles = root.querySelectorAll(".result-title");
  if (titles.length !== results.length) return;
  let gi = -1; // envelope index, incremented at each title in order
  for (const block of root.querySelectorAll(".block.result")) {
    for (const el of block.children) {
      if (el.classList.contains("result-title")) {
        gi++;
        continue;
      }
      if (
        gi >= 0 &&
        el.classList.contains("code-block") &&
        !el.textContent.trim() &&
        IMAGE_MIMES[runArtifacts[gi].ext]
      ) {
        el.remove();
      }
    }
  }
}

// Attach a small copy button to each rendered code block (idempotent).
// It copies just the code contents — not the fence markers.
function addCopyButtons(root) {
  root.querySelectorAll("pre.code-block").forEach((pre) => {
    if (pre.querySelector(".block-copy")) return;
    const btn = document.createElement("button");
    btn.type = "button";
    btn.className = "block-copy";
    btn.title = "Copy code";
    btn.setAttribute("aria-label", "Copy code");
    btn.innerHTML =
      '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" ' +
      'stroke-linecap="round" stroke-linejoin="round">' +
      '<rect x="9" y="9" width="13" height="13" rx="2" ry="2"></rect>' +
      '<path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"></path></svg>';
    btn.addEventListener("click", () => {
      const code = pre.querySelector("code");
      copyText(code ? code.textContent : pre.textContent, btn, "Copy code");
    });
    pre.appendChild(btn);
  });
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
      fence,
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
  head.textContent = seg.header || seg.lang || "code";
  div.appendChild(head);
  const pre = document.createElement("pre");
  pre.className = "code-block";
  const code = document.createElement("code");
  code.className = "hljs language-" + (seg.lang || "text");
  // Highlight with the SAME highlighter as the OverType Run Code editor,
  // so interleaved source blocks mirror the editor: shebang markers,
  // per-language hljs, and the auto-detect fallback for unknown
  // languages (instead of the plain text highlightElement left behind).
  const html = highlightFenceBody(seg.code, seg.lang);
  if (html !== null) code.innerHTML = html;
  else code.textContent = seg.code;
  pre.appendChild(code);
  div.appendChild(pre);
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
  box.classList.add("interleaved");
  if (!lastMarkdown) return;
  const results = splitResults(lastResultText);
  // Pure positional 1:1 — inline each artifact under its run's result
  // ONLY in the clean one-artifact-per-run case (the fan-out norm, where
  // artifact frames + run envelopes both arrive in document order). Any
  // other shape (no results, missing/extra artifacts, multi-file runs)
  // falls back to the rolled-up artifact block at the end.
  const inlineAll =
    runArtifacts.length > 0 && runArtifacts.length === results.length;
  let ri = 0;
  let ai = 0;
  for (const seg of splitMarkdown(lastMarkdown)) {
    if (seg.type === "prose") {
      box.appendChild(renderProseSeg(seg));
    } else {
      box.appendChild(renderCodeSeg(seg));
      if (!seg.isSeed && ri < results.length) {
        box.appendChild(resultElement(results[ri++]));
        if (inlineAll) {
          box.appendChild(
            renderArtifactInline(runArtifacts[ai++], artifactLang(lastMarkdown))
          );
        }
      }
    }
  }
  leftoverArtifacts = inlineAll ? [] : runArtifacts;
  addCopyButtons(box);
  pruneEmptyStdoutForImages(box);
  appendArtifactsIfAny();
  lastStderrEl = stderrElement();
  if (lastStderrEl) box.appendChild(lastStderrEl);
  box.scrollTop = box.scrollHeight;
}

// Normal (results-only) view, rebuilt from the raw markdown we captured.
function renderNormal() {
  const box = $("output");
  box.textContent = "";
  box.classList.remove("interleaved");
  if (lastOutputRaw) box.appendChild(resultElement(lastOutputRaw));
  addCopyButtons(box);
  pruneEmptyStdoutForImages(box);
  appendArtifactsIfAny();
  lastStderrEl = stderrElement();
  if (lastStderrEl) box.appendChild(lastStderrEl);
  box.scrollTop = box.scrollHeight;
}

// Re-append the current run's artifacts after a view rebuild — both view
// renderers wipe the output box, so the image previews + download/copy bar
// must be re-added or they silently vanish on toggle. The interleaved
// view places artifacts inline under their run's result and only rolls up
// the leftovers; the normal view keeps the full rollup.
function appendArtifactsIfAny() {
  const list = interleaved ? leftoverArtifacts : runArtifacts;
  if (list.length) renderArtifacts(list, lastMarkdown);
}

// The interleaved document as plain Markdown: the original source with each
// block's result spliced in after its code fence. Copy uses this while the
// interleave toggle is on, so the clipboard matches what's on screen.
function interleavedMarkdown() {
  const parts = [];
  const results = splitResults(lastResultText);
  let ri = 0;
  for (const seg of splitMarkdown(lastMarkdown)) {
    if (seg.type === "prose") {
      parts.push(seg.text);
    } else {
      parts.push(seg.fence + seg.header + "\n" + seg.code + "\n" + seg.fence);
      if (!seg.isSeed && ri < results.length) parts.push(results[ri++]);
    }
  }
  return parts.join("\n").replace(/\n+$/, "");
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

// Short, human-readable name for a warm-image pill in the cluster list:
// strip the docker.io registry, the library/ namespace, and the noise
// `:latest` tag — keep tags that actually differ (alpine, slim, act-22.04).
function shortImageName(img) {
  let s = img;
  if (s.startsWith("docker.io/")) s = s.slice("docker.io/".length);
  if (s.startsWith("library/")) s = s.slice("library/".length);
  if (s.endsWith(":latest")) s = s.slice(0, -":latest".length);
  return s;
}

// Pick the best compute node for a doc: probe-verified peers first (the
// swarm refresh proved they actually run jobs), then warm-image coverage,
// then lowest RTT. A node that merely answers hello (unverified — e.g.
// learned via a peer's hello frame, or listed but offline last refresh)
// only wins when no verified peer exists at all.
function pickTarget(doc) {
  const all = [...peers.values()].sort(
    (a, b) => ((a.probe === "ok" ? 0 : 1) - (b.probe === "ok" ? 0 : 1)) ||
              ((a.rtt_ms ?? 1e9) - (b.rtt_ms ?? 1e9))
  );
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
    if (p.images && p.images.length) {
      // One language-chip-style pill per warm LANGUAGE — the same names the
      // editor badges. An image can back several languages (gcc -> c/cpp/
      // fortran), so each gets its own pill, deduped; the full image ref
      // stays on hover. Images with no language mapping (custom images)
      // fall back to the short ref so nothing is hidden.
      const seen = new Set();
      for (const img of p.images) {
        const langs = IMAGE_LANGUAGES[img];
        const names = langs && langs.length ? langs : [shortImageName(img)];
        for (const name of names) {
          if (seen.has(name)) continue;
          seen.add(name);
          const chip = document.createElement("span");
          chip.className = "lang-chip ok";
          chip.textContent = name;
          chip.title = img;
          imgs.appendChild(chip);
        }
      }
    } else {
      imgs.textContent = "no warm images";
    }
    li.appendChild(imgs);
    const rtt = document.createElement("span");
    rtt.className = "rtt";
    rtt.textContent = p.rtt_ms == null ? "" : `${p.rtt_ms} ms`;
    li.appendChild(rtt);
    list.appendChild(li);
  }
  $("clusterCount").textContent = `${peers.size} node${peers.size === 1 ? "" : "s"}`;

  // "Good neighbors": alive peers (measured RTT) with >= threshold warm images.
  // This count surfaces in the status pill tooltip as a quick swarm-health signal.
  goodNeighborCount = sorted.filter(
    (p) => p.rtt_ms != null && p.images && p.images.length >= GOOD_NEIGHBOR_THRESHOLD
  ).length;
  renderMode();
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
        // Probe-verified nodes are dialed first (probe: "ok" means the
        // swarm refresh actually ran a job on them). Entries that were
        // reachable but failed the probe (probe: "failed") are skipped
        // entirely — a node that answers hello but can't run jobs is a
        // zombie, not a peer. Unreachable entries are dialed but ranked
        // last, so a flapping node still gets a chance to rejoin.
        if (n.probe === "failed") continue;
        push({
          node_id: n.node_id || null,
          relay: n.relay || null,
          ticket: n.ticket || null,
          probe: n.probe === "ok" ? "ok" : "unverified",
        });
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
      // Carry the list's probe verdict onto the stored peer so pickTarget
      // can prefer verified nodes even after the dial (the wasm hello
      // summary doesn't include it). Peers learned via another node's
      // hello frame have no list entry -> unverified by default.
      if (res.seed && res.seed.node_id) {
        const prev = peers.get(res.seed.node_id);
        peers.set(res.seed.node_id, {
          ...res.seed,
          seed: true,
          probe: prev && prev.probe ? prev.probe : (c.probe === "ok" ? "ok" : "unverified"),
        });
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

// Parse the location fragment for `seed` and `relay` params,
// and the query string for `code` (base64-encoded editor payload).
function parseUrlBootstrap() {
  const out = { seed: null, relay: null, code: null };
  const raw = (location.hash || "").replace(/^#/, "");
  if (raw) {
    for (const part of raw.split("&")) {
      const eq = part.indexOf("=");
      if (eq < 0) continue;
      const k = decodeParam(part.slice(0, eq)).trim();
      const v = decodeParam(part.slice(eq + 1)).trim();
      if (k === "seed" && v) out.seed = v;
      else if (k === "relay" && v) out.relay = v;
    }
  }
  // ?code=<base64> in the query string pre-fills the editor.
  const qs = location.search.replace(/^\?/, "");
  if (qs) {
    for (const part of qs.split("&")) {
      const eq = part.indexOf("=");
      if (eq < 0) continue;
      const k = decodeParam(part.slice(0, eq)).trim();
      const v = decodeParam(part.slice(eq + 1)).trim();
      if (k === "code" && v) {
        try {
          // Decode base64 — use atob on the raw value (URL-safe base64
          // normalized on the fly).  If the result contains control chars
          // or is empty, ignore it.
          const b64 = v.replace(/-/g, "+").replace(/_/g, "/");
          const decoded = atob(b64);
          if (decoded) out.code = decoded;
        } catch (_) { /* ignore malformed base64 */ }
      }
    }
  }
  return out;
}

// Debounced URL sync: updates ?code=<base64> in the query string so the
// current editor content is always shareable via the URL bar.
let _urlSyncTimer = null;
function syncCodeToUrl() {
  if (_urlSyncTimer) clearTimeout(_urlSyncTimer);
  _urlSyncTimer = setTimeout(() => {
    _urlSyncTimer = null;
    try {
      const val = editor ? editor.getValue() : "";
      const b64 = btoa(val)
        .replace(/\+/g, "-")
        .replace(/\//g, "_")
        .replace(/=+$/, "");
      const qs = b64 ? `?code=${b64}` : "";
      // Preserve hash (seed/relay) while updating the query string.
      const url = qs + location.hash;
      history.replaceState(null, "", url || location.pathname);
    } catch (_) { /* quota or encoding error — skip */ }
  }, 500);
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
  // Displayable swarm name for the disclaimer: the seed node's short id
  // when the link is `node_id@relay`, nothing for a bare EndpointTicket.
  const nr = splitNodeAtRelay(url.seed);
  setMode(!!url.seed, nr ? shortId(nr.node_id) : null);
  $("ticket").value = localStorage.getItem("ephemeral.ticket") || "";
  $("relay").value = localStorage.getItem("ephemeral.relay") || BOOTSTRAP.relay || "";
  setStatus("loading wasm…");
  await init();
  // Pre-fill editor from ?code=<base64> query parameter.
  if (url.code && editor) {
    editor.setValue(url.code);
    // Clear the query string so a refresh doesn't re-inject stale code.
    history.replaceState(null, "", location.pathname + location.hash);
  }
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
  leftoverArtifacts = [];
  outputRaw = "";
  lastMarkdown = markdown;
  lastResultText = "";
  lastStderr = "";
  lastExitCode = 0;
  warningsOn = false;
  lastStderrEl = null;
  $("warnings").hidden = true;
  $("output").textContent = "";
  $("output").classList.remove("interleaved");
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
      if (evt.stderr) appendStderr(evt.stderr, evt.exit_code);
      else $("warnings").hidden = true;
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
    // Skip aliases containing "fuck" (brainfuck) — this PWA is aimed at
    // younger students, and the reminder otherwise echoes every alias.
    const als = (ALIAS_MAP[l] || []).filter((a) => !a.toLowerCase().includes("fuck"));
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

// Timestamped filename for the .md export (ephemeral-YYYY-MM-DD-HHMMSS.md).
function timestampedFileName(ext) {
  const d = new Date();
  const p = (n) => String(n).padStart(2, "0");
  return `ephemeral-${d.getFullYear()}-${p(d.getMonth() + 1)}-${p(d.getDate())}-${p(d.getHours())}${p(d.getMinutes())}${p(d.getSeconds())}.${ext}`;
}

// Print ONLY the rendered Output box, formatted for paper (white theme):
// copies the box's HTML into a throwaway window with a light print
// stylesheet (plus the light highlight.js theme so code blocks print
// readable), then triggers the browser's print dialog. The temp window
// closes after the dialog, so it never lingers.
function printOutput() {
  const box = $("output");
  if (!box.textContent.trim()) return;
  const win = window.open("", "_blank");
  if (!win) return; // popup blocked
  const css = `
* { box-sizing: border-box; }
body {
  background: #fff;
  color: #111;
  font: 12pt/1.5 "Segoe UI", system-ui, sans-serif;
  max-width: 820px;
  margin: 0 auto;
  padding: 24px 28px;
}
pre, code { font-family: ui-monospace, Consolas, monospace; }
.line { margin: 0; white-space: pre-wrap; word-break: break-word; }
.line.log-stderr, .line.stderr-muted { color: #555; }
.line.done { color: #1a7f37; }
.line.err, .unsafe-note { color: #b00020; }
.unsafe-note { font-size: 11pt; margin: 6px 0 0; }
.result-title,
.source-head {
  color: #0a66c2;
  font-size: 11pt;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: .06em;
  margin: 14px 0 6px;
}
.result-step { color: #555; font-size: 10.5pt; margin: 8px 0 4px; }
.result-line { color: #111; white-space: pre-wrap; word-break: break-word; margin: 2px 0; }
.interleave-h1, .interleave-h2, .interleave-h3 { font-weight: 700; margin: 14px 0 4px; }
.interleave-h1 { font-size: 15pt; }
.interleave-h2 { font-size: 13pt; }
.interleave-h3 { font-size: 11pt; color: #555; }
.code-block {
  background: #f6f8fa;
  border: 1px solid #d8dee4;
  border-radius: 4px;
  padding: 8px 10px;
  margin: 6px 0 10px;
  white-space: pre-wrap;
  word-break: break-word;
  page-break-inside: avoid;
}
.code-block code { background: transparent; padding: 0; }
.block-copy, .artifact-bar { display: none; }
.artifact-img {
  max-width: 100%;
  max-height: 320px;
  border: 1px solid #ddd;
  border-radius: 4px;
  margin: 6px 0;
}
.block.reminder {
  border: 1px solid #d4a72c;
  background: #fdf6e3;
  border-radius: 4px;
  padding: 8px 10px;
  margin: 8px 0;
  page-break-inside: avoid;
}
.reminder-title { color: #7a5b00; font-weight: 600; margin-bottom: 4px; }
.reminder-als { color: #555; }
.reminder-hint { color: #555; margin-top: 4px; }
.reminder code { background: rgba(0, 0, 0, .06); border-radius: 3px; padding: 1px 5px; }
@media print { body { padding: 0; } }
`;
  win.document.write(
    `<!DOCTYPE html>\n<html lang="en">\n<head>\n<meta charset="utf-8" />\n` +
    `<title>Ephemeral — Output</title>\n` +
    `<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/atom-one-light.min.css" />\n` +
    `<style>${css}</style>\n</head>\n<body>\n<div class="print-output">\n` +
    box.innerHTML +
    `\n</div>\n</body>\n</html>`
  );
  win.document.close();
  win.addEventListener("load", () => { win.focus(); win.print(); });
  win.onafterprint = () => win.close();
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

// Render a SINGLE artifact inline under its run's result (interleaved
// view): image preview (or a file row for non-images) plus a Download /
// Copy-image action bar. Used when positional 1:1 matching holds — the
// one-artifact-per-run fan-out case.
function renderArtifactInline(a, lang) {
  const div = document.createElement("div");
  div.className = "block artifacts inline";
  if (IMAGE_MIMES[a.ext]) {
    const img = document.createElement("img");
    img.className = "artifact-img";
    img.src = `data:${a.mime};base64,${a.b64}`;
    img.alt = a.name;
    img.title = a.name;
    div.appendChild(img);
  } else {
    const row = document.createElement("div");
    row.className = "artifact-file";
    row.textContent = "📄 " + a.name;
    div.appendChild(row);
  }
  const bar = document.createElement("div");
  bar.className = "artifact-bar";
  const label = document.createElement("span");
  label.className = "artifact-count";
  label.textContent = a.name;
  bar.appendChild(label);
  if (IMAGE_MIMES[a.ext]) {
    const copy = document.createElement("button");
    copy.className = "secondary artifact-btn";
    copy.textContent = "Copy image";
    copy.addEventListener("click", () => copyArtifactImage(a, copy));
    bar.appendChild(copy);
  }
  const dl = document.createElement("button");
  dl.className = "secondary artifact-btn";
  dl.textContent = "Download";
  dl.addEventListener("click", () => downloadArtifacts([a], lang));
  bar.appendChild(dl);
  div.appendChild(bar);
  return div;
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
// Share URL: force-sync the current editor content into the URL and copy it.
$("shareBtn").addEventListener("click", () => {
  syncCodeToUrl();  // immediate, not debounced
  const url = location.href;
  copyText(url, $("shareBtn"), "Share URL");
});
// Ctrl+Enter (Cmd+Enter on Mac) anywhere on the page runs the current
// document — same path as the button, re-entry-guarded inside run().
document.addEventListener("keydown", (e) => {
  if ((e.ctrlKey || e.metaKey) && e.key === "Enter") {
    e.preventDefault();
    run();
  }
});
$("refresh").addEventListener("click", () => { peers.clear(); refreshPeers(); });

// The trailing (i) pill shown at the end of the language-chip row. Built
// once, then re-appended after every updateLangStatus wipe (same node, so
// its popover state and listeners survive). Lists every supported
// language (canonical + aliases) plus the /output artifact contract.
let langHelpPill = null;
function langHelpPillEl() {
  if (langHelpPill) return langHelpPill;
  const pill = document.createElement("span");
  pill.className = "lang-chip lang-help-chip";
  pill.title = "Supported languages & artifacts";
  pill.setAttribute("role", "button");
  pill.setAttribute("tabindex", "0");
  pill.setAttribute("aria-label", "Supported languages & artifacts");
  pill.setAttribute("aria-expanded", "false");
  pill.textContent = "i";

  const pop = document.createElement("div");
  pop.className = "lang-help-pop";
  pop.hidden = true;
  const title = document.createElement("div");
  title.className = "lang-help-title";
  title.textContent = "Supported languages";
  pop.appendChild(title);
  const list = document.createElement("div");
  list.className = "lang-help-list";
  for (const l of CANONICAL_LANGUAGES) {
    const span = document.createElement("span");
    span.className = "lang-help-item";
    span.setAttribute("role", "button");
    span.setAttribute("tabindex", "0");
    span.title = "Append a " + l + " demo to the Run Code editor";
    const code = document.createElement("code");
    code.textContent = l;
    span.appendChild(code);
    // Show at most one alias, and only when the whole entry stays short
    // enough to fit its grid cell on one line — long alias lists (node's,
    // pwsh's, lisp's, actionlint's lint-action…) wrap and mangle the grid.
    // Aliases containing "fuck" (brainfuck) are hidden too — this PWA is
    // aimed at younger students.
    const als = (ALIAS_MAP[l] || [])[0] || "";
    if (als && !als.toLowerCase().includes("fuck") && l.length + als.length + 2 <= 14) {
      const s = document.createElement("span");
      s.className = "als";
      s.textContent = " (" + als + ")";
      span.appendChild(s);
    }
    // Click appends the language's demo snippet (a markdown code fence) to
    // the Run Code editor, keeping whatever is already there. The click
    // still bubbles to the document-level close handler, so the popover
    // dismisses after appending — natural for "add one, run it".
    const appendSnippet = () => {
      const snip = LANG_SNIPPETS[l];
      if (!snip || !editor) return;
      const cur = editor.getValue().trimEnd();
      editor.setValue(cur ? cur + "\n\n" + snip + "\n" : snip + "\n");
      updateLangStatus(); // setValue doesn't fire OverType's onChange
      highlightCodeHeaders();
    };
    span.addEventListener("click", appendSnippet);
    span.addEventListener("keydown", (e) => {
      if (e.key === "Enter" || e.key === " ") {
        e.preventDefault();
        appendSnippet();
      }
    });
    list.appendChild(span);
  }
  pop.appendChild(list);
  const note = document.createElement("div");
  note.className = "lang-help-note";
  const noteCode = document.createElement("code");
  noteCode.textContent = "/output";
  note.append(
    "Artifacts: write to ", noteCode,
    " — downloads (images preview inline).");
  pop.appendChild(note);
  pill.appendChild(pop);

  // Position the popover exactly over the Run Code editor box on open,
  // so it never covers the header or the action buttons — the box is a
  // natural-sized placeholder, and the semi-transparent popover lets the
  // code underneath stay visible. Uses viewport (fixed) coordinates, so
  // the popover must be taken out of the pill's absolute-positioning
  // context while open.
  const positionPop = () => {
    const ed = $("editor").getBoundingClientRect();
    pop.style.position = "fixed";
    pop.style.left = ed.left + "px";
    // Anchor to the editor box's bottom edge (growing upward), so the
    // popover reads as attached to the (i) pill just below the box.
    pop.style.bottom = window.innerHeight - ed.bottom + "px";
    pop.style.width = ed.width + "px";
    // Hug the content height (no dead space at the bottom), but never
    // extend past the editor box (that would cover the Run buttons below)
    // — the compact, auto-column list fits the box with no scrolling.
    pop.style.height = ""; // natural size first, so scrollHeight is real
    pop.style.height = Math.min(pop.scrollHeight, ed.height) + "px";
  };
  const resetPop = () => {
    pop.style.position = "";
    pop.style.left = "";
    pop.style.bottom = "";
    pop.style.width = "";
    pop.style.height = "";
  };
  const setOpen = (open) => {
    pop.hidden = !open;
    if (open) positionPop();
    else resetPop();
    pill.classList.toggle("active", open);
    pill.setAttribute("aria-expanded", String(open));
  };
  window.addEventListener("resize", () => {
    if (!pop.hidden) positionPop();
  });
  pill.addEventListener("click", (e) => {
    e.stopPropagation();
    setOpen(pop.hidden);
  });
  pill.addEventListener("keydown", (e) => {
    if (e.key === "Enter" || e.key === " ") {
      e.preventDefault();
      setOpen(pop.hidden);
    }
  });
  // Hover bridging: the popover floats over the editor box, above the pill,
  // so there's a gap between them. Closing instantly on mouseout made that
  // gap uncrossable — dragging the cursor from the pill up to the popover
  // dismissed it. Instead, leaving the pill/popover zone starts a short
  // grace timer; re-entering (or reaching the popover) cancels it, so the
  // popover survives the crossing but still dismisses once the pointer
  // genuinely stays away.
  let closeTimer = null;
  const cancelClose = () => {
    if (closeTimer) { clearTimeout(closeTimer); closeTimer = null; }
  };
  const scheduleClose = () => {
    if (closeTimer) return;
    closeTimer = setTimeout(() => {
      closeTimer = null;
      setOpen(false);
    }, 250);
  };
  pill.addEventListener("mouseenter", () => { cancelClose(); setOpen(true); });
  document.addEventListener("mouseover", (e) => {
    if (pop.hidden) return;
    if (e.target.closest(".lang-help-chip, .lang-help-pop")) cancelClose();
    else scheduleClose();
  });
  document.addEventListener("click", () => { cancelClose(); setOpen(false); });
  document.addEventListener("keydown", (e) => {
    if (e.key === "Escape") setOpen(false);
  });
  langHelpPill = pill;
  return pill;
}
$("clearOutput").addEventListener("click", () => {
  $("output").textContent = "";
  outputRaw = "";
  runArtifacts = [];
  lastMarkdown = lastResultText = lastOutputRaw = "";
  lastStderr = "";
  lastExitCode = 0;
  warningsOn = false;
  lastStderrEl = null;
  $("warnings").hidden = true;
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

// Warnings toggle: reveals/collapses the exit-0 stderr lines (hidden by
// default). The button itself is the presence signal — yellow in both
// states — so there's no badge; the click just flips the element.
$("warnings").addEventListener("click", () => {
  warningsOn = !warningsOn;
  syncWarningsButton($("warnings"));
  if (lastStderrEl) lastStderrEl.hidden = !warningsOn;
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
  const text = interleaved && lastMarkdown
    ? interleavedMarkdown()
    : outputRaw.replace(/\n+$/, "");
  copyText(text, $("copyOutput"), "Copy output");
});
$("printOutput").addEventListener("click", printOutput);
$("copyCode").addEventListener("click", () => {
  copyText(editor.getValue(), $("copyCode"), "Copy code");
});
// Import a .md/.txt file into the Run Code editor via the native file
// picker — replaces whatever is currently there.
$("importCode").addEventListener("click", () => {
  const input = document.createElement("input");
  input.type = "file";
  input.accept = ".md,.markdown,.txt,text/markdown,text/plain";
  input.addEventListener("change", () => {
    const file = input.files && input.files[0];
    if (!file) return;
    const reader = new FileReader();
    reader.onload = () => {
      const text = String(reader.result || "");
      if (!text) return;
      editor.setValue(text);
      updateLangStatus(); // setValue doesn't fire OverType's onChange
      highlightCodeHeaders();
      syncCodeToUrl();
    };
    reader.readAsText(file);
  });
  input.click();
});
// Export the Run Code editor as a timestamped .md file download.
$("exportCode").addEventListener("click", () => {
  const markdown = editor.getValue();
  if (!markdown.trim()) return;
  const blob = new Blob([markdown], { type: "text/markdown;charset=utf-8" });
  triggerDownload(blob, timestampedFileName("md"));
});
// The ✕ Clear button wipes the editor with a programmatic setValue(""),
// which browsers never record in the textarea's native undo stack — so
// Ctrl+Z right after clearing would normally do nothing and the code is
// gone for good. Snapshot the pre-clear content here and restore it on
// Ctrl+Z (document-wide: the click leaves focus on the ✕ button, not the
// editor). The snapshot is dropped the moment the user types anything
// new, so Ctrl+Z then falls back to the browser's native undo of those
// keystrokes instead of resurrecting the old document.
let clearUndoValue = null;
$("clearCode").addEventListener("click", () => {
  if (editor.getValue()) clearUndoValue = editor.getValue();
  editor.setValue("");
  updateLangStatus();
  highlightCodeHeaders();
});
// OverType's hidden textarea (or the fallback one) lives inside #editor,
// so input events bubble here regardless of editor flavor. Only drop the
// snapshot once real content exists — a programmatic clear leaves the
// value empty, so that transient event can't race the click handler.
$("editor").addEventListener("input", () => {
  if (editor && editor.getValue()) clearUndoValue = null;
});
document.addEventListener("keydown", (e) => {
  if (
    (e.ctrlKey || e.metaKey) && !e.shiftKey && !e.altKey &&
    (e.key === "z" || e.key === "Z") &&
    clearUndoValue && editor && !editor.getValue()
  ) {
    e.preventDefault();
    editor.setValue(clearUndoValue);
    clearUndoValue = null;
    updateLangStatus();
    highlightCodeHeaders();
    syncCodeToUrl();
  }
});
initEditor();
updateLangStatus();
highlightCodeHeaders();
start();
