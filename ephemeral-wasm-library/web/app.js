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
import { LANG_BLURBS } from "./blurbs.js";

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
// Interleave button tristate: "auto" follows the document shape after
// each run (prose → interleaved, bare code → results), "on" always
// interleaves, "off" always shows results-only. Cycles on click.
let interleaveMode = "auto";
let lastMarkdown = "";
// The raw (folds intact) document of that run, for the interleaved view.
let lastDisplayMarkdown = "";
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

// --- live document preview -------------------------------------------------
// Prose is never executed, so the interleaved view can track the editor as it
// is typed without submitting a job. Results stay attached to the fences that
// produced them (matched by content, not position), so prose edits keep every
// result on screen and only an edited block loses its own — now stale —
// result. While a job is in flight the box carries streamed logs, so live
// re-rendering is suppressed until the run's own final render.
let runInFlight = false;
let liveTimer = null;
let liveLastAt = 0;
const LIVE_PREVIEW_MS = 220;
// The document the interleaved view currently shows (skip no-op renders).
let lastRenderedDoc = null;

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
        styleSeedFolds(); // folded seedfile payloads get their accent treatment
        syncMoreHint(); // content grew/shrunk — re-check the fold indicator
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
// Lifts the core's default 300s cap; the swarm strips it, like `unsafe`.
const NO_TIMEOUT_FLAGS = new Set(["notimeout", "no-timeout"]);
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
    out.push({
      lang: tokens[0].toLowerCase(),
      params: tokens.slice(1),
      body,
      // Raw fence + header, so per-block mini-documents can be rebuilt
      // verbatim (seed/file blocks and exec blocks alike).
      header: m[1].trim(),
      fence: "```",
      // Byte offsets of the body in the raw document, so a seedfile payload
      // can be folded away (and restored) with a surgical splice.
      start: m.index,
      bodyStart,
      bodyEnd: closeIdx === -1 ? markdown.length : closeIdx,
    });
  }
  return out;
}

// ---------- seedfiles: append, import, fold --------------------------------
// A seedfile fence (` ```name.ext [b64] ` + payload + closing fence) is staged
// into the job's workdir by the node before any code runs: text rides inline,
// everything else as base64 — the same contract the desktop tray's Ctrl+Win+X
// clipboard converter writes.
//
// Base64 payloads are big enough to bury a document, so a seedfile's body can
// fold down to a single placeholder line. The real body is kept in
// `foldedSeeds` and spliced back in for run / copy / export / share, so folding
// is purely a view — the node always receives the complete document.
const SEED_B64_LIMIT = 1_000_000; // base64 characters (~750 KB of raw bytes)
const SEED_TEXT_LIMIT = 512_000;  // characters of a plain-text seedfile
const SEED_AUTO_FOLD = 500;       // body length that folds itself away on import
// A folded body is exactly one size line and nothing else: `<80,000 chars; ~58.6 KB>`.
// It has to live in the document (OverType's preview is an overlay whose lines
// must match the invisible textarea, so hiding the payload in the DOM would
// desync every caret below it). The payload itself is kept out of the document
// in `foldedSeeds`, keyed by the fence name + that char count — both of which
// the folded block still shows.
const SEED_FOLD_RE = /^<(.+?) chars; ~(.+?)>$/;
const MARKDOWN_FILE_RE = /\.(md|markdown|mdown|mkd|mdx)$/i;
const FOLD_CACHE_LIMIT = 24;

// `${fence name}\0${payload chars}` -> payload (without the outer newlines).
// Editing the size line or the fence header unkeys a block, which only ever
// loses the payload (a loud "re-add the file" on run) — never mixes two files
// up, except for the pathological case of two same-named files of exactly the
// same size, where the most recently folded one wins.
const foldedSeeds = new Map();

function isSeedFence(f) {
  return !!f && !SUPPORTED_LANGUAGES.has(f.lang) && f.lang.includes(".");
}

function seedName(f) {
  return (f.header || "").split(/\s+/)[0] || "seedfile";
}

function parseFoldLine(body) {
  const first = String(body || "").trim().split("\n")[0].trim();
  const m = SEED_FOLD_RE.exec(first);
  if (!m) return null;
  const chars = Number(m[1].replace(/\D/g, ""));
  return Number.isFinite(chars) && chars > 0 ? { chars, size: m[2] } : null;
}

function isFoldedBody(body) {
  return !!parseFoldLine(body);
}

function foldKey(f, chars) {
  return f.lang + "\u0000" + chars;
}

// Seedfile bodies are counted the way the size line reads them: base64 ignores
// whitespace (paste-wrapped base64 is still one payload), text counts chars.
function payloadChars(payload, b64) {
  return b64 ? payload.replace(/\s+/g, "").length : payload.length;
}

function formatBytes(n) {
  if (n < 1024) return n + " B";
  if (n < 1024 * 1024) return (n / 1024).toFixed(1) + " KB";
  return (n / (1024 * 1024)).toFixed(1) + " MB";
}

// Folded payloads live in memory only, so a handful covers the undo/redo
// round-trips — each one can be a megabyte.
function rememberFold(key, payload) {
  foldedSeeds.set(key, payload);
  while (foldedSeeds.size > FOLD_CACHE_LIMIT) {
    foldedSeeds.delete(foldedSeeds.keys().next().value);
  }
}

function touchFold(key) {
  const payload = foldedSeeds.get(key);
  if (payload === undefined) return undefined;
  foldedSeeds.delete(key);
  foldedSeeds.set(key, payload);
  return payload;
}

function foldPlaceholder(payload, b64) {
  const chars = payloadChars(payload, b64);
  const bytes = b64
    ? Math.floor((chars * 3) / 4)
    : new TextEncoder().encode(payload).length;
  return `<${chars.toLocaleString("en-US")} chars; ~${formatBytes(bytes)}>`;
}

// Splice every folded payload back in (last fence first, so earlier offsets
// stay valid). A block whose payload is gone — restored from a share link, or
// its key edited — keeps its size line; the submit paths check lostFolds()
// first and refuse to send that.
function expandFolds(text) {
  if (!foldedSeeds.size) return text;
  const fences = fenceInfo(text);
  for (let i = fences.length - 1; i >= 0; i--) {
    const f = fences[i];
    if (!isSeedFence(f)) continue;
    const line = parseFoldLine(f.body);
    if (!line) continue;
    const payload = touchFold(foldKey(f, line.chars));
    if (payload === undefined) continue;
    text = text.slice(0, f.bodyStart) + "\n" + payload + "\n" + text.slice(f.bodyEnd);
  }
  return text;
}

// Names of folded seedfiles whose payloads are no longer available.
function lostFolds(markdown) {
  const lost = [];
  for (const f of fenceInfo(markdown)) {
    if (!isSeedFence(f)) continue;
    const line = parseFoldLine(f.body);
    if (!line) continue;
    if (!foldedSeeds.has(foldKey(f, line.chars))) lost.push(seedName(f));
  }
  return lost;
}

// The document as the node (or a paste into the desktop app) must see it:
// every folded payload restored.
function liveMarkdown() {
  return editor ? expandFolds(editor.getValue()) : "";
}

// Refresh everything the document drives. OverType's onChange covers typing,
// but every programmatic edit (setValue, the splice below) has to call this
// itself.
function refreshEditorChrome() {
  updateLangStatus();
  highlightCodeHeaders();
  styleSeedFolds();
  syncCodeToUrl();
}

// Replace one fence's body in place. setRangeText edits the textarea by exact
// character range (no clipboard/execCommand round-trip) and leaves the caret
// and scroll position alone; the synthetic input event is what OverType listens
// to, so the preview follows. Falls back to setValue if the range edit does not
// land exactly, which keeps the document and the view in sync no matter what.
function spliceEditorBody(fence, newBody) {
  const doc = editor.getValue();
  const next = doc.slice(0, fence.bodyStart) + newBody + doc.slice(fence.bodyEnd);
  const ta = document.querySelector("#editor .overtype-input");
  if (ta && typeof ta.setRangeText === "function") {
    ta.setRangeText(newBody, fence.bodyStart, fence.bodyEnd, "end");
    if (ta.value === next) {
      ta.dispatchEvent(new Event("input", { bubbles: true }));
      refreshEditorChrome();
      return;
    }
    ta.value = doc; // undo a partial edit before taking the slow path
  }
  editor.setValue(next);
  refreshEditorChrome();
}

// Clicking a folded payload's reminder line unfolds it. Folding stays the
// pill's job; this is only the way back out. OverType's invisible textarea
// covers the preview, so the click leaves a collapsed caret in it — unfold the
// seedfile whose body contains it.
$("editor").addEventListener("click", () => {
  if (!editor) return;
  const ta = document.querySelector("#editor .overtype-input");
  if (!ta || ta.selectionStart !== ta.selectionEnd) return; // a drag-select
  const pos = ta.selectionStart;
  const idx = fenceInfo(editor.getValue()).findIndex(
    (f) => isSeedFence(f) && isFoldedBody(f.body) && pos > f.bodyStart && pos <= f.bodyEnd
  );
  if (idx !== -1) toggleSeedFold(idx);
});

// Fold or unfold one seedfile block. Only seed/file fences get this — a
// language pill's click re-runs its block instead.
function toggleSeedFold(idx) {
  if (!editor) return;
  const fence = fenceInfo(editor.getValue())[idx];
  if (!isSeedFence(fence)) return;
  const line = parseFoldLine(fence.body);
  if (line) {
    const payload = touchFold(foldKey(fence, line.chars));
    if (payload === undefined) {
      appendOut(
        `${seedName(fence)} is folded, but its contents are no longer in ` +
        "memory — re-add the file.",
        "err"
      );
      return;
    }
    spliceEditorBody(fence, "\n" + payload + "\n");
    return;
  }
  const payload = fence.body.replace(/^\n+|\n+$/g, "");
  if (!payload.trim()) return; // nothing to fold away
  const b64 = /(^|\s)b64(\s|$)/i.test(fence.header);
  rememberFold(foldKey(fence, payloadChars(payload, b64)), payload);
  spliceEditorBody(fence, "\n" + foldPlaceholder(payload, b64) + "\n");
}

// Style folded payloads in OverType's preview: the placeholder is plain code,
// so mark its block for the accent treatment in CSS. Runs after every render
// (OverType rebuilds the preview DOM on each keystroke).
function styleSeedFolds() {
  const preview = document.querySelector("#editor .overtype-preview");
  if (!preview) return;
  for (const pre of preview.querySelectorAll("pre.code-block")) {
    const code = pre.firstElementChild;
    const first = ((code && code.textContent) || "").split("\n")[0].trim();
    pre.classList.toggle("seed-fold", SEED_FOLD_RE.test(first));
  }
}

// A header token only reads as a seedfile when it ends in a short alphanumeric
// extension, so a sanitized basename must keep one — and a name must survive
// being interpolated into a fence header and a shell heredoc.
function seedFileName(filename) {
  let base = String(filename || "").split(/[\\/]/).pop().trim();
  base = base.replace(/[^A-Za-z0-9._-]+/g, "_").replace(/^[._]+/, "").slice(0, 64);
  if (!base) base = "file";
  if (!/\.[A-Za-z0-9]{1,8}$/.test(base)) base = base.replace(/\.+$/, "") + ".bin";
  return base;
}

// `b64` is the parser's base64 marker; `origin=` keeps the original filename
// readable when sanitizing changed it. Both are seedfile bookkeeping, not
// language parameters.
function seedHeaderFor(name, original, b64) {
  const bits = [name];
  if (b64) bits.push("b64");
  const origin = String(original || "").replace(/[^A-Za-z0-9._-]+/g, "_");
  if (origin && origin !== name) bits.push("origin=" + origin.slice(0, 96));
  return bits.join(" ");
}

function bytesToBase64(bytes) {
  let binary = "";
  const CHUNK = 0x8000;
  for (let i = 0; i < bytes.length; i += CHUNK) {
    binary += String.fromCharCode.apply(null, bytes.subarray(i, i + CHUNK));
  }
  return btoa(binary);
}

// UTF-8 text with no NUL bytes rides inline; anything else (images, archives,
// PDFs, a binary that happens to decode) becomes base64.
function decodeSeedText(bytes) {
  try {
    const text = new TextDecoder("utf-8", { fatal: true }).decode(bytes);
    return text.includes("\u0000") ? null : text;
  } catch (_) {
    return null;
  }
}

function seedFenceText(header, body) {
  return "```" + header + "\n" + String(body || "").replace(/\n+$/, "") + "\n```";
}

// One uploaded file -> the markdown this client appends. Markdown documents
// keep their own semantics (their code blocks stay runnable); every other file
// becomes a seedfile the node stages into the workdir.
async function fileToMarkdown(file) {
  const bytes = new Uint8Array(await file.arrayBuffer());
  if (MARKDOWN_FILE_RE.test(file.name)) {
    return new TextDecoder("utf-8").decode(bytes).replace(/^\uFEFF/, "");
  }
  const name = seedFileName(file.name);
  const text = decodeSeedText(bytes);
  if (text !== null) {
    const body = text.replace(/^\uFEFF/, "").replace(/\n+$/, "");
    if (body.length > SEED_TEXT_LIMIT) {
      throw new Error(
        `${body.length.toLocaleString()} characters is over the ` +
        `${SEED_TEXT_LIMIT.toLocaleString()}-character text cap`
      );
    }
    return seedFenceText(seedHeaderFor(name, file.name, false), body);
  }
  const b64 = bytesToBase64(bytes);
  if (b64.length > SEED_B64_LIMIT) {
    throw new Error(
      `${formatBytes(bytes.length)} encodes to ${b64.length.toLocaleString()} ` +
      `base64 characters, over the ${SEED_B64_LIMIT.toLocaleString()}-character cap`
    );
  }
  return seedFenceText(seedHeaderFor(name, file.name, true), b64);
}

// Append to the document — never replace it (the ✕ Clear button is the way to
// start over).
function appendToEditor(snippet) {
  const cur = editor.getValue();
  const sep = !cur.trim()
    ? ""
    : cur.endsWith("\n\n") ? "" : cur.endsWith("\n") ? "\n" : "\n\n";
  editor.setValue(cur + sep + snippet.replace(/\n+$/, "") + "\n");
  refreshEditorChrome();
}

async function addFiles(fileList) {
  const files = Array.from(fileList || []);
  if (!files.length || !editor) return;
  const firstFence = fenceInfo(editor.getValue()).length;
  const parts = [];
  const errors = [];
  for (const file of files) {
    try {
      parts.push(await fileToMarkdown(file));
    } catch (e) {
      errors.push(`${file.name}: ${(e && e.message) || e}`);
    }
  }
  // A rejected file is reported at the very END: everything before that point
  // rebuilds the output panel (the live document preview), which would wipe
  // the message straight back off the screen.
  if (!parts.length) {
    reportUploadErrors(errors);
    return;
  }
  appendToEditor(parts.join("\n\n"));
  // Fold the payloads that would otherwise bury the document. Fences are
  // addressed by index and folding never adds or removes one, so walking
  // backwards from the newest is safe even as each toggle rewrites the doc.
  const fences = fenceInfo(editor.getValue());
  for (let i = fences.length - 1; i >= firstFence; i--) {
    const f = fences[i];
    const body = f.body.replace(/^\n+|\n+$/g, "");
    if (isSeedFence(f) && body.length > SEED_AUTO_FOLD) toggleSeedFold(i);
  }
  if (errors.length) reportUploadErrors(errors);
}

// Rejected uploads go to the header pill: the output panel is a live mirror of
// the document and repaints a couple of hundred milliseconds after an edit, so
// a line written there would vanish. The label carries the headline, the
// tooltip the reason.
function reportUploadErrors(errors) {
  if (!errors.length) return;
  setStatus(
    errors.length === 1 ? "1 file was not added" : `${errors.length} files were not added`,
    "err"
  );
  setDetail(errors.join("\n"));
}

// ---------- pill ↔ editor linking ------------------------------------------
// OverType renders a fence as a run of sibling nodes in the preview: the
// ```lang opening line, the highlighted code <pre>, then the closing ``` line.
// Grouping them gives every fence index a contiguous highlight range, so a
// pill hover can light up the exact block it describes.
function previewFenceGroups() {
  const preview = document.querySelector("#editor .overtype-preview");
  if (!preview) return [];
  const groups = [];
  let cur = null;
  for (const el of preview.children) {
    if (el.classList.contains("code-block-line")) {
      const text = (el.textContent || "").trim();
      const bare = /^(`{3,}|~{3,})$/.test(text);
      if (!bare && /^(`{3,}|~{3,})\s*\S/.test(text)) {
        cur = [el];
        groups.push(cur);
        continue;
      }
      if (cur) {
        cur.push(el);
        if (bare) cur = null; // the closing fence ends this block
      }
      continue;
    }
    if (cur) cur.push(el);
  }
  return groups;
}

// Light up the block behind a pill. OverType wipes the preview DOM on every
// keystroke, so the class is transient by design — it is re-applied on hover.
function setPreviewHighlight(idx) {
  for (const el of document.querySelectorAll("#editor .overtype-preview .block-hover")) {
    el.classList.remove("block-hover");
  }
  if (idx == null || !editor) return;
  const groups = previewFenceGroups();
  // Only trust the grouping when it lines up 1:1 with the parsed fences (a
  // bare, headerless fence can throw the scan off) — highlighting the wrong
  // block would be worse than highlighting none.
  if (groups.length !== fenceInfo(editor.getValue()).length) return;
  const group = groups[idx];
  if (!group || !group.length) return;
  for (const el of group) el.classList.add("block-hover");
  revealPreviewBlock(group);
}

// Bring a highlighted block into view — but only when it is off screen, so
// sweeping across the pills doesn't yank the page around.
function revealPreviewBlock(group) {
  const first = group[0];
  const last = group[group.length - 1];
  const top = first.getBoundingClientRect().top;
  const bottom = last.getBoundingClientRect().bottom;
  const margin = 80; // leave room for the sticky toolbar above / dock below
  if (top >= margin && bottom <= window.innerHeight - margin) return;
  first.scrollIntoView({ block: "center", behavior: "smooth" });
}

// Every pill that belongs to a fence points back at it: hovering (or focusing)
// the pill highlights that block in the editor.
function linkPillToBlock(el, idx) {
  el.dataset.idx = String(idx);
  el.addEventListener("mouseenter", () => setPreviewHighlight(idx));
  el.addEventListener("mouseleave", () => setPreviewHighlight(null));
  return el;
}

function updateLangStatus() {
  if (!editor) return; // OverType fires onChange during its own _init,
  // before the constructor's destructuring assigns `editor`.
  const el = $("langStatus");
  el.textContent = "";
  const fences = fenceInfo(editor.getValue());
  // A chained document cannot re-run one block in isolation (it would miss the
  // files handed down from above), so its pills replay from the root instead.
  const chainedDoc = chainRequested(fences);
  for (let i = 0; i < fences.length; i++) {
    const f = fences[i];
    // Language pill: blue for a supported language, orange for an unknown
    // one, neutral for seed/file blocks. No validity icon — the per-block
    // run status (spinner/check/triangle/x) is what lives inside the pill
    // while a run is in flight, matched back to the fence via data-idx.
    const ok = SUPPORTED_LANGUAGES.has(f.lang);
    const isFile = !ok && f.lang.includes(".");
    // A seedfile pill folds its payload away (and back): a megabyte of base64
    // is unreadable inline, but the fence itself still has to be visible.
    const foldable = isFile && !!f.body.trim();
    const folded = foldable && isFoldedBody(f.body);
    // One pill per fence: the header is a single declaration, so the
    // language and its params share a pill ("llm chain" is one decision, not
    // two). Splitting them into a declaration pill plus a pill per param read
    // as separate blocks and pushed the artifact chip between them.
    //
    // The pill colour is the strictest verdict across the whole header:
    // stripped tokens (`unsafe`, `notimeout`, an `image=` override) or an
    // unknown language go amber, an unrecognized token greys it, and a
    // supported language with honored flags stays blue. The tooltip names the
    // token responsible, and a stripped token keeps its inline ✗ — being
    // honored is what a blue pill already means, so flags do not repeat a ✓.
    const labels = [];
    const notes = [];
    let strippedToken = false;
    let unknownToken = false;
    for (const raw of f.params) {
      const p = raw.toLowerCase();
      if (isFile && (p === "b64" || p.startsWith("origin="))) {
        // Seedfile bookkeeping, not a language parameter: `b64` says the body
        // is base64, `origin=` remembers the uploaded filename. Neither is a
        // warning.
        if (p === "b64") {
          labels.push(raw);
          notes.push("staged as base64 — the node decodes it into the workdir before the run");
        }
      } else if (NETWORK_FLAGS.has(p) || NO_TIMEOUT_FLAGS.has(p)) {
        labels.push("✗ " + raw);
        notes.push(`'${raw}' is not honored on the distributed network — jobs run sandboxed, with a node-side timeout`);
        strippedToken = true;
      } else if (DROPPED_OVERRIDES.some((o) => p.startsWith(o + "="))) {
        labels.push("✗ " + raw);
        notes.push(`'${raw}' is dropped on remote jobs — the node operator decides the image`);
        strippedToken = true;
      } else if (CHAIN_FLAGS.has(p) || NO_CHAIN_FLAGS.has(p)) {
        labels.push(raw);
        notes.push(`'${raw}' is honored by the node`);
      } else {
        labels.push(raw);
        notes.push(`'${raw}' is not a header token — ignored by the parser`);
        unknownToken = true;
      }
    }
    const chip = document.createElement("span");
    // `lang-name` marks the declaration pill itself: run-status overlays and
    // artifact pills share the fence index, so updates must not grab them.
    chip.className = "lang-chip lang-name " + (
      (!ok && !isFile) || strippedToken ? "invalid"
        : isFile || unknownToken ? "file"
          : "valid");
    chip.title = [
      ok
        ? chainedDoc
          ? "supported — chained: click to replay from the root through this block"
          : "supported — click to run just this block"
        : isFile
          ? foldable
            ? folded
              ? "seedfile (folded) — click to show its contents"
              : "seedfile — click to fold its contents away"
            : "seedfile — empty (nothing to stage)"
          : "not in the language map — this block will be rejected",
      ...notes,
    ].join("\n");
    chip.dataset.baseTitle = chip.title;
    linkPillToBlock(chip, i);
    // A supported language pill doubles as the "re-run just this block"
    // button: it is reachable by keyboard and never re-submits the document.
    if (ok) {
      chip.classList.add("rerun");
      chip.setAttribute("role", "button");
      chip.setAttribute("tabindex", "0");
      chip.addEventListener("focus", () => setPreviewHighlight(i));
      chip.addEventListener("blur", () => setPreviewHighlight(null));
      chip.addEventListener("click", () => rerunBlock(i));
      chip.addEventListener("keydown", (e) => {
        if (e.key === "Enter" || e.key === " ") {
          e.preventDefault();
          rerunBlock(i);
        }
      });
    }
    // A seedfile pill is also the fold toggle: keyboard-reachable, and it
    // never submits anything (unlike a language pill's re-run).
    if (foldable && !ok) {
      chip.classList.add("foldable");
      chip.setAttribute("role", "button");
      chip.setAttribute("tabindex", "0");
      chip.addEventListener("focus", () => setPreviewHighlight(i));
      chip.addEventListener("blur", () => setPreviewHighlight(null));
      chip.addEventListener("click", () => toggleSeedFold(i));
      chip.addEventListener("keydown", (e) => {
        if (e.key === "Enter" || e.key === " ") {
          e.preventDefault();
          toggleSeedFold(i);
        }
      });
    }
    const runInd = document.createElement("span");
    runInd.className = "chip-run";
    chip.appendChild(runInd);
    if (foldable && !ok) chip.appendChild(document.createTextNode(folded ? "▸ " : "▾ "));
    chip.appendChild(document.createTextNode([f.lang, ...labels].join(" ")));
    // A fence's pills share a straight edge (see .pill-group): the declaration
    // and the artifacts it writes read as one group, not as loose controls.
    const group = document.createElement("span");
    group.className = "pill-group";
    group.appendChild(chip);
    // Artifact pills: the block references /output, so the run is expected to
    // return a downloadable artifact (single images preview inline). One pill
    // per declared name, so two outputs give three edge-sharing pills.
    const artifactNames = artifactNamesFromScript(f.body);
    if (/(?<![\w/])\/output(?=[/'"\s]|$)/.test(f.body || "")) {
      for (const name of artifactNames.length ? artifactNames : [null]) {
        const achip = document.createElement("span");
        achip.className = "lang-chip artifact";
        achip.textContent = name ? "📦 " + name : "📦 artifact";
        achip.title = name
          ? `writes ${name} to /output — returned as a downloadable artifact after the run (images preview inline)`
          : "writes to /output — returned as a downloadable artifact after the run (images preview inline)";
        linkPillToBlock(achip, i);
        group.appendChild(achip);
      }
    }
    el.appendChild(group);
  }
  // The (i) help pill is always the trailing chip — even with no fences,
  // so it's a static affordance, not tied to whatever the doc declares.
  el.appendChild(langHelpPillEl());
  el.hidden = false;
  // Chips appearing/disappearing changes how tall the dock is — re-check
  // the "more below" indicator against the dock's new top edge.
  syncMoreHint();
  // Every path that changes the document (typing, setValue from import /
  // snippet / clear / undo / URL pre-fill) reaches updateLangStatus, so this
  // is also the one place the live document preview is kept in sync.
  scheduleLivePreview();
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
    let first = true; // the first token is the language declaration
    for (const part of m[2].split(/(\s+)/)) {
      if (/^\s+$/.test(part)) {
        span.appendChild(document.createTextNode(part));
      } else {
        const t = document.createElement("span");
        t.className = "code-header";
        if (first) {
          // Mirror the language pills: blue for a supported language, orange
          // for an unknown one (seed/file info-strings stay neutral).
          const lang = part.toLowerCase();
          if (SUPPORTED_LANGUAGES.has(lang)) t.classList.add("valid");
          else if (!lang.includes(".")) t.classList.add("invalid");
          first = false;
        }
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
//
// Fences follow CommonMark: a run of N>=3 backticks is closed only by a run
// of at least N. Scanning for a literal ``` broke on output that contains
// fences of its own (an LLM reply, a printed README) — the wrapper ended
// early and the rest spilled out of the panel as loose lines. The core wraps
// such a body in a longer fence (executor._md_fence); when the body still
// contains fences it is re-rendered as markdown so the nested block gets its
// own panel instead of one panel full of literal backticks.
const RESULT_MAX_NESTING = 3;

function renderResult(text, depth = 0) {
  const esc = (s) => s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
  const html = [];
  for (const seg of splitMarkdown(text)) {
    if (seg.type === "prose") {
      for (const line of seg.text.split("\n")) {
        if (/^### /.test(line)) html.push(`<div class="result-step">${esc(line.slice(4))}</div>`);
        else if (/^## /.test(line)) {
          const title = esc(line.slice(3));
          // Failure envelopes ("## Python Run 1 Failed", "... Timed Out",
          // "... Failed (dependency resolution)") render the title red.
          const failed = /\b(Failed|Timed Out)\b/i.test(title);
          html.push(`<div class="result-title${failed ? " failed" : ""}">${title}</div>`);
        }
        else if (line.trim()) html.push(`<div class="result-line">${esc(line)}</div>`);
      }
      continue;
    }
    // A failed run with no stdout ships an empty fence — the blank code box
    // is just noise next to the error text, so skip it entirely.
    if (!seg.code.trim()) continue;
    if (depth < RESULT_MAX_NESTING && /^[ \t]*`{3,}/m.test(seg.code)) {
      html.push(renderResult(seg.code, depth + 1));
      continue;
    }
    const lang = esc(seg.lang || "text");
    html.push(`<pre class="code-block"><code class="hljs language-${lang}">${esc(seg.code)}</code></pre>`);
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
  // Normalise CRLF first: `.` never matches \r in JS, so a fence line ending
  // in \r failed the open regex and a Windows-authored doc rendered as one
  // wall of prose.
  const lines = md.replace(/\r\n?/g, "\n").split("\n");
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

// Highlighted fence bodies, keyed by language + exact code. The live preview
// re-renders the whole document on (throttled) keystrokes; re-running
// highlight.js over unchanged blocks every time would make typing stutter, so
// unchanged fences reuse their HTML. Cleared wholesale at the cap (bounded
// memory — these are classroom-sized documents).
const hlCache = new Map();
const HL_CACHE_MAX = 400;

function cachedHighlight(seg) {
  const key = (seg.lang || "") + "\u0000" + seg.code;
  if (hlCache.has(key)) return hlCache.get(key);
  const html = highlightFenceBody(seg.code, seg.lang);
  if (hlCache.size >= HL_CACHE_MAX) hlCache.clear();
  hlCache.set(key, html);
  return html;
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
  const html = cachedHighlight(seg);
  if (html !== null) code.innerHTML = html;
  else code.textContent = seg.code;
  pre.appendChild(code);
  div.appendChild(pre);
  return div;
}

// Lightweight Markdown renderer for interleaved prose — blockquotes
// (callouts), horizontal rules, lists, tables, emphasis, links, inline
// code. markdown-it with html:false escapes raw HTML in node output
// instead of executing it (output is untrusted). Heading/paragraph
// tokens are re-tagged with the classes the existing stylesheet uses so
// the interleaved view keeps its current look.
let proseMd = null;
function proseRenderer() {
  if (proseMd) return proseMd;
  if (!window.markdownit) return null; // CDN blocked — caller falls back
  proseMd = new window.markdownit({ html: false, linkify: true, breaks: false });
  const defaultOpen = proseMd.renderer.rules.heading_open ||
    ((t, i, o, e, self) => self.renderToken(t, i, o));
  proseMd.renderer.rules.heading_open = (t, i, o, e, self) => {
    t[i].attrJoin("class", "interleave-h" + t[i].tag.slice(1));
    return defaultOpen(t, i, o, e, self);
  };
  const defaultPara = proseMd.renderer.rules.paragraph_open ||
    ((t, i, o, e, self) => self.renderToken(t, i, o));
  proseMd.renderer.rules.paragraph_open = (t, i, o, e, self) => {
    t[i].attrJoin("class", "result-line");
    return defaultPara(t, i, o, e, self);
  };
  return proseMd;
}

function renderProseSeg(seg) {
  const div = document.createElement("div");
  div.className = "block prose";
  const md = proseRenderer();
  if (md) {
    div.innerHTML = md.render(seg.text);
    return div;
  }
  // Fallback (markdown-it CDN unreachable): line-by-line headings + text.
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

// The markdown the interleaved view renders: the LIVE editor content, so
// prose follows typing without a re-run (only fenced code is ever submitted).
// Falls back to the last submitted document when the editor is empty, so
// clearing the code box doesn't blank the output panel.
function displayDoc() {
  // The RAW editor value: the output mirrors what is on screen, so a folded
  // seedfile shows its placeholder line rather than a wall of base64.
  const live = editor ? editor.getValue() : "";
  return live.trim() ? live : (lastDisplayMarkdown || lastMarkdown);
}

// Identity of a runnable fence: header + body. Pairing on content (not
// position) is what keeps results attached to their block while the prose
// around them is edited.
function fenceKey(seg) {
  return seg.header + "\u0000" + seg.code;
}

function runnableFences(segs) {
  return segs.filter((s) => s.type === "code" && !s.isSeed);
}

// Pair each executable fence in `segs` with the result the last run produced
// for it — matched by content and consumed in document order. Prose-only
// edits keep every result, an edited block drops just its own (stale) result,
// and inserting or deleting a fence shifts nothing.
function pairResults(segs, results) {
  const runKeys = runnableFences(splitMarkdown(lastMarkdown)).map(fenceKey);
  const pairs = new Map(); // segment index -> result envelope text
  let from = 0;
  segs.forEach((seg, i) => {
    if (seg.type !== "code" || seg.isSeed) return;
    const key = fenceKey(seg);
    for (let j = from; j < runKeys.length; j++) {
      if (runKeys[j] !== key) continue;
      if (j < results.length) pairs.set(i, results[j]);
      from = j + 1;
      return;
    }
  });
  return pairs;
}

// True when the displayed document's runnable fences are exactly the ones the
// last run executed, in the same order — the only case where inline artifacts
// can be trusted to line up with their block.
function docMatchesRun(segs) {
  const live = runnableFences(segs).map(fenceKey);
  const run = runnableFences(splitMarkdown(lastMarkdown)).map(fenceKey);
  return live.length === run.length && live.every((k, i) => k === run[i]);
}

// Placeholder under a code block that has no result yet: nothing has run, the
// block was added after the run, or its code changed since. Without it an
// unrun block would just silently have no output area.
function pendingElement(seg) {
  const div = document.createElement("div");
  div.className = "block pending";
  div.textContent =
    "▶ not run — press Run to execute this " + (seg.lang || "code") + " block";
  return div;
}

// Interleaved view: the document with each executable code block's result
// slotted in right after it. The document is the live editor content, so
// prose tracks typing; results are paired to fences by content.
function renderInterleaved() {
  const box = $("output");
  box.textContent = "";
  box.classList.add("interleaved");
  const doc = displayDoc();
  lastRenderedDoc = doc;
  if (!doc) return;
  const segs = splitMarkdown(doc);
  const results = splitResults(lastResultText);
  const pairs = pairResults(segs, results);
  // Inline each artifact under its own run's result ONLY in the clean
  // one-artifact-per-run case for the exact document that ran (the fan-out
  // norm, where artifact frames + run envelopes both arrive in document
  // order). Any other shape — no results, missing/extra artifacts, multi-file
  // runs, or a document edited since the run — falls back to the rolled-up
  // artifact block at the end.
  const inlineAll =
    docMatchesRun(segs) &&
    runArtifacts.length > 0 &&
    runArtifacts.length === results.length &&
    pairs.size === runnableFences(segs).length;
  let ai = 0;
  segs.forEach((seg, i) => {
    if (seg.type === "prose") {
      box.appendChild(renderProseSeg(seg));
      return;
    }
    box.appendChild(renderCodeSeg(seg));
    if (seg.isSeed) return;
    if (pairs.has(i)) {
      box.appendChild(resultElement(pairs.get(i)));
      if (inlineAll) {
        box.appendChild(
          renderArtifactInline(runArtifacts[ai++], artifactLang(lastMarkdown))
        );
      }
    } else {
      box.appendChild(pendingElement(seg));
    }
  });
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
  lastRenderedDoc = null;
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
  const segs = splitMarkdown(displayDoc());
  const pairs = pairResults(segs, splitResults(lastResultText));
  const parts = [];
  segs.forEach((seg, i) => {
    if (seg.type === "prose") {
      parts.push(seg.text);
      return;
    }
    parts.push(seg.fence + seg.header + "\n" + seg.code + "\n" + seg.fence);
    if (pairs.has(i)) parts.push(pairs.get(i));
  });
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

// Transport failures are transient: a node that is restarting (or whose relay
// handshake is still warming up) makes the wasm client reject with a bare
// string — usually "timed out" — instead of a terminal job frame. Replaying the
// block once the node answers again is far better than surfacing a failed run
// for a job that never started. Guarded by the frame counter below so a job
// that DID start can never be executed twice.
const TRANSIENT_SUBMIT_ERROR = /timed out|timeout|closed|reset|refused|unreachable|network|connection/i;

async function submitWithRetry(sub, attempts = 3) {
  const deadline = performance.now() + 90000; // never stretch a run past ~90s of dialing
  for (let attempt = 1; ; attempt++) {
    const framesBefore = sub.state.frames || 0;
    try {
      return await sub.submit();
    } catch (e) {
      // wasm-bindgen rejects with a plain string, not an Error.
      const msg = String((e && e.message) || e || "connection failed");
      const replayable =
        (sub.state.frames || 0) === framesBefore && TRANSIENT_SUBMIT_ERROR.test(msg);
      if (!replayable || attempt >= attempts || performance.now() > deadline) throw e;
      if (attempt === 1) {
        // The node is briefly unreachable, not broken: say so instead of
        // letting a silent delay look like a hang.
        appendOut(
          `[block ${sub.idx}] ${msg} — node not answering, retrying…`,
          "err"
        );
      }
      await new Promise((r) => setTimeout(r, 500 * attempt));
    }
  }
}

// Languages used in a Markdown doc (code fence info strings).
function languagesIn(markdown) {
  const langs = new Set();
  const re = /```([A-Za-z0-9_+.-]+)/g;
  let m;
  while ((m = re.exec(markdown)) !== null) langs.add(m[1].toLowerCase());
  return [...langs];
}

// Canonical language for a fence lang: aliases (svg, py, js, md, ...) map to
// their canonical name (html, python, node, pandoc, ...), which is what image
// refs and IMAGE_LANGUAGES use. Unknown langs pass through unchanged.
function canonicalLang(lang) {
  const l = lang.toLowerCase();
  for (const [canon, als] of Object.entries(ALIAS_MAP)) {
    if (canon.toLowerCase() === l || als.some((a) => a.toLowerCase() === l)) return canon;
  }
  return l;
}

function imageMatches(image, lang) {
  const l = canonicalLang(lang);
  if (image.toLowerCase().includes("/" + l + ":") || image.toLowerCase().includes(l + "-")) return true;
  // Fall back to the explicit image->language map: covers images whose name
  // doesn't echo the language (e.g. docker.io/chromedp/headless-shell -> html),
  // so warm-image routing actually prefers the node that has them. The map
  // keys both bare refs and their :latest forms — check both.
  const base = image.replace(/:latest$/, "");
  const langs = IMAGE_LANGUAGES[image] || IMAGE_LANGUAGES[base] || [];
  return langs.some((x) => x.toLowerCase() === l);
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
          chip.className = "lang-chip valid";
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
      // A folded seedfile is a view, so the shared payload is the full
      // document — the link has to run exactly what the editor means.
      const val = editor ? liveMarkdown() : "";
      const b64 = btoa(val)
        .replace(/\+/g, "-")
        .replace(/\//g, "_")
        .replace(/=+$/, "");
      const qs = b64 ? `?code=${b64}` : "";
      // Preserve hash (seed/relay) while updating the query string.
      const url = qs + location.hash;
      history.replaceState(null, "", url || location.pathname);
      // Keep an on-screen "Try it here" footer current as the payload
      // changes, so the link never points at stale code.
      syncTryItHere();
    } catch (_) { /* quota or encoding error — skip */ }
  }, 500);
}

// Build the shareable URL for the CURRENT editor content synchronously
// (origin + path + ?code=<base64url> + any hash), so callers never race
// the debounced URL-bar sync above — the Share button copies this and the
// printed "Try it here" footer links to it.
function currentShareUrl() {
  const val = editor ? liveMarkdown() : "";
  let url = location.origin + location.pathname;
  if (val) {
    try {
      const b64 = btoa(val)
        .replace(/\+/g, "-")
        .replace(/\//g, "_")
        .replace(/=+$/, "");
      url += `?code=${b64}`;
    } catch (_) { /* quota or encoding error — share the bare page */ }
  }
  return url + location.hash;
}

// --- fan-out helpers -----------------------------------------------------
// Best node for a single block's language: warm-image match first, then the
// global best node from pickTarget() (probe-verified first, lowest RTT).
// Reuses the same ranking the single-job path uses, but scoped to one block.
function pickBlockTarget(lang) {
  // Reuse pickTarget()'s ranking (verified-first, then RTT) so fan-out and
  // single-job routing stay consistent.
  const ranked = [...peers.values()].sort(
    (a, b) => ((a.probe === "ok" ? 0 : 1) - (b.probe === "ok" ? 0 : 1)) ||
              ((a.rtt_ms ?? 1e9) - (b.rtt_ms ?? 1e9))
  );
  if (!ranked.length) return null;
  const warm = ranked.find((p) => (p.images || []).some((img) => imageMatches(img, lang)));
  return warm || ranked[0];
}

// A per-block mini-markdown document: the fence(s) leading into this block
// (seed + file blocks attach to the first executable block that follows them),
// plus this block's own fence. Mirrors how the local executor groups seeds with
// the run that follows them.
function buildBlockDocument(allFences, blockIdx) {
  const parts = [];
  // Seed/file blocks attach to the first executable block that follows them,
  // so walk BACKWARD from just before this block (blockIdx itself is the
  // executable block and must not terminate the scan).
  let i = blockIdx - 1;
  while (i >= 0) {
    const f = allFences[i];
    if (f.lang && !SUPPORTED_LANGUAGES.has(f.lang) && f.lang.includes(".")) {
      // seed/file block: include it and keep walking backward.
      // NOTE: the fence must stay 3 backticks — the node's parser strips
      // any line starting with 4+ backticks as a markdown wrapper.
      parts.unshift(`${f.fence}${f.header}\n${f.body}\n${f.fence}`);
      i--;
      continue;
    }
    break;
  }
  const b = allFences[blockIdx];
  parts.push(`${b.fence}${b.header}\n${b.body}\n${b.fence}`);
  return parts.join("\n") + "\n";
}

// A request with `chain` on any block runs strictly in document order, one
// sub-job at a time, carrying each run's /output files into the next run as
// `name b64` seed fences — the same contract the local executor uses
// (chained_files + run). `nopipe` still overrides `chain` within one header,
// exactly as the parser resolves it.
function chainRequested(fences) {
  return fences.some((f) => {
    if (!SUPPORTED_LANGUAGES.has(f.lang) || f.lang.includes(".")) return false;
    const params = f.params.map((p) => p.toLowerCase());
    if (params.some((p) => NO_CHAIN_FLAGS.has(p))) return false;
    return params.some((p) => CHAIN_FLAGS.has(p));
  });
}

// A finished run's artifacts, rendered as seed fences for the run after it.
// The node stages every `name b64` block into the workdir before the code
// runs, so `import generated` (or `require`, or `sh generated.sh`) finds it.
function chainedSeeds(artifacts) {
  return (artifacts || [])
    .filter((a) => a && a.name && a.b64)
    .map((a) => "```" + a.name + " b64\n" + a.b64 + "\n```")
    .join("\n");
}

// One sub-job for a single fence: the block's mini-document (seeds above it
// included) submitted to its target node. Shared by a full run's fan-out, a
// lone pill re-run and a chained replay so every path submits identically.
function blockSubmission(fences, idx, state, target) {
  const doc = buildBlockDocument(fences, idx);
  return {
    state,
    idx,
    node: target,
    doc,
    // Chained runs submit the block's document plus whatever the previous
    // run produced, so `payload` is authoritative when one is passed.
    submit: (payload = doc) => target.node_id && target.relay
      ? client.submit_job_to_node(target.node_id, target.relay, b64encode(payload), 300, makeBlockEventHandler(idx))
      : client.submit_job(target.ticket, b64encode(payload), 300, makeBlockEventHandler(idx)),
  };
}

// Clear the per-run results of one block before it (re)runs.
function resetBlockRun(state) {
  state.warn = false;
  state.logs = [];
  state.stdout = "";
  state.stderr = "";
  state.exitCode = null;
  state.errors = [];
  state.artifacts = [];
  state.frames = 0;
}

// Submit chained sub-jobs one at a time, in document order: each run starts
// only after the previous one has finished, and inherits its /output files.
// A run that failed (or never started) carries nothing forward, matching the
// core, where a failed container produces no chained files.
async function submitChained(submissions) {
  const results = [];
  let carrying = "";
  for (const sub of submissions) {
    sub.state.status = "running";
    applyBlockStatus(sub.state);
    const doc = carrying ? carrying + "\n" + sub.doc : sub.doc;
    try {
      results.push({
        status: "fulfilled",
        value: await submitWithRetry({ ...sub, submit: () => sub.submit(doc) }),
      });
    } catch (e) {
      results.push({ status: "rejected", reason: e });
    }
    carrying = sub.state.exitCode === 0 ? chainedSeeds(sub.state.artifacts) : "";
  }
  return results;
}

// Build a per-block event handler that routes incoming frames into the block's
// state and updates its status chip in place. Routing is by construction: each
// handler is attached to its own sub-job's stream, so frames can never land on
// the wrong block even when they arrive out of order.
function makeBlockEventHandler(blockIdx) {
  return (jsonStr) => {
    const evt = JSON.parse(jsonStr);
    const state = blockStates.get(blockIdx);
    if (!state) return;
    state.frames = (state.frames || 0) + 1;

    if (evt.type === "job_log") {
      const data = new TextDecoder().decode(base64_decode(evt.data));
      state.logs.push({ channel: evt.channel, data });
      applyBlockStatus(state);
      // Also stream the log into the shared output so the existing Output panel
      // still sees live logs as they arrive (prefixed with the block index so a
      // multi-node run is readable when blocks stream back concurrently).
      appendOut(`[block ${blockIdx}] ${data}`, "log-" + evt.channel);
    } else if (evt.type === "artifact") {
      const ext = evt.ext || "";
      const a = {
        name: String(evt.name || "artifact" + ext),
        ext,
        size: evt.size || 0,
        b64: evt.data,
        mime: IMAGE_MIMES[ext] || "application/octet-stream",
      };
      state.artifacts.push(a);
      applyBlockStatus(state);
    } else if (evt.type === "job_done") {
      // Routing is by construction: each sub-job's handler is attached to its
      // own stream, so never filter on evt.job_id here — the wasm client's
      // job_request carries a client-generated id, so the echoed job_id never
      // matches a client-side run id and every job_done would be dropped.
      state.stdout = evt.stdout || "";
      state.stderr = evt.stderr || "";
      state.exitCode = evt.exit_code ?? 0;
      state.status = state.exitCode === 0 ? "done" : "failed";
      // A clean exit that still wrote stderr counts as a warning — same
      // semantics as the output panel's yellow warnings toggle.
      state.warn = state.exitCode === 0 && String(state.stderr || "").trim().length > 0;
      applyBlockStatus(state);
    } else if (evt.type === "error") {
      state.errors.push(String(evt.message || "rejected"));
      state.status = "failed";
      state.exitCode = state.exitCode ?? 1;
      applyBlockStatus(state);
    }
  };
}

// ---------- per-block run status inside the language pills ------------------
// Single-flight run state shared with the per-block event handlers and the
// pill status updates (the Run button is disabled while a run is in flight,
// so one module-level snapshot is enough).
let blockStates = new Map();

// Build the fresh per-run state snapshot for every fence in document order
// (seed/file blocks are inert "skip" chips; only executable blocks run).
function initBlockStates(fences) {
  blockStates = new Map();
  for (let i = 0; i < fences.length; i++) {
    const f = fences[i];
    const exec = SUPPORTED_LANGUAGES.has(f.lang) && !f.lang.includes(".");
    blockStates.set(i, {
      idx: i,
      lang: f.lang,
      header: f.header,
      // Header + body: a re-run carries a prior result forward only when the
      // fence is unchanged (see rerunBlock).
      key: f.header + "\u0000" + f.body,
      exec,
      // Chained runs queue every block behind the one in flight; the pill
      // shows "..." while a block waits for the one above it.
      chained: false,
      warn: false,
      status: exec ? "queued" : "skip",
      nodeId: null,
      // Frames seen for this block; a retry is only safe while this is 0.
      frames: 0,
      logs: [],
      stdout: "",
      stderr: "",
      exitCode: null,
      errors: [],
      artifacts: [],
    });
  }
}

// Map a block's run state to the pill's status class + icon: grey spinner
// while in flight, green check on success, yellow triangle when the block
// exited 0 but wrote warnings (stderr), red x on failure or no match.
function blockStatusVisual(state) {
  if (state.status === "queued") {
    // A queued chain block is not working yet: it is waiting for the block
    // above it, so it gets a "..." instead of the running spinner.
    return { cls: "waiting", icon: "..." };
  }
  if (state.status === "running") {
    return { cls: "running", icon: "" }; // spinner drawn via CSS
  }
  if (state.status === "done") {
    return state.warn
      ? { cls: "warn", icon: "⚠" }
      : { cls: "done", icon: "✓" };
  }
  if (state.status === "failed" || state.status === "no-match") {
    return { cls: "err", icon: "✕" };
  }
  return { cls: "", icon: "" }; // skip (seed/file)
}

// Tooltip text for a block's live state.
function statusLabel(state) {
  if (state.status === "queued") {
    return state.chained ? "waiting for the previous block to finish" : "queued";
  }
  if (state.status === "running") {
    const node =
      state.nodeId == null ? "—" :
      state.nodeId === "(manual)" ? "manual" :
      shortId(state.nodeId);
    return `running on ${node}`;
  }
  if (state.status === "done") {
    return state.warn
      ? `finished with warnings (exit ${state.exitCode ?? 0})`
      : `ok (exit ${state.exitCode ?? 0})`;
  }
  if (state.status === "failed") {
    return state.errors.length ? state.errors[0] : `failed (exit ${state.exitCode ?? 1})`;
  }
  if (state.status === "no-match") return state.errors[0] || "no match";
  if (state.status === "idle") return "not run — click this pill to run the block";
  if (state.status === "skip") return "seed/file";
  return state.status;
}

// A bounded output preview for the pill tooltip: stdout on a clean or warning
// run, stderr on a failed one (falling back to the rejection reason), and the
// live log tail while still in flight. Capped so the hover text stays a big
// tooltip without becoming a wall of output.
function blockOutputPreview(state) {
  const cap = 280;
  let text = "";
  if (state.status === "failed") {
    text = String(state.stderr || "").trim() || (state.errors[0] || "");
  } else if (state.status === "done") {
    text = String(state.stdout || "").trim();
  } else if (state.status === "running" || state.status === "queued") {
    text = state.logs.map((l) => l.data).join("").trim();
  }
  if (!text) return "";
  const clipped = text.length > cap ? text.slice(0, cap).trimEnd() + "…" : text;
  return `\n\n${clipped}`;
}

// Reflect a block's state on its language pill (matched by fence index, which
// aligns fenceInfo() with the updateLangStatus() pill order).
function applyBlockStatus(state) {
  const chip = document.querySelector(`#langStatus .lang-chip.lang-name[data-idx="${state.idx}"]`);
  if (!chip) return;
  const { cls, icon } = blockStatusVisual(state);
  chip.classList.remove("waiting", "running", "done", "warn", "err");
  if (cls) chip.classList.add(cls);
  const ind = chip.querySelector(".chip-run");
  if (ind) ind.textContent = icon;
  chip.title = `${chip.dataset.baseTitle || "block"} — ${statusLabel(state)}${blockOutputPreview(state)}`;
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
    // setValue is programmatic — OverType's onChange never fires, so refresh
    // the language pills and the live preview the same way the import,
    // snippet and undo paths do.
    updateLangStatus();
    highlightCodeHeaders();
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

// Fold the per-block states into the run-level result contract the output
// panel and the interleaved view consume: stdout in document order, stderr,
// the first non-zero exit code, and every artifact. Shared by a full run and
// a single-block re-run so both render from exactly one source of truth.
function mergeBlockResults() {
  const stdoutParts = [];
  const stderrParts = [];
  let exitCode = 0;
  const mergedArtifacts = [];
  for (const [, state] of blockStates) {
    if (!state.exec) continue;
    if (state.stdout) stdoutParts.push(state.stdout);
    if (state.stderr) stderrParts.push(state.stderr);
    if (state.exitCode !== null && state.exitCode !== 0 && exitCode === 0) {
      exitCode = state.exitCode;
    }
    for (const a of state.artifacts) mergedArtifacts.push(a);
    for (const e of state.errors) {
      stderrParts.push(e);
      if (exitCode === 0) exitCode = 1;
    }
  }
  lastResultText = stdoutParts.join("\n");
  lastStderr = stderrParts.join(
    stdoutParts.length && stderrParts.length ? "\n" : ""
  );
  lastExitCode = exitCode || 0;
  runArtifacts = mergedArtifacts;
  // "idle" blocks were never part of this run (single-block re-runs), so they
  // cannot count as passing or failing.
  const ran = [...blockStates.values()].filter(
    (s) => s.exec && s.status !== "idle"
  );
  const failed = ran.filter(
    (s) => s.status === "failed" || (s.exitCode ?? 0) !== 0
  ).length;
  return { ran, failed };
}

async function run() {
  if ($("run").disabled) return; // already running — swallow re-entry
  // Submit the expanded document: a folded seedfile is only a view, and the
  // node must receive every payload in full.
  const markdown = liveMarkdown();
  if (!markdown.trim()) {
    setStatus("paste some Markdown with code blocks", "err");
    return;
  }
  const lost = lostFolds(markdown);
  if (lost.length) {
    appendOut(
      `Folded seedfile content is missing (${lost.join(", ")}) — add the file again before running.`,
      "err"
    );
    setStatus("a folded seedfile lost its contents — re-add the file", "err");
    return;
  }

  const manual = $("ticket").value.trim();
  if (!manual && !pickTarget(markdown)) {
    setStatus("no compute nodes discovered — paste a seed ticket", "err");
    return;
  }
  localStorage.setItem("ephemeral.ticket", manual);
  localStorage.setItem("ephemeral.relay", $("relay").value.trim());

  // --- fan-out: one sub-job per executable codeblock ------------------------
  // Each executable block becomes its own mini-markdown document submitted to
  // the best node whose warm images cover that block's language. Blocks that
  // share a language can land on the same node; the per-block status row still
  // tracks each block independently. Blocks declaring `chain` are the
  // exception: the request then runs in-order and each run hands its /output
  // to the next one (see chainRequested/submitChained).
  const fences = fenceInfo(markdown);
  // `chain` anywhere opts the whole request out of fan-out: blocks run in
  // document order, one after the other, so the second block can use what the
  // first one wrote (this is what makes generated-code chaining work).
  const chained = chainRequested(fences);
  // Executable blocks with their fence index — blockStates is keyed by fence
  // index, so seed/file blocks that precede a run must not shift positions.
  const codeBlocks = [];
  for (let i = 0; i < fences.length; i++) {
    const f = fences[i];
    if (SUPPORTED_LANGUAGES.has(f.lang) && !f.lang.includes(".")) {
      codeBlocks.push({ idx: i, f });
    }
  }
  // Fresh per-run state (single-flight, so reusing one module-level Map is
  // safe); the language pills carry the per-block status for this run.
  initBlockStates(fences);

  // Clear prior output + status state, then render the live status row.
  runArtifacts = [];
  leftoverArtifacts = [];
  outputRaw = "";
  lastMarkdown = markdown;
  lastDisplayMarkdown = editor.getValue();
  lastResultText = "";
  lastStderr = "";
  lastExitCode = 0;
  warningsOn = false;
  lastStderrEl = null;
  $("warnings").hidden = true;
  $("output").textContent = "";
  $("output").classList.remove("interleaved");
  lastRenderedDoc = null;
  setBusy(true);
  // Streamed logs own the output box until the run's own final render, so the
  // live document preview stands down for the duration.
  runInFlight = true;
  // Rebuild the language pills from the current editor content so no
  // run-status overlay from a previous run survives.
  updateLangStatus();

  // Assign each executable block to a target node.
  const submissions = [];
  for (const { idx, f } of codeBlocks) {
    const state = blockStates.get(idx);
    // Warm-image peer first, then the best global node, then a pasted ticket
    // (so a manual ticket still works when discovery finds no peers).
    let target = pickBlockTarget(f.lang);
    if (!target && manual) target = { node_id: null, relay: null, ticket: manual };
    if (!target) {
      // No reachable node covers this language — mark and skip submit.
      state.status = "no-match";
      state.errors.push(`no node with a warm ${f.lang} image`);
      applyBlockStatus(state);
      continue;
    }
    state.status = "queued";
    state.chained = chained;
    state.nodeId = target.node_id || (manual ? "(manual)" : null);
    submissions.push(blockSubmission(fences, idx, state, target));
  }

  if (!chained) {
    // Unchained: they all start now, so queue -> running up front.
    for (const sub of submissions) {
      sub.state.status = "running";
      applyBlockStatus(sub.state);
    }
  } else {
    // Chained: only the first block is live; the rest show "..." until
    // submitChained flips each one to running in turn.
    for (const sub of submissions) applyBlockStatus(sub.state);
  }

  // Transport-level failures (relay drop, node unreachable) surface as promise
  // rejections with no terminal frame, so map each result back to its block and
  // mark it failed explicitly — Promise.allSettled swallows rejections, so the
  // per-block status must too. Chained requests go through the sequential
  // submitter, which returns the same {status, value|reason} shape.
  const results = chained
    ? await submitChained(submissions)
    : await Promise.allSettled(submissions.map((s) => submitWithRetry(s)));
  setBusy(false);
  refreshPeers(); // re-sync the peer table after each run (non-blocking)
  for (let i = 0; i < submissions.length; i++) {
    const r = results[i];
    if (r.status === "rejected") {
      const sub = submissions[i];
      const msg = String(r.reason || "connection failed");
      const node = sub.node && sub.node.node_id ? ` on node ${sub.node.node_id.slice(0, 8)}…` : "";
      sub.state.status = "failed";
      sub.state.exitCode = sub.state.exitCode ?? 1;
      sub.state.errors.push(msg);
      applyBlockStatus(sub.state);
      appendOut(`[block ${sub.idx}] ${msg}${node}`, "err");
    }
  }

  // --- merge per-block results into the existing output contract -------------
  // Preserve document order for the merged result text (best-effort positional
  // matching the existing interleaved view already relies on).
  const { ran, failed } = mergeBlockResults();

  // Rebuild the merged result + stderr the same way the single-job path did.
  if (lastResultText) appendResult(lastResultText);
  if (lastStderr) appendStderr(lastStderr, lastExitCode);
  else $("warnings").hidden = true;
  if (runArtifacts.length) {
    renderArtifacts(runArtifacts, markdown);
  }
  // The declaration-time "writes to /output" hints now become real, one-click
  // downloads for whatever this run actually returned.
  syncArtifactChips();
  setDetail(
    failed === 0
      ? `done (exit 0)`
      : `finished with ${ran.length} block(s), ${failed} failing`
  );

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
  // `notimeout` lifts the core's 300s cap, but the node strips it before the
  // document runs: how long a remote payload may hold the host is the node's
  // decision. Say so instead of letting the flag look honored.
  const triedNoTimeout = fenceInfo(markdown).some((f) =>
    f.params.some((p) => NO_TIMEOUT_FLAGS.has(p.toLowerCase()))
  );
  if (triedNoTimeout) {
    const note = document.createElement("div");
    note.className = "unsafe-note";
    note.textContent =
      "Note: the `notimeout` flag is ignored on the distributed network — " +
      "remote jobs are always capped at the node's timeout (300s).";
    const box = $("output");
    box.appendChild(note);
    box.scrollTop = box.scrollHeight;
  }
  // The merge is done — typing may drive the live preview again from here on
  // (the final render below re-reads the live document, so keystrokes entered
  // during the run are already reflected).
  runInFlight = false;
  lastOutputRaw = outputRaw;
  // View follows the selected mode: auto keys off the document shape
  // (prose outside the fences → interleaved, bare code → results-only),
  // while "on"/"off" pin the view regardless of content.
  setInterleaved(effectiveInterleave());
  // The pip now reflects the decision this run produced (blue if
  // interleaved, hollow if results-only; grey stays for pre-execute).
  syncInterleaveButton();
  if (interleaved) renderInterleaved();
  // Mirror the "Try it here" footer into the box (checkbox-gated), so
  // the payload link is visible on screen and rides along in print.
  syncTryItHere();
}

// ---------------------------------------------------------------------------
// Single-block re-run: a supported language pill is a button that executes
// just that fence (with the seed/file fences above it — the same mini-document
// a full run submits by fan-out), leaving every other block's result alone.
// Output, artifacts and pill states are then re-derived from the per-block
// results, so the panel and the interleaved view stay consistent.
async function rerunBlock(idx) {
  if ($("run").disabled) return; // a run is already in flight (single flight)
  if (!editor) return;
  const markdown = liveMarkdown();
  const fences = fenceInfo(markdown);
  const f = fences[idx];
  // Only an executable fence has a re-run affordance; seed/file blocks and
  // unknown languages stay inert pills.
  if (!f || !SUPPORTED_LANGUAGES.has(f.lang) || f.lang.includes(".")) return;

  const manual = $("ticket").value.trim();
  // `chain` anywhere makes every block depend on the ones above it, so a
  // dependent pill can never run on its own (it would fail for want of the
  // files chained into it). Clicking one replays the chain from its root (the
  // first executable block) through the clicked block.
  const chained = chainRequested(fences);
  const runIdxs = [];
  if (chained) {
    for (let i = 0; i <= idx; i++) {
      const b = fences[i];
      if (SUPPORTED_LANGUAGES.has(b.lang) && !b.lang.includes(".")) runIdxs.push(i);
    }
  } else {
    runIdxs.push(idx);
  }

  // Route each block the same way a full run does: warm-image peer, then the
  // best global node, then a pasted ticket. A chain stops at the first block
  // that cannot be placed, since everything after it depends on that run.
  const targets = new Map();
  for (const i of runIdxs) {
    const lang = fences[i].lang;
    const target = pickBlockTarget(lang) ||
      (manual ? { node_id: null, relay: null, ticket: manual } : null);
    if (!target) {
      setStatus(`no compute nodes discovered for ${lang} - paste a seed ticket`, "err");
      return;
    }
    targets.set(i, target);
  }
  localStorage.setItem("ephemeral.ticket", manual);
  localStorage.setItem("ephemeral.relay", $("relay").value.trim());

  // Re-key the per-block states onto the LIVE document. A result is carried
  // over only when that fence's header+body is unchanged, so a block edited
  // since the last run loses its stale result (and renders as "not run")
  // instead of showing output for code that is no longer there.
  const previous = blockStates;
  initBlockStates(fences);
  for (const [i, state] of blockStates) {
    const old = previous.get(i);
    if (!old || !state.exec || !old.exec || old.key !== state.key) continue;
    state.status = old.status;
    state.nodeId = old.nodeId;
    state.warn = old.warn;
    state.logs = old.logs;
    state.stdout = old.stdout;
    state.stderr = old.stderr;
    state.exitCode = old.exitCode;
    state.errors = old.errors;
    state.artifacts = old.artifacts;
  }
  // Blocks that never ran stay idle: this re-run is scoped to the chain.
  for (const state of blockStates.values()) {
    if (state.exec && state.status === "queued") state.status = "idle";
  }
  // Repaint the carried-over results: typing rebuilt the pill row (the editor's
  // onChange runs updateLangStatus), which drops the status overlays.
  for (const state of blockStates.values()) {
    if (state.exec && state.status !== "idle") applyBlockStatus(state);
  }

  // Queue exactly what this click runs: the clicked block alone, or a chained
  // replay from the root. Only the first is live; the rest wait their turn
  // and say so with a "..." pill (submitChained flips them in order).
  for (const i of runIdxs) {
    const state = blockStates.get(i);
    resetBlockRun(state);
    state.chained = chained;
    state.status = "queued";
    state.nodeId = targets.get(i).node_id || (manual ? "(manual)" : null);
    // A lone re-run flips straight to running in submitChained; the waiting
    // "..." is only meaningful for the blocks queued behind a chain.
    if (chained) applyBlockStatus(state);
  }
  if (chained) {
    // Replaying the chain replaced the inputs every later block ran on, so
    // their results are stale: drop them (back to "not run") rather than
    // leaving output that no longer matches the document.
    for (let i = idx + 1; i < fences.length; i++) {
      const state = blockStates.get(i);
      if (!state.exec) continue;
      resetBlockRun(state);
      state.status = "idle";
      applyBlockStatus(state);
    }
  }

  const submissions = runIdxs.map((i) =>
    blockSubmission(fences, i, blockStates.get(i), targets.get(i))
  );

  // Single flight: the Run button spins (and is disabled) exactly as during a
  // full run, so a second pill click can't start a concurrent job. The
  // sequential submitter is used even for one block so a lone re-run and a
  // chained replay share the same status transitions.
  runInFlight = true; // streamed logs own the box until the re-render below
  setBusy(true);
  try {
    const results = await submitChained(submissions);
    for (let i = 0; i < submissions.length; i++) {
      const r = results[i];
      if (r.status !== "rejected") continue;
      const sub = submissions[i];
      const reason = r.reason;
      const msg = String((reason && reason.message) || reason || "connection failed");
      const node = sub.node && sub.node.node_id ? ` on node ${shortId(sub.node.node_id)}` : "";
      sub.state.status = "failed";
      sub.state.exitCode = sub.state.exitCode ?? 1;
      sub.state.errors.push(msg + node);
      applyBlockStatus(sub.state);
    }
  } finally {
    setBusy(false);
  }
  refreshPeers(); // re-sync the peer table after the job (non-blocking)

  // These results belong to the document as it is now - pairing (interleaved
  // view) reads lastMarkdown, so keep it in step with the editor.
  lastMarkdown = markdown;
  const { failed } = mergeBlockResults();
  paintMergedOutput();
  syncArtifactChips();
  const what = chained && submissions.length > 1
    ? `${submissions.length}-block chain from the root`
    : `${f.lang} block`;
  setDetail(
    failed === 0
      ? `${what} re-ran - exit 0`
      : `${what} re-ran - exit ${lastExitCode}`
  );
}

// Rebuild the output panel from the merged per-block results. A re-run keeps
// the previous panel on screen while its job streams, so this clears and
// re-renders instead of appending (no duplicated log lines, and the re-run
// block's fresh result replaces its old one).
function paintMergedOutput() {
  runInFlight = false;
  lastStderrEl = null;
  lastOutputRaw = lastResultText
    ? (lastResultText.endsWith("\n") ? lastResultText : lastResultText + "\n") + "\n"
    : "";
  outputRaw = lastOutputRaw;
  if (lastStderr && lastExitCode === 0) showWarningsButton();
  else $("warnings").hidden = true;
  setInterleaved(effectiveInterleave());
  syncInterleaveButton();
  if (interleaved) renderInterleaved();
  else renderNormal();
  syncTryItHere();
}

// Reflect the artifacts a run actually returned as clickable pills: each
// block's pre-run "writes to /output" hint becomes one pill per file, and a
// click downloads exactly that file (the rolled-up artifact block still zips
// the whole run together).
function syncArtifactChips() {
  for (const el of document.querySelectorAll("#langStatus .lang-chip.artifact-ready")) {
    el.remove();
  }
  for (const [idx, state] of blockStates) {
    if (!state.exec || !state.artifacts.length) continue;
    const chip = document.querySelector(`#langStatus .lang-chip.lang-name[data-idx="${idx}"]`);
    if (!chip) continue;
    // The declaration-time placeholders are redundant once the real names are
    // known (and they must go before the per-file pills are inserted).
    for (const ph of document.querySelectorAll(
      `#langStatus .lang-chip.artifact:not(.artifact-ready)[data-idx="${idx}"]`
    )) ph.remove();
    const lang = String(state.lang).replace(/[^a-zA-Z0-9]/g, "_");
    let anchor = chip;
    for (const a of state.artifacts) {
      const achip = document.createElement("span");
      achip.className = "lang-chip artifact artifact-ready";
      achip.textContent = "📦 " + a.name;
      achip.title = `download ${a.name} — produced by this ${state.lang} block`;
      achip.setAttribute("role", "button");
      achip.setAttribute("tabindex", "0");
      const download = () => downloadArtifact(a, lang);
      achip.addEventListener("click", download);
      achip.addEventListener("keydown", (e) => {
        if (e.key === "Enter" || e.key === " ") {
          e.preventDefault();
          download();
        }
      });
      linkPillToBlock(achip, idx);
      anchor.after(achip);
      anchor = achip;
    }
  }
}

// One-click download of a single artifact pill.
function downloadArtifact(a, lang) {
  if (!a || !a.b64) return;
  triggerDownload(
    b64ToBlob(a.b64, a.mime || "application/octet-stream"),
    artifactFileName(lang || "custom", a.name)
  );
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

// "Share link output" mode: what the output ends with. none = nothing,
// link = the "Try it here" hyperlink, qr = that link plus a QR code so
// a printed page can be scanned straight back to the payload.
function shareLinkMode() {
  const sel = document.querySelector('input[name="shareLinkMode"]:checked');
  return sel ? sel.value : "none";
}

// QR PNG data URL for a payload share URL (qrcode-generator, CDN).
// Returns null when the library hasn't loaded (offline CDN) — callers
// degrade to link-only rather than failing the footer.
function qrDataUrl(text) {
  try {
    if (typeof qrcode !== "function") return null;
    const qr = qrcode(0, "M"); // type 0 = auto size, M = medium error correction
    qr.addData(text);
    qr.make();
    return qr.createDataURL(4, 8); // 4px modules, 8-module quiet zone
  } catch {
    return null;
  }
}

// The on-screen mirror of the print "Try it here" footer: a single,
// always-current element pinned at the end of the Output box while the
// "Share link output" mode is link or qr and the box has content.
// Both view renderers wipe the box, so this is re-synced after every
// rebuild (run end, interleave toggle, clear, radio change) and on
// URL sync — the link then always matches the current payload, on screen
// and in the print snapshot alike.
function syncTryItHere() {
  const box = $("output");
  const mode = shareLinkMode();
  let el = box.querySelector(".try-it-here");
  if (mode === "none" || !box.textContent.trim()) {
    if (el) el.remove();
    return;
  }
  const url = currentShareUrl();
  if (!el) {
    el = document.createElement("div");
    el.className = "try-it-here";
    const a = document.createElement("a");
    a.target = "_blank";
    a.rel = "noopener";
    // No URL text is rendered anywhere — the address rides in the href
    // (clickable in printed PDFs) and the hover title for on-screen
    // discoverability.
    a.title = url;
    el.appendChild(a);
    box.appendChild(el);
  }
  const a = el.querySelector("a");
  a.href = url;
  a.title = url;
  a.textContent = "Try it here";
  // QR mode: keep a single <img> in sync (replace on URL change). If the
  // QR library is unavailable the footer degrades to link-only.
  let img = el.querySelector("img.try-qr");
  if (mode === "qr") {
    const data = qrDataUrl(url);
    if (data) {
      if (!img) {
        // The QR itself is a link to the same share URL, so even the
        // printed-paper image can be clicked in a PDF viewer (and the
        // phone camera treats it as plain QR). One <a> wraps the img.
        const wrap = document.createElement("a");
        wrap.className = "try-qr-link";
        wrap.target = "_blank";
        wrap.rel = "noopener";
        wrap.title = url;
        img = document.createElement("img");
        img.className = "try-qr";
        img.alt = "QR code linking to this output's share URL";
        wrap.appendChild(img);
        el.appendChild(wrap);
      }
      const wrapA = img.closest("a.try-qr-link");
      if (wrapA) {
        wrapA.href = url;
        wrapA.title = url;
      }
      if (img.src !== data) img.src = data;
    } else if (img) {
      img.remove();
    }
  } else if (img) {
    // Link-only mode: drop the whole QR wrapper.
    const wrap = img.closest("a.try-qr-link");
    (wrap || img).remove();
  }
  box.scrollTop = box.scrollHeight;
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
.result-title.failed { color: #b00020; }
.result-step { color: #555; font-size: 10.5pt; margin: 8px 0 4px; }
.block.pending {
  color: #666;
  font-size: 10.5pt;
  font-style: italic;
  margin: 0 0 10px 18px;
  padding-left: 10px;
  border-left: 2px solid #d8dee4;
}
.result-line { color: #111; white-space: pre-wrap; word-break: break-word; margin: 2px 0; }
.interleave-h1, .interleave-h2, .interleave-h3 { font-weight: 700; margin: 14px 0 4px; }
.interleave-h1 { font-size: 15pt; }
.interleave-h2 { font-size: 13pt; }
.interleave-h3 { font-size: 11pt; color: #555; }
.block.prose > :first-child { margin-top: 0; }
.block.prose p { margin: 2px 0; }
.block.prose blockquote {
  border-left: 3px solid #c9189e;
  background: #faf3f8;
  border-radius: 0 4px 4px 0;
  margin: 8px 0;
  padding: 8px 12px;
}
.block.prose hr { border: 0; border-top: 1px dashed #999; margin: 12px 0; }
.block.prose ul, .block.prose ol { margin: 6px 0 10px; padding-left: 24px; }
.block.prose li { margin: 2px 0; }
.block.prose code { background: rgba(0, 0, 0, .07); border-radius: 3px; padding: 1px 5px; font-size: 10.5pt; }
.block.prose a { color: #0a66c2; }
.block.prose table { border-collapse: collapse; margin: 8px 0; }
.block.prose th, .block.prose td { border: 1px solid #d8dee4; padding: 4px 10px; text-align: left; }
.block.prose th { background: #f0f2f5; }
.code-block {
  background: #f6f8fa;
  border: 1px solid #d8dee4;
  border-radius: 4px;
  padding: 8px 10px;
  margin: 6px 0 10px;
  white-space: pre-wrap;
  word-break: break-word;
}
.code-block code { background: transparent; padding: 0; }
.block-copy, .artifact-tools { display: none; }
.artifact-img {
  max-width: 100%;
  max-height: 320px;
  border: 1px solid #ddd;
  border-radius: 4px;
  margin: 6px 0;
}
.artifact-count, .artifact-name { color: #666; font-size: 10.5pt; margin-top: 2px; }
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
/* Optional "Try it here" footer (.try-it-here — same class as the
   on-screen mirror, so a mirrored footer already in the output box prints
   identically). Just the link: the payload URL lives in the href, which
   stays clickable in a printed PDF, and in the hover title on screen.
   No bare URL text is rendered. */
.try-it-here {
  margin-top: 18px;
  padding-top: 10px;
  border-top: 1px solid #d8dee4;
}
.try-it-here a { color: #0a66c2; font-weight: 600; text-decoration: none; overflow-wrap: anywhere; }
/* QR code in the footer (Link + QR mode): crisp on paper at 4px
   modules; the quiet zone comes from the data URL itself. */
.try-it-here .try-qr {
  display: block;
  width: 92px;
  height: 92px;
  margin-top: 8px;
}
@media print { body { padding: 0; } }
`;
  // Optional footer: when a share-link mode is on and the on-screen
  // mirror isn't already in the box (it normally is — syncTryItHere()
  // pins it after every run), end the page with a "Try it here" link
  // to this payload's share URL (built from the CURRENT editor content,
  // like the Share button), plus the QR when the mode asks for it.
  const mode = shareLinkMode();
  let footerHtml = "";
  if (mode !== "none" && !box.querySelector(".try-it-here")) {
    const esc = (s) => s.replace(/&/g, "&amp;").replace(/"/g, "&quot;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
    const rawUrl = currentShareUrl();
    const url = esc(rawUrl);
    const data = mode === "qr" ? qrDataUrl(rawUrl) : null;
    footerHtml =
      `\n<div class="try-it-here">\n` +
      `<a href="${url}" target="_blank" rel="noopener" title="${url}">Try it here</a>\n` +
      (data ? `<a class="try-qr-link" href="${url}" target="_blank" rel="noopener" title="${url}"><img class="try-qr" src="${data}" alt="QR code linking to this output's share URL" /></a>\n` : "") +
      `</div>`;
  }
  win.document.write(
    `<!DOCTYPE html>\n<html lang="en">\n<head>\n<meta charset="utf-8" />\n` +
    `<title>Ephemeral — Output</title>\n` +
    `<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/atom-one-light.min.css" />\n` +
    `<style>${css}</style>\n</head>\n<body>\n<div class="print-output">\n` +
    box.innerHTML +
    footerHtml +
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
// write — Chrome/Edge; Firefox requires a permission). The button is an icon
// by then, so success/failure reads through the .ok flash + tooltip rather
// than swapped label text.
async function copyArtifactImage(a, btn) {
  const label = btn.title;
  try {
    await navigator.clipboard.write([new ClipboardItem({ [a.mime]: b64ToBlob(a.b64, a.mime) })]);
    btn.classList.add("ok");
    btn.title = "Copied image";
  } catch (e) {
    btn.title = "Copy failed";
  }
  setTimeout(() => {
    btn.classList.remove("ok");
    btn.title = label;
  }, 1200);
}

// Icons for the floating artifact actions — the same glyphs as the per-code
// -block copy button and the toolbar's export/download button, so an artifact
// preview carries the identical affordance instead of a row of text buttons.
const ICON_COPY =
  '<rect x="9" y="9" width="13" height="13" rx="2" ry="2"></rect>' +
  '<path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"></path>';
const ICON_DOWNLOAD =
  '<path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"></path>' +
  '<polyline points="7 10 12 15 17 10"></polyline>' +
  '<line x1="12" y1="15" x2="12" y2="3"></line>';

function artifactIconButton(svg, label) {
  const btn = document.createElement("button");
  btn.type = "button";
  btn.className = "artifact-tool";
  btn.title = label;
  btn.setAttribute("aria-label", label);
  btn.innerHTML =
    '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" ' +
    'stroke-linecap="round" stroke-linejoin="round">' + svg + "</svg>";
  return btn;
}

// Floating action icons pinned to the top-right of an artifact: Copy image
// (single image only — the clipboard write needs one image) and Download
// (the file itself, or a .zip when a run produced several).
function artifactTools(artifacts, lang) {
  const tools = document.createElement("div");
  tools.className = "artifact-tools";
  const single = artifacts.length === 1 ? artifacts[0] : null;
  if (single && IMAGE_MIMES[single.ext]) {
    const copy = artifactIconButton(ICON_COPY, "Copy image");
    copy.addEventListener("click", () => copyArtifactImage(single, copy));
    tools.appendChild(copy);
  }
  const dl = artifactIconButton(
    ICON_DOWNLOAD,
    single ? "Download " + single.name : `Download all ${artifacts.length} as .zip`
  );
  dl.addEventListener("click", () => downloadArtifacts(artifacts, lang));
  tools.appendChild(dl);
  return tools;
}

// Render a SINGLE artifact inline under its run's result (interleaved
// view): image preview (or a file row for non-images) with the floating
// Copy-image / Download icons over it, plus the filename as a caption. Used
// when positional 1:1 matching holds — the one-artifact-per-run fan-out case.
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
  div.appendChild(artifactTools([a], lang));
  const caption = document.createElement("div");
  caption.className = "artifact-name";
  caption.textContent = a.name;
  div.appendChild(caption);
  return div;
}

// Render a run's artifacts in the output: inline image previews with the
// floating Copy-image / Download icons over them (Copy only when the run
// produced a single image), and the artifact count as a caption.
function renderArtifacts(artifacts, markdown) {
  const lang = artifactLang(markdown);
  const box = $("output");
  const div = document.createElement("div");
  div.className = "block artifacts";
  for (const a of artifacts) {
    if (!IMAGE_MIMES[a.ext]) {
      // Non-image artifacts have no preview, so list them by name — the
      // rolled-up block is the only place a multi-file run names its files
      // (the download icon above ships them together as a .zip).
      const row = document.createElement("div");
      row.className = "artifact-file";
      row.textContent = "📄 " + a.name;
      div.appendChild(row);
      continue;
    }
    const img = document.createElement("img");
    img.className = "artifact-img";
    img.src = `data:${a.mime};base64,${a.b64}`;
    img.alt = a.name;
    img.title = a.name;
    div.appendChild(img);
  }
  div.appendChild(artifactTools(artifacts, lang));
  const label = document.createElement("div");
  label.className = "artifact-count";
  label.textContent = `${artifacts.length} artifact${artifacts.length === 1 ? "" : "s"}`;
  div.appendChild(label);
  box.appendChild(div);
  box.scrollTop = box.scrollHeight;
}

$("run").addEventListener("click", run);
// Share URL: force-sync the current editor content into the URL bar and
// copy the shareable URL — built synchronously so the copied value can't
// lag the debounced URL-bar sync.
$("shareBtn").addEventListener("click", () => {
  syncCodeToUrl();  // URL bar too (debounced)
  copyText(currentShareUrl(), $("shareBtn"), "Share URL");
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
    // Tooltip: what the language IS (one sentence), then what clicking does.
    // A bare name like `cjam` or `piet` tells a beginner nothing, so the
    // blurb rides on the same hover they already get.
    const blurb = LANG_BLURBS[l];
    span.title =
      (blurb ? blurb + "\n\n" : "") +
      "Append a " + l + " demo to the Run Code editor";
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
    // Anchor to the TOP edge of the run dock — the chip row the (i) pill
    // lives in, which is pinned to the bottom of the viewport while the
    // run section is in view. Anchoring to the editor box's bottom edge
    // instead (as this used to) only worked when that edge happened to be
    // on screen: the editor grows with its content, so at the top of a
    // long document its bottom sits far below the viewport and the
    // popover landed off-screen. Measuring from the dock keeps the
    // popover always opening upward from the pill, above the hovering
    // bottom controls rather than behind or below them.
    // Fall back to whatever bottom-anchored control row exists (the dock, its
    // action row, the section itself) so a markup change degrades the popover
    // to "roughly right" instead of throwing and leaving it unpositioned.
    const dockEl =
      document.querySelector("section.run .run-dock") ||
      document.querySelector("section.run .actions") ||
      document.querySelector("section.run");
    if (!ed || !dockEl) return;
    const dock = dockEl.getBoundingClientRect();
    pop.style.position = "fixed";
    pop.style.left = ed.left + "px";
    pop.style.bottom = window.innerHeight - dock.top + "px";
    pop.style.width = ed.width + "px";
    // Hug the content height (no dead space at the bottom), but never
    // taller than the code actually visible above the dock: the floor is
    // the sticky toolbar row (when it's floating) or the editor's top
    // edge, whichever is lower, so the popover can't cover the header row.
    pop.style.height = ""; // natural size first, so scrollHeight is real
    const headEl = document.querySelector("section.run .section-head");
    if (!headEl) return;
    const head = headEl.getBoundingClientRect();
    // The 0 floor keeps the popover inside the viewport when the dock has
    // been pushed up near the top (the run section ending) — there it
    // shrinks to the room above the dock instead of opening past the edge.
    // When the dock itself has scrolled off the top the pill is off screen
    // too, so a zero-height popover simply follows it out.
    const ceiling = Math.max(0, ed.top, Math.min(head.bottom, dock.top));
    const room = Math.max(0, dock.top - ceiling - 8);
    pop.style.height = Math.min(pop.scrollHeight, room) + "px";
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
  // The dock is sticky, so its viewport position shifts as the end of the
  // run section comes into view — keep the popover glued to it while open.
  window.addEventListener("scroll", () => {
    if (!pop.hidden) positionPop();
  }, { passive: true });
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
  $("output").classList.remove("interleaved");
  if (liveTimer) { clearTimeout(liveTimer); liveTimer = null; }
  lastRenderedDoc = null;
  outputRaw = "";
  runArtifacts = [];
  lastMarkdown = lastResultText = lastOutputRaw = "";
  lastStderr = "";
  lastExitCode = 0;
  warningsOn = false;
  lastStderrEl = null;
  $("warnings").hidden = true;
  // Also clear the per-block state so a cleared run doesn't leave stale
  // final states on the language pills, then rebuild the pill row from the
  // document — that drops the per-artifact download pills too, whose blobs
  // are gone with the results.
  blockStates.clear();
  updateLangStatus();
  // updateLangStatus schedules the live document preview; a cleared box stays
  // cleared until the next keystroke (which re-arms it) or the next run.
  if (liveTimer) { clearTimeout(liveTimer); liveTimer = null; }
  // Back to the pre-execute state: grey pip until the next run decides.
  syncInterleaveButton();
  // The "Try it here" footer lives inside the box, so it is gone with
  // the wipe; syncTryItHere() re-applies the checkbox rule (nothing yet).
  syncTryItHere();
});

// Set the current view state (interleaved or results-only). The view is
// driven by the effectiveInterleave() decision (pinned modes or the auto
// document-shape check); the pip on the button mirrors that decision.
function setInterleaved(on) {
  interleaved = on;
}

// True when the document has any non-whitespace prose outside its fenced
// code blocks — i.e. a report-style doc that benefits from the interleaved
// view, as opposed to a bare collection of code blocks.
function docHasProse(md) {
  if (!md) return false;
  return splitMarkdown(md).some((s) => s.type === "prose" && s.text.trim());
}

// The view decision implied by the current mode: pinned modes decide
// outright; auto keys off the document shape (prose outside the fences →
// interleaved, bare code → results-only). The shape is read off the LIVE
// document, so the preview can appear as soon as prose is typed — before
// anything has run.
function effectiveInterleave() {
  return (
    interleaveMode === "on" ||
    (interleaveMode === "auto" && docHasProse(displayDoc()))
  );
}

// --- live preview scheduling ----------------------------------------------
// A throttled (leading + trailing) re-render of the interleaved view. Prose
// is not executed, so the document view can follow typing with zero cluster
// work; only the highlighted code blocks are rebuilt, and those come out of
// hlCache. Throttling (not debouncing) keeps long dictation updating while
// the student is still typing.
function scheduleLivePreview() {
  if (runInFlight) return;
  if (!interleaved && !canStartPreview()) return;
  const wait = Math.max(0, LIVE_PREVIEW_MS - (performance.now() - liveLastAt));
  if (liveTimer) clearTimeout(liveTimer);
  liveTimer = setTimeout(renderLivePreview, wait);
}

// Auto mode may flip into the document view before the first run — that's the
// point of the live preview. After a run the view stays where the run's
// decision put it (its results are still valid for the submitted document).
function canStartPreview() {
  return lastMarkdown === "" && lastOutputRaw === "" && effectiveInterleave();
}

function renderLivePreview() {
  liveTimer = null;
  if (runInFlight) return; // streamed logs own the box right now
  liveLastAt = performance.now();
  if (!interleaved) {
    if (!canStartPreview()) return;
    setInterleaved(true);
  }
  if (displayDoc() === lastRenderedDoc) return; // nothing visible changed
  renderInterleaved();
  // The renderer wipes the box, so re-pin the footer mirror (it is part of
  // the box, not of the document view).
  syncTryItHere();
}

// Keep the Interleave button's affordance in sync with the effective
// view decision: the corner pip is grey while nothing has run yet (auto
// pre-execute, decision pending), blue when the view is interleaved, and
// hollow when it is results-only. The tooltip spells out the mode and
// the next click target.
function syncInterleaveButton() {
  const btn = $("interleave");
  // A run has happened once lastMarkdown holds the submitted document;
  // before that the auto decision is still pending.
  const decided = lastMarkdown !== "";
  const interleaving = decided && effectiveInterleave();
  btn.classList.toggle("pip-on", interleaving);
  btn.classList.toggle("pip-off", decided && !interleaving);
  btn.classList.toggle("active", interleaveMode === "on");
  btn.classList.toggle("mode-off", interleaveMode === "off");
  btn.title =
    interleaveMode === "on"
      ? "Interleave: always on — results after each code block (click: always off)"
      : interleaveMode === "off"
        ? "Interleave: always off — results only (click: auto)"
        : "Interleave: auto — follows the document (click: always on)";
  btn.setAttribute("aria-label", "Interleave results — mode: " + interleaveMode);
  btn.removeAttribute("aria-pressed"); // tri-state, not a boolean toggle
}

$("interleave").addEventListener("click", () => {
  interleaveMode =
    interleaveMode === "auto" ? "on" : interleaveMode === "on" ? "off" : "auto";
  syncInterleaveButton();
  const on = effectiveInterleave();
  setInterleaved(on);
  if (interleaved) renderInterleaved();
  else if (lastOutputRaw) renderNormal();
  else {
    // Results-only view with nothing run yet: drop the document preview so
    // the pinned "off" mode shows an empty box, not a stale preview.
    $("output").textContent = "";
    $("output").classList.remove("interleaved");
    lastRenderedDoc = null;
  }
  // The view renderers wiped the box, so re-pin the footer mirror.
  syncTryItHere();
});
syncInterleaveButton(); // start in auto mode (title/aria for the initial state)

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
// "Include share link in output": toggling the checkbox immediately
// shows/removes the on-screen mirror (and thereby the print footer too).
document.querySelectorAll('input[name="shareLinkMode"]').forEach((r) =>
  r.addEventListener("change", syncTryItHere)
);
$("copyCode").addEventListener("click", () => {
  // Folded seedfiles are a view: copy the payloads, not the placeholders.
  copyText(liveMarkdown(), $("copyCode"), "Copy code");
});
// Add files to the Run Code editor via the native picker. Always APPENDS — the
// ✕ Clear button is how you start over. Markdown documents are appended as
// documents; every other file becomes a seedfile fence (text inline, anything
// else base64), the same shape the desktop tray's convert hotkey produces.
$("importCode").addEventListener("click", () => {
  const input = document.createElement("input");
  input.type = "file";
  input.multiple = true;
  input.addEventListener("change", () => addFiles(input.files));
  input.click();
});
// Export the Run Code editor as a timestamped .md file download.
$("exportCode").addEventListener("click", () => {
  const markdown = liveMarkdown();
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
/* --- Layout sync: floating toolbars + floating run-area dock ---
   The Run Code / Output toolbar rows float (sticky) over long content;
   .floating is toggled while a row is stuck so it renders translucent
   instead of reading as a second real header. The run area's bottom
   controls live in a sticky dock pinned to the bottom of the viewport, so
   they are always available; the editor grows with its content and the
   page keeps ONE universal scroll — the dock simply floats over the
   document and covers the code beneath it. A small down-triangle at the
   dock's top edge shows while more code is still hidden below the fold. */
let floatTick = false;
function syncFloating() {
  floatTick = false;
  document.querySelectorAll(".section-head").forEach((head) => {
    const r = head.getBoundingClientRect();
    // Stuck = pinned at the top edge of the viewport. The `bottom > 1`
    // guard matters once the row is pushed out by the end of its
    // containing block (the code area's last line): top stays <= 1 while
    // it slides away, and we don't want to keep the translucent style on
    // a row that is no longer floating over anything.
    head.classList.toggle("floating", r.top <= 1 && r.bottom > 1);
  });
}
window.addEventListener("scroll", () => {
  if (!floatTick) {
    floatTick = true;
    requestAnimationFrame(() => {
      syncFloating();
      syncMoreHint();
    });
  }
}, { passive: true });
// Cheap timer poll as a universal fallback — some embedded webviews never
// deliver scroll / rAF / IntersectionObserver callbacks, so the floating
// state and the "more below" triangle are re-checked on a light interval
// as well (two rect reads per tick; no layout thrash).
setInterval(() => {
  syncFloating();
  syncMoreHint();
}, 250);

function syncMoreHint() {
  const editor = $("editor");
  const hint = $("editorMore");
  const dock = document.querySelector("section.run .run-dock");
  if (!editor || !hint || !dock) return;
  // More code hides below the fold whenever the editor's bottom edge is
  // still below the dock's top edge — i.e. the page hasn't been scrolled
  // far enough to expose the whole expanded document above the dock. The
  // small epsilon keeps the triangle from flickering when the two edges
  // are flush (last line just above the dock).
  const moreBelow =
    editor.getBoundingClientRect().bottom >
    dock.getBoundingClientRect().top + 2;
  // .visible (not [hidden]): the chevron eases in/out via its opacity
  // transition, and display can't animate — the element stays in the
  // DOM and merely fades to transparent when the fold is fully shown.
  hint.classList.toggle("visible", moreBelow);
}

window.addEventListener("resize", () => {
  syncMoreHint();
  syncFloating();
});

initEditor();
updateLangStatus();
highlightCodeHeaders();
start();
