# Ephemeral — Developer & Ops Reference

Notes for maintainers, operators, and advanced self-hosters that are too detailed for the main README.

## Contents

- [The default swarm — full mechanism](#the-default-swarm--full-mechanism)
- [Bastion server (paper-light clients)](#bastion-server-paper-light-clients)
- [Implementation history](#implementation-history)
- [CI/CD Pipeline](#cicd-pipeline)
- [Hosting the web demo on GitHub Pages](#hosting-the-web-demo-on-github-pages)
- [Dropping into a Lithic-UK deployment](#dropping-into-a-lithic-uk-deployment)

## The default swarm — full mechanism

Every distributed binary joins the **same public swarm by default** — no configuration required. Run `Ephemeral-Distributed.exe`, the distributed AppImage, or `install_self_host.sh distributed` and your node is part of the network, discoverable by the web SPA and every other member. The mechanism:

- **No compiled-in seeds — the list *is* the bootstrap.** Nothing is hard-coded into the binaries (there is no `DEFAULT_SWARM_NODES` to keep in sync). When `EPHEMERAL_SEED_NODES` / `EPHEMERAL_SEEDS` are unset, every node fetches the **live swarm list** (`docs/swarm.json`, served by GitHub Pages / raw GitHub) at startup and dials the current members by **stable node id + relay** (iroh-native; tickets only as a fallback for legacy entries), re-fetching every maintenance cycle so freshly-picked-up members are learned without a restart. `ephemeral_net/swarm.py` → `fetch_swarm_list()` is the single implementation.
- **Stable identity** — every distributed binary persists a 32-byte secret to `~/.ephemeral/secret_key.bin` (or `EPHEMERAL_STATE_DIR`) and reuses it across restarts, so its node id is permanent. Verified live: a node restarts, keeps its id, and is dialed again by id + relay with no ticket.
- **The list, not a box, is the lynchpin.** The always-on anchor is `docs/swarm.json` (GitHub never sleeps), and a single **genesis anchor** exists only to bootstrap the *first-ever, empty* list or rescue a list whose every member went dark — afterwards the list regenerates from its own members and the genesis node can go offline forever. **Nothing is hard-coded:** the anchor's identity is resolved at refresh time from a **public bastion URL** — the `SWARM_GENESIS_URL` repo variable (set it to your bastion's URL, e.g. `https://your-bastion.up.railway.app`); `scripts/update_swarm_json.py` GETs the bastion's `/ready` and reads its `node_id`, `relay` and seed `ticket` (exposed on `/ready` so `/health` stays a trivial platform liveness probe), then dials it. An explicit `SWARM_GENESIS` (comma-separated `node_id@relay`) is an alternative pin. The same URL pre-wakes a sleeping Railway bastion so its iroh endpoint is bound before the QUIC dial. **Adding an always-on node is fully automatic:** install any distributed flavor on a box, and the next scheduled refresh (within ~6 h, or *Run workflow* on the action) discovers it through its peers and lists it — no code edits, no swap-in, no hard-coded ids.
- **Mesh healing** — every node periodically re-dials known peers from its peer table (by id + relay first, ticket as fallback, with backoff for dead peers), so the swarm repairs itself around a dead member. Existing members reconnect to each other directly; only the *very first contact* for brand-new nodes ever needs a reachable list entry.
- **Bootstrap list (automated)** — a scheduled GitHub Action (every 6 h, `.github/workflows/swarm-bootstrap.yml`) joins the swarm as a throwaway client and commits the live node list to `docs/swarm.json` (served by GitHub Pages / raw.githubusercontent; the README's **live nodes** badge reads the same census from `docs/swarm-status.json`, written alongside it each refresh). Thin and first-time joiners fetch that file instead of running a node themselves; the list merges newly-seen nodes with the previous one, so it keeps regenerating from any reachable member even if the genesis anchor is down — the anchor is only dialed when no previous member answers. **There is nothing to install to be listed:** just run any distributed flavor (the gateway, `ephemeral-distributed`, the self-host build) on an always-on box with default bootstrap — it fetches the list, joins the swarm, and the next refresh (within ~6 h, or immediately via *Run workflow* on the action) writes that node into `docs/swarm.json` with its id, relay, ticket, and warm images. The refresh then re-dials it every run, so the entry stays fresh as long as the box is up. If the list ever goes stale, a manual *Run workflow* with the **reset** checkbox forgets every entry and regenerates a fresh census from the genesis anchor.
- **Every listed node is probe-verified** — a dial + hello handshake proves a node speaks the wire protocol, but not that it is a live compute node. So each refresh actually sends every reachable entry a real job (a tiny bash `echo` payload carrying a fresh per-node nonce) and records the verdict in `docs/swarm.json` (`probe: "ok"|"failed"|"unreachable"`, `probe_at`, `probe_detail`); a node is only ranked as verified when it executed the payload and echoed the nonce back, which a bot that merely answers hello cannot fake. Entries that are reachable but never run the probe are evicted after 3 failed probes; entries that go silent are kept for ~36 h (6 runs) in case they're just offline — but only if they've ever been seen alive (a node that has never once answered a dial has no recovery to wait for, so it ages out after just 2 runs, ~12 h). The genesis anchor is exempt from eviction only while it is the active bootstrap source for that run; otherwise it ages out like any other node. Run a manual check anytime with `python scripts/update_swarm_json.py --no-probe --out /tmp/swarm.json` (dial-only) or just watch the per-node lines in the action log.
- **Make thick nodes super-seeds** — `python scripts/hydrate_images.py` pulls **every** language-map image in one shot (skips ones already cached, retries failures with backoff, `--dry-run`/`--only python,node`/`--parallel N` flags). Run it once on an always-on gateway and the box starts with the full warm set — offloaded jobs land on it without a registry pull, and its hello frames advertise the whole set so nearest-neighbor routing prefers it. The hydrate set is derived from the same `LANG_MAP` as the receiver-side allowlist, so it covers exactly what remote jobs may request (verified by a test invariant).
- **DNS TXT redundancy (optional)** — when the list itself is unreachable (e.g. GitHub is down), first contact can still happen through **DNS**: the same scheduled Action keeps a TXT record that **mirrors the top of the list** (the two fastest/ranked nodes as compact `iroh1:<node_id>;<relay>` entries, comma-separated in one 255-char string — tickets are too long for DNS and arrive via the hello handshake anyway), and every node *and* the wasm SPA resolve it via DNS-over-HTTPS as a final fallback. DNS is tiered, cached infrastructure, so it's an independent path to the swarm. Configure it in the repo settings: `EPHEMERAL_DNS_TXT` (variable — the TXT record hostname) + `EPHEMERAL_DNS_TOKEN` (secret — a Cloudflare API token with DNS edit); `EPHEMERAL_DNS_ZONE` is optional and auto-detected. Runtime falls back to `EPHEMERAL_DNS_TXT` (env) automatically — no code changes. Two entries fit one 255-char TXT string and are enough for first contact — dialing either reveals the whole swarm via `hello`.
- **Opt out** — set `EPHEMERAL_SEED_NODES` (comma-separated `node_id@relay`) or `EPHEMERAL_SEEDS` (EndpointTickets) explicitly to bootstrap a private cluster instead (private networks run their own bootstrap — the public list only serves the implicit public swarm); set `EPHEMERAL_SECRET` to pin an identity without touching disk.

> **The browser client is iroh-native too.** The wasm SPA dials by the same stable node id + relay — no asymmetry between tiers. Tickets remain only as a fallback for legacy peers that don't report a relay.

## Bastion server (paper-light clients)

The bastion (`main_bastion.py`) is the realized **paper-thin client** tier — an HTTP(S) gateway that turns a curl-friendly `POST /ephemeral/api/v1/run` into a swarm job.

- **Same routing as the SPA.** It joins as an iroh node and forwards each request through `FanoutExecutor → OffloadingExecutor → CoreJobExecutor`, so a request lands on a warm-image peer first, then the most idle, then the lowest RTT — the same preference sequence `pickTarget()` uses in the wasm SPA.
- **Orchestration-first, compute-optional.** `EPHEMERAL_COMPUTE=0` (or a host with no Podman) keeps it a pure forwarder: the local executor becomes `OrchestrationOnlyExecutor`, which only rejects when no peer can run the job. With `EPHEMERAL_COMPUTE=1` (or Podman auto-detected) it is a full coderunner node and runs its own requests locally as a fallback.
- **Guardrails.** `TokenBucketLimiter` (per client IP, `EPHEMERAL_RATE_LIMIT_PER_MIN`/`EPHEMERAL_RATE_LIMIT_BURST`) + `ConcurrencyLimiter` (`EPHEMERAL_MAX_CONCURRENT`), and a `ResultCache` keyed on the exact `document_blob` + timeout (`EPHEMERAL_CACHE_MAX`/`EPHEMERAL_CACHE_TTL`). Cache hits return `X-Ephemeral-Cache: hit`.
- **Discovery.** The bastion advertises `EPHEMERAL_PUBLIC_URL` (falling back to Railway's `RAILWAY_PUBLIC_DOMAIN`) in its `hello` frames; `Node`/`PeerInfo`/`PeerTable` carry the `url` field, so the refresh Action learns it through the mesh.

### `docs/swarm.json` bastions list

The refresh (`scripts/update_swarm_json.py`) partitions discovered peers by the presence of a `url`: URL-bearing peers go into a separate **`bastions`** array instead of `nodes`, and are verified with an HTTP `GET {url}/health` (200 = `ok`) rather than the bash-echo compute probe — an orchestration-only bastion can therefore stay listed even though it could never execute a probe job. Each entry carries `url`, `node_id`, `relay`, `probe`/`probe_detail`/`probe_ms`, and the same `probe_fails`/`misses` staleness counters as nodes (failed health checks evict after 3 runs; unreachable ones age out after ~36 h, or ~12 h if never once seen healthy). Bastions are ranked by measured HTTP latency, so paper-light clients pick the fastest listed endpoint. The DNS TXT mirror and the README badge still count only compute `nodes`.

### Railway

`railway.json` builds `Dockerfile.bastion` (orchestration-only by default — no Podman socket needed) and sets `healthcheckPath: /health`, `restartPolicyType: ON_FAILURE` (max retries 5), and `generateDomain: true`. `generateDomain` makes Railway auto-generate the service's `.up.railway.app` domain, which it exposes as `RAILWAY_PUBLIC_DOMAIN`; `main_bastion.py` reads that automatically, so a deployed bastion self-registers in `swarm.json` on the next refresh with no manual URL step. There is deliberately **no** `sleepApplication` — an always-on bastion stays in the swarm (and stays listed as healthy) instead of sleeping to zero.

> Railway has deprecated Config as Code (`railway.json`/`railway.toml`): new services cannot opt into it and it stops being read on 2026-12-01. The migration target is Infrastructure as Code — `.railway/railway.ts` in this repo, applied via `railway config plan` / `railway config apply`. The beta IaC DSL doesn't yet document fields for the Dockerfile path, restart policy, or the generated-domain toggle, so those three remain one-time dashboard settings (or are imported with `railway config pull --force`). A service can't be managed by both files at once: once the IaC service is applied, remove `railway.json`. Pin `EPHEMERAL_SECRET` (32-byte hex) so the bastion keeps one node id across redeploys instead of re-keying each deploy and churning its `swarm.json` entry.

## Implementation history

The distributed tiers were rolled out in phases:

* **Phase 1 — `ephemeral_net`:** QUIC transport, hello handshake, seed-mediated discovery, job streaming over a single connection.
* **Phase 2 — receiver-side sandboxing & offloading:** incoming jobs are sanitized before execution — image allowlist, `unsafe` stripped (network is gated behind a node-operator flag), `image=`/`cmd=`/`entrypoint=` overrides ignored, and `--memory 2g`/`--cpus 2`/`--pids-limit 512`/`--network none` enforced (limits scale down to ~half of host RAM on nodes with ≤ 2.5 GiB, so small VPS nodes can't be OOM-killed by one job). Nearest-neighbor offloading: when an image isn't warm locally, the job forwards to the nearest node that has it while the image pulls in the background. **Idle-first routing:** hello frames advertise each node's current load (`active_jobs`/`max_jobs`); saturated nodes are never chosen and the least-loaded warm node wins (RTT breaks ties).
* **Phase 2 — parallel multi-block execution:** artifact chaining is now **off by default** (`chain`/`piping`/`pipe` opts in). Without it, multi-language runs execute concurrently — up to 4 runs in parallel per host, and `FanoutExecutor` (wired into every distributed entry point) additionally splits multi-run documents across idle warm peers, merging the event streams back into a single response. Declaring chaining anywhere restores the sequential in-order path so artifacts keep flowing run-to-run.
* **Phase 2.5 — `ephemeral-self-host-distributed`:** `main_distributed.py`, a REST gateway that joins the cluster as a compute node.
* **Phase 3 — browser client & desktop tier:** the WebAssembly thin client and the `ephemeral-distributed` desktop tier (`main_distributed_client.py`). Both desktop tiers build for Windows (EXE) and Linux (AppImage).

## CI/CD Pipeline

The official build pipeline runs on every push (`.github/workflows/build.yml`) and is also triggerable manually:

1. **Test** — `test_core.py`, `test_space.py` (disk-space pull guardrail), `test_api.py`, `test_net.py` (including two live iroh cluster integrations), and `test_self_host.py` on Python 3.10 and 3.12.
2. **Build** — Windows EXEs via PyInstaller (`Ephemeral.exe` + `Ephemeral-Distributed.exe`), both Linux AppImages via PyInstaller onedir + appimagetool (with `--self-check` smoke tests), the wasm/SPA tarball, and both self-host tarballs (each import-smoke-tested), plus a smoke test that runs `install_self_host.sh` for both flavors.
3. **Release** — `workflow_dispatch` with the *Create a new release* checkbox; attaches all seven artifacts.

## Hosting the web demo on GitHub Pages

The thin-client SPA (`ephemeral-wasm-library/web/`) is fully static — the compiled wasm glue (`wbg/`) is checked in — so it hosts on GitHub Pages with no build step:

1. **Settings → Pages** → *Source: Deploy from a branch* → branch `main`, folder **`/` (root)** — the whole repo is served, so `ephemeral-wasm-library/web/` is reachable.
2. The root `.nojekyll` skips Jekyll processing (it would otherwise mangle the repo's files).
3. The bare site URL (`https://<owner>.github.io/Ephemeral.exe/`) lands on the root `index.html`, which redirects straight to the SPA at `ephemeral-wasm-library/web/` (hitting `/docs/` directly works too — `docs/index.html` redirects the same way).

> **Note:** don't select the `/docs` folder as the Pages source — a `/docs`-only deploy publishes *just* that folder, so the redirect target (the SPA elsewhere in the repo) would 404.

## Dropping into a Lithic-UK deployment

[Lithic-UK](https://github.com/Xyvir/Lithic-UK) can provision the Ephemeral backend itself: its `deploy/install-lxc.sh` honors `ENABLE_EPHEMERAL=true`, which clones this repo and runs the root `./install.sh` as part of the LXC setup. `install.sh` creates a dedicated `ephemeral` system user, installs the API to `/opt/ephemeral` with its own venv, initializes rootless Podman for that user, provisions the artifact directory, and registers/starts the `ephemeral-api` systemd service bound to `127.0.0.1:8787`.

Lithic's generated Caddyfile then proxies the sidecar:

```caddy
handle /ephemeral/api/v1/* {
    reverse_proxy ${EPHEMERAL_HOST:-127.0.0.1}:8787
}
```

The contract is: serve the REST API under the `/ephemeral/api/v1` prefix on port `8787` (which `main_api.py` already does), and let Caddy handle HTTPS + Basic Auth at the edge — the API itself needs no auth. Artifacts are written to `/data/ephemeral/` (`WEBDAV_PATH` in `main_api.py`, created on demand by the artifact code), which sits inside Lithic's WebDAV root `/data`, so they're delivered back to the front end through the `/sync` WebDAV endpoints the REST response names. Run Caddy and Ephemeral on different hosts by setting `EPHEMERAL_HOST` in the Lithic service environment.
