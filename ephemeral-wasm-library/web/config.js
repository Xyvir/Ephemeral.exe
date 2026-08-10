// Bootstrap configuration for the Ephemeral Web SPA.
//
// This is *bootstrap* configuration, not a job-routing dependency: it is
// compiled into the bundle and used once at startup to join the cluster.
// The actual job-execution path runs entirely over the iroh network
// (relay + QUIC) — no HTTP endpoint is involved.
//
// `relay`: null uses the public n0 relays; set a URL to use a
//          self-hosted relay (e.g. "https://relay.example.com.").
// `swarmJson`: bootstrap-list URLs tried at startup — the live node
//          list (docs/swarm.json) is refreshed every 6 h by a scheduled
//          GitHub Action, so the SPA dials CURRENT members instead of
//          relying on any compiled-in seed. The relative path works
//          when the SPA is served from the GitHub Pages repo root; the
//          raw fallback covers any other origin.
// `dnsTxt`: hostname of a DNS TXT record that MIRRORS the top of the
//          live swarm list — two compact `iroh1:<node_id>;<relay>`
//          entries, comma-separated in one 255-char string — kept in
//          sync by the same scheduled Action. Used only when every
//          `swarmJson` URL is unreachable — DNS is tiered/cached
//          infrastructure, so it is an independent path to first
//          contact. Empty = disabled.
// `nodes` / `seedTickets`: intentionally EMPTY — the distributed binaries
//          ship with no hard-coded seeds (one big implicit swarm joined
//          purely through the live list). Operators running a private
//          swarm may add stable node ids here (node_id + relay) or a
//          legacy ticket, but the public build relies on `swarmJson`.
//
// There is no `ephemeral_net/swarm.py` constant to keep in sync — the
// Python tiers bootstrap from the same docs/swarm.json list.
export const BOOTSTRAP = {
  relay: null,
  swarmJson: [
    "../../docs/swarm.json",
    "https://raw.githubusercontent.com/Xyvir/Ephemeral.exe/main/docs/swarm.json",
    "https://xyvir.github.io/Ephemeral.exe/docs/swarm.json",
  ],
  dnsTxt: "", // e.g. "_ephemeral-swarm.example.com" — TXT: `iroh1:<node_id>;<relay>`
  nodes: [
    // { node_id: "...", relay: "https://relay.example.com." } — private swarms only
  ],
  seedTickets: [
    // "endpoint..." — legacy tickets, private swarms only
  ],
};
