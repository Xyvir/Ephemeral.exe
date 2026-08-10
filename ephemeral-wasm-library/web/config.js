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
//          relying on a static compiled seed. The relative path works
//          when the SPA is served from the GitHub Pages repo root; the
//          raw fallback covers any other origin.
// `seeds`: compiled EndpointTickets used when the swarm list can't be
//          fetched or is empty. Every seed's `hello` reply carries its
//          known peers (with dialable tickets + warm images), so one
//          seed is enough to learn the whole cluster with zero input.
//
// One big implicit swarm: `ephemeral_net/swarm.py` (DEFAULT_SWARM_NODES)
// holds the same seed by node id + relay — keep them in sync.
export const BOOTSTRAP = {
  relay: null,
  swarmJson: [
    "../../docs/swarm.json",
    "https://raw.githubusercontent.com/Xyvir/Ephemeral.exe/main/docs/swarm.json",
  ],
  seeds: [
    // Placeholder: the original demo node's ticket. Swap in your
    // always-on node's ticket (printed at gateway startup) so the
    // public Pages demo always has a live seed.
    "endpointaaku44yiw2xtcdpvoxd4sc6i72dbi3h66a3kycmgmj3iutz4iepmkbaaenuhi5dqom5c6l3vonstcljrfzzgk3dbpexg4mbonfzg62bonruw42zof4aqasvhfs7zd3qdaeakyhcaagi64aybadakqaaushxag",
  ],
};
