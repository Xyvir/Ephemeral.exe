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
// `nodes`: compiled seed nodes as STABLE NODE ID + relay — the
//          iroh-native bootstrap (no tickets). Every seed's `hello`
//          reply carries its known peers (with relays, tickets and warm
//          images), so one seed is enough to learn the whole cluster
//          with zero input. Used when the swarm list can't be fetched
//          or is empty.
// `seedTickets`: legacy EndpointTickets for the same seeds — used to
//          dial seeds that don't report a relay (old nodes) and as a
//          last-resort fallback.
//
// One big implicit swarm: `ephemeral_net/swarm.py` (DEFAULT_SWARM_NODES)
// holds the same seeds by node id + relay — keep them in sync.
export const BOOTSTRAP = {
  relay: null,
  swarmJson: [
    "../../docs/swarm.json",
    "https://raw.githubusercontent.com/Xyvir/Ephemeral.exe/main/docs/swarm.json",
  ],
  nodes: [
    {
      // Placeholder: the original demo node. Swap in your always-on
      // node's id + relay (printed at gateway startup as SWARM NODE_ID /
      // SWARM RELAY) so the public Pages demo always has a live seed.
      node_id:
        "154e7308b6af310df575c7c90bc8fe86146cfef036ac098662768a4f3c411ec5",
      relay: "https://use1-1.relay.n0.iroh.link.",
    },
  ],
  seedTickets: [
    "endpointaaku44yiw2xtcdpvoxd4sc6i72dbi3h66a3kycmgmj3iutz4iepmkbaaenuhi5dqom5c6l3vonstcljrfzzgk3dbpexg4mbonfzg62bonruw42zof4aqasvhfs7zd3qdaeakyhcaagi64aybadakqaaushxag",
  ],
};
