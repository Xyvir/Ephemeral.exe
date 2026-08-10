// Bootstrap configuration for the Ephemeral Web SPA.
//
// This is *bootstrap* configuration, not a job-routing dependency: it is
// compiled into the bundle and used once at startup to join the cluster.
// The actual job-execution path runs entirely over the iroh network
// (relay + QUIC) — no HTTP endpoint is involved.
//
// `relay`: null uses the public n0 relays; set a URL to use a
//          self-hosted relay (e.g. "https://relay.example.com.").
// `seeds`: EndpointTickets of seed/compute nodes the SPA auto-connects
//          to on load. Every seed's `hello` reply carries its known
//          peers (with dialable tickets + warm images), so one seed is
//          enough to learn the whole cluster with zero user input.
//
// One big implicit swarm: keep this list in sync with
// `ephemeral_net/swarm.py` (DEFAULT_SWARM_SEEDS) — every distributed
// binary joins the same public network by default and discovers it
// through this seed. The seed must be an ALWAYS-ON node with a
// persisted identity (run the self-host distributed gateway; its
// startup log prints "SWARM SEED TICKET ...").
export const BOOTSTRAP = {
  relay: null,
  seeds: [
    // Placeholder: the original demo node's ticket. Swap in your
    // always-on node's ticket (printed at gateway startup) so the
    // public Pages demo always has a live seed.
    "endpointaaku44yiw2xtcdpvoxd4sc6i72dbi3h66a3kycmgmj3iutz4iepmkbaaenuhi5dqom5c6l3vonstcljrfzzgk3dbpexg4mbonfzg62bonruw42zof4aqasvhfs7zd3qdaeakyhcaagi64aybadakqaaushxag",
  ],
};
