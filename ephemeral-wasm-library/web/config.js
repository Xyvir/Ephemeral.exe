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
export const BOOTSTRAP = {
  relay: null,
  seeds: [
    // Demo compute node on the public n0 relay. Replace with your own
    // cluster's seed tickets for a private deployment.
    "endpointaaku44yiw2xtcdpvoxd4sc6i72dbi3h66a3kycmgmj3iutz4iepmkbaaenuhi5dqom5c6l3vonstcljrfzzgk3dbpexg4mbonfzg62bonruw42zof4aqasvhfs7zd3qdaeakyhcaagi64aybadakqaaushxag",
  ],
};
