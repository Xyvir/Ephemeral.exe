"""
Ephemeral Distributed Client — Windows tray application (``ephemeral-distributed.exe``).

This is now a thin entry point: the tray front end and the distributed
backend live in the ``ephemeral_ui`` package.

* ``ephemeral_ui.tray``                    — the unified tray front end
  (menu, hotkeys, tray/one-shot/headless modes)
* ``ephemeral_ui.backends.distributed``    — the distributed backend
  (per-user iroh node, warmest-neighbor offloading, private mode,
  pre-hydration)

Per-user node model: the tray runs its own compute node with a stable
identity (one per user account, persisted under the profile), available
while the user is logged in or the PC is locked. For an always-on node
that keeps serving while no one is logged in, self-host the Linux gateway
(install_self_host.sh).

Cluster configuration (environment variables):

    EPHEMERAL_RELAY          "n0" (default) | "minimal" | "disabled"
    EPHEMERAL_SEED_NODES     comma-separated node_id[@relay] to bootstrap from;
                             unset joins the default swarm by node id
                             (see ephemeral_net.swarm) — iroh-native, no tickets
    EPHEMERAL_SEEDS          comma-separated EndpointTickets (private networks /
                             backward compat; overrides SEED_NODES when set)
    EPHEMERAL_SECRET         hex-encoded 32-byte secret for a persistent node id;
                             unset, a stable identity is auto-persisted to disk
    EPHEMERAL_ALLOW_NETWORK  "1" to let remote jobs use network access (default "0")
    EPHEMERAL_PRIVATE        "1" (or ``--private``) — skip the public swarm list;
                             this node is its own seed for a private cluster
                             (also toggled live via the tray's "Private Mode" item)

Usage:
    python main_distributed_client.py                 # Tray mode
    python main_distributed_client.py --private       # Tray mode, private swarm
    python main_distributed_client.py script.md       # One-shot mode
    python main_distributed_client.py --cli script.md # Headless CLI mode
    python main_distributed_client.py --self-check    # Print node id and exit
"""
from __future__ import annotations

import sys

from ephemeral_ui import tray
from ephemeral_ui.backends.distributed import DistributedBackend


if __name__ == '__main__':
    tray.run(DistributedBackend())
    sys.exit(0)
