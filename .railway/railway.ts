// Ephemeral bastion — Railway Infrastructure as Code (.railway/railway.ts).
//
// This is the replacement for the deprecated config-as-code railway.json.
// Unlike railway.json (read at deploy time), IaC is applied through the
// Railway CLI:
//
//   railway login
//   railway link
//   railway config plan      # preview
//   railway config apply     # apply (--yes for non-interactive)
//
// The IaC DSL is still in beta and does not yet document a field for every
// service setting. Set these three once in the dashboard, because there is
// no documented IaC field for them yet:
//
//   * Builder        -> Dockerfile, path "Dockerfile.bastion"
//   * Restart policy -> ON_FAILURE, max retries 5
//   * Generate Domain -> on (creates the .up.railway.app domain that becomes
//                        RAILWAY_PUBLIC_DOMAIN at runtime, which the bastion
//                        advertises automatically)
//
// Tip: after `railway link`, `railway config pull --force` imports the
// linked project's current settings into this file — run it if you already
// configured the above in the dashboard, then re-plan/apply.
import { defineRailway, github, project, service } from "railway/iac";

export default defineRailway(() => {
  const bastion = service("ephemeral-bastion", {
    source: github("Xyvir/Ephemeral.exe", { branch: "main" }),
    healthcheck: "/health",
    healthcheckTimeout: 100,
    // Always-on: no sleepApplication, so the bastion stays in the swarm and
    // stays listed in docs/swarm.json while idle.
    replicas: 1,
    // Pin a stable node identity across redeploys so the bastion keeps one
    // node id (and one swarm.json entry) instead of re-keying every deploy.
    // Generate once:
    //   python -c "import secrets; print(secrets.token_bytes(32).hex())"
    // env: {
    //   EPHEMERAL_SECRET: "...",
    // },
  });

  return project("ephemeral", {
    resources: [bastion],
  });
});
