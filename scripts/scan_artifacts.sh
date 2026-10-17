#!/usr/bin/env bash
# ============================================================================
# ClamAV canary for built artifacts — best effort, never a gate.
# ============================================================================
#
# ClamAV is a single engine: it does not reproduce the PyInstaller false
# positives Windows Defender produces, and its database lags vendor ML
# verdicts. So this is wired into CI as a *soft* signal — it reports what it
# found and always exits 0. An antivirus opinion must never block a release;
# the honest verdict for a machine that actually flags the build comes from
# that machine (or from a signed build, which is the real fix).
#
# The three outcomes are reported distinctly, because "we did not scan" must
# never be mistaken for "we scanned and it was clean":
#
#   clean        scanned, nothing found
#   DETECTED     one or more signatures matched (warn only)
#   NOT SCANNED  clamscan missing, or it errored before finishing
#
# Usage:
#   bash scripts/scan_artifacts.sh [options] [dir-or-file ...]
#
# Options (defaults come from the environment so CI needs no flags):
#   --scanner PATH   scanner binary           (env CLAMSCAN, default: clamscan)
#   --report PATH    full clamscan log         (env REPORT, default: clamav-report.txt)
#   --summary PATH   markdown summary to append (env GITHUB_STEP_SUMMARY)
#
# Environment:
#   MAX_FILE   per-file scan cap (default: 300M)
#   MAX_SCAN   total scan cap (default: 600M)
#
# Why the caps are explicit: clamscan defaults to --max-filesize=25M and
# --max-scansize=100M, which SILENTLY SKIPS the ~28 MB one-file PyInstaller
# builds and reports the artifact clean. A canary that quietly skips its
# subject is worse than no canary, so both caps are raised here.
#
# Exit code: always 0.
set -uo pipefail

CLAMSCAN="${CLAMSCAN:-clamscan}"
REPORT="${REPORT:-clamav-report.txt}"
STEP_SUMMARY="${GITHUB_STEP_SUMMARY:-}"
MAX_FILE="${MAX_FILE:-300M}"
MAX_SCAN="${MAX_SCAN:-600M}"
IN_ACTIONS="${GITHUB_ACTIONS:-false}"

TARGETS=()
while [ $# -gt 0 ]; do
    case "$1" in
        --scanner) CLAMSCAN="${2:-}" ; shift 2 ;;
        --report) REPORT="${2:-}" ; shift 2 ;;
        --summary) STEP_SUMMARY="${2:-}" ; shift 2 ;;
        --) shift; TARGETS+=("$@"); break ;;
        *) TARGETS+=("$1") ; shift ;;
    esac
done
[ ${#TARGETS[@]} -eq 0 ] && TARGETS=("artifacts")

note() { printf '%s\n' "$*"; }

warn() {
    printf 'WARN: %s\n' "$*"
    [ "$IN_ACTIONS" = "true" ] && printf '::warning::%s\n' "$*"
    return 0
}

summary() {
    [ -n "$STEP_SUMMARY" ] && printf '%s\n' "$*" >> "$STEP_SUMMARY"
    return 0
}

# Zips are unpacked first so their contents get scanned — the distributed-beta
# artifact ships a whole folder inside a zip, and the folder is the point.
for target in "${TARGETS[@]}"; do
    [ -d "$target" ] || continue
    while IFS= read -r zip; do
        dest="${zip%.zip}-extracted"
        mkdir -p "$dest"
        if command -v unzip >/dev/null 2>&1 && unzip -q -o "$zip" -d "$dest"; then
            note "unpacked $(basename "$zip") -> $dest"
        else
            warn "could not unpack $zip (its contents will not be scanned)"
        fi
    done < <(find "$target" -name '*.zip' -type f)
done

if ! command -v "$CLAMSCAN" >/dev/null 2>&1; then
    note "NOT SCANNED: $CLAMSCAN is not installed — the canary did not run."
    warn "ClamAV canary did not run: $CLAMSCAN not found"
    summary "### ClamAV canary: **did not run**"
    summary ""
    summary "\`$CLAMSCAN\` was not available. No antivirus signal was recorded for this build."
    exit 0
fi

note "clamscan: $("$CLAMSCAN" --version 2>/dev/null | head -1)"

"$CLAMSCAN" --recursive --infected --no-summary \
    --max-filesize="$MAX_FILE" --max-scansize="$MAX_SCAN" \
    --log="$REPORT" "${TARGETS[@]}"
rc=$?

echo  # clamscan's match output has no trailing newline

detections=""
[ -f "$REPORT" ] && detections="$(grep -F 'FOUND' "$REPORT" || true)"

case "$rc" in
    0)
        note "=== ClamAV canary: clean (exit 0) ==="
        summary "### ClamAV canary: **clean**"
        summary ""
        summary "Single engine, best effort: it does not reproduce Windows Defender"
        summary "false positives on unsigned PyInstaller builds."
        ;;
    1)
        note "=== ClamAV canary: DETECTIONS FOUND (exit 1) ==="
        note "$detections"
        warn "ClamAV flagged artifact(s) — soft gate, this build is NOT blocked"
        summary "### ClamAV canary: **detection(s) found** (soft gate — not blocking)"
        summary ""
        summary '```'
        summary "$detections"
        summary '```'
        summary ""
        summary "ClamAV is one engine and is prone to false positives on packed/unsigned"
        summary "Python builds. Confirm with the reporting vendor before acting on it."
        ;;
    *)
        note "=== ClamAV canary: did NOT complete (exit $rc) ==="
        warn "ClamAV canary did not complete (exit $rc) — no antivirus signal recorded"
        summary "### ClamAV canary: **did not complete** (exit $rc)"
        ;;
esac

exit 0
