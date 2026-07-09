#!/usr/bin/env bash
#
# Regenerate the "Compiler CI status" table in README.md from the latest
# `master` builds on https://ci.zw3rk.com.
#
# The table reflects the `roots.ghc` Hydra jobs (i.e. GHC itself) for each
# compiler and platform.  cache.zw3rk.com holds the outputs of these builds, so
# a ✅ here means that compiler/platform is cached.
#
# Rather than downloading a whole evaluation (thousands of jobs, a heavy and
# flaky request), we query each job individually via
#   /job/<project>/<jobset>/<job>/latest-finished
# which cheaply redirects to that job's most recent finished build.  A 404
# means the job does not exist for that pin/host/target/compiler and is simply
# left blank in the table.
#
# The set of columns (COMPILERS) and rows (PLATFORMS) below is the CI matrix we
# advertise.  When a GHC version, cross target, or nixpkgs pin is added to or
# removed from CI (see ci.nix and docs/reference/supported-ghc-versions.md),
# update these lists to match.  Per-compiler gaps within a platform (e.g. no
# wasm build for an older GHC) do not need listing here — they are discovered
# automatically because the corresponding job 404s.
#
# Only README.md is touched, and only when the rendered table differs from
# what is already there — so running this on an unchanged CI status is a no-op
# and produces no commit.
#
# Requires: bash, curl, jq (>= 1.7).  No Nix evaluation.

set -euo pipefail

HYDRA="${HYDRA:-https://ci.zw3rk.com}"
PROJECT="${PROJECT:-input-output-hk-haskell-nix}"
JOBSET="${JOBSET:-master}"
PARALLEL="${PARALLEL:-12}"

# Compiler columns, in the resolved names Hydra uses (left to right).
COMPILERS=(ghc967 ghc984 ghc9103 ghc9124 ghc9141 ghc9141llvm)

# Table rows, one per "<nixpkgs-pin> <build-host-system> <target>".  `native`
# means "no cross compilation".  All other targets are cross targets built on
# the given host.
PLATFORMS=(
  # nixpkgs `unstable`: full cross matrix
  "unstable x86_64-linux native"
  "unstable x86_64-linux static"
  "unstable x86_64-linux musl64"
  "unstable x86_64-linux musl32"
  "unstable x86_64-linux aarch64-multiplatform"
  "unstable x86_64-linux aarch64-multiplatform-musl"
  "unstable x86_64-linux aarch64-android-prebuilt"
  "unstable x86_64-linux armv7a-android-prebuilt"
  "unstable x86_64-linux ghcjs"
  "unstable x86_64-linux wasi32"
  "unstable x86_64-linux mingwW64"
  "unstable x86_64-linux ucrt64"
  "unstable aarch64-darwin native"
  "unstable aarch64-darwin ghcjs"
  "unstable aarch64-darwin wasi32"
  "unstable x86_64-darwin native"
  "unstable x86_64-darwin ghcjs"
  # nixpkgs `R2511`: native only
  "R2511 x86_64-linux native"
  "R2511 aarch64-darwin native"
  "R2511 x86_64-darwin native"
)

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo="$(cd "$here/.." && pwd)"
readme="$repo/README.md"
jq_prog="$here/ci-status.jq"

start_marker="<!-- CI-STATUS:START -->"
end_marker="<!-- CI-STATUS:END -->"

log() { printf '%s\n' "$*" >&2; }

# Query one job's latest finished build.  Prints a compact JSON object
# {job,buildstatus,finished} on success, nothing if the job does not exist
# (404) or cannot be fetched.  Transient 5xx/network errors are retried;
# 404 fails fast (curl -f, without --retry-all-errors, does not retry it).
probe_job() {
  local name="$1" json
  json="$(curl -fsSL -H 'Accept: application/json' \
            --retry 3 --retry-delay 5 --connect-timeout 20 --max-time 60 \
            "$HYDRA/job/$PROJECT/$JOBSET/$name/latest-finished" 2>/dev/null)" || return 0
  printf '%s' "$json" | jq -c '{job, buildstatus, finished}' 2>/dev/null || return 0
}
export -f probe_job
export HYDRA PROJECT JOBSET

# 1. Build the list of candidate job names (row x compiler).
candidates=()
for row in "${PLATFORMS[@]}"; do
  read -r pin system target <<<"$row"
  for compiler in "${COMPILERS[@]}"; do
    candidates+=("$system.$pin.$compiler.$target.roots.ghc")
  done
done
log "Probing ${#candidates[@]} candidate roots.ghc jobs on $HYDRA (parallel=$PARALLEL) ..."

# 2. Probe them in parallel and collect the ones that exist into a JSON array.
# SC2016: the single quotes are intentional — $1 must expand in the child
# shell that xargs starts (fed by `_ {}`), not in this parent shell.
# shellcheck disable=SC2016
builds="$(printf '%s\n' "${candidates[@]}" \
  | xargs -P "$PARALLEL" -I{} bash -c 'probe_job "$1"' _ {} \
  | jq -s '.')"

found="$(printf '%s' "$builds" | jq 'length')"
log "Found $found existing roots.ghc builds."
if [ "${found:-0}" -lt 3 ]; then
  log "ERROR: too few builds found ($found); Hydra may be unreachable. Refusing to update."
  exit 1
fi

# 3. Warn about any advertised platform that returned no builds at all — this
#    usually means a renamed pin/host/target that needs fixing above.
found_jobs="$(printf '%s' "$builds" | jq -r '.[].job')"
for row in "${PLATFORMS[@]}"; do
  read -r pin system target <<<"$row"
  if ! printf '%s\n' "$found_jobs" \
       | grep -qE "^${system}\.${pin}\..*\.${target}\.roots\.ghc$"; then
    log "WARNING: advertised platform '$pin $system $target' returned no builds (renamed/removed?)."
  fi
done

# 4. Render the Markdown table.
table="$(printf '%s' "$builds" | jq -r -f "$jq_prog")"

# 5. Splice the table between the markers in README.md.
if ! grep -qF "$start_marker" "$readme" || ! grep -qF "$end_marker" "$readme"; then
  log "ERROR: markers not found in $readme."
  log "Add the following where the table should appear:"
  log "  $start_marker"
  log "  $end_marker"
  exit 1
fi

block="$(cat <<EOF
$start_marker
<!-- This table is generated by scripts/update-ci-status.sh from the latest
     master builds on ci.zw3rk.com. Do not edit it by hand. -->

$table
$end_marker
EOF
)"

tmp="$(mktemp)"
trap 'rm -f "$tmp"' EXIT
# Replace everything between (and including) the markers with the new block.
BLOCK="$block" awk -v s="$start_marker" -v e="$end_marker" '
  index($0, s) { print ENVIRON["BLOCK"]; skip = 1; next }
  index($0, e) { skip = 0; next }
  !skip { print }
' "$readme" > "$tmp"

if cmp -s "$tmp" "$readme"; then
  log "CI status table unchanged; nothing to do."
  exit 0
fi

mv "$tmp" "$readme"
trap - EXIT
log "README.md updated."
