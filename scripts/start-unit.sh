#!/usr/bin/env bash
## Run a script as its own systemd user unit on this host, so it survives the ssh session and the IDE's
## memory limits, from the project root, logging to logs/<prefix><tag>.log with an exit marker. --collect
## lets a failed unit's name be reused. UNIT_PREFIX (default landweb-) names the unit.
## Usage: [UNIT_PREFIX=lw-] scripts/start-unit.sh <tag> <script> [args...]
## Environment variables the script needs must be passed with ENV_PASS="NAME1 NAME2".
set -euo pipefail
tag=$1
script=$2
shift 2
prefix=${UNIT_PREFIX:-landweb-}
proj=$(git -C "$(dirname "$0")" rev-parse --show-toplevel)
cd "$proj"
mkdir -p logs
log="$proj/logs/$prefix$tag.log"
envs=()
for v in ${ENV_PASS:-}; do
  envs+=(--setenv="$v=${!v}")
done
cmd="bash $(printf '%q ' "$script" "$@")> '$log' 2>&1; rc=\$?; echo \"=== finished \$(date -Iseconds), exit=\$rc ===\" >> '$log'"
systemd-run --user --collect --unit="$prefix$tag" --working-directory="$proj" "${envs[@]}" /usr/bin/bash -lc "$cmd"
echo "unit $prefix$tag, log $log"
