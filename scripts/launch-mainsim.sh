#!/usr/bin/env bash
## Launch a LandWeb targets build in a detached `screen` session ON A COMPUTE NODE
## (e.g. pinus), using the LOCAL crew fallback (no `_hosts.R` on the node). Because the
## tar_make coordinator AND its crew workers then both live on the node, the run survives
## a CONTROLLER (larix) reboot / SSH disconnect -- nothing about it lives on larix.
##
## USAGE (run on the node itself, or over ssh from the controller):
##   ssh pinus 'cd ~/GitHub/LandWeb && scripts/launch-mainsim.sh [TARGET]'
##   TARGET defaults to `mainSim_WesternAlbertaUpland`; pass any target name to build it
##   (its upstream deps build first; completed/stored targets are skipped).
##
## MONITOR / RESUME:
##   screen -ls                         # list sessions
##   screen -r <SCREEN_NAME>            # attach (Ctrl+a d to detach again)
##   tail -F <the log path printed below>
##
## STOP -- IMPORTANT: `tar_make()` wraps the pipeline in a `callr` child process, which does
## NOT die when you `screen quit` (it holds the `_targets/meta/process` lock). To hard-stop:
##   1. screen -X -S <SCREEN_NAME> quit
##   2. ps -u "$USER" -o pid,etime,cmd | grep -E 'callr|exec/R' | grep -v grep   # find the child
##   3. kill -TERM <pid>
##   4. (if a later launch still complains about the lock)
##      Rscript-4.6.1 -e 'targets::tar_unblock_process(store = "_targets")'
set -euo pipefail

cd "$(dirname "$0")/.."
PROJECT_DIR="$(pwd)"

## Must run where the local crew fallback applies (no _hosts.R). On the controller (larix)
## _hosts.R is present and tar_make would dispatch via crew.ssh instead -- refuse there so a
## controller reboot can't orphan the run.
if [[ -f _hosts.R ]]; then
  echo "ERROR: _hosts.R is present -> tar_make would dispatch via crew.ssh (not node-local)." >&2
  echo "  Run this ON a compute node (e.g. pinus), where _hosts.R is absent." >&2
  exit 1
fi

TARGET="${1:-mainSim_WesternAlbertaUpland}"
SCREEN_NAME="landweb_${TARGET}"
TS="$(date '+%Y%m%dT%H%M%S')"
LOG="${PROJECT_DIR}/logs/${SCREEN_NAME}_${TS}.log"   # logs live in logs/, never outputs/ (see CLAUDE.md)
mkdir -p "$(dirname "$LOG")"

if screen -ls 2>/dev/null | grep -q "[.]${SCREEN_NAME}[[:space:]]"; then
  echo "ERROR: screen session '${SCREEN_NAME}' already exists -- resume or kill it first:" >&2
  echo "  screen -r ${SCREEN_NAME}         # resume" >&2
  echo "  screen -X -S ${SCREEN_NAME} quit # kill the screen (then kill the callr child; see header)" >&2
  exit 1
fi

R_SCRIPT="
  setwd('${PROJECT_DIR}')
  message('=== ${SCREEN_NAME} launch ${TS} on ', Sys.info()[['nodename']], ' ===')
  targets::tar_make(names = tidyselect::any_of('${TARGET}'), reporter = 'verbose')
  message('=== ${SCREEN_NAME} done ${TS} ===')
"

echo "host:   $(hostname)"
echo "target: ${TARGET}"
echo "screen: ${SCREEN_NAME}"
echo "log:    ${LOG}"
screen -dmS "${SCREEN_NAME}" bash -c "Rscript-4.6.1 -e \"${R_SCRIPT}\" > '${LOG}' 2>&1"
sleep 1
if screen -ls 2>/dev/null | grep -q "[.]${SCREEN_NAME}[[:space:]]"; then
  echo "launched. monitor with:  tail -F ${LOG}    (resume: screen -r ${SCREEN_NAME})"
else
  echo "ERROR: screen session failed to start; check ${LOG}" >&2
  exit 1
fi
