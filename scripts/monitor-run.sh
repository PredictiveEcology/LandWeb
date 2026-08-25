#!/usr/bin/env bash
## Sample CPU/memory while a long run is in progress, so its cost can be recorded rather than
## estimated. Writes a CSV to logs/ (never outputs/ -- see CLAUDE.md).
##
## USAGE (on the compute node running the job):
##   ssh <node> 'cd ~/GitHub/LandWeb && scripts/monitor-run.sh <label> [interval_s]'
## Runs detached; stops on its own once no R processes remain for 5 consecutive samples, so it
## does not outlive the run it is watching.
##
## Columns: iso time, epoch, 1-min load, R process count, summed RSS of R procs (GB),
##          node memory used/total (GB).
set -euo pipefail
cd "$(dirname "$0")/.."

LABEL="${1:-run}"
INTERVAL="${2:-60}"
TS="$(date '+%Y%m%dT%H%M%S')"
OUT="logs/monitor_${LABEL}_${TS}.csv"
mkdir -p logs

echo "time,epoch,load1,n_R,rss_R_gb,mem_used_gb,mem_total_gb" > "$OUT"

idle=0
while true; do
  n=$(pgrep -c '^R$' 2>/dev/null || ps -eo comm | grep -c '^R$' || echo 0)
  ## summed RSS of R processes, in GB
  rss=$(ps -eo comm,rss --no-headers | awk '$1=="R"{s+=$2} END{printf "%.2f", s/1048576}')
  load=$(cut -d' ' -f1 /proc/loadavg)
  read -r used total < <(free -g | awk '/^Mem:/{print $3, $2}')
  printf '%s,%s,%s,%s,%s,%s,%s\n' \
    "$(date -Iseconds)" "$(date +%s)" "$load" "$n" "$rss" "$used" "$total" >> "$OUT"

  if [[ "$n" -eq 0 ]]; then idle=$((idle+1)); else idle=0; fi
  if [[ "$idle" -ge 5 ]]; then
    echo "# no R processes for 5 samples; stopping" >> "$OUT"
    break
  fi
  sleep "$INTERVAL"
done
