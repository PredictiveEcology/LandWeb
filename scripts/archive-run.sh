#!/usr/bin/env bash
## Archive a finished run before the next one overwrites it: each study area's outputs/<sa>/{mainSim,
## postprocess,logs,dataPrep} to outputs/_reference/run_<label>/<sa>/, the stage values to <sa>/state/,
## and the targets metadata, shared growth traits, run notes, the run commits' renv.lock files, the run's
## tar_make logs and this host's library manifest to _state/.
## archive-guard.R (next to this script) must pass for every study area before anything is copied. Everything is
## copied to run_<label>.partial (a rerun resumes it), verified by checksum, then renamed; ARCHIVE_OK
## (md5s of the state files) is written last.
## Usage, on a compute node, from the project root, as its own unit (scripts/start-unit.sh):
##   RUN_NOTE="..." RUN_COMMITS="<sha> ..." RUN_LOGS="logs/<file> ..." \
##     scripts/archive-run.sh <label> [<study area> ...]
## With no study areas it archives every one with an outputs/<sa>/mainSim folder.
## ARCHIVE_PARTS overrides the parts copied; ARCHIVE_SINCE is passed to the guard.
set -euo pipefail
label=$1
shift
here=$(cd "$(dirname "$0")" && pwd)
cd "$(git -C "$here" rev-parse --show-toplevel)"
r() { Rscript-4.6.1 "$@" 2>&1 | { /usr/bin/grep -v -E "out-of-sync|renv::status" || true; } || true; }
out=outputs
dst="$out/_reference/run_${label}"
part="$dst.partial"
parts=${ARCHIVE_PARTS:-"mainSim postprocess logs dataPrep"}
if [ $# -gt 0 ]; then
  sas="$*"
else
  sas=$(cd "$out" && ls -d */mainSim 2>/dev/null | cut -d/ -f1 | /usr/bin/grep -v '^_' | tr '\n' ' ' || true)
fi
[ -n "$sas" ] || { echo "REFUSING: no study areas"; exit 1; }
echo "[archive] $label: $(echo $sas | wc -w) study areas, from $(git rev-parse --short HEAD), $(date -Iseconds)"
[ ! -e "$dst" ] || { echo "REFUSING: $dst exists"; exit 1; }

guard=$(r "$here/archive-guard.R" $sas)
echo "$guard"
echo "$guard" | /usr/bin/grep -q '^GUARD OK' || { echo "REFUSING: guard failed"; exit 1; }

mkdir -p "$part/_state"
for log in ${RUN_LOGS:-}; do
  cp -p "$log" "$part/_state/"
done
for c in ${RUN_COMMITS:-}; do
  git show "$c:renv.lock" > "$part/_state/renv.lock.$(git rev-parse --short "$c")"
done
{
  echo "label: $label"
  echo "archived at $(git rev-parse HEAD), $(date -Iseconds)"
  echo "run commits: ${RUN_COMMITS:-unrecorded}"
  echo "${RUN_NOTE:-}"
} > "$part/_state/README.txt"
r "$here/library-manifest.R" "$part/_state/library_at_archive.csv"

for sa in $sas; do
  mkdir -p "$part/$sa"
  for p in $parts; do
    [ -d "$out/$sa/$p" ] || { echo "FAILED: no $out/$sa/$p"; exit 1; }
    rsync -a --delete "$out/$sa/$p/" "$part/$sa/$p/"
    n=$(rsync -rlcn --delete --out-format='%n' "$out/$sa/$p/" "$part/$sa/$p/" | { /usr/bin/grep -v '/$' || true; } | wc -l)
    echo "$sa/$p: $(find "$part/$sa/$p" -type f | wc -l) files, $n differ"
    [ "$n" -eq 0 ] || { echo "FAILED: $sa/$p copy differs"; exit 1; }
  done
done

res=$(r "$here/archive-state.R" "$part" $sas)
echo "$res"
echo "$res" | /usr/bin/grep -q '^state saved$' || { echo "FAILED: state not saved"; exit 1; }

mv -T "$part" "$dst"
( cd "$dst" && find _state */state -type f | sort | xargs md5sum > ARCHIVE_OK.tmp && mv ARCHIVE_OK.tmp ARCHIVE_OK )
du -sh "$dst"
echo "ARCHIVE OK $label $(date -Iseconds)"
