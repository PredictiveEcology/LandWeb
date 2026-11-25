#!/usr/bin/env bash
## Run the targets pipeline ON A COMPUTE NODE with the local crew pool, so the coordinator and its
## workers all live on that node and the run survives a control-node reboot or a lost ssh session.
## _targets.R uses the local pool (sized by local$local_workers, capped by the node's cores) whenever
## _hosts.R is absent, so this refuses to run where _hosts.R exists (the control node), where the run
## would instead dispatch to the nodes via crew.ssh.
##
## Launch it as its own systemd user unit, from the project root on the node:
##   scripts/start-unit.sh <tag> scripts/node-tar-make.sh [--shortcut] [--dry-run] [TARGET ...]
## Stop it with `systemctl --user stop landweb-<tag>`. The pipeline runs in this R process
## (callr_function = NULL), so stopping the unit stops the coordinator and the workers together.
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: scripts/node-tar-make.sh [--shortcut] [--dry-run] [TARGET ...]

Builds TARGET(s), or the whole pipeline if none are given, with the local crew pool. Run it on a
compute node, ideally as its own unit: scripts/start-unit.sh <tag> scripts/node-tar-make.sh ...

  TARGET ...   target names; or set LW_TARGETS="name1 name2" (spaces or commas). An unknown name
               is an error before anything is built.
  --shortcut   build only the named targets from the stored upstream values, without checking or
               rebuilding their dependencies (tar_make(shortcut = TRUE)); or set LW_SHORTCUT=1.
               Check that the stored upstream is the one you mean.
  --dry-run    check the target names against the pipeline and print what would run; build nothing.
  -h, --help   show this help.

Logs to logs/node-tar-make_<first target or "all">_<timestamp>.log on this node.
With start-unit.sh, pass the variables through: ENV_PASS="LW_TARGETS LW_SHORTCUT".
EOF
}

shortcut="${LW_SHORTCUT:-0}"
dry_run=0
targets=()
while (($#)); do
  case "$1" in
    -h | --help) usage; exit 0 ;;
    --shortcut) shortcut=1 ;;
    --dry-run) dry_run=1 ;;
    -*) echo "ERROR: unknown option '$1'" >&2; usage >&2; exit 2 ;;
    *) targets+=("$1") ;;
  esac
  shift
done
if ((${#targets[@]} == 0)) && [[ -n "${LW_TARGETS:-}" ]]; then
  read -r -a targets <<<"${LW_TARGETS//,/ }"
fi
if [[ "$shortcut" == "1" ]] && ((${#targets[@]} == 0)); then
  echo "ERROR: --shortcut needs target names" >&2
  exit 2
fi

cd "$(git -C "$(dirname "$0")" rev-parse --show-toplevel)"

if [[ -f _hosts.R ]]; then
  echo "ERROR: _hosts.R is present, so the pipeline would dispatch via crew.ssh, not run on this node." >&2
  echo "  Run this on a compute node, where _hosts.R is absent." >&2
  exit 1
fi

label="${targets[0]:-all}"
[[ "$shortcut" == "1" ]] && label="${label}_shortcut"
label="${label//[^A-Za-z0-9_.-]/_}"
ts="$(date '+%Y%m%dT%H%M%S')"
log="$PWD/logs/node-tar-make_${label}_${ts}.log" # logs/, never outputs/ (CLAUDE.md)

## Target names reach R through the environment, so no name is ever pasted into R code.
export NODE_TAR_MAKE_NAMES="${targets[*]:-}"
export NODE_TAR_MAKE_SHORTCUT="$shortcut"

r_check='
nm <- strsplit(Sys.getenv("NODE_TAR_MAKE_NAMES"), " ", fixed = TRUE)[[1L]]
nm <- nm[nzchar(nm)]
all_nm <- targets::tar_manifest(fields = name, callr_function = NULL)$name
bad <- setdiff(nm, all_nm)
if (length(bad)) stop("not in the pipeline: ", paste(bad, collapse = ", "))
cat("pipeline has", length(all_nm), "targets;", if (length(nm)) paste("would build", paste(nm, collapse = ", ")) else "would build all of them", "\n")
'
r_run='
nm <- strsplit(Sys.getenv("NODE_TAR_MAKE_NAMES"), " ", fixed = TRUE)[[1L]]
nm <- nm[nzchar(nm)]
shortcut <- identical(Sys.getenv("NODE_TAR_MAKE_SHORTCUT"), "1")
message("=== start ", format(Sys.time()), " on ", Sys.info()[["nodename"]], ": ",
        if (length(nm)) paste(nm, collapse = " ") else "all targets", if (shortcut) " (shortcut)")
if (length(nm)) {
  targets::tar_make(names = tidyselect::all_of(nm), shortcut = shortcut,
                    callr_function = NULL, reporter = "timestamp")
} else {
  targets::tar_make(callr_function = NULL, reporter = "timestamp")
}
message("=== done ", format(Sys.time()), " ===")
'

echo "targets:  ${NODE_TAR_MAKE_NAMES:-<all>}"
echo "shortcut: $([[ "$shortcut" == "1" ]] && echo yes || echo no)"
if ((dry_run)); then
  echo "dry run: checking target names; nothing is built"
  exec Rscript-4.6.1 -e "$r_check"
fi

mkdir -p logs
echo "log:      $log"
rc=0
Rscript-4.6.1 -e "$r_run" >"$log" 2>&1 || rc=$?
echo "=== finished $(date -Iseconds), exit=$rc ===" >>"$log"
exit "$rc"
