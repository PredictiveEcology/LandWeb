#!/usr/bin/env Rscript
## sync-nodes.R -- TEMPLATE shipped with crew.ssh. Copy into your project (e.g.
## scripts/sync-nodes.R):
##   file.copy(system.file("templates/sync-nodes.R", package = "crew.ssh"), "scripts/sync-nodes.R")
##
## Brings the project up to date on every worker node listed in _hosts.R, in
## PARALLEL. Project-AGNOSTIC: all project specifics (hosts, project dir, branch,
## symlinks, Rscript) come from _hosts.R (see the project-setup vignette), so this
## script is copied unchanged across projects. Assumes the project uses git (+
## submodules) and renv.
##
## Per node, over SSH: fast-forward the configured git branch, update submodules,
## (re)create the configured symlinks, and renv::restore(). Non-destructive
## (reports FAILED rather than resetting); supports --dry-run.
##
## PARALLELISM + the renv cache. When RENV_PATHS_CACHE is on a shared (networked)
## filesystem, all nodes that share an OS *codename* also share one set of compiled
## packages: renv keys its cache by codename (.../v5/linux-<id>-<codename>/...).
## Restoring N such nodes cold and concurrently would have them all compile the
## same missing packages at once -- redundant work plus contention on the shared
## cache. So sync runs in two phases, grouped by codename:
##   1. WARM: the FIRST node of each codename group (in _hosts.R order) restores
##      first, on its own, populating the shared cache for that codename.
##   2. FAN-OUT: the remaining nodes of each group restore in parallel, now just
##      linking the warm cache instead of recompiling.
## Distinct codename groups warm concurrently (separate cache subtrees, so no
## contention between them).
##
## The CONTROL node is preflighted too: any worker group whose codename MATCHES the
## control node is assumed already warmed by the control node's own renv library
## (it shares the same cache subtree), so those workers skip the warm step and fan
## out immediately. If that cache is in fact incomplete, those nodes simply compile
## in parallel -- no worse than a cold run.
##
## Run from the PROJECT ROOT with the project's R (renv + jsonlite available):
##   Rscript scripts/sync-nodes.R            # do it
##   Rscript scripts/sync-nodes.R --dry-run  # print the plan + remote script, change nothing
##   Rscript scripts/sync-nodes.R --force    # fan a group out even if its warm node failed
##
## NOTE: each node needs git access to any (private) submodules. Parallelism uses
## base `parallel::mclapply` (fork); on Windows it degrades to sequential.

args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args
force <- "--force" %in% args

if (!file.exists("_hosts.R")) {
  stop(
    "_hosts.R not found. Run from the project root; copy _hosts.R.example to _hosts.R first.",
    call. = FALSE
  )
}
source("_hosts.R")

nodes <- getOption("crew.ssh.nodes")
projdir <- getOption("crew.ssh.projdir")
if (is.null(nodes) || is.null(projdir)) {
  stop("set crew.ssh.nodes and crew.ssh.projdir in _hosts.R", call. = FALSE)
}
hosts <- names(nodes)

## git branch to sync (default: the control node's current branch)
branch <- getOption("crew.ssh.branch")
if (is.null(branch)) {
  branch <- tryCatch(
    system2("git", c("rev-parse", "--abbrev-ref", "HEAD"), stdout = TRUE),
    error = function(e) "main"
  )
}

## R version on the nodes: single source of truth = renv.lock (-> Rscript-<version>);
## override with crew.ssh.rscript.
r_version_from_lock <- function(lock = "renv.lock") {
  if (!file.exists(lock)) {
    return(NULL)
  }
  tryCatch(jsonlite::read_json(lock)$R$Version, error = function(e) NULL)
}
rscript <- getOption("crew.ssh.rscript")
if (is.null(rscript)) {
  version <- r_version_from_lock()
  if (is.null(version)) {
    stop(
      "could not read the R version from renv.lock; set crew.ssh.rscript in _hosts.R",
      call. = FALSE
    )
  }
  rscript <- sprintf("Rscript-%s", version)
}

## symlinks to (re)create in projdir on each node: named character vector
## linkname -> target. Optional (NULL -> skip).
symlinks <- getOption("crew.ssh.symlinks")
link_cmds <- if (length(symlinks)) {
  paste(sprintf("ln -sfn %s %s", unname(symlinks), names(symlinks)), collapse = "\n")
} else {
  "true"
}

remote_script <- sprintf(
  paste(
    "set -e",
    "cd %s",
    "echo '[git] fetch + fast-forward %s'",
    "git fetch --quiet origin %s",
    "git checkout --quiet %s",
    "git merge --ff-only --quiet origin/%s",
    "echo '[git] submodules'",
    "git submodule sync --quiet --recursive",
    "git submodule update --init --recursive --quiet",
    "echo '[symlinks]'",
    "%s",
    "echo '[renv] restore (R %s)'",
    "%s -e 'renv::restore(prompt = FALSE)'",
    "echo '[done]'",
    sep = "\n"
  ),
  shQuote(projdir),
  branch,
  branch,
  branch,
  branch,
  link_cmds,
  sub("^Rscript-?", "", rscript),
  rscript
)

ssh_opts <- c("-o", "BatchMode=yes", "-o", "ConnectTimeout=10")

## --- helpers -----------------------------------------------------------------

## Resolve the renv-cache-relevant OS codename of a host (h = NULL means the local
## control node). renv keys its package cache by codename, so nodes that share a
## codename AND a shared renv cache also share compiled packages. Prefer
## `lsb_release`; fall back to /etc/os-release. Returns a single string.
codename_cmd <- paste(
  'cn="$(lsb_release -cs 2>/dev/null)";',
  '[ -n "$cn" ] || cn="$(. /etc/os-release 2>/dev/null; echo "$VERSION_CODENAME")";',
  'echo "${cn:-unknown}"'
)
host_codename <- function(h) {
  out <- suppressWarnings(
    if (is.null(h)) {
      system2("bash", "-s", input = codename_cmd, stdout = TRUE, stderr = FALSE)
    } else {
      system2("ssh", c(ssh_opts, h, "bash -s"), input = codename_cmd, stdout = TRUE, stderr = FALSE)
    }
  )
  st <- attr(out, "status")
  if (!is.null(st) && st != 0L) {
    return(if (is.null(h)) "unknown" else "unreachable")
  }
  out <- out[nzchar(out)]
  if (length(out)) out[length(out)] else "unknown"
}

## Run the full provisioning script on one host over SSH, capturing combined
## output + exit status (so parallel runs do not interleave on the terminal).
run_sync <- function(h) {
  out <- suppressWarnings(system2(
    "ssh",
    c(ssh_opts, h, "bash -s"),
    input = remote_script,
    stdout = TRUE,
    stderr = TRUE
  ))
  st <- attr(out, "status")
  list(host = h, status = if (is.null(st)) 0L else st, output = out)
}

## Run run_sync() across hosts concurrently (one fork per host; I/O-bound on SSH).
run_parallel <- function(hs) {
  if (!length(hs)) {
    return(list())
  }
  res <- parallel::mclapply(hs, run_sync, mc.cores = length(hs), mc.preschedule = FALSE)
  res <- lapply(seq_along(hs), function(i) {
    r <- res[[i]]
    if (is.list(r) && !is.null(r$status)) {
      r
    } else {
      list(host = hs[i], status = 1L, output = "local error launching ssh for this host")
    }
  })
  setNames(res, hs)
}

## Print one host's captured output as a labelled block.
emit <- function(r) {
  cat(sprintf("\n===== %s =====\n", r$host))
  if (length(r$output)) {
    cat(r$output, sep = "\n")
    cat("\n")
  }
  if (!is.na(r$status) && r$status != 0L) {
    cat(sprintf("!! %s FAILED (exit %d)\n", r$host, r$status))
  }
}

## --- preflight: group nodes by OS codename ----------------------------------

cat(sprintf("Syncing %d node(s): %s\n", length(hosts), paste(hosts, collapse = ", ")))
cat(sprintf("Branch: %s | remote Rscript: %s\n", branch, rscript))
cat("\n[preflight] resolving OS codenames (read-only SSH) ...\n")

control_codename <- host_codename(NULL)
worker_codenames <- unlist(
  parallel::mclapply(hosts, host_codename, mc.cores = max(1L, length(hosts))),
  use.names = FALSE
)
names(worker_codenames) <- hosts

## groups: codename -> hosts (preserving _hosts.R order within each group)
group_keys <- unique(worker_codenames)
groups <- lapply(group_keys, function(k) hosts[worker_codenames == k])
names(groups) <- group_keys

## Partition into (a) warm nodes [first of each non-control-codename group],
## (b) prewarmed nodes [groups matching the control node], and (c) gated fan-out
## [the rest of each warmed group, keyed by their warm node].
warm_hosts <- character(0)
prewarmed <- character(0)
gated <- list()
for (k in group_keys) {
  g <- groups[[k]]
  if (identical(k, control_codename)) {
    prewarmed <- c(prewarmed, g)
  } else {
    warm_hosts <- c(warm_hosts, g[1L])
    if (length(g) > 1L) {
      gated[[g[1L]]] <- g[-1L]
    }
  }
}

cat("\n[plan] OS codename grouping:\n")
cat(sprintf("  control node : %s\n", control_codename))
for (k in group_keys) {
  g <- groups[[k]]
  if (identical(k, control_codename)) {
    cat(sprintf(
      "  %-12s pre-warmed by control node -> fan out (parallel): %s\n",
      k,
      paste(g, collapse = ", ")
    ))
  } else {
    rest <- g[-1L]
    cat(sprintf(
      "  %-12s warm: %s%s\n",
      k,
      g[1L],
      if (length(rest)) {
        sprintf(" -> fan out (parallel): %s", paste(rest, collapse = ", "))
      } else {
        ""
      }
    ))
  }
}

if (dry_run) {
  cat("\n--- DRY RUN: remote script that WOULD run on each host ---\n")
  cat(remote_script, "\n")
  quit(save = "no")
}

## --- phase 1: warm + control-codename fan-out (parallel) --------------------

batchA_hosts <- c(warm_hosts, prewarmed)
cat(sprintf(
  "\n[phase 1] warm + control-codename fan-out: %s\n",
  if (length(batchA_hosts)) paste(batchA_hosts, collapse = ", ") else "(none)"
))
resA <- run_parallel(batchA_hosts)
for (h in batchA_hosts) {
  emit(resA[[h]])
}

## --- phase 2: fan out the rest of each warmed group (parallel) ---------------

eligible <- character(0)
skipped <- list()
for (w in names(gated)) {
  rest <- gated[[w]]
  if (force || isTRUE(resA[[w]]$status == 0L)) {
    eligible <- c(eligible, rest)
  } else {
    for (h in rest) {
      skipped[[h]] <- list(
        host = h,
        status = NA_integer_,
        output = sprintf(
          "SKIPPED -- warm node %s failed (exit %s); fix it and rerun, or pass --force.",
          w,
          resA[[w]]$status
        )
      )
    }
  }
}

cat(sprintf(
  "\n[phase 2] fan-out: %s\n",
  if (length(eligible)) paste(eligible, collapse = ", ") else "(none)"
))
resB <- run_parallel(eligible)
for (h in eligible) {
  emit(resB[[h]])
}
for (h in names(skipped)) {
  emit(skipped[[h]])
}

## --- summary -----------------------------------------------------------------

all_res <- c(resA, resB, skipped)
status_label <- function(h) {
  r <- all_res[[h]]
  if (is.null(r)) {
    return("MISSING")
  }
  if (is.na(r$status)) {
    "SKIPPED"
  } else if (r$status == 0L) {
    "OK"
  } else {
    "FAILED"
  }
}

cat("\n===== summary =====\n")
for (h in hosts) {
  cat(sprintf("%-14s %s\n", h, status_label(h)))
}
if (!all(vapply(hosts, function(h) status_label(h) == "OK", logical(1L)))) {
  quit(save = "no", status = 1L)
}
