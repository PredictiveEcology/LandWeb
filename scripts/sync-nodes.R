#!/usr/bin/env Rscript
## sync-nodes.R -- bring the project up to date on every worker node in _hosts.R.
##
## Project-AGNOSTIC: every project specific (hosts, project dir, branch, symlinks,
## Rscript) comes from _hosts.R (see _hosts.R.example), so this script can be
## copied unchanged into any project that uses crew.ssh.
##
## Per node, over SSH: fast-forward the configured git branch, update submodules,
## (re)create the configured symlinks, and renv::restore() (which installs/updates
## whatever renv.lock specifies, including local packages once snapshotted).
## Non-destructive (reports FAILED rather than resetting); supports --dry-run.
##
## Run from the PROJECT ROOT with the project's R (renv + jsonlite available):
##   Rscript scripts/sync-nodes.R            # do it
##   Rscript scripts/sync-nodes.R --dry-run  # print the remote script, change nothing
##
## NOTE: each node needs git access to any (private) submodules.

args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args

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

## extra renv profiles to restore on each node, beyond the default (named character vector, e.g.
## "landr"). Each is restored from its versioned renv/profiles/<p>/renv.lock; fast because packages
## symlink from the shared renv cache. RENV_PROFILE is set BEFORE R starts so renv/activate.R (sourced
## by .Rprofile) picks up the right profile; .Rprofile package preloads are requireNamespace-guarded.
extra_profiles <- getOption("crew.ssh.renv_profiles")
profile_cmds <- if (length(extra_profiles)) {
  paste(
    sprintf(
      "echo '[renv] restore profile: %s'\nRENV_PROFILE=%s %s -e 'renv::restore(prompt = FALSE)'",
      extra_profiles,
      extra_profiles,
      rscript
    ),
    collapse = "\n"
  )
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
    "git submodule update --init --recursive --quiet",
    "echo '[symlinks]'",
    "%s",
    "echo '[renv] restore default profile (R %s)'",
    "%s -e 'renv::restore(prompt = FALSE)'",
    "%s",
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
  rscript,
  profile_cmds
)

ssh_opts <- c("-o", "BatchMode=yes", "-o", "ConnectTimeout=10")

cat(sprintf("Syncing %d node(s): %s\n", length(hosts), paste(hosts, collapse = ", ")))
cat(sprintf("Branch: %s | remote Rscript: %s\n", branch, rscript))
if (dry_run) {
  cat("\n--- DRY RUN: remote script that WOULD run on each host ---\n")
  cat(remote_script, "\n")
  quit(save = "no")
}

results <- vapply(
  hosts,
  function(h) {
    cat(sprintf("\n===== %s =====\n", h))
    ## Pipe the script to a remote `bash -s` via stdin instead of cramming it into
    ## argv: avoids the dash-vs-bash ambiguity (the remote login shell may differ
    ## from the user's interactive shell) AND the multi-line-argv quirk where the
    ## multi-line argument gets re-interpreted by the local shell.
    status <- system2("ssh", c(ssh_opts, h, "bash -s"), input = remote_script)
    if (status != 0L) {
      cat(sprintf("!! %s FAILED (exit %d)\n", h, status))
    }
    status
  },
  integer(1L)
)

cat("\n===== summary =====\n")
for (h in hosts) {
  cat(sprintf("%-14s %s\n", h, if (results[[h]] == 0L) "OK" else "FAILED"))
}
if (any(results != 0L)) {
  quit(save = "no", status = 1L)
}
