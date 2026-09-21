#!/usr/bin/env Rscript

## Derive this project's Linux system dependencies from renv.lock and print them, one per
## line, for the workflow to hand to apt.
##
## WHY. The manual workflow installs R packages with `setup-renv`, which restores a
## lockfile and does NOT resolve system requirements -- unlike `setup-r-dependencies`,
## which delegates to pak and installs them itself. The apt list was therefore maintained
## by hand, and every omission surfaced identically: a binary package installs fine, then
## fails to LOAD on a missing shared object several minutes into the job, naming only the
## .so. Deriving the list means it tracks the lockfile instead of drifting from it.
##
## WHAT THIS DOES NOT CATCH -- measured against this lockfile, not assumed:
##
##  * `renv::sysreqs()` reports what a package needs to be BUILT, from the sysreqs
##    database. A P3M *binary* can link a library the database never mentions, because the
##    source build vendored it. git2r is exactly that: it resolves to `libssh2-1-dev`,
##    while its binary links `libgit2.so.1.7`.
##  * the database is simply incomplete for some packages: it gives `textshaping` only
##    `libfreetype6-dev` (a source build also wants harfbuzz and fribidi) and `fs` only
##    `cmake` (fs 2.x also wants libuv).
##  * 39 of this lockfile's 441 entries are GitHub sources the database does not know, so
##    they contribute nothing and warn.
##
## Hence EXTRA_SYSREQS. Every entry names the package and the symptom, so a later reader
## can tell a hand-added entry from a derived one and knows what to re-test before
## dropping it.

EXTRA_SYSREQS <- c(
  ## git2r: binary links libgit2.so.1.7; DB lists only libssh2-1-dev (source vendors it)
  "libgit2-dev",
  ## textshaping: DB lists only libfreetype6-dev
  "libfribidi-dev",
  "libharfbuzz-dev",
  ## fs >= 2.0: DB lists only cmake
  "libuv1-dev",
  ## sf/terra: normally pulled in by libgdal-dev, named explicitly so a GDAL packaging
  ## change cannot quietly remove them
  "libproj-dev",
  "libsqlite3-dev"
)

## The list is written to a FILE, not stdout. renv puts its autoloader notice ("the
## project is out-of-sync") and its [n/N] progress counter on stdout, and the counter is
## backspace-rewritten, so the first package name ends up appended to it: capturing stdout
## yields a line of progress junk with `cmake` glued to the end, and apt then fails on the
## junk while silently missing the package. Measured, not hypothetical.
args <- commandArgs(trailingOnly = TRUE)
lockfile <- if (length(args) >= 1L) args[[1]] else "renv.lock"
outfile <- if (length(args) >= 2L) args[[2]] else "sysreqs.txt"
stopifnot(file.exists(lockfile))

pkgs <- names(renv::lockfile_read(lockfile)$Packages)

## check = FALSE: report what is REQUIRED rather than what is missing locally -- the runner
## starts bare. report = FALSE: return the data, do not print "sudo apt install" lines,
## which would otherwise land on stdout and be parsed as package names.
reqs <- suppressWarnings(
  renv::sysreqs(packages = pkgs, check = FALSE, report = FALSE)
)

## Each non-NULL entry is list(packages = <apt names>, constraints = <os/distro>). Take
## `packages` only: unlist()ing a whole entry also yields "linux"/"ubuntu"/"debian", which
## are not installable and which apt would choke on.
derived <- unlist(lapply(reqs, `[[`, "packages"), use.names = FALSE)
derived <- unique(derived[nzchar(derived)])

apt <- sort(union(derived, EXTRA_SYSREQS))

stopifnot(all(grepl("^[a-z0-9][a-z0-9.+-]*$", apt)))  ## nothing but apt package names
writeLines(apt, outfile)

message(sprintf(
  "sysreqs: %d apt packages from %d lockfile entries (%d derived, %d hand-added) -> %s",
  length(apt), length(pkgs), length(derived), length(setdiff(EXTRA_SYSREQS, derived)), outfile
))
