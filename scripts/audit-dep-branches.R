#!/usr/bin/env Rscript

## Flag any GitHub dependency this project requests at two DIFFERENT COMMITS.
##
## `pak` solves a dependency request as a single problem, so one repo wanted at two branches makes
## the WHOLE solve fail -- and it then reports every other package as a "dependency conflict",
## which reads like a far bigger problem than it is. Read past that list to the
## `Can't find package` / `Conflicts with` lines, or just run this, which finds the class in one
## pass. Two real instances (2026-08-20):
##
##   * Biomass_speciesParameters asked for "PredictiveEcology/LandR" with no branch (so `main`)
##     while its own dependency ianmseddy/PSPclean@development requires LandR@development;
##   * LandWebUtils + LandWeb_preamble pinned FOR-CAST/workflowtools@development (0.0.13) while
##     burnSummaries requires >= 0.0.16, which existed only on `main`.
##
## Two things this gets right that a naive version does not:
##
##   * it reads each dependency's DESCRIPTION on the branch ACTUALLY REQUESTED, not the repo
##     default. LandWebUtils' `main` lists `LandR@LandWeb`, but `development` -- the branch
##     everything installs -- has no LandR remote at all, so reading defaults invents conflicts.
##   * it resolves branch names to COMMITS before comparing, so two names that point at the same
##     commit (e.g. after a fast-forward) are reported as agreeing, not as a conflict.
##
## Pure base R + the `gh` CLI (authenticated); no package dependencies, so CI needs no install
## step. Exits 1 if any real conflict is found, so it can gate a workflow.

`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a

## branch may be "" (meaning "the repo default"), which %||% deliberately does NOT treat as absent
orDefault <- function(branch, fallback) if (nzchar(branch)) branch else fallback

## system2() hands the argument vector to a shell, so anything containing a space (the Accept
## header below) has to be quoted or it splits into two arguments.
gh <- function(...) {
  out <- suppressWarnings(system2("gh", shQuote(c(...)), stdout = TRUE, stderr = FALSE))
  if (!is.null(attr(out, "status")) && attr(out, "status") != 0L) return("")
  paste(out, collapse = "\n")
}

.descCache <- new.env(parent = emptyenv())
.shaCache <- new.env(parent = emptyenv())
## An audit that cannot see a dependency must NOT report "clean" -- a silent false pass is worse
## than no audit at all. Anything unreadable is collected here and makes the run inconclusive.
.unreadable <- new.env(parent = emptyenv())

## DESCRIPTION of `repo` on `branch` (NULL/"" = repo default), as a single string.
remoteDescription <- function(repo, branch) {
  key <- paste0(repo, "@", branch)
  if (!is.null(.descCache[[key]])) return(.descCache[[key]])
  url <- paste0("repos/", repo, "/contents/DESCRIPTION",
                if (nzchar(branch)) paste0("?ref=", branch) else "")
  ## The `raw` media type returns the file's text directly, so this needs no base64 decoder and
  ## therefore no extra package -- the script stays pure base R + the `gh` CLI, which is what
  ## lets it run in CI without an install step.
  txt <- gh("api", url, "-H", "Accept: application/vnd.github.raw")
  if (!nzchar(txt)) assign(key, TRUE, envir = .unreadable)
  .descCache[[key]] <- txt
  txt
}

## The commit a branch name points at -- this is what we actually compare.
remoteSha <- function(repo, branch) {
  key <- paste0(repo, "@", branch)
  if (!is.null(.shaCache[[key]])) return(.shaCache[[key]])
  br <- orDefault(branch, gh("repo", "view", repo, "--json", "defaultBranchRef",
                            "--jq", ".defaultBranchRef.name"))
  sha <- substr(gh("api", paste0("repos/", repo, "/commits/", br), "--jq", ".sha"), 1L, 12L)
  if (!nzchar(sha)) assign(paste0(key, " (commit)"), TRUE, envir = .unreadable)
  .shaCache[[key]] <- if (nzchar(sha)) sha else "?"
  .shaCache[[key]]
}

## GitHub refs listed under a DESCRIPTION's `Remotes:` field.
remotesOf <- function(desc) {
  if (!nzchar(desc)) return(character())
  lines <- strsplit(desc, "\n", fixed = TRUE)[[1L]]
  start <- grep("^Remotes:", lines)
  if (!length(start)) return(character())
  out <- character()
  for (l in lines[seq.int(start[1L] + 1L, length(lines))]) {
    if (!grepl("^[ \t]", l)) break
    s <- sub(",$", "", trimws(l))
    if (grepl("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+(@[A-Za-z0-9_.-]+)?$", s)) out <- c(out, s)
  }
  out
}

splitRef <- function(ref) {
  parts <- strsplit(ref, "@", fixed = TRUE)[[1L]]
  list(repo = parts[1L], branch = if (length(parts) > 1L) parts[2L] else "")
}

wanted <- new.env(parent = emptyenv())
note <- function(repo, branch, who) {
  key <- repo
  cur <- get0(key, envir = wanted, ifnotfound = list())
  b <- if (nzchar(branch)) branch else "<default>"
  cur[[b]] <- union(cur[[b]] %||% character(), who)
  assign(key, cur, envir = wanted)
}

## Record `repo@branch`, then follow that DESCRIPTION's own Remotes one level down.
walk <- function(repo, branch, who, depth = 0L) {
  note(repo, branch, who)
  if (depth > 1L) return(invisible())
  for (rem in remotesOf(remoteDescription(repo, branch))) {
    r <- splitRef(rem)
    walk(r$repo, r$branch, paste0(repo, "@", if (nzchar(branch)) branch else "default"), depth + 1L)
  }
}

## --- collect what the project asks for -------------------------------------------------------

## module `reqdPkgs`
for (f in Sys.glob(file.path("modules", "*", "*.R"))) {
  mod <- basename(dirname(f))
  if (tools::file_path_sans_ext(basename(f)) != mod) next
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
  m <- regmatches(txt, regexpr("reqdPkgs\\s*=\\s*list\\((?s).*?\\n  \\)", txt, perl = TRUE))
  if (!length(m)) next
  refs <- regmatches(m, gregexpr('"[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+(@[A-Za-z0-9_.-]+)?', m))[[1L]]
  for (ref in unique(sub('^"', "", refs))) {
    r <- splitRef(ref)
    walk(r$repo, r$branch, paste0(mod, ":reqdPkgs"))
  }
}

## local package DESCRIPTIONs
for (d in Sys.glob(file.path("packages", "*", "DESCRIPTION"))) {
  for (rem in remotesOf(paste(readLines(d, warn = FALSE), collapse = "\n"))) {
    r <- splitRef(rem)
    walk(r$repo, r$branch, paste0("packages/", basename(dirname(d)), ":Remotes"))
  }
}

## --- local coverage: declared submodules that are not actually present ------------------------
##
## A submodule that is not checked out contributes no reqdPkgs/Remotes, which would quietly shrink
## what the audit can see. Report those. AUDIT_SKIP_PATHS lists paths that are known-unavailable
## ON PURPOSE (e.g. a private repo CI cannot clone) so that a deliberate gap reads differently
## from an accidental one.
skipPaths <- trimws(strsplit(Sys.getenv("AUDIT_SKIP_PATHS", ""), ",", fixed = TRUE)[[1L]])
skipPaths <- skipPaths[nzchar(skipPaths)]
missingSub <- character()
if (file.exists(".gitmodules")) {
  decl <- suppressWarnings(system2("git",
    c("config", "-f", ".gitmodules", "--get-regexp", shQuote("^submodule\\..*\\.path$")),
    stdout = TRUE, stderr = FALSE))
  for (line in decl) {
    sp <- sub("^\\S+\\s+", "", line)
    if (!nzchar(sp) || sp %in% skipPaths) next
    src <- if (startsWith(sp, "modules/")) file.path(sp, paste0(basename(sp), ".R")) else
           if (startsWith(sp, "packages/")) file.path(sp, "DESCRIPTION") else NA_character_
    if (!is.na(src) && !file.exists(src)) missingSub <- c(missingSub, sp)
  }
}

## --- report -----------------------------------------------------------------------------------

nReal <- 0L
nBenign <- 0L
for (repo in sort(ls(wanted))) {
  brs <- get(repo, envir = wanted)
  if (length(brs) < 2L) next
  shas <- vapply(names(brs), function(b) remoteSha(repo, if (b == "<default>") "" else b), "")
  if (length(unique(shas)) == 1L) {
    nBenign <- nBenign + 1L
    message(sprintf("  ok       %s: %s all resolve to %s",
                    repo, paste(names(brs), collapse = ", "), unique(shas)))
  } else {
    nReal <- nReal + 1L
    message(sprintf("\n  CONFLICT %s", repo))
    for (b in names(brs)[order(-lengths(brs))]) {
      message(sprintf("     %-14s %s  (%d) %s", b, shas[[b]], length(brs[[b]]),
                      paste(sort(brs[[b]]), collapse = ", ")))
    }
  }
}
if (length(skipPaths)) {
  message(sprintf("\n  skipped by AUDIT_SKIP_PATHS (%d, declared): %s",
                  length(skipPaths), paste(skipPaths, collapse = ", ")))
}
if (length(missingSub)) {
  message(sprintf("\n  NOT CHECKED OUT (%d) -- coverage is reduced:", length(missingSub)))
  for (s in missingSub) message("     ", s)
}
bad <- sort(ls(.unreadable))
if (length(bad)) {
  message(sprintf("\n  UNREADABLE (%d) -- result is INCONCLUSIVE, not clean:", length(bad)))
  for (b in bad) message("     ", b)
  message("  Check `gh auth status`, repo visibility, and API rate limits.")
}
message(sprintf("\n=> %d real conflict(s), %d benign, %d unreadable, %d not checked out.",
                nReal, nBenign, length(bad), length(missingSub)))
quit(status = if (nReal > 0L || length(bad) > 0L || length(missingSub) > 0L) 1L else 0L)
