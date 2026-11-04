## The guard of scripts/archive-run.sh: for each study area, every replicate and both summaries must have
## been built after the latest of their upstream stages, without error, and every file they wrote must
## exist and be no older than that. The replicates' files are those their *_files branches track; the
## summaries' are read from the summaries' own values, since a build that names only the summaries leaves
## their *_files companions unbuilt. Branches are the patterns' current children, so records of branches
## from an earlier pattern do not count. Prints one line per study area, then "GUARD OK" or "GUARD FAILED".
## Run from the project root:
##   [ARCHIVE_SINCE="2026-10-26 13:50"] Rscript-4.6.1 scripts/archive-guard.R <study area> [...]
## ARCHIVE_SINCE moves the bound later, e.g. to the launch of a run that rebuilt only the replicates.
sas <- commandArgs(TRUE)
since <- Sys.getenv("ARCHIVE_SINCE")
m <- targets::tar_meta(
  fields = c("name", "time", "error", "path", "children")
)
nReps <- length(targets::tar_read(rep_index))
time_of <- function(names) as.POSIXct(m$time[m$name %in% names])
children <- function(pattern) unlist(m$children[m$name == pattern])
ok <- length(sas) > 0
for (sa in sas) {
  upstream <- c(
    "growthTraits",
    paste0(
      c("preamble_", "speciesData_", "sppList_", "dataPrep_", "speciesTraits_"),
      sa
    )
  )
  bound <- max(c(time_of(upstream), if (nzchar(since)) as.POSIXct(since)))
  reps <- children(paste0("mainSim_", sa))
  summaries <- paste0("summaries_", c("nrv", "burn"), "_", sa)
  built <- c(reps, summaries)
  fileTargets <- children(paste0("mainSim_", sa, "_files"))
  sel <- m[m$name %in% c(built, fileTargets), ]
  absent <- setdiff(c(built, fileTargets), sel$name)
  stale <- sel$name[
    is.na(sel$time) | as.POSIXct(sel$time) <= bound | !is.na(sel$error)
  ]
  paths <- unique(c(
    unlist(sel$path[sel$name %in% fileTargets]),
    unlist(lapply(intersect(summaries, sel$name), function(nm) {
      targets::tar_read_raw(nm)[["files"]]
    }))
  ))
  missing <- paths[!file.exists(paths)]
  older <- paths[file.exists(paths) & file.mtime(paths) <= bound]
  problems <- c(
    if (!any(m$name == paste0("dataPrep_", sa))) "no dataPrep target;",
    if (length(reps) != nReps) paste(length(reps), "of", nReps, "replicates;"),
    if (length(absent)) paste("no record:", paste(absent, collapse = ", "), ";"),
    if (length(stale)) paste("not rebuilt:", paste(stale, collapse = ", "), ";"),
    if (length(missing)) paste(length(missing), "tracked files missing;"),
    if (length(older)) paste(length(older), "tracked files older than the bound;"),
    if (!length(paths)) "no tracked files;"
  )
  cat(
    if (length(problems)) "FAILED" else "ok",
    sa,
    "| bound",
    format(bound),
    "| replicates",
    length(reps),
    "| tracked files",
    length(paths),
    if (length(problems)) paste("|", paste(problems, collapse = " ")),
    "\n"
  )
  ok <- ok && !length(problems)
}
cat(if (ok) "GUARD OK" else "GUARD FAILED", "\n")
