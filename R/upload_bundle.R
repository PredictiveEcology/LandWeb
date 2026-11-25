## Stage one study-area group's results for upload to Drive (R/targets_upload.R).
##
## build_upload_bundle() writes, under outputs/<sa>/upload/, exactly the files that go to the
## group's folder on Drive, laid out as they will be there:
##
##   05-<sa>.pdf                      report 05
##   RUN_INFO.md                      which run the files come from (no sessionInfo(): its library
##                                    paths name the user's home directory)
##   burnSummaries_*.csv              burnSummaries' tables
##   <sa>_tables.zip                  NRV_summary's tables (postprocess/csv/), as csv/...
##   <sa>_figures_<kind>.zip          NRV_summary's figures (postprocess/figures/<kind>/), as <kind>/...
##   figures/                         report 05's figures and the burn-summary figures
##   figures/<kind>/...               NRV_summary's figures, loose, as in the zips
##
## Loose files are hard links to the run's own files where the file system allows (the bundle is on
## the same disk as the run), copies otherwise, so staging costs little space. Left out: the
## `_aggregates` parquet datasets, the `_reportingMaps` rasters, the burn rasters and the logs.

#' Stage a study-area group's results for upload
#'
#' @param study_area Study-area group name.
#' @param report Value of `report05_<sa>`: the report PDF and its figure PNGs.
#' @param nrv_files Value of `summaries_nrv_<sa>_files`: every file NRV_summary wrote.
#' @param burn_files Value of `summaries_burn_<sa>_files`: the files burnSummaries saved. Its
#'   figures are not among them, so they are listed from `burn_fig_dir`.
#' @param n_reps,sim_end Replicates and simulated years of the run, for RUN_INFO.md.
#' @param out_dir The bundle directory. Removed and rebuilt, so it must be named `upload`.
#' @param postprocess_dir NRV_summary's output directory.
#' @param burn_fig_dir burnSummaries' figure directory.
#' @param project_dir The LandWeb checkout, for the commits in RUN_INFO.md.
#'
#' @return Paths of the bundle's files, sorted.
build_upload_bundle <- function(
  study_area,
  report,
  nrv_files,
  burn_files,
  n_reps,
  sim_end,
  out_dir = file.path("outputs", study_area, "upload"),
  postprocess_dir = file.path("outputs", study_area, "postprocess"),
  burn_fig_dir = file.path("outputs", study_area, "mainSim", "figures", "burnSummaries"),
  project_dir = "."
) {
  if (!identical(basename(out_dir), "upload")) {
    stop("build_upload_bundle(): out_dir must be named 'upload', not ", out_dir, call. = FALSE)
  }
  pdf <- report[grepl("[.]pdf$", report)]
  if (length(pdf) != 1L) {
    stop("build_upload_bundle(): expected one report PDF, got ", length(pdf), call. = FALSE)
  }
  fig_prefix <- paste0(file.path(postprocess_dir, "figures"), "/")
  csv_prefix <- paste0(file.path(postprocess_dir, "csv"), "/")
  nrv_figs <- sort(nrv_files[startsWith(nrv_files, fig_prefix) & grepl("[.]png$", nrv_files)])
  nrv_csvs <- sort(nrv_files[startsWith(nrv_files, csv_prefix) & grepl("[.]csv$", nrv_files)])
  burn_csvs <- sort(burn_files[grepl("^burnSummaries_.*[.]csv$", basename(burn_files))])
  burn_pngs <- sort(list.files(burn_fig_dir, pattern = "[.]png$", full.names = TRUE))
  report_pngs <- sort(report[grepl("[.]png$", report)])

  ## loose files: source -> path in the bundle
  fig_rel <- substring(nrv_figs, nchar(fig_prefix) + 1L) ## <kind>/...
  loose <- c(
    stats::setNames(pdf, basename(pdf)),
    stats::setNames(burn_csvs, basename(burn_csvs)),
    stats::setNames(report_pngs, file.path("figures", basename(report_pngs))),
    stats::setNames(burn_pngs, file.path("figures", basename(burn_pngs))),
    stats::setNames(nrv_figs, file.path("figures", fig_rel))
  )
  dup <- unique(names(loose)[duplicated(names(loose))])
  if (length(dup)) {
    stop("build_upload_bundle(): two files for ", toString(dup), call. = FALSE)
  }

  unlink(out_dir, recursive = TRUE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  for (d in unique(dirname(file.path(out_dir, names(loose))))) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  ok <- mapply(upload_link_or_copy, unname(loose), file.path(out_dir, names(loose)))
  if (!all(ok)) {
    stop("build_upload_bundle(): could not stage ", toString(loose[!ok]), call. = FALSE)
  }

  ## zips: one of the tables, one per figure kind
  out_abs <- normalizePath(out_dir, mustWork = TRUE)
  zips <- character()
  if (length(nrv_csvs)) {
    zips <- c(
      zips,
      upload_zip(
        file.path(out_abs, paste0(study_area, "_tables.zip")),
        files = file.path("csv", substring(nrv_csvs, nchar(csv_prefix) + 1L)),
        root = postprocess_dir,
        compression_level = 9L
      )
    )
  }
  kinds <- sort(unique(sub("/.*$", "", fig_rel[grepl("/", fig_rel)])))
  for (k in kinds) {
    zips <- c(
      zips,
      upload_zip(
        file.path(out_abs, paste0(study_area, "_figures_", k, ".zip")),
        files = fig_rel[startsWith(fig_rel, paste0(k, "/"))],
        root = file.path(postprocess_dir, "figures"),
        compression_level = 0L ## PNGs are compressed already
      )
    )
  }

  info <- upload_run_info(
    study_area = study_area,
    n_reps = n_reps,
    sim_end = sim_end,
    outputs = c(nrv_files, burn_files),
    report = pdf,
    project_dir = project_dir
  )
  writeLines(format_run_info(info), file.path(out_dir, "RUN_INFO.md"), useBytes = TRUE)

  sort(c(
    file.path(out_dir, names(loose)),
    file.path(out_dir, basename(zips)),
    file.path(out_dir, "RUN_INFO.md")
  ))
}

## Hard-link `from` to `to`, or copy it where a link is not possible.
upload_link_or_copy <- function(from, to) {
  isTRUE(suppressWarnings(file.link(from, to))) ||
    isTRUE(file.copy(from, to, overwrite = TRUE, copy.date = TRUE))
}

## Zip `files` (paths below `root`) into `zipfile`, members named by those paths.
upload_zip <- function(zipfile, files, root, compression_level) {
  zip::zip(
    zipfile,
    files = files,
    root = root,
    mode = "mirror",
    include_directories = FALSE,
    compression_level = compression_level
  )
  zipfile
}

#' What RUN_INFO.md records about a run
#'
#' @param study_area,n_reps,sim_end As for [build_upload_bundle()].
#' @param outputs Files the run's summaries wrote; the latest of their modification times dates the
#'   run.
#' @param report The report PDF; its modification time dates the report.
#' @param project_dir The LandWeb checkout.
#' @param built When the bundle was built.
#'
#' @return A list for [format_run_info()].
upload_run_info <- function(
  study_area,
  n_reps,
  sim_end,
  outputs,
  report,
  project_dir = ".",
  built = Sys.time()
) {
  git <- function(args) {
    out <- tryCatch(
      suppressWarnings(system2(
        "git",
        c("-C", shQuote(project_dir), args),
        stdout = TRUE,
        stderr = FALSE
      )),
      error = function(e) character()
    )
    if (!is.null(attr(out, "status"))) character() else out
  }
  day <- function(f) {
    t <- suppressWarnings(max(file.mtime(f[file.exists(f)])))
    if (is.finite(t)) format(t, "%Y-%m-%d", tz = "UTC") else NA_character_
  }
  lock <- file.path(project_dir, "renv.lock")
  head <- git(c("rev-parse", "HEAD"))
  branch <- git(c("rev-parse", "--abbrev-ref", "HEAD"))
  list(
    study_area = study_area,
    n_reps = n_reps,
    sim_end = sim_end,
    outputs_date = day(outputs),
    report_date = day(report),
    built = format(built, "%Y-%m-%d %H:%M UTC", tz = "UTC"),
    commit = if (length(head)) head[[1L]] else NA_character_,
    branch = if (length(branch)) branch[[1L]] else NA_character_,
    renv_lock_md5 = if (file.exists(lock)) unname(tools::md5sum(lock)) else NA_character_,
    components = parse_submodule_status(git(c("submodule", "status")))
  )
}

## `git submodule status` lines as a data frame: component, kind (module or package), commit, state.
parse_submodule_status <- function(lines) {
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) {
    return(data.frame(
      component = character(),
      kind = character(),
      commit = character(),
      state = character(),
      stringsAsFactors = FALSE
    ))
  }
  flag <- substr(lines, 1L, 1L)
  parts <- strsplit(trimws(substring(lines, 2L)), "[[:space:]]+")
  path <- vapply(parts, `[`, character(1), 2L)
  out <- data.frame(
    component = basename(path),
    kind = ifelse(startsWith(path, "modules/"), "module", "package"),
    commit = vapply(parts, `[`, character(1), 1L),
    state = unname(c(
      " " = "as pinned",
      "+" = "differs from pinned",
      "-" = "not checked out",
      "U" = "merge conflict"
    )[flag]),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$kind, out$component), ]
  rownames(out) <- NULL
  out
}

#' RUN_INFO.md text
#'
#' @param info A list from [upload_run_info()].
#'
#' @return Character vector, one line each.
format_run_info <- function(info) {
  na <- function(x) if (length(x) != 1L || is.na(x)) "unknown" else as.character(x)
  sa <- info$study_area
  comp <- info$components
  table <- if (NROW(comp)) {
    c(
      "| Component | Kind | Commit | State |",
      "|---|---|---|---|",
      sprintf("| %s | %s | `%s` | %s |", comp$component, comp$kind, comp$commit, comp$state)
    )
  } else {
    "No module or package commits could be read."
  }
  c(
    paste0("# LandWeb v3 results: ", sa),
    "",
    paste0(
      "These files come from one LandWeb v3 run for the ",
      sa,
      " study-area group: ",
      na(info$n_reps),
      " replicates of ",
      na(info$sim_end),
      " simulated years."
    ),
    "",
    paste0("- Summaries of the simulations written: ", na(info$outputs_date)),
    paste0("- Report 05 rendered: ", na(info$report_date)),
    paste0("- Files gathered for upload: ", na(info$built)),
    "",
    "## Files",
    "",
    paste0("- `05-", sa, ".pdf`: the study-area report."),
    "- `figures/`: the report's figures and the fire-summary figures.",
    paste0(
      "- `figures/<kind>/`: the natural-range-of-variation figures by kind ",
      "(`lm` landscape metrics, `pm` patch metrics, `boxplots` leading vegetation types by age ",
      "class, `histograms` large patches); ",
      "the same figures are in `",
      sa,
      "_figures_<kind>.zip`."
    ),
    paste0("- `", sa, "_tables.zip`: the tables those figures are drawn from (CSV)."),
    "- `burnSummaries_*.csv`: the tables behind the fire-summary figures.",
    "",
    "## Code",
    "",
    paste0(
      "The LandWeb code checked out when these files were gathered. The simulations ran with ",
      "the same code unless it changed after they ran."
    ),
    "",
    paste0("- LandWeb commit: `", na(info$commit), "` (branch ", na(info$branch), ")"),
    paste0("- `renv.lock` md5: `", na(info$renv_lock_md5), "`"),
    "",
    table
  )
}
