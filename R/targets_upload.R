## Upload targets: results to the "LandWeb Results" shared drive (LandWeb#141). Opt-in.
##
## Gated by getOption("landweb.upload"), which _local.R sets from LANDWEB_UPLOAD on the control
## node (workers do not source _local.R):
##   FALSE  (the default) no upload targets in the pipeline;
##   dry    the bundles are built, and each upload lists Drive and returns its plan (what it would
##          create, upload, update or skip) without writing anything there;
##   TRUE   upload.
## Whether a run is dry is baked into the upload commands, so going from dry to TRUE re-runs the
## uploads but not the bundles. Drive ids are baked in too: options are not tracked dependencies.
##
## Per study-area group `sa`, branched statically as report 05 is:
##   uploadBundle_<sa>  build_upload_bundle() (R/upload_bundle.R): stages in outputs/<sa>/upload/
##                      exactly the files to send (report 05, figures loose and zipped, the
##                      stand-age animation, tables, burn summaries, RUN_INFO.md). On a worker;
##                      format = "file".
##   upload_<sa>        drive_sync() (R/drive_sync.R) of the bundle to v3/<sa>/, updated in place.
##                      On the main process (deployment = "main"), so uploads run one at a time.
##                      Returns a manifest: each file's action, md5 and Drive id.
## Domain-wide, only when the extended analyses are on too (their PDFs exist only then):
##   upload_reports     drive_sync() of the domain-wide report PDFs (reports/pdf/) to the top-level
##                      _REPORTS/ folder, which must already exist.
##
## Run them on their own, on a finished run, so an upload never starts a rebuild of the summaries:
## tar_make(names = tidyselect::starts_with("upload"), shortcut = TRUE). A file deleted on Drive by
## hand is not noticed; tar_invalidate(tidyselect::starts_with("upload_")) and a re-run put it
## back, skipping every unchanged file.

## Folders of the "LandWeb Results" shared drive. An id grants nothing without the drive's sharing.
landweb_drive <- list(
  ## the shared drive; its id is also the id of its top folder
  drive = "0AEyFltUAISU-Uk9PVA",
  ## v3/: one folder per study-area group
  v3 = "1NJ34HhUbRFFJCBrt25z7ySkyVOd35EWF",
  ## top-level folder of the domain-wide reports, found by name in `drive`
  reports = "_REPORTS"
)

## the domain-wide reports uploaded (00-04, 06, 07): the _pub targets, whose values are the PDFs in
## reports/pdf/. All are defined under the extended-analyses gate, as upload_reports is.
upload_report_names <- c(
  "report_overview_pub",
  "report_study_area_vegetation_pub",
  "report_fire_model_inputs_pub",
  "report_code_module_changes_pub",
  "report_output_differences_pub",
  "report_provenance_pub",
  "report_best_practices_pub"
)

#' The upload gate's value
#'
#' @param x The `landweb.upload` option: `"FALSE"`, `"dry"` or `"TRUE"` (any case). Unset or empty
#'   means `"FALSE"`.
#'
#' @return `"FALSE"`, `"dry"` or `"TRUE"`.
landweb_upload_mode <- function(x = getOption("landweb.upload", "FALSE")) {
  x <- toupper(trimws(as.character(x)))
  if (!length(x) || is.na(x[[1L]]) || !nzchar(x[[1L]])) {
    return("FALSE")
  }
  switch(
    x[[1L]],
    "FALSE" = "FALSE",
    "DRY" = "dry",
    "TRUE" = "TRUE",
    stop("LANDWEB_UPLOAD must be FALSE, dry or TRUE, not '", x[[1L]], "'", call. = FALSE)
  )
}

#' Upload targets for one study-area group
#'
#' @param sa Study-area group name.
#' @param dry_run `TRUE` to plan the upload without writing to Drive.
#' @param n_reps,sim_end Replicates and simulated years (`local$n_reps`, `local$sim_end`), for
#'   RUN_INFO.md.
#' @param folder Drive id of the folder holding one folder per study-area group.
#'
#' @return A list of two targets: `uploadBundle_<sa>` and `upload_<sa>`.
upload_targets_sa <- function(sa, dry_run, n_reps, sim_end, folder = landweb_drive$v3) {
  bundle <- paste0("uploadBundle_", sa)
  bundle_dir <- file.path("outputs", sa, "upload")
  list(
    targets::tar_target_raw(
      bundle,
      bquote(build_upload_bundle(
        study_area = .(sa),
        report = .(as.name(paste0("report05_", sa))),
        nrv_files = .(as.name(paste0("summaries_nrv_", sa, "_files"))),
        burn_files = .(as.name(paste0("summaries_burn_", sa, "_files"))),
        n_reps = .(n_reps),
        sim_end = .(sim_end),
        out_dir = .(bundle_dir)
      )),
      format = "file"
    ),
    targets::tar_target_raw(
      paste0("upload_", sa),
      bquote(drive_sync(
        files = .(as.name(bundle)),
        root = .(bundle_dir),
        folder = .(folder),
        path = .(sa),
        dry_run = .(isTRUE(dry_run))
      )),
      deployment = "main"
    )
  )
}

#' The upload target of the domain-wide reports
#'
#' @param dry_run `TRUE` to plan the upload without writing to Drive.
#' @param reports Names of the targets whose values are the report PDFs.
#' @param folder Drive id of the folder holding `path`.
#' @param path Name of the existing folder in `folder` that receives the PDFs.
#'
#' @return The `upload_reports` target.
upload_reports_target <- function(
  dry_run,
  reports = upload_report_names,
  folder = landweb_drive$drive,
  path = landweb_drive$reports
) {
  targets::tar_target_raw(
    "upload_reports",
    bquote(drive_sync(
      files = .(as.call(c(as.name("c"), lapply(reports, as.name)))),
      root = "reports/pdf",
      folder = .(folder),
      path = .(path),
      dry_run = .(isTRUE(dry_run)),
      create_folders = FALSE
    )),
    deployment = "main"
  )
}

#' All upload targets, as the gate allows
#'
#' @param mode The gate, from [landweb_upload_mode()].
#' @param study_areas Study-area group names (`local$study_areas`).
#' @param extended Whether the extended analyses, and so the domain-wide reports, are on.
#' @param n_reps,sim_end As for [upload_targets_sa()].
#'
#' @return A list of targets; empty when `mode` is `"FALSE"`.
upload_targets <- function(mode, study_areas, extended, n_reps, sim_end) {
  if (identical(mode, "FALSE")) {
    return(list())
  }
  dry_run <- identical(mode, "dry")
  c(
    lapply(study_areas, upload_targets_sa, dry_run = dry_run, n_reps = n_reps, sim_end = sim_end),
    if (isTRUE(extended)) list(upload_reports_target(dry_run))
  )
}
