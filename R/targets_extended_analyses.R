## Factory for the gated "extended analyses" targets (off unless
## getOption("landweb.extended_analyses") is TRUE; set in _local.R, which the
## crew workers do NOT source -- so this list, and the quarto inspection inside
## tar_quarto(), are only ever constructed on the control node).
##
## Currently: the SCANFI per-species cover summary over the full LandWeb (LTHFC v10)
## study area, plus its study-area vegetation report.
##
## The summary scan is multi-hour over the ~3.0e6 km^2 domain, so:
##  - scanfi_veg_summary() is cache-aware (returns the existing CSV unchanged);
##  - the CSV is a format = "file" target, pre-seeded with the completed run, so
##    targets records its hash and never re-runs the scan unless the file is
##    deleted or the SCANFI/LTHFC version stamp in its name changes.

get_targets_extended_analyses <- function(local) {
  inputs_dir <- normalizePath(local$paths$inputPath, mustWork = TRUE)
  out_csv <- file.path(
    normalizePath(local$paths$outputPath, mustWork = TRUE),
    "extended_analyses",
    "scanfi_lthfc_v10_2020_veg_summary.csv"
  )

  list(
    ## The expensive scan, gated + cache-aware. Paths are baked in as literals
    ## (bquote) so the command is self-contained on the worker. NO deployment =
    ## "main": when it does run, the scan is heavy and belongs on a compute node;
    ## when pre-seeded it is a trivial file.exists() check either way.
    tar_target_raw(
      "scanfi_veg_summary_csv",
      bquote(scanfi_veg_summary(out_csv = .(out_csv), inputs_dir = .(inputs_dir))),
      format = "file"
    ),

    ## The report. tar_quarto scans the .qmd at definition time for tar_read()
    ## calls (here: scanfi_veg_summary_csv) and wires them as deps. Render on the
    ## control node (LaTeX + the git checkout live there; the render is light and
    ## reads the file target off NFS).
    tarchetypes::tar_quarto(
      report_study_area_vegetation,
      path = "reports/01-study-area-vegetation.qmd",
      deployment = "main"
    ),

    ## Copy the rendered PDF into the git-tracked reports/pdf/ on the controller.
    tar_target(
      report_study_area_vegetation_pub,
      publish_pdf(report_study_area_vegetation),
      format = "file",
      deployment = "main"
    )
  )
}
