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
  outputs_dir <- normalizePath(local$paths$outputPath, mustWork = TRUE)
  out_csv <- file.path(
    outputs_dir,
    "extended_analyses",
    "scanfi_lthfc_v10_2020_veg_summary.csv"
  )
  out_csv_knn <- file.path(outputs_dir, "extended_analyses", "knn_lthfc_v10_2001_veg_summary.csv")
  out_csv_lthfc <- file.path(outputs_dir, "extended_analyses", "lthfc_version_summary.csv")
  out_gpkg_lthfc <- file.path(outputs_dir, "extended_analyses", "lthfc_change_map.gpkg")
  out_tif_lthfc <- file.path(outputs_dir, "extended_analyses", "lthfc_diff.tif")
  out_csv_clog <- file.path(outputs_dir, "extended_analyses", "module_changelog.csv")

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

    ## Beaudoin kNN (2001) companion summary for report 01's v2-vs-v3
    ## initial-conditions comparison. kNN is 250 m so this scan is light
    ## (~40 s, cache-aware); still pre-seeded as a format = "file" target.
    tar_target_raw(
      "knn_veg_summary_csv",
      bquote(knn_veg_summary(out_csv = .(out_csv_knn), inputs_dir = .(inputs_dir))),
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
    ),

    ## --- Report 02: fire-model (LandMine / LTHFC) inputs ---
    ## Cache-aware, cheap (three ~500-polygon shapefiles): area-by-FRI per LTHFC
    ## version (v10 vs v8c vs SLS v3). Paths baked in as literals for worker
    ## self-containment; light enough to run anywhere, so no deployment pin.
    tar_target_raw(
      "lthfc_version_summary_csv",
      bquote(lthfc_version_summary(out_csv = .(out_csv_lthfc), inputs_dir = .(inputs_dir))),
      format = "file"
    ),
    tar_target_raw(
      "lthfc_change_map_gpkg",
      bquote(lthfc_change_map_data(out_gpkg = .(out_gpkg_lthfc), inputs_dir = .(inputs_dir))),
      format = "file"
    ),
    tar_target_raw(
      "lthfc_diff_tif",
      bquote(lthfc_diff_raster(out_tif = .(out_tif_lthfc), inputs_dir = .(inputs_dir))),
      format = "file"
    ),

    ## --- Report 03: code & module changes v2 -> v3 ---
    ## Live git-log summary (regenerated each run) feeding report 03's appendix.
    ## deployment = "main": the submodule checkouts live on the control node.
    tar_target_raw(
      "module_changelog_csv",
      bquote(module_changelog(out_csv = .(out_csv_clog))),
      format = "file",
      deployment = "main"
    ),
    tarchetypes::tar_quarto(
      report_code_module_changes,
      path = "reports/03-code-and-module-changes.qmd",
      deployment = "main"
    ),
    tar_target(
      report_code_module_changes_pub,
      publish_pdf(report_code_module_changes),
      format = "file",
      deployment = "main"
    ),

    tarchetypes::tar_quarto(
      report_fire_model_inputs,
      path = "reports/02-fire-model-inputs.qmd",
      deployment = "main"
    ),
    tar_target(
      report_fire_model_inputs_pub,
      publish_pdf(report_fire_model_inputs),
      format = "file",
      deployment = "main"
    ),

    ## --- Report 00: overview (narrative + ggplot workflow schematic; no data deps) ---
    tarchetypes::tar_quarto(
      report_overview,
      path = "reports/00-overview.qmd",
      deployment = "main"
    ),
    tar_target(
      report_overview_pub,
      publish_pdf(report_overview),
      format = "file",
      deployment = "main"
    ),

    ## --- Report 06: provenance (input manifest + auto-synced data-source bib) ---
    ## The manifest is hand-curated (R/build_input_manifest.R) + committed; track it
    ## as a file input, and regenerate the sidecar .bib from it. Controller-only.
    tar_target(
      input_manifest_file,
      "_input_manifest.json",
      format = "file",
      deployment = "main"
    ),
    tar_target(
      data_sources_bib,
      {
        input_manifest_file # dep: rebuild when the manifest changes
        workflowtools::sync_manifest_to_bibtex(
          manifest = "_input_manifest.json",
          out = "citations/data-sources.bib",
          references_bib = "citations/references.bib"
        )
      },
      format = "file",
      deployment = "main"
    ),
    tarchetypes::tar_quarto(
      report_provenance,
      path = "reports/06-provenance.qmd",
      deployment = "main"
    ),
    tar_target(
      report_provenance_pub,
      publish_pdf(report_provenance),
      format = "file",
      deployment = "main"
    )
  )
}
