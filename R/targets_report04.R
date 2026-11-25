## Report 04 targets: the differences between the v2 and v3 outputs, one domain-wide PDF.
##
## Gated like the other domain-wide reports (00-03, 06, R/targets_extended_analyses.R): _targets.R
## adds them only when getOption("landweb.extended_analyses") is TRUE, which _local.R sets from
## LANDWEB_EXTENDED_ANALYSES on the control node (workers do not source _local.R, and tar_quarto()
## inspects the report with quarto when the pipeline is defined).
##
##   report04Tenures_<sa>  build_report04_sa() (R/report04_data.R), one per study-area group, branched
##                         statically as report 05 is: the v3 outputs of the group's tenures that have
##                         a v2 run, that run's summary files and current-condition maps, and the
##                         boundary comparison. NULL for a group with no such tenure. Runs on a
##                         worker; `summaries_nrv_<sa>_files` is named in the command so that a re-run
##                         of the summaries re-runs it.
##   report04Data          build_report04_data(): the groups combined into the tables the report shows.
##   report_output_differences      tar_quarto() render of reports/04-output-differences.qmd into
##                                  outputs/_reports/, on the controller as for reports 00-03 and 06.
##   report_output_differences_pub  publish_pdf(): the PDF copied into the git-tracked reports/pdf/.
##
## The v2 runs are read from `v2_root` (relative, as each worker sees it); they never change, so no
## target tracks them.

## files the report renders from that tar_quarto() does not find in the .qmd itself
report04_extra_files <- c(
  "reports/_04-tenure.qmd",
  "reports/_provenance-appendix.qmd",
  "R/report04_helpers.R",
  "R/report05_helpers.R",
  "_quarto.yml",
  "reports/_filters/clearpage.lua"
)

#' Report 04 targets
#'
#' @param study_areas Study-area group names (`local$study_areas`).
#' @param pkg_fp Installed identity of the co-developed packages the builders call (from
#'   `pkg_fingerprint()` in `_targets.R`), baked into the commands so that a new version re-runs them.
#' @param v2_root Directory of the v2 runs, relative to the project root.
#'
#' @return A list of targets.
report04_targets <- function(study_areas, pkg_fp, v2_root = "outputs_v2") {
  part_names <- paste0("report04Tenures_", study_areas)
  per_sa <- lapply(study_areas, function(sa) {
    targets::tar_target_raw(
      paste0("report04Tenures_", sa),
      bquote({
        pkgs <- .(pkg_fp) ## re-run when a co-developed package changes
        .(as.name(paste0("summaries_nrv_", sa, "_files"))) ## dependency anchor: the summaries' files
        build_report04_sa(
          study_area = .(sa),
          reporting_polygons = .(as.name(paste0("reportingPolygons_", sa))),
          postprocess_dir = .(file.path("outputs", sa, "postprocess")),
          v2_root = .(v2_root),
          crosswalk = v2_v3_crosswalk()
        )
      })
    )
  })
  parts <- as.call(c(as.name("list"), lapply(part_names, as.name)))
  list(
    per_sa,
    targets::tar_target_raw(
      "report04Data",
      bquote(build_report04_data(.(parts), crosswalk = v2_v3_crosswalk()))
    ),
    tarchetypes::tar_quarto(
      report_output_differences,
      path = "reports/04-output-differences.qmd",
      extra_files = report04_extra_files,
      deployment = "main"
    ),
    targets::tar_target(
      report_output_differences_pub,
      publish_pdf(report_output_differences),
      format = "file",
      deployment = "main"
    )
  )
}
