## Per-study-area report targets (report 05; ported from BC_HRV's R/targets_reports.R).
##
## For each study-area group `sa`, three targets:
##   report05Data_<sa>  build_report05_data() (R/report_data.R): reads the area's reporting polygons,
##                      species traits and species list (targets) and NRV_summary's parquet datasets
##                      and current-condition map (files under outputs/<sa>/postprocess/). Runs on a
##                      worker. `summaries_nrv_<sa>_files` is named in the command so that a re-run of
##                      the summaries re-runs it.
##   report05_<sa>      render_report05() (R/render_report.R): renders reports/05-study-area-report.qmd
##                      from a per-area copy, so concurrent renders do not share quarto's intermediate
##                      files, into outputs/<sa>/reports/05-<sa>.pdf, plus standalone figure PNGs in
##                      outputs/<sa>/reports/figures/. Runs on a worker (quarto and LaTeX are there).
##   report05_<sa>_pub  publish_pdf(): copies the PDF into the git-tracked reports/pdf/ on the
##                      controller (deployment = "main"), as for the domain-wide reports.
## Branched statically, like the study-area stages in _targets.R (one "_<sa>"-suffixed set per area),
## not with `pattern = map(study_areas)`: the stored `study_areas` target is a record, not the source
## of the branches.
##
## The template and the files it reads that no target tracks (its include, its helper functions, the
## project config and its filter) form one shared file target, report05Template, so an edit to any of
## them re-renders every area. The R functions the commands call are tracked by targets itself.

## files report 05 renders from, beyond the R functions the target commands call
report05_template_files <- c(
  "reports/05-study-area-report.qmd",
  "reports/_05-tenure.qmd",
  "reports/_provenance-appendix.qmd",
  "R/report05_helpers.R",
  "_quarto.yml",
  "reports/_filters/clearpage.lua"
)

#' The shared file target over report 05's template files
report05_template_target <- function() {
  targets::tar_target_raw(
    "report05Template",
    as.call(c(as.name("c"), as.list(report05_template_files))), ## one expression: c("...", ...)
    format = "file",
    deployment = "main"
  )
}

#' Report 05 targets for one study-area group
#'
#' @param sa Study-area group name.
#' @param pkg_fp Installed identity of the co-developed packages the data builder calls (from
#'   `pkg_fingerprint()` in `_targets.R`), baked into the command so that a new version re-runs it.
#' @param veg_leading_prop The run's `vegLeadingProportion` (`globals` in `_targets.R`), reported as
#'   the leading-type threshold.
#' @param v2_root Directory of the version 2 runs, relative to the project root, as each worker sees
#'   it. Relative, so the command is the same on every host; a worker without it reports the boundary
#'   comparison as unavailable.
#'
#' @return A list of three targets.
report05_targets <- function(sa, pkg_fp, veg_leading_prop, v2_root = "outputs_v2") {
  sym <- function(stem) as.name(paste0(stem, "_", sa))
  data_name <- paste0("report05Data_", sa)
  render_name <- paste0("report05_", sa)
  list(
    targets::tar_target_raw(
      data_name,
      bquote({
        pkgs <- .(pkg_fp) ## re-run when a co-developed package changes
        .(as.name(paste0("summaries_nrv_", sa, "_files"))) ## dependency anchor: the summaries' files
        build_report05_data(
          study_area = .(sa),
          reporting_polygons = .(sym("reportingPolygons")),
          species_traits = .(sym("speciesTraits")),
          spp_list = .(sym("sppList")),
          postprocess_dir = .(file.path("outputs", sa, "postprocess")),
          v2_root = .(v2_root),
          veg_leading_prop = .(veg_leading_prop)
        )
      })
    ),
    targets::tar_target_raw(
      render_name,
      bquote({
        report05Template ## dependency anchor: the template files
        render_report05(.(sa), .(as.name(data_name)))
      }),
      format = "file"
    ),
    targets::tar_target_raw(
      paste0(render_name, "_pub"),
      bquote(publish_pdf(.(as.name(render_name)))),
      format = "file",
      deployment = "main"
    )
  )
}
