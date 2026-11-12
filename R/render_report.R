## Render a per-study-area report from an isolated copy of its template (ported from BC_HRV).
##
## Report 05 is one template rendered once per study-area group, and the renders can run at the same
## time on the crew workers. quarto names its intermediate files after the INPUT basename and writes
## them next to the input (`reports/<input>.tex`, `reports/<input>_files/`), so two concurrent renders
## of one template clobber each other's intermediates. Each render therefore runs from a copy of the
## template with a unique name, placed in `reports/` itself (not a subdirectory) so that the copy's
## relative includes (`{{< include _provenance-appendix.qmd >}}`) and the project `_quarto.yml`
## (execute-dir, resource-path, filters) resolve exactly as for the template. The copy and quarto's
## leftovers are removed afterwards, so a worker checkout keeps no stray file.
##
## The PDF goes to `output_dir` (`outputs/<studyArea>/reports`, per the repo layout), which overrides
## the project's `output-dir` (the shared `outputs/_reports/`) for this render only.

#' Render a per-branch report from an isolated copy of its template
#'
#' @param template Path to the report template, e.g. `"reports/05-study-area-report.qmd"`.
#' @param output_file Final PDF basename, unique per branch (e.g. `"05-WesternAlbertaUpland.pdf"`).
#'   Also names the render copy, so it must differ between branches.
#' @param output_dir Directory, relative to the project root, that receives the PDF.
#' @param execute_params Named list passed to the template's `params`.
#' @param metadata Named list of document metadata (e.g. `title`, `subtitle`), or `NULL`.
#' @param reports_dir Directory holding the templates (default `"reports"`).
#' @param quiet Passed to [quarto::quarto_render()]. `TRUE` hides quarto's error message too.
#'
#' @return Path to the rendered PDF, `file.path(output_dir, output_file)`.
render_report_isolated <- function(
  template,
  output_file,
  output_dir,
  execute_params = list(),
  metadata = NULL,
  reports_dir = "reports",
  quiet = FALSE
) {
  if (!file.exists(template)) {
    stop("render_report_isolated(): template not found: ", template, call. = FALSE)
  }
  stub <- tools::file_path_sans_ext(output_file)
  tmp_input <- file.path(reports_dir, paste0(stub, ".render.qmd"))
  if (!file.copy(template, tmp_input, overwrite = TRUE)) {
    stop("render_report_isolated(): could not copy ", template, " to ", tmp_input, call. = FALSE)
  }
  ## remove the copy and quarto's per-input leftovers whatever happens, so a failed render leaves
  ## nothing behind in the checkout
  on.exit(
    unlink(
      c(
        tmp_input,
        file.path(reports_dir, paste0(stub, ".render", c(".tex", ".log", ".pdf", "_files"))),
        file.path(reports_dir, output_file)
      ),
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  out <- file.path(output_dir, output_file)
  unlink(out) ## a stale PDF must not pass for this render's
  ## quarto::quarto_render() keeps quarto's own message out of its error; the failed processx::run()
  ## under it holds the output (`stderr`). Write it beside the PDF and quote its end. With `quiet`,
  ## the quarto CLI gets `--quiet` and prints nothing, errors included, so the log would be empty.
  log_file <- file.path(output_dir, paste0(stub, ".quarto.log"))
  tryCatch(
    quarto::quarto_render(
      input = tmp_input,
      output_file = output_file,
      execute_params = execute_params,
      metadata = metadata,
      quarto_args = c("--output-dir", output_dir),
      quiet = quiet
    ),
    error = function(e) {
      p <- e
      while (!is.null(p) && is.null(p$stderr) && is.null(p$stdout)) {
        p <- p$parent
      }
      txt <- if (is.null(p)) conditionMessage(e) else c(p$stdout, p$stderr)
      writeLines(txt, log_file)
      lines <- strsplit(paste(txt, collapse = "\n"), "\n", fixed = TRUE)[[1L]]
      stop(
        "render_report_isolated(): quarto failed on ",
        template,
        " (",
        Sys.info()[["nodename"]],
        "); ",
        "its output is in ",
        log_file,
        ". Last lines:\n",
        paste(utils::tail(lines, 15L), collapse = "\n"),
        call. = FALSE
      )
    }
  )

  ## In project mode quarto mirrors the input's project-relative directory under the output
  ## directory, so the PDF lands in `<output_dir>/reports/`; some versions leave it next to the
  ## input instead. Move it to `out`, the path the target declares.
  alts <- c(file.path(output_dir, reports_dir, output_file), file.path(reports_dir, output_file))
  alt <- alts[file.exists(alts)][1L]
  if (!is.na(alt)) {
    if (!file.copy(alt, out, overwrite = TRUE)) {
      stop("render_report_isolated(): could not move ", alt, " to ", out, call. = FALSE)
    }
    unlink(alt)
    mirror <- file.path(output_dir, reports_dir)
    if (dir.exists(mirror) && !length(list.files(mirror, all.files = TRUE, no.. = TRUE))) {
      unlink(mirror, recursive = TRUE)
    }
  }
  if (!file.exists(out)) {
    stop(
      "render_report_isolated(): rendered PDF not found at ",
      paste(c(out, alts), collapse = " or "),
      call. = FALSE
    )
  }
  out
}

#' Render report 05 for one study-area group
#'
#' Writes `data` (from [build_report05_data()]) to a temporary file that the template reads, renders
#' `reports/05-study-area-report.qmd` into `outputs/<study_area>/reports/`, and returns the PDF and the
#' standalone figure PNGs the template writes next to it (`figures/`).
#'
#' @param study_area Study-area group name.
#' @param data The list from [build_report05_data()].
#' @param template Report template.
#' @param output_dir Output directory (default `outputs/<study_area>/reports`).
#' @param quiet Passed to [quarto::quarto_render()]. `TRUE` hides quarto's error message too.
#'
#' @return Character vector: the PDF path first, then the figure PNGs.
render_report05 <- function(
  study_area,
  data,
  template = "reports/05-study-area-report.qmd",
  output_dir = file.path("outputs", study_area, "reports"),
  quiet = FALSE
) {
  data_file <- tempfile(paste0("report05_", study_area, "_"), fileext = ".rds")
  saveRDS(data, data_file)
  on.exit(unlink(data_file), add = TRUE)
  fig_dir <- file.path(output_dir, "figures")
  unlink(fig_dir, recursive = TRUE) ## only this render's figures are returned
  pdf <- render_report_isolated(
    template = template,
    output_file = paste0("05-", study_area, ".pdf"),
    output_dir = output_dir,
    execute_params = list(study_area = study_area, data_file = data_file, fig_dir = fig_dir),
    metadata = list(subtitle = paste(data$study_area_label, "study-area group")),
    quiet = quiet
  )
  c(pdf, sort(list.files(fig_dir, pattern = "[.]png$", full.names = TRUE)))
}
