#' Copy a rendered report PDF into the git-tracked `reports/pdf/` directory
#'
#' Reports render on a worker to the NFS-shared `outputs/reports/` (per
#' `_quarto.yml::project.output-dir`); this copies the final PDF to the
#' git-tracked `reports/pdf/` so it travels with the repo. Use as a
#' `format = "file"`, `deployment = "main"` target (runs on the controller,
#' which is where the git checkout lives).
#'
#' @param rendered_paths character vector returned by `tarchetypes::tar_quarto()`
#'   (the rendered output files); the `.pdf` among them is selected.
#' @return the git-tracked destination path (the file-target value).
publish_pdf <- function(rendered_paths) {
  pdf_src <- rendered_paths[grepl("\\.pdf$", rendered_paths)][1]
  dst_dir <- file.path("reports", "pdf")
  dir.create(dst_dir, recursive = TRUE, showWarnings = FALSE)
  pdf_dst <- file.path(dst_dir, basename(pdf_src))
  file.copy(pdf_src, pdf_dst, overwrite = TRUE)
  pdf_dst
}
