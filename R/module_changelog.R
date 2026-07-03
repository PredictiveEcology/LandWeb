## Live git-log change summary for report 03 (code & module changes v2 -> v3).
##
## NEWS files were not maintained across the v2 -> v3 window, so report 03
## reconstructs the change history from git. This produces the report's
## auto-generated "change-summary appendix": for each SpaDES module submodule and
## owned package, the number of commits since the v3 window opened, the last-update
## date, and the currently-pinned branch + short SHA.
##
## NOT cache-aware: a changelog should reflect the live history, so it regenerates
## whenever the target runs. It is cheap (a few `git` calls) and runs on the control
## node (deployment = "main"), where the submodule checkouts live.

## Run a git command in `dir` and return its first line of stdout (or NA).
.git_out <- function(dir, args) {
  out <- suppressWarnings(
    system2("git", c("-C", dir, args), stdout = TRUE, stderr = FALSE)
  )
  if (length(out)) out[1] else NA_character_
}

#' Per-submodule / package git-log change summary
#'
#' @param out_csv path to write (and the value returned, for the file target).
#' @param since ISO date; commits on/after this open the "v3 window" count.
#' @param module_dir,package_dir dirs holding the module / package submodules.
#' @return `out_csv`.
module_changelog <- function(
  out_csv,
  since = "2023-01-01",
  module_dir = "modules",
  package_dir = "packages"
) {
  paths <- c(
    list.dirs(module_dir, recursive = FALSE),
    list.dirs(package_dir, recursive = FALSE)
  )
  paths <- paths[file.exists(file.path(paths, ".git"))]
  rows <- lapply(paths, function(d) {
    kind <- if (identical(dirname(d), module_dir)) "module" else "package"
    n <- .git_out(d, c("rev-list", "--count", paste0("--since=", since), "HEAD"))
    data.table::data.table(
      name = basename(d),
      kind = kind,
      commits_since = suppressWarnings(as.integer(n)),
      last_update = .git_out(d, c("log", "-1", "--pretty=%ad", "--date=short")),
      branch = .git_out(d, c("rev-parse", "--abbrev-ref", "HEAD")),
      sha = .git_out(d, c("rev-parse", "--short", "HEAD"))
    )
  })
  dt <- data.table::rbindlist(rows)
  data.table::setorder(dt, kind, -commits_since)
  dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(dt, out_csv)
  out_csv
}
