## DEFUNCT - will be removed
if (FALSE) {
  # install and load packages -------------------------------------------------------------------

  pkgDir <- file.path(tools::R_user_dir(basename(prjDir), "data"), "packages",
                      version$platform, getRversion()[, 1:2])
  dir.create(pkgDir, recursive = TRUE, showWarnings = FALSE)
  .libPaths(pkgDir, include.site = FALSE)
  message("Using libPaths:\n", paste(.libPaths(), collapse = "\n"))

  if (!"tmpdir" %in% rownames(installed.packages(lib.loc = .libPaths()[1]))) {
    remotes::install_github("achubaty/tmpdir")
  }

  if (!"remotes" %in% rownames(installed.packages(lib.loc = .libPaths()[1]))) {
    install.packages("remotes")
  }

  Require.version <- "0.3.1"
  if (!"Require" %in% rownames(installed.packages(lib.loc = .libPaths()[1])) ||
      packageVersion("Require", lib.loc = .libPaths()[1]) < Require.version) {
    remotes::install_cran("Require") ## use CRAN version
  }

  library(Require)

  setLinuxBinaryRepo()

  Require(c(
    "PredictiveEcology/SpaDES.project@transition (>= 0.0.7.9003)", ## TODO: use development once merged
    "PredictiveEcology/SpaDES.config@development (>= 0.0.2.9065)"
  ), upgrade = FALSE, standAlone = TRUE)

  ## install additional (non-SpaDES) packages
  c("archive", "details", "DBI", "s-u/fastshp", "logging",
    "Rcpp (>= 1.0.10)", "RPostgres", "FOR-CAST/notifications", "terra (>= 1.7-3)") |>
    Install(standAlone = TRUE, upgrade = FALSE)

  ## install SpaDES-related packages from a specific branch or tag, with all dependencies
  tag <- "dev-stable"
  c(
    paste0("PredictiveEcology/LandR@", tag),
    paste0("PredictiveEcology/reproducible@", tag, " (>= 1.2.16.9024)"),
    paste0("PredictiveEcology/SpaDES.core@", tag, " (>= 1.1.1)")
  ) |>
    Install(dependencies = TRUE, standAlone = TRUE, upgrade = FALSE)

  ## install additional packages used in modules
  packagesInModules(modulePath = file.path(prjDir, "m")) |>
    unlist() |>
    unname() |>
    unique() |>
    Install(standAlone = TRUE, upgrade = FALSE)

  ## load packages for use with project
  ## NOTE: always load packages LAST, after installation above; ensure plyr loaded before dplyr.
  Require(c("data.table", "plyr", "pryr", "SpaDES.core",
            "googledrive", "httr", "LandR", "LandWebUtils", "notifications", "sessioninfo"),
          upgrade = FALSE, standAlone = TRUE)
}
