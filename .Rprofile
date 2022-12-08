## trying to force Rstudio to behave when preloading packages...
## at the very least, use an empty .Rprofile to prevent loading of ~/.Rprofile
local({
  prjName <- "LandWeb"
  prjDir <- file.path("~", "GitHub", prjName) ## TODO: make more general

  message(paste("initializing", prjName, "project...\n"))

  pkgDir <- file.path(tools::R_user_dir(basename(prjDir), "data"), "packages",
                      version$platform, getRversion()[, 1:2])
  dir.create(pkgDir, recursive = TRUE, showWarnings = FALSE)
  .libPaths(pkgDir, include.site = FALSE)
  message("Using libPaths:\n", paste(.libPaths(), collapse = "\n"))

  message("\n...done")
})
