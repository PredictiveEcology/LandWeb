## use empty .Rprofile to prevent loading of user's .Rprofile
local({
  message("initializing LandWeb project...")

  ## TODO: trying force Rstudio to behave...
  local({
    pkgDir <- file.path("packages", version$platform, substr(getRversion(), 1, 3))
    dir.create(pkgDir, recursive = TRUE, showWarnings = FALSE)
    .libPaths(pkgDir, include.site = FALSE)
    message("Using libPaths:\n", paste(.libPaths(), collapse = "\n"))
  })

  message("...done")
})
