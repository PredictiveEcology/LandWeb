options(
  Ncpus = 8L,
  # renv.config.pak.enabled = FALSE,
  renv.config.mran.enabled = FALSE,
  renv.lockfile.version = 1, ## TODO: workflowtools#1
  renv.paths.prefix.auto = TRUE
)

source("renv/activate.R")

## Project-local env overrides (untracked secrets). MUST run AFTER renv/activate.R,
## which re-reads ~/.Renviron -- this overrides the global GOOGLEDRIVE_AUTH with the
## landweb service account that can read the project's restricted Drive data (SCANFI,
## etc.). Sourced in callr children too, so the targets pipeline + workers pick it up.
if (file.exists("LandWeb.Renviron")) readRenviron("LandWeb.Renviron")
