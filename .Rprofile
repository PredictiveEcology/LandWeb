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

## GOOGLEDRIVE_AUTH is recorded project-relative in LandWeb.Renviron; resolve it to an
## absolute path now (cwd is the project root at startup) so it still resolves after
## reproducible/prepInputs setwd() to a scratch dir mid-download on a crew worker --
## otherwise googledrive::drive_auth() cannot find the service-account JSON and falls
## back to a (failing, non-interactive) prompt.
local({
  gda <- Sys.getenv("GOOGLEDRIVE_AUTH")
  if (nzchar(gda) && !startsWith(gda, "/") && file.exists(gda)) {
    Sys.setenv(GOOGLEDRIVE_AUTH = normalizePath(gda, mustWork = FALSE))
  }
})

## Pre-authenticate googledrive with the service account on non-interactive
## (pipeline / crew worker) sessions. Some download helpers -- e.g.
## LandR::prepSpeciesLayers_SCANFI -- call `googledrive::drive_ls()` DIRECTLY to
## resolve a shared-drive folder, BEFORE reproducible's prepInputs auto-auth runs,
## so a token must already exist or that raw call falls back to a failing
## interactive `drive_auth()` ("Can't get Google credentials"). Interactive
## sessions are left to the user's own credentials.
local({
  gda <- Sys.getenv("GOOGLEDRIVE_AUTH")
  if (!interactive() && nzchar(gda) && file.exists(gda) &&
      requireNamespace("googledrive", quietly = TRUE)) {
    try(googledrive::drive_auth(path = gda), silent = TRUE)
  }
})
