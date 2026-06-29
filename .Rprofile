options(
  Ncpus = 8L,
  # renv.config.pak.enabled = FALSE,
  renv.lockfile.version = 1, ## TODO: workflowtools#1
  renv.paths.prefix.auto = TRUE
)

source("renv/activate.R")

## Read env files AFTER renv/activate.R (which can reset the process environment):
## first ~/.Renviron (GITHUB_PAT etc. -- without it renv's many GitHub remote fetches
## fall back to the anonymous 60/hr rate limit and error with "code 22" on workers),
## then the untracked project LandWeb.Renviron, whose landweb service-account
## GOOGLEDRIVE_AUTH must override the global one. The old 00-main.R read both; the
## targets migration dropped the ~/.Renviron read. Sourced in callr children too, so
## the targets pipeline + crew workers pick these up.
if (file.exists("~/.Renviron")) {
  readRenviron("~/.Renviron")
}
if (file.exists("LandWeb.Renviron")) {
  readRenviron("LandWeb.Renviron")
}

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
  if (
    !interactive() &&
      nzchar(gda) &&
      file.exists(gda) &&
      requireNamespace("googledrive", quietly = TRUE)
  ) {
    try(googledrive::drive_auth(path = gda), silent = TRUE)
  }
})
