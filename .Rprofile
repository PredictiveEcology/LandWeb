options(
  Ncpus = 8L,
  # renv.config.pak.enabled = FALSE,
  renv.lockfile.version = 1, ## TODO: workflowtools#1
  renv.paths.prefix.auto = TRUE
)

source("renv/activate.R")

## Read env files AFTER renv/activate.R (which can reset the process environment). First
## ~/.Renviron, for GITHUB_PAT etc.: without it renv's many GitHub remote fetches fall back to the
## anonymous 60/hr rate limit and error with "code 22" on workers. Sourced in callr children too,
## so the targets pipeline + crew workers pick these up.
if (file.exists("~/.Renviron")) {
  readRenviron("~/.Renviron")
}

## Google Drive auth for non-interactive sessions (pipeline runs, crew workers): the LandWeb
## service account. Its JSON key lives outside the repo (~/.config/landweb/drive-sa.json,
## owner-only), and the untracked LandWeb.Renviron in the project root sets GOOGLEDRIVE_AUTH to it;
## copy both to every machine that runs the pipeline. Key paths set elsewhere (e.g. another
## project's key in ~/.Renviron) are cleared in every session first, so no other account's key is
## used here, and a key from another Google Cloud project is refused.
##
## Non-interactive sessions are pre-authenticated because some download helpers -- e.g.
## LandR::prepSpeciesLayers_SCANFI -- call `googledrive::drive_ls()` DIRECTLY, before
## reproducible's prepInputs auto-auth runs; with no token that call falls back to a failing
## interactive `drive_auth()` ("Can't get Google credentials"). Interactive sessions are left to
## the user's own credentials.
Sys.unsetenv(c("GOOGLEDRIVE_AUTH", "GARGLE_SERVICE_ACCOUNT", "GOOGLE_APPLICATION_CREDENTIALS"))
if (file.exists("LandWeb.Renviron")) {
  readRenviron("LandWeb.Renviron")
  local({
    key <- Sys.getenv("GOOGLEDRIVE_AUTH")
    if (nzchar(key)) {
      ## absolute, so it still resolves after reproducible/prepInputs setwd() to a scratch dir
      ## mid-download on a crew worker
      key <- normalizePath(path.expand(key), mustWork = FALSE)
      Sys.setenv(GOOGLEDRIVE_AUTH = key)
      if (!interactive() && requireNamespace("googledrive", quietly = TRUE)) {
        tryCatch(
          {
            if (!file.exists(key)) {
              stop("no key file at ", key)
            }
            if (!identical(jsonlite::read_json(key)$project_id, "landweb-343704")) {
              stop("the key is not from the landweb-343704 project")
            }
            googledrive::drive_auth(path = key)
          },
          error = function(e) {
            Sys.unsetenv("GOOGLEDRIVE_AUTH")
            message("LandWeb: Drive service-account authentication failed: ", conditionMessage(e))
          }
        )
      }
    }
  })
}

## pre-load core packages so {targets} / tarborist static analysis resolves cleanly.
## Guarded so a not-yet-populated library (e.g. mid renv rebuild) doesn't abort startup.
suppressWarnings(suppressMessages(
  for (.pkg in c("dplyr", "sf", "targets", "geotargets")) {
    if (requireNamespace(.pkg, quietly = TRUE)) library(.pkg, character.only = TRUE)
  }
))
