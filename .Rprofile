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
## Non-interactive sessions log in as the service account when googledrive loads, because some
## download helpers -- e.g. LandR::prepSpeciesLayers_SCANFI -- call `googledrive::drive_ls()`
## DIRECTLY, before reproducible's prepInputs auto-auth runs; with no token that call falls back
## to a failing interactive `drive_auth()` ("Can't get Google credentials"). They never use a
## cached personal token (gargle_oauth_email = FALSE): if the service-account login fails, Drive
## calls fail instead of running as a person. Interactive sessions use your own login (run
## googledrive::drive_auth() once).
if (!interactive()) {
  options(gargle_oauth_email = FALSE)
}
Sys.unsetenv(c("GOOGLEDRIVE_AUTH", "GARGLE_SERVICE_ACCOUNT", "GOOGLE_APPLICATION_CREDENTIALS"))
if (file.exists("LandWeb.Renviron")) {
  readRenviron("LandWeb.Renviron")
  local({
    key <- Sys.getenv("GOOGLEDRIVE_AUTH")
    if (!nzchar(key)) {
      return(invisible())
    }
    ## absolute, so it still resolves after reproducible/prepInputs setwd() to a scratch dir
    ## mid-download on a crew worker
    key <- normalizePath(path.expand(key), mustWork = FALSE)
    ## a fixed message on a bad key file: a JSON parse error would quote part of the key
    info <- if (file.exists(key)) tryCatch(jsonlite::read_json(key), error = function(e) NULL)
    problem <- if (!file.exists(key)) {
      paste("no key file at", key)
    } else if (is.null(info)) {
      "the key file could not be read as JSON"
    } else if (!identical(info$project_id, "landweb-343704")) {
      "the key is not from the landweb-343704 project"
    }
    if (!is.null(problem)) {
      Sys.unsetenv("GOOGLEDRIVE_AUTH")
      message("LandWeb: not using the Drive service account: ", problem)
      return(invisible())
    }
    Sys.setenv(GOOGLEDRIVE_AUTH = key)
    if (!interactive()) {
      setHook(packageEvent("googledrive", "onLoad"), function(...) {
        tryCatch(
          gargle::with_cred_funs(
            list(credentials_service_account = gargle::credentials_service_account),
            googledrive::drive_auth(path = key)
          ),
          error = function(e) {
            message("LandWeb: Drive service-account login failed: ", conditionMessage(e))
          }
        )
      })
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
