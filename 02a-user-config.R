## user + machine specific configs

config.user <- switch(
  .user,

  ## Alex ------------------------------------------------------------------------------------------
  achubaty = list(
    args = list(
      cloud = list(
        googleUser = "achubaty@for-cast.ca",
        useCloud = FALSE
      ),
      notifications = list()
    ),
    options = list(
      reproducible.cacheSaveFormat = "rds", ## use "qs" when caching is fixed upstream
      reproducible.conn = SpaDES.config::dbConnCache("postgresql"),
      reproducible.useTerra = TRUE, ## TODO: add to config
      spades.memoryUseInterval = FALSE, ## TODO: temporary until callr/future cache bug fixed
      spades.useRequire = FALSE
    ),
    params = list(
      LandWeb_summary = list(
        .clInit = function() {
          ## NOTE: everything here has to be able to run from clean R session
          if (file.exists("LandWeb.Renviron")) readRenviron("LandWeb.Renviron") ## database credentials

          try(options(reproducible.conn = NULL)) ## ensure it's not set
          options(reproducible.conn = SpaDES.config::dbConnCache("postgresql"))

          return(invisible(NULL))
        },
        upload = FALSE ## TODO: use TRUE once `uploadTo` specified per study area
      )
    ),
    paths = list(
      scratchPath = if (dir.exists("/mnt/scratch"))  {
        file.path("/mnt/scratch", .user, basename(prjDir))
      } else {
        file.path("/tmp/scratch", basename(prjDir))
      }
    )
  ),

  ## Eliot -----------------------------------------------------------------------------------------
  emcintir = list(
    args = list(
      cloud = list(
        googleUser = "eliotmcintire@gmail.com",
        useCloud = FALSE
      ),
      notifications = list(
        slackChannel = "@eliotmcintire"
      )
    ),
    params = list(
      .plotInitialTime = NA
    ),
    paths = list(
      inputPaths = "~/data" ## aka dataCachePath
    )
  ),

  ## docker (user rstudio) -------------------------------------------------------------------------
  rstudio = list(
    args = list(
      cloud = list(
        googleUser = "", ## TODO
        useCloud = FALSE
      ),
      notifications = list(
        slackChannel = "" ## TODO
      )
    ),
    paths = list(
      cachePath = "cache_sqlite"
    )
  ),

  ## default (i.e, no changes) ---------------------------------------------------------------------
  list(
    args = list(
      cloud = list(
        googleUser = "",
        useCloud = FALSE
      )
    )
  )
)
