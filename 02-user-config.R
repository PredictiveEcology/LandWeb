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
      notifications = list(
        slackChannel = "@alex.chubaty"
      )
    ),
    options = list(
      reproducible.cacheSaveFormat = "qs",
      reproducible.conn = SpaDES.config::dbConnCache("postgresql"),
      reproducible.useTerra = TRUE, ## TODO: add to config
      spades.memoryUseInterval = FALSE ## TODO: temporary until callr/future cache bug fixed
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
      scratchPath = switch(.nodename,
                           `larix.for-cast.ca` = file.path("/tmp/scratch", basename(prjDir)),
                           file.path("/mnt/scratch", .user, basename(prjDir)))
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
  list()
)
