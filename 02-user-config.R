## user + machine specific configs

config.landweb.user <- switch(
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
      reproducible.conn = SpaDES.config::dbConnCache("postgresql")
    ),
    params = list(
      LandWeb_summary = list(
        upload = FALSE ## TODO: use TRUE once `uploadTo` specified per study area
      )
    ),
    paths = list(
      scratchPath = switch(.nodename,
                           `larix.for-cast.ca` = "/tmp/scratch/LandWeb",
                           `pinus.for-cast.ca` = "/mnt/scratch/achubaty/LandWeb",
                          "/mnt/scratch/LandWeb")
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
