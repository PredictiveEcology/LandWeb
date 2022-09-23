quickPlot::dev.useRSGD(useRSGD = quickPlot::isRstudioServer())

# helper functions ----------------------------------------------------------------------------
dbConnCache <- function(type = "sql") {
  conn <- if (type == "sql") {
    Require("RSQLite")
    NULL
  } else if (type == "postgresql") {
    Require("RPostgres")
    DBI::dbConnect(drv = RPostgres::Postgres(),
                   host = Sys.getenv("PGHOST"),
                   port = Sys.getenv("PGPORT"),
                   dbname = Sys.getenv("PGDATABASE"),
                   user = Sys.getenv("PGUSER"),
                   password = Sys.getenv("PGPASSWORD"))
  }

  return(conn)
}

getDispersal <- function(runName) {
  disp <- grep("Dispersal", strsplit(runName, "_")[[1]], value = TRUE)
  disp <- if (length(disp) > 0) disp else ""
  return(disp)
}

getFRImultiple <- function(runName) {
  stopifnot(!is.null(runName), is.character(runName))
  frim <- strsplit(runName, "_")[[1]] %>%
    grep("fri", ., value = TRUE) %>%
    substr(., 4, 6) %>%
    as.numeric(.)

  if (identical(frim, numeric(0))) frim <- 1 ## use 1 when not specified, e.g. for old runs

  frim
}

getMapResFact <- function(runName) {
  stopifnot(!is.null(runName), is.character(runName))
  res <- strsplit(runName, "_")[[1]] %>%
    grep("res", ., value = TRUE) %>%
    substr(., 4, 6) %>%
    as.integer(.)

  if (identical(res, integer(0))) res <- 250 ## use 250 when not specified, e.g. for old runs

  if (res %in% c(50, 125, 250)) {
    250 / res
  } else {
    warning("res should be one of 250, 125, or 50. using value specified by config.yml.")
    config::get("mapResFact")
  }
}

getOutputPath <- function(runName) {
  file.path("outputs", getRunNamePostProcess(runName),
            if (is.na(getRep(runName))) "" else sprintf("rep%02d", getRep(runName)))
}

getRunNamePostProcess <- function(runName) {
  strsplit(runName, "_")[[1]] %>%
    grep("rep", ., value = TRUE, invert = TRUE) %>%
    paste(., collapse = "_")
}

getRep <- function(runName) {
  rep <- as.integer(gsub("rep", "", grep("rep", strsplit(runName, "_")[[1]], value = TRUE)))
  rep <- if (length(rep) > 0) rep else NA_integer_
  return(rep)
}

getROS <- function(runName) {
  ros <- grep("ROS", strsplit(runName, "_")[[1]], value = TRUE)
  ros <- if (length(ros) > 0) ros else ""
  return(ros)
}

getStudyAreaName <- function(runName) {
  if (grepl("FMU", runName)) {
    paste(strsplit(runName, "_")[[1]][1:2], collapse = "_")
  } else {
    strsplit(runName, "_")[[1]][1]
  }
}

getVersion <- function(runName) {
  v <- grep("^v[0-9]$", strsplit(runName, "_")[[1]], value = TRUE)
  v <- if (length(v) > 0) as.integer(substr(v, 2, nchar(v))) else 2L
  return(v)
}

# project config ------------------------------------------------------------------------------
config.landweb.default = list(
  args = list(
    cloud = list(
      cacheDir = "LandWeb_cloudCache",
      googleUser = "",
      useCloud = FALSE
    ),
    delayStart = 0,
    endTime = 1000,
    notifications = list(
      slackChannel = ""
    )
  ),
  options = list(
    fftempdir = file.path(dirname(tempdir()), "scratch", "LandWeb", "ff"),
    future.globals.maxSize = 1000*1024^2,
    LandR.assertions = FALSE,
    LandR.verbose = 1,
    map.dataPath = "inputs", # not used yet
    map.overwrite = TRUE,
    map.tilePath = file.path("outputs", "tiles"),
    map.useParallel = FALSE, ## TODO: parallel processing in map needs to be fixed!
    rasterMaxMemory = 5e+9,
    rasterTmpDir = file.path(dirname(tempdir()), "scratch", "raster"),
    reproducible.cacheSaveFormat = "rds", ## can be "qs" or "rds"
    reproducible.conn = dbConnCache("sqlite"), ## "sqlite" or "postgresql"
    reproducible.destinationPath = normPath("inputs"),
    reproducible.inputPaths = NULL,
    reproducible.nThreads = 2,
    reproducible.overwrite = TRUE,
    reproducible.showSimilar = TRUE,
    reproducible.useGDAL = FALSE, ## NOTE: gdal is faster, but mixing gdal with raster causes inconsistencies
    reproducible.useTerra = FALSE, ## TODO: update + test with terra
    Require.RPackageCache = "default", ## will use default package cache directory: `RequirePkgCacheDir()`
    spades.memoryUseInterval = 60, ## track memory use every X seconds
    spades.messagingNumCharsModule = 36,
    spades.moduleCodeChecks = FALSE,
    spades.qsThreads = 4,
    spades.recoveryMode = FALSE,
    spades.useRequire = FALSE # Don't use Require... meaning assume all pkgs installed
  ),
  params = list(
    .globals = list(
      fireTimestep = 1L,
      sppEquivCol = "LandWeb",
      successionTimestep = 10,
      vegLeadingProportion = 0.8,
      .plotInitialTime = 0,
      .plots = c("object", "png", "raw", "screen"),
      .sslVerify = 0L, ## TODO: temporary to deal with NFI server SSL issues
      .studyAreaName = "random",
      .useCache = c(".inputObjects", "init"),
      .useParallel = 2 ## doesn't benefit from more DT threads
    ),
    Biomass_borealDataPrep = list(
      biomassModel = quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode +
                           (logAge + cover | ecoregionGroup))),
      ecoregionLayerField = "ECOREGION", # "ECODISTRIC"
      forestedLCCClasses = c(1:15, 20, 32, 34:36), ## should match preamble's treeClassesLCC
      LCCClassesToReplaceNN = 34:36,
      # next two are used when assigning pixelGroup membership; what resolution for
      #   age and biomass
      pixelGroupAgeClass = 2 * 10,  ## twice the successionTimestep; can be coarse because initial conditions are irrelevant
      pixelGroupBiomassClass = 1000, ## 1000 / mapResFact^2; can be coarse because initial conditions are irrelevant
      subsetDataAgeModel = 100,
      subsetDataBiomassModel = 100,
      speciesUpdateFunction = list(
        quote(LandR::speciesTableUpdate(sim$species, sim$speciesTable, sim$sppEquiv, P(sim)$sppEquivCol)),
        quote(LandR::updateSpeciesTable(sim$species, sim$speciesParams))
      ),
      useCloudForStats = TRUE
    ),
    Biomass_core = list(
      initialBiomassSource = "cohortData",
      seedingAlgorithm = "wardDispersal",
      .maxMemory = if (format(pemisc::availableMemory(), units = "GiB") > 130) 5 else 2 ## GB
    ),
    Biomass_regeneration = list(
      ## use module defaults (unless specified in .globals)
    ),
    Biomass_speciesData = list(
      omitNonVegPixels = TRUE,
      types = c("KNN", "CASFRI", "Pickell", "ForestInventory")
    ),
    LandMine = list(
      biggestPossibleFireSizeHa = 5e5,
      burnInitialTime = 1L,
      maxReburns = 20L,
      maxRetriesPerID = 4L,
      minPropBurn = 0.90,
      ROSother = 30L,
      useSeed = NULL ## NULL to avoid setting a seed, which makes all simulation identical!
    ),
    LandWeb_output = list(
      summaryInterval = 100
    ),
    LandWeb_preamble = list(
      bufferDist = 20000,        ## 20 km buffer
      bufferDistLarge = 50000,   ## 50 km buffer
      dispersalType = "default",
      friMultiple = 1L,
      mapResFact = 1L,
      minFRI = 25L,
      ROStype = "default",
      treeClassesLCC = c(1:15, 20, 32, 34:36) ## should match B_bDP's forestedLCCClasses
    ),
    LandWeb_summary = list(
      ageClasses = c("Young", "Immature", "Mature", "Old"), ## LandWebUtils:::.ageClasses
      ageClassCutOffs = c(0, 40, 80, 120),                  ## LandWebUtils:::.ageClassCutOffs
      ageClassMaxAge = 400L, ## was `maxAge` previously
      reps = 1L:15L, ## TODO: used elsewhere to setup runs (expt table)?
      simOutputPath = paths$postProcessingPath, ## TODO:
      summaryPeriod = c(700, 1000), ## TODO: this as also used in 09-pre-sim.R
      summaryInterval = 100,
      timeSeriesTimes = 601:650,
      upload = FALSE
    ),
    timeSinceFire = list(
      startTime = 1L,
      .useCache = c(".inputObjects") ## faster without caching for "init"
    )
  ),
  paths = list(
    cachePath = "cache",
    inputPath = "inputs",
    inputPaths = NULL, ## aka dataCachePath
    modulePath = "m",
    outputPath = "outputs",
    projectPath = "~/GitHub/LandWeb",
    scratchPath = file.path(dirname(tempdir()), "scratch", "LandWeb"),
    tilePath = file.path("outputs", "tiles")
  )
)

## additional configs to apply ---------------------------------------------------------------------

config.landweb.profile <- list(
  args = list(
    endTime = 20,
    successionTimestep = 10,
    summaryPeriod = c(10, 20),
    summaryInterval = 10,
    timeSeriesTimes = 10
  ),
  params = list(
    .globals = list(
      .plotInitialTime = 0,
      .studyAreaName = "Tolko_SK"
    )
  )
)

config.landweb.testing <- list(
  args = list(
    endTime = 300,
    successionTimestep = 10,
    summaryPeriod = c(250, 300),
    summaryInterval = 10,
    timeSeriesTimes = 201:210
  )
)

config.landweb.production <- list(
  args = list(
    cloud = list(
      useCloud = TRUE
    ),
    delayStart = sample(5L:15L, 1), # 5-15 minute delay to stagger starts
    endTime = 1000,
    successionTimestep = 10,
    summaryPeriod = c(700, 1000),
    summaryInterval = 100,
    timeSeriesTimes = 601:650
  ),
  params = list(
    .globals = list(
      .plots = c("object", "png", "raw") ## don't plot to screen
    )
  )
)

config.landweb.studyarea.fmu <- list(
  params = list(
    Biomass_borealDataPrep = list(
      biomassModel = quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode + (1 | ecoregionGroup)))
    )
  )
)

## TODO: need separate config per study area

config.landweb.studyarea.provMB <- list(
  params = list(
    Biomass_specieData - list(
      types = c("KNN", "CASFRI", "Pickell", "MBFRI")
    ),
    LandWeb_summary = list(
      uploadTo = "" ## TODO: use google-ids.csv to define these per WBI?
    )
  )
)

config.landweb.nosuccession <- list(
  modules = list(
    main = list("LandMine", "LandWeb_output", "timeSinceFire")
  )
)

config.landweb.postprocess <- list(
  modules = list(
    dataPrep = list(), ## don't run dataPrep
    main = list(), ## don't run main
    postprocess = list("LandWeb_summaries")
  ),
  paths = list()
)

config.landweb.dispersal.none <- list(
  params = list(
    Biomass_core = c(
      seedingAlgorithm = "noDispersal"
    ),
    LandWeb_preamble = list(
      dispersalType = "none"
    )
  )
)

config.landweb.dispersal.aspen <- list(
  params = list(
    LandWeb_preamble = list(
      dispersalType = "aspen"
    )
  )
)

config.landweb.dispersal.high <- list(
  params = list(
    LandWeb_preamble = list(
      dispersalType = "high"
    )
  )
)

config.landweb.ros.equal <- list(
  params = list(
    LandMine = list(
      ROSother = 1L
    ),
    LandWeb_preamble = list(
      ROStype = "equal"
    )
  )
)

config.landweb.ros.log <- list(
  params = list(
    LandMine = list(
      ROSother = log(30L)
    ),
    LandWeb_preamble = list(
      ROStype = "log"
    )
  )
)

config.landweb.resprout.forced <- list(
  params = list(
    LandWeb_preamble = list(
      forceResprout = TRUE
    )
  )
)

config.landweb.frimultiple <- list(
  params = list(
    Biomass_borealDataPrep = list(
      pixelGroupBiomassClass = 1000 / (mapResFact^2), ## 1000 / mapResFact^2; can be coarse because initial conditions are irrelevant
    )
  )
)

config.landweb.pixelsize <- list(
  params = list(
    Biomass_borealDataPrep = list(
      pixelGroupBiomassClass = 1000 / (mapResFact^2), ## 1000 / mapResFact^2; can be coarse because initial conditions are irrelevant
    )
  )
)

config.landweb.user.achubaty <- list(
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
    reproducible.conn = dbConnCache("postgresql")
  ),
  params = list(
    LandWeb_summary = list(
      upload = TRUE, ## `uploadTo` specified per study area
    )
  ),
  paths = list(
    scratchPath = switch(.nodename,
                        "larix.for-cast.ca" = "/tmp/scratch/LandWeb",
                        "/mnt/scratch/LandWeb")
  )
)

config.landweb.user.emcintir <- list(
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
)

## for docker
config.landweb.user.rstudio <- list(
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
)

################
# update config based on context
config <- updateConfig(config, context = c("production",
                                           user = .user,
                                           machine = .nodename,
                                           studyAreaName = "provMB",
                                           postProcess = "no",
                                           dispersalType = "aspen",
                                           forceResprout = TRUE,
                                           ROStype = "log",
                                           friMultiple = 1L,
                                           pixelSize = 250)) ## TODO: include run/rep
####################

## update config based on user config values
if (is.na(getRep(runName))) {
  config$postProcessOnly <- TRUE
}

if (isTRUE(config$batchMode)) {
  config$runInfo$runName <- runName
} else  {
  config$runInfo$runName <- paste0(
    config$runInfo$studyarea,
    "_", config$runInfo$scenarioDisp,
    "_", config$runInfo$scenarioFire,
    "_fri", config$runInfo$friMultiple,
    "_res", 250 / config$runInfo$mapResFact,
    if (isTRUE(config$test)) "_test" else "",
    if (isTRUE(config$postProcessOnly)) "" else sprintf("_rep%02g", config$runInfo$rep)
  )
}

config$runInfo$runNamePostProcess <- getRunNamePostProcess(runName)
rm(runName)

config <- Require::modifyList2(
  config, list(
    args = list(
      analysesOutputsTimes = seq(config$params$summaryPeriod[1], config$params$summaryPeriod[2],
                                 by = config$params$summaryInterval)
    ),
    paths = list(
      outputPath = getOutputPath(config$runInfo$runName),
      tilePath = asPath(file.path("outputs", config$runInfo$runNamePostProcess, "tiles"))
    )
  )
)

# validate config -----------------------------------------------------------------------------

## TODO: generalize and put these in a package somewhere

nullConfigVals <- rapply(config, is.null, how = "unlist")

stopifnot(
  all(nullConfigVals %in% FALSE),
  identical(length(unique(names(nullConfigVals))), length(unique(tolower(names(nullConfigVals)))))
)

## NB: only use accessor to retrieve values (to guard against partial matching and NULL values)
config.get <- function(config, name) {
  val <- switch(length(name),
                `1` = config[[name[1]]],
                `2` = config[[name[1]]][[name[2]]],
                `3` = config[[name[1]]][[name[2]]][[name[3]]],
                `4` = config[[name[1]]][[name[2]]][[name[3]]][[name[4]]],
                `5` = config[[name[1]]][[name[2]]][[name[3]]][[name[4]]][[name[5]]])

  stopifnot(!is.null(val))

  return(val)
}

## USAGE:
## config.get(config, c("params", "ageClasses"))
## config.get(config, c("params", "forestedLCCClasses"))
