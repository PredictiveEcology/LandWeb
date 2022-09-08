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

# project config ------------------------------------------------------------------------------
config.default = list(
  batchMode = FALSE,
  cloud = list(
    cacheDir = "LandWeb_cloudCache",
    googleUser = "",
    useCloud = FALSE
  ),
  delayStart = 0,
  deleteSpeciesLayers = FALSE,
  options = list(
    fftempdir = file.path(dirname(tempdir()), "scratch", "LandWeb", "ff"),
    future.globals.maxSize = 1000*1024^2,
    LandR.assertions = FALSE,
    LandR.verbose = 1,
    map.dataPath = "inputs", # not used yet
    map.overwrite = TRUE,
    map.tilePath = file.path("outputs", "tiles"),
    map.useParallel = TRUE,
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
    spades.messagingNumCharsModule = 36,
    spades.moduleCodeChecks = FALSE,
    spades.qsThreads = 4,
    spades.recoveryMode = FALSE,
    spades.restartR.restartDir = "outputs",
    spades.useRequire = FALSE # Don't use Require... meaning assume all pkgs installed
  ),
  params = list(
    ageClasses = c("Young", "Immature", "Mature", "Old"), ## LandWebUtils:::.ageClasses
    ageClassCutOffs = c(0, 40, 80, 120), ## LandWebUtils:::.ageClassCutOffs
    endTime = 1000,
    eventCaching = c(".inputObjects", "init"),
    fireTimestep = 1,
    forestedLCCClasses = c(1:15, 20, 32, 34:36),
    maxAge = 400,
    maxFireReburns = 4,
    maxFireRetries = 10,
    minFRI = 25,
    successionTimeStep = 10,
    summaryPeriod = c(700, 1000),
    summaryInterval = 100,
    timeSeriesTimes = 601:650,
    vegLeadingProportion = 0.8, # indicates what proportion the stand must be in one species group for it to be leading.
                                # If all are below this, then it is a "mixed" stand
    .plotInitialTime = 0,
    .plots = c("object", "png", "raw", "screen")
  ),
  paths = list(
    cachePath = "cache",
    inputPath = "inputs",
    inputPaths = NULL,
    modulePath = "m",
    outputPath = "outputs",
    projectPath = "~/GitHub/LandWeb",
    scratchPath = file.path(dirname(tempdir()), "scratch", "LandWeb"),
    tilePath = file.path("outputs", "tiles")
  ),
  POM = list(
    useDEoptim = FALSE,
    usePOM = FALSE ## NOTE: TO and FROM indices must be defined
  ),
  postProcessOnly = FALSE,
  rerunDataPrep = TRUE,
  rerunSpeciesLayers = TRUE,
  restartR = list(
    interval = NA ## 100 (NA to disable)
  ),
  runInfo = list(
    friMultiple = 1,
    mapResFact = 1,
    rep = 1,
    scenarioDisp = "", # "aspenDispersal", "highDispersal", "noDispersal", or "" for default
    scenarioFire = "", # "equalROS", "logROS", or "" for default,
    studyAreaName = "random",
    succession = TRUE
  ),
  test = FALSE,
  useParallel = 2, ## values > 2 use WAY too much RAM for very little speed increase (too much overhead)
  useSpades = TRUE,
  version = 2
)

config.profile <- Require::modifyList2(
  config.default, list(
    params = list(
      endTime = 20,
      successionTimeStep = 10,
      summaryPeriod = c(10, 20),
      summaryInterval = 10,
      timeSeriesTimes = 10,
      .plotInitialTime = 0
    ),
    runInfo = list(
      rep = 1,
      studyarea = "Tolko_SK",
      scenarioDisp = "",
      scenarioFire = ""
    )
  )
)

config.testing <- Require::modifyList2(
  config.default, list(
    params = list(
      endTime = 300,
      successionTimeStep = 10,
      summaryPeriod = c(250, 300),
      summaryInterval = 10,
      timeSeriesTimes = 201:210
    ),
    test = TRUE
  )
)

config.production <- Require::modifyList2(
  config.default, list(
    batchMode = TRUE, ## if TRUE, don't specify mapResFact, rep, studyarea, scenario*
    cloud = list(
      useCloud = TRUE
    ),
    delayStart = sample(5L:15L, 1), # 5-15 minute delay to stagger starts
    params = list(
      endTime = 1000,
      successionTimeStep = 10,
      summaryPeriod = c(700, 1000),
      summaryInterval = 100,
      timeSeriesTimes = 601:650,
      .plots = c("object", "png", "raw") ## don't plot to screen
    )
  )
)

if (identical(.user, "achubaty")) {
  config <- Require::modifyList2(
    config.production, list(
      cloud = list(
        googleUser = "achubaty@for-cast.ca",
        useCloud = FALSE
      ),
      options = list(
        reproducible.conn = dbConnCache("postgresql")
      ),
      paths = list(
        scratchPath = switch(.nodename,
                            "larix.for-cast.ca" = "/tmp/scratch/LandWeb",
                            "/mnt/scratch/LandWeb")
      ),
      slackChannel = "@alex.chubaty",
      version = 2 # 3
    )
  )
}

if (identical(.user, "eliot")) {
  config <- Require::modifyList2(
    config.testing, list(
      batchMode = TRUE, ## if TRUE, don't specify rep, studyarea, scenario*
      cloud = list(
        googleUser = "eliotmcintire@gmail.com",
        useCloud = FALSE
      ),
      params = list(
        .plotInitialTime = NA
      ),
      paths = list(
        gitpkgpath = "~/GitHub",
        inputPaths = "~/data"
      ),
      slackChannel = "@eliotmcintire",
      runInfo = list(
        # studyarea = "LandWeb",
        # scenarioDisp = "highDispersal",
        # scenarioFire = "logROS"
        run = 1,
      ),
      useSpades = TRUE
    )
  )
}

## docker user
if (identical(.user, "rstudio")) {
  config <- Require::modifyList2(
    config.production, list(
      cloud = list(
        googleUser = "achubaty@for-cast.ca",
        useCloud = FALSE
      ),
      paths = list(
        cachedir = "cache_sqlite"
      ),
      slackChannel = "@alex.chubaty"
    )
  )
}

## update config based on user config values
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

config <- Require::modifyList2(
  config, list(
    params = list(
      forestedLCCClasses = ifelse(grepl("no2032", config$runInfo$runName), c(1:15, 34:36), config$params$forestedLCCClasses)
    ),
    paths = list(
      tilePath = file.path(config$paths$outputPath, config$runInfo$runName, "tiles")
    ),
    rerunDataPrep = ifelse(grepl("LandWeb", config$runInfo$runName), FALSE, config$rerunDataPrep),
    rerunSpeciesLayers = ifelse(grepl("LandWeb", config$runInfo$runName), FALSE, config$rerunSpeciesLayers),
    runInfo = list(
      friMultiple = getFRImultiple(config$runInfo$runName),
      mapResFact = getMapResFact(config$runInfo$runName),
      studyAreaName = if (grepl("FMU", config$runInfo$runName)) {
        paste(strsplit(config$runInfo$runName, "_")[[1]][1:2], collapse = "_")
      } else {
        strsplit(config$runInfo$runName, "_")[[1]][1]
      },
      succession = !grepl("noSuccession", config$runInfo$runName)
    )
  )
)
