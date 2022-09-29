# project basics ------------------------------------------------------------------------------

if (file.exists(".Renviron")) readRenviron(".Renviron") ## GITHUB_PAT and database credentials

.mode <- "development"  # "development", "postprocess", "production", "profile"
.nodename <- Sys.info()[["nodename"]]
.starttime <- Sys.time()
.studyAreaName <- "provMB"
.user <- Sys.info()[["user"]]

prjDir <- "~/GitHub/LandWeb"

options(
  Ncpus = min(parallel::detectCores() / 2, 120),
  repos = c(CRAN = "https://cran.rstudio.com"),
  Require.RPackageCache = "default", ## will use default package cache directory: `RequirePkgCacheDir()`
  Require.usepak = FALSE ## pkg deps too complicated for pak
)

# install and load packages -------------------------------------------------------------------

pkgDir <- file.path("packages", version$platform, substr(getRversion(), 1, 3))
dir.create(pkgDir, recursive = TRUE, showWarnings = FALSE)
.libPaths(pkgDir, include.site = FALSE)
message("Using libPaths:\n", paste(.libPaths(), collapse = "\n"))

if (!"remotes" %in% rownames(installed.packages(lib.loc = .libPaths()[1]))) {
  install.packages("remotes")
}

Require.version <- "PredictiveEcology/Require@development"
if (!"Require" %in% rownames(installed.packages(lib.loc = .libPaths()[1])) ||
    packageVersion("Require", lib.loc = .libPaths()[1]) < "0.1.3") {
  remotes::install_github(Require.version)
}
library(Require)

setLinuxBinaryRepo()

Require(c("PredictiveEcology/SpaDES.project@transition (>= 0.0.7)", ## TODO: use development once merged
          "PredictiveEcology/SpaDES.config@development (>= 0.0.2.9002)"),
        upgrade = FALSE, standAlone = TRUE)

if (FALSE) {
  # install.packages("pak")
  # pak::pkg_install(.spatialPkgs)
  # install.packages(.spatialPkgs, repos = "https://cran.r-project.org")
  # install.packages(c("raster", "terra"), repos = "https://rspatial.r-universe.dev")
  sf::sf_extSoftVersion() ## want at least GEOS 3.9.0, GDAL 3.2.1, PROJ 7.2.1
}

modulePkgs <- unname(unlist(packagesInModules(modulePath = file.path(prjDir, "m"))))
otherPkgs <- c("animation", "archive", "assertthat", "config", "crayon", "devtools", "DBI",
               "s-u/fastshp",
               "PredictiveEcology/LandR@development (>= 1.1.0.9001)",
               "PredictiveEcology/LandWebUtils@development",
               "lhs", "logging", "parallel", "qs", "quickPlot", "RCurl", "RPostgres",
               "PredictiveEcology/SpaDES.core@development (>= 1.1.0.9000)",
               "scales", "slackr", "XML")

Require(unique(c(modulePkgs, otherPkgs)), require = FALSE, standAlone = TRUE, upgrade = FALSE)

## NOTE: always load packages LAST, after installation above;
##       ensure plyr loaded before dplyr or there will be problems
Require(c("data.table", "plyr", "pryr", "SpaDES.core",
          "archive", "future", "future.callr", "googledrive", "httr", "magrittr", "slackr"),
        upgrade = FALSE, standAlone = TRUE)

# configure project ---------------------------------------------------------------------------

config <- SpaDES.config::landwebConfig$new()$update(
  paths = list(
    projectPath = prjDir
  )
)$validate() ## TODO: wrap in a helper e.g. SpaDES.config::useConfig(type = "LandWeb")

if (FALSE) { ## TODO: implement exptTbl stuff
  fExptTbl <- file.path(prjDir, "experimentTable.csv")
  if (!file.exists(fExptTbl)) {
    exptTbl <- expand.grid(
      .studyAreaName = c("ANC", "AlPac", "BlueRidge", "DMI", "Edson", "FMANWT",
                         "LandWeb", "LP_BC", "LP_MB",
                         "Manning", "MillarWestern", "Mistik", "SprayLake", "Sundre", "Tolko", ## TODO: check e.g. Tolko_SK etc.
                         "Vanderwell", "WeyCo", "WestFraser",
                         "provAB", "provMB", "provNWT", "provSK"),
      delayStart = TRUE,
      dispersalType = c("default"),
      endTime = 1000,
      forceResprout = c(FALSE),
      friMultiple = c(1L),
      pixelSize = 250,
      rep = c(1L:15L, NA_integer_), ## NA for postprocessing runs
      ROStype = c("default"),
      succession = c(TRUE)
    )
    exptTbl$postProcessOnly <- FALSE

    ## postprocessing runs -----------
    exptTbl[is.na(exptTbl$rep), ]$postProcessOnly <- TRUE
    exptTbl[is.na(exptTbl$rep), ]$delayStart <- FALSE

    ## scheduling --------------------
    exptTbl$._targetMachine <- NA
    exptTbl[exptTbl$.studyAreaName %in% c("LandWeb", "provMB"), ]$._targetMachine <- "pseudotsuga.for-cast.ca"

    exptTbl$._targetMemory <- NA

    ## status and tracking -----------
    ## TODO: populate these from completed sims for MB
    exptTbl$._status <- NA ## "queued", "started", "completed", "error"
    exptTbl$._runtime <- NA
    exptTbl$._memory <- NA

    write.csv(exptTbl, fExptTbl)
  } else {
    ## TODO: local csv; google sheet; database
    #exptTbl <- getExperimentTable(fExptTbl)
    exptTable <- read.csv(fExptTbl)
  }
}

context <- SpaDES.config::useContext("LandWeb", mode = .mode, studyAreaName = .studyAreaName, version = 3)
#context <- SpaDES.config::updateContext(context, exptTbl) ## TODO: use context to filter row in the exptTbl
config <- SpaDES.config::updateLandWebConfig(config, context = context)

## TODO: apply user and machine context settings here
source("02-user-config.R")
config$update(
  args = config.landweb.user$args,
  #modules = config.landweb.user$modules, ## no modules should differ among users/machines
  options = config.landweb.user$options,
  params = config.landweb.user$params,
  paths = config.landweb.user$paths
)$validate() ## TODO: wrap in a helper e.g. SpaDES.config::userConfig(config, config.user)

# print run info ------------------------------------------------------------------------------
SpaDES.config::printRunInfo(context)
config$modules
config$paths

# project paths -------------------------------------------------------------------------------
stopifnot(identical(checkPath(config$paths$projectPath), getwd()))
paths <- SpaDES.config::paths4spades(config$paths)

# project options -----------------------------------------------------------------------------
opts <- SpaDES.config::setProjectOptions(config)

quickPlot::dev.useRSGD(useRSGD = quickPlot::isRstudioServer())

SpaDES.config::authGoogle(tryToken = "landweb", tryEmail = config$args$cloud$googleUser)

# begin simulations ---------------------------------------------------------------------------

do.call(SpaDES.core::setPaths, paths)

objects1 <- list()

parameters1 <- list(
  .globals = config$params$.globals,
  LandWeb_preamble = config$params$LandWeb_preamble
)

preambleFile <- file.path(Paths$inputPath, paste0(
  "simOutPreamble_", context$studyAreaName, ".qs"
))

simOutPreamble <- Cache(simInitAndSpades,
                        times = list(start = 0, end = 1),
                        params = parameters1, ## TODO: use config$params
                        modules = c("LandWeb_preamble"), ## TODO: use config$modules
                        objects = objects1,
                        paths = paths,
                        debug = 1,
                        omitArgs = c("debug", "paths", ".plotInitialTime"),
                        useCache = TRUE,
                        useCloud = config$args$cloud$useCloud,
                        cloudFolderID = config$args$cloud$cacheDir)
simOutPreamble@.xData[["._sessionInfo"]] <- projectSessionInfo(prjDir)
saveRDS(simOutPreamble$ml, file.path(Paths$outputPath, "ml_preamble.rds")) ## TODO: use `qs::qsave()`
saveSimList(Copy(simOutPreamble), preambleFile, fileBackend = 2)

# Species layers ------------------------------------------------------------------------------

## TODO: RESUME (HERE)

parameters2 <- list(
  .globals = config$params$.globals,
  Biomass_speciesData = config$params$Biomass_speciesData
)

objects2 <- list(
  #nonTreePixels = simOutPreamble[["nonTreePixels"]], ## TODO: confirm no longer required
  rasterToMatchLarge = simOutPreamble[["rasterToMatchLarge"]],
  sppColorVect = simOutPreamble[["sppColorVect"]],
  sppEquiv = simOutPreamble[["sppEquiv"]],
  studyAreaLarge = simOutPreamble[["studyAreaLarge"]],
  studyAreaReporting = simOutPreamble[["studyAreaReporting"]]
)

sppLayersFile <- file.path(Paths$inputPath, paste0(
  "simOutSpeciesLayers_", context$studyAreaName, ".qs"
))

simOutSpeciesLayers <- Cache(simInitAndSpades,
                             times = list(start = 0, end = 1),
                             params = parameters2, ## TODO: use config$params
                             modules = c("Biomass_speciesData"),  ## TODO: use config$modules
                             objects = objects2,
                             omitArgs = c("debug", "paths", ".plotInitialTime"),
                             useCache = TRUE,
                             useCloud = config$args$cloud$useCloud,
                             cloudFolderID = config$args$cloud$cacheDir,
                             ## make .plotInitialTime an argument, not a parameter:
                             ##  - Cache will see them as unchanged regardless of value
                             paths = paths,
                             debug = 1)
simOutSpeciesLayers@.xData[["._sessionInfo"]] <- projectSessionInfo(prjDir)
saveSimList(Copy(simOutSpeciesLayers), sppLayersFile, fileBackend = 2)

if ("screen" %in% config$params$.globals$.plots) {
  lapply(dev.list(), function(x) {
    try(quickPlot::clearPlot(force = TRUE))
    try(dev.off())
  })
  quickPlot::dev(3, width = 18, height = 10)
  grid::grid.rect(0.90, 0.03, width = 0.2, height = 0.06, gp = gpar(fill = "white", col = "white"))
  grid::grid.text(label = context$studyAreaName, x = 0.90, y = 0.03)

  Plot(simOutSpeciesLayers$speciesLayers)
}

# Boreal data prep + main sim -----------------------------------------------------------------

if (context$mode != "postprocess") {
  parameters2a <- list(
    .globals = config$params$.globals,
    Biomass_borealDataPrep = config$params$Biomass_borealDataPrep
  )

  objects2a <- list(
    cloudFolderID = config$args$cloud$cacheDir,
    rstLCC = simOutPreamble[["LCC"]],
    rasterToMatch = simOutPreamble[["rasterToMatch"]],
    rasterToMatchLarge = simOutPreamble[["rasterToMatchLarge"]],
    speciesLayers = simOutSpeciesLayers[["speciesLayers"]],
    speciesParams = simOutPreamble[["speciesParams"]],
    speciesTable = simOutPreamble[["speciesTable"]],
    speciesTableAreas = c("BSW", "BP", "MC"), ## TODO: should we remove BP? MC?
    sppColorVect = simOutPreamble[["sppColorVect"]],
    sppEquiv = simOutPreamble[["sppEquiv"]],
    standAgeMap = simOutPreamble[["CC TSF"]],
    studyArea = simOutPreamble[["studyArea"]],
    studyAreaLarge = simOutPreamble[["studyAreaLarge"]]
  )

  ## TODO: confirm filename ok w/ diff parameterizations (e.g. v2 vs v3)
  dataPrepFile <- file.path(Paths$inputPath, paste0("simOutDataPrep_", context$studyAreaName, ".qs"))
  simOutDataPrep <- Cache(simInitAndSpades,
                          times = list(start = 0, end = 1),
                          params = parameters2a, ## TODO: use config$params
                          modules = c("Biomass_borealDataPrep"), ## TODO: use config$modules
                          objects = objects2a,
                          omitArgs = c("debug", "paths", ".plotInitialTime"),
                          useCache = TRUE, ## TODO: use param useCache??
                          useCloud = config$args$cloud$useCloud,
                          cloudFolderID = config$args$cloud$cacheDir,
                          .plots = config$params$.globals$.plots,
                          paths = paths,
                          debug = 1)
  simOutDataPrep@.xData[["._sessionInfo"]] <- projectSessionInfo(prjDir)
  saveSimList(simOutDataPrep, dataPrepFile, fileBackend = 2)

  source("10-main-sim.R")
} else {
  #mySimOut <- loadSimList(simFile("mySimOut", Paths$outputPath, 1000))
  source("12-postprocessing.R")
}

SpaDES.project::reproducibilityReceipt()

#source("11-post-sim.R")
