# project basics ------------------------------------------------------------------------------

if (file.exists("~/.Renviron")) readRenviron("~/.Renviron") ## GITHUB_PAT
if (file.exists("LandWeb.Renviron")) readRenviron("LandWeb.Renviron") ## database credentials

###### allow setting run context info from outside this script (e.g., bash script) -----------------
if (exists(".mode", .GlobalEnv)) {
  stopifnot(.mode %in% c("development", "postprocess", "production", "profile"))
} else {
  .mode <- "development"
}

if (exists(".rep", .GlobalEnv)) {
  .rep <- if (.mode == "postprocess") NA_integer_ else as.integer(.rep)
} else {
  .rep <- if (.mode == "postprocess") NA_integer_ else 1L
}

if (exists(".res", .GlobalEnv)) {
  stopifnot(.res %in% c(50, 125, 250))
} else {
  .res <- 250
}

if (!exists(".studyAreaName", .GlobalEnv)) {
  .studyAreaName <- "provMB"
}

if (exists(".version", .GlobalEnv)) {
  .version <- as.integer(.version)
  stopifnot(.version %in% c(2L, 3L))
} else {
  .version <- 2L ## 3L
}

.ncores <- min(parallel::detectCores() / 2, 24L)
.nodename <- Sys.info()[["nodename"]]
.user <- Sys.info()[["user"]]

if (.version == 2L) {
  if (exists(".dispersalType", .GlobalEnv)) {
    stopifnot(.dispersalType %in% c("default", "aspen", "high", "none"))
  } else {
    .dispersalType <- "high"
  }

  if (exists(".ROStype", .GlobalEnv)) {
    stopifnot(.ROStype %in% c("default", "burny", "equal", "log"))
  } else {
    .ROStype <- "log"
  }
}
#####

prjDir <- "~/GitHub/LandWeb"

stopifnot(identical(normalizePath(prjDir), normalizePath(getwd())))

## set new temp dir in scratch directory (existing /tmp too small for large callr ops in postprocessing)
## see https://github.com/r-lib/callr/issues/172
if (grepl("for-cast[.]ca", .nodename) && !grepl("larix", .nodename)) {
  oldTmpDir <- tempdir()
  newTmpDir <- file.path("/mnt/scratch", .user, basename(prjDir), "tmp")
  if (!dir.exists(newTmpDir)) dir.create(newTmpDir, recursive = TRUE)
  newTmpDir <- tools::file_path_as_absolute(newTmpDir)
  Sys.setenv(TMPDIR = newTmpDir)
  unlink(oldTmpDir, recursive = TRUE)
  tempdir(check = TRUE)
}

options(
  Ncpus = .ncores,
  repos = c(CRAN = "https://cran.rstudio.com")
)

# install and load packages -------------------------------------------------------------------

pkgDir <- file.path(tools::R_user_dir(basename(prjDir), "data"), "packages",
                    version$platform, getRversion()[, 1:2])
dir.create(pkgDir, recursive = TRUE, showWarnings = FALSE)
.libPaths(pkgDir, include.site = FALSE)
message("Using libPaths:\n", paste(.libPaths(), collapse = "\n"))

if (!"remotes" %in% rownames(installed.packages(lib.loc = .libPaths()[1]))) {
  install.packages("remotes")
}

Require.version <- "PredictiveEcology/Require@v0.2.6" ## use CRAN version
if (!"Require" %in% rownames(installed.packages(lib.loc = .libPaths()[1])) ||
    packageVersion("Require", lib.loc = .libPaths()[1]) < "0.2.6") {
  remotes::install_github(Require.version)
}

library(Require)

setLinuxBinaryRepo()

Require("sf (>= 1.0.10)", repos = "https://r-spatial.r-universe.dev/") ## TODO: temporary until this version is on CRAN

Require(c(
  "PredictiveEcology/SpaDES.project@transition (>= 0.0.7.9003)", ## TODO: use development once merged
  "PredictiveEcology/SpaDES.config@development (>= 0.0.2.9065)"
), upgrade = FALSE, standAlone = TRUE)

modulePkgs <- unname(unlist(packagesInModules(modulePath = file.path(prjDir, "m"))))
otherPkgs <- c("archive", "details", "DBI", "s-u/fastshp", "logging",
               "Rcpp (>= 1.0.10)", "RPostgres", "slackr",
               "PredictiveEcology/reproducible@development (>= 1.2.16.9024)",
               "PredictiveEcology/SpaDES.core@development (>= 1.1.1)",
               "terra (>= 1.7-3)")

Require(unique(c(modulePkgs, otherPkgs)), require = FALSE, standAlone = TRUE, upgrade = FALSE)

## NOTE: always load packages LAST, after installation above;
##       ensure plyr loaded before dplyr or there will be problems
Require(c("data.table", "plyr", "pryr", "SpaDES.core",
          "googledrive", "httr", "LandR", "LandWebUtils", "magrittr", "sessioninfo", "slackr"),
        upgrade = FALSE, standAlone = TRUE)

# configure project ---------------------------------------------------------------------------

## TODO: implement exptTbl stuff to pass values to config
if (FALSE) {
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

config <- SpaDES.config::useConfig(projectName = "LandWeb", projectPath = prjDir,
                                   mode = .mode, rep = .rep, res = .res,
                                   studyAreaName = .studyAreaName, version = .version)

if (.version == 2) {
  config$context[["dispersalType"]] <- .dispersalType
  config$context[["ROStype"]] <- .ROStype

  config$update()
  config$validate()
}

## apply user and machine context settings here
source("02-user-config.R")
config$args <- config.user$args
#config$modules <- config.user$modules ## no modules should differ among users/machines
config$options <- config.user$options
config$params <- config.user$params
config$paths <- config.user$paths

# print run info ------------------------------------------------------------------------------
SpaDES.config::printRunInfo(config$context)
config$modules

# project paths -------------------------------------------------------------------------------
config$paths
stopifnot(identical(checkPath(config$paths[["projectPath"]]), getwd()))

checkPath(config$paths[["logPath"]], create = TRUE) ## others will be created as needed below

paths <- SpaDES.config::paths4spades(config$paths)

# project options -----------------------------------------------------------------------------
opts <- SpaDES.config::setProjectOptions(config)

quickPlot::dev.useRSGD(useRSGD = quickPlot::isRstudioServer())

SpaDES.config::authGoogle(tryToken = "landweb", tryEmail = config$args[["cloud"]][["googleUser"]])

# begin simulations ---------------------------------------------------------------------------

do.call(SpaDES.core::setPaths, paths)

if (config$args[["delayStart"]] > 0) {
  message(crayon::green("\nStaggered job start: delaying by", config$args[["delayStart"]], "minutes."))
  Sys.sleep(config$args[["delayStart"]]*60)
}

objects1 <- list()

parameters1 <- list(
  .globals = config$params[[".globals"]],
  LandWeb_preamble = config$params[["LandWeb_preamble"]]
)

preambleFile <- file.path(paths[["outputPath"]], paste0(
  "simOutPreamble_", config$context[["studyAreaName"]], ".qs"
))

simOutPreamble <- Cache(simInitAndSpades,
                        times = list(start = 0, end = 1),
                        params = parameters1, ## TODO: use config$params
                        modules = c("LandWeb_preamble"), ## TODO: use config$modules
                        objects = objects1,
                        paths = paths,
                        debug = 1,
                        omitArgs = c("debug", "paths", ".plotInitialTime"),
                        useCloud = config$args[["cloud"]][["useCloud"]],
                        cloudFolderID = config$args[["cloud"]][["cacheDir"]],
                        userTags = c(config$studyAreaName, config$context[["runName"]], "preamble"))

if (isTRUE(attr(simOutPreamble, ".Cache")[["newCache"]])) {
  simOutPreamble@.xData[["._sessionInfo"]] <- projectSessionInfo(prjDir)
  saveRDS(simOutPreamble$ml, file.path(paths[["outputPath"]], "ml_preamble.rds")) ## TODO: use `qs::qsave()`
  saveSimList(simOutPreamble, preambleFile, fileBackend = 2)
}

# Species layers ------------------------------------------------------------------------------

parameters2 <- list(
  .globals = config$params[[".globals"]],
  Biomass_speciesData = config$params[["Biomass_speciesData"]]
)

objects2 <- list(
  #nonTreePixels = simOutPreamble[["nonTreePixels"]], ## TODO: confirm no longer required
  rasterToMatchLarge = simOutPreamble[["rasterToMatchLarge"]],
  sppColorVect = simOutPreamble[["sppColorVect"]],
  sppEquiv = simOutPreamble[["sppEquiv"]],
  studyAreaLarge = simOutPreamble[["studyAreaLarge"]],
  studyAreaReporting = simOutPreamble[["studyAreaReporting"]]
)

sppLayersFile <- file.path(paths[["outputPath"]], paste0(
  "simOutSpeciesLayers_", config$context[["studyAreaName"]], ".qs"
))

simOutSpeciesLayers <- Cache(simInitAndSpades,
                             times = list(start = 0, end = 1),
                             params = parameters2, ## TODO: use config$params
                             modules = c("Biomass_speciesData"),  ## TODO: use config$modules
                             objects = objects2,
                             paths = paths,
                             debug = 1,
                             omitArgs = c("debug", "paths", ".plotInitialTime"),
                             useCloud = config$args[["cloud"]][["useCloud"]],
                             cloudFolderID = config$args[["cloud"]][["cacheDir"]],
                             userTags = c(config$studyAreaName, config$context[["runName"]], "speciesLayers"))
if (isTRUE(attr(simOutSpeciesLayers, ".Cache")[["newCache"]])) {
  simOutSpeciesLayers@.xData[["._sessionInfo"]] <- projectSessionInfo(prjDir)
  saveSimList(simOutSpeciesLayers, sppLayersFile, fileBackend = 2)
}

if (config$context[["mode"]] != "postprocess") {
  # Boreal data prep + main sim -----------------------------------------------------------------
  parameters2a <- list(
    .globals = config$params[[".globals"]],
    Biomass_borealDataPrep = config$params[["Biomass_borealDataPrep"]]
  )

  objects2a <- list(
    cloudFolderID = config$args[["cloud"]][["cacheDir"]],
    rstLCC = simOutPreamble[["LCC"]],
    rasterToMatch = simOutPreamble[["rasterToMatch"]],
    rasterToMatchLarge = simOutPreamble[["rasterToMatchLarge"]],
    speciesLayers = simOutSpeciesLayers[["speciesLayers"]],
    speciesParams = simOutPreamble[["speciesParams"]],
    speciesTable = simOutPreamble[["speciesTable"]],
    sppColorVect = simOutPreamble[["sppColorVect"]],
    sppEquiv = simOutPreamble[["sppEquiv"]],
    standAgeMap = simOutPreamble[["CC TSF"]],
    studyArea = simOutPreamble[["studyArea"]],
    studyAreaLarge = simOutPreamble[["studyAreaLarge"]],
    studyAreaReporting = simOutPreamble[["studyAreaReporting"]]
  )

  dataPrepFile <- file.path(paths[["outputPath"]], paste0("simOutDataPrep_", config$context[["studyAreaName"]], ".qs"))
  simOutDataPrep <- Cache(simInitAndSpades,
                          times = list(start = 0, end = 1),
                          params = parameters2a, ## TODO: use config$params
                          modules = c("Biomass_borealDataPrep"), ## TODO: use config$modules
                          objects = objects2a,
                          paths = paths,
                          debug = 1,
                          omitArgs = c("debug", "paths", ".plotInitialTime"),
                          useCloud = config$args[["cloud"]][["useCloud"]],
                          cloudFolderID = config$args[["cloud"]][["cacheDir"]],
                          userTags = c(config$studyAreaName, config$context[["runName"]], "dataPrep"))
  if (isTRUE(attr(simOutDataPrep, ".Cache")[["newCache"]])) {
    simOutDataPrep@.xData[["._sessionInfo"]] <- projectSessionInfo(prjDir)
    ## TODO: enforce correct species table types (LandR#90)
    if (is(simOutDataPrep$species$postfireregen, "character")) {
      simOutDataPrep$species$postfireregen <- as.factor(simOutDataPrep$species$postfireregen)
    }

    saveSimList(simOutDataPrep, dataPrepFile, fileBackend = 2)
  }

  source("10-main-sim.R")
} else {
  ## postprocessing --------------------------------------------------------------------------------
  if (grepl("Manning", config$context[["runName"]])) {
    config$params[["timeSeriesTimes"]] <- 450:500
  }

  modules4 <- if (grepl("provMB", config$context[["studyAreaName"]])) {
    list(
      "HSI_Caribou_MB", ## TODO: add to config
      "LandWeb_summary"
    )
  } else {
    list(
      "LandWeb_summary"
    )
  }

  ## don't cache the init event!
  config$params[["HSI_Caribou_MB"]][[".useCache"]] <- c(".inputObjects", "postprocess")
  config$params[["LandWeb_summary"]][[".useCache"]] <- c(".inputObjects", "animation", "postprocess")

  ## NOTE: previous .useParallel value is too low for this module
  config$params[[".globals"]][[".useParallel"]] <- getOption("map.useParallel")
  config$params[["LandWeb_summary"]][[".useParallel"]] <- getOption("map.useParallel")

  getOption("map.maxNumCores") ## TODO: 48; why is this set so high??
  options(map.maxNumCores = .ncores)

  parameters4 <- list(
    .globals = config$params[[".globals"]],
    HSI_caribou_MB = config$params[["HSI_Caribou_MB"]], ## TODO: add to config
    LandWeb_summary = config$params[["LandWeb_summary"]]
  )

  objects4 <- list(
    ml = simOutPreamble[["ml"]],
    speciesLayers = simOutSpeciesLayers[["speciesLayers"]],
    sppColorVect = simOutPreamble[["sppColorVect"]],
    sppEquiv = simOutPreamble[["sppEquiv"]]
  )

  outputs4 <- NULL

  fsim <- simFile("simOutSummaries", paths[["outputPath"]], NULL, "qs")

  tryCatch({
    simOutSummaries <- Cache(simInitAndSpades,
                             times = list(start = 0, end = 1),
                             params = parameters4, ## TODO: use config$params
                             modules = modules4, ## TODO: use config$modules
                             #outputs = outputs4,
                             objects = objects4,
                             paths = paths,
                             loadOrder = unlist(modules4), ## TODO: use config$modules
                             #cl = cl, ## TODO: get parallel processing working !!!
                             debug = list(file = list(file = file.path(config$paths[["logPath"]], "summaries.log"),
                                                      append = TRUE), debug = 1),
                             useCloud = FALSE, ## TODO param useCloud??
                             cloudFolderID = config$args[["cloud"]][["cacheDir"]],
                             omitArgs = c("debug", "paths"),
                             userTags = c(config$context[["runName"]], "postprocess"))
    cat(capture.output(warnings()), file = file.path(config$paths[["logPath"]], "warnings_postprocess.txt"), sep = "\n")
  }, error = function(e) {
    if (requireNamespace("slackr") & file.exists("~/.slackr")) {
      slackr::slackr_setup()
      slackr::slackr_msg(
        paste0("ERROR in post-processing `", config$context[["runName"]],
               "` on host `", config$context[["machine"]], "`.\n",
               "```\n", e$message, "\n```"),
        channel = config$args[["notifications"]][["slackChannel"]], preformatted = FALSE
      )
      stop(e$message)
    }
  })

  if (isTRUE(attr(simOutSummaries, ".Cache")[["newCache"]])) {
    simOutSummaries@.xData[["._sessionInfo"]] <- projectSessionInfo(prjDir)
    message("Saving simulation to: ", fsim)
    saveSimList(sim = simOutSummaries, filename = fsim, fileBackend = 2)

    # save simulation stats -----------------------------------------------------------------------

    elapsed <- elapsedTime(simOutSummaries)
    data.table::fwrite(elapsed, file.path(config$paths[["logPath"]], "elapsedTime_summaries.csv"))
    qs::qsave(elapsed, file.path(config$paths[["logPath"]], "elapsedTime_summaries.qs"))

    memory <- memoryUse(simOutSummaries, max = TRUE)
    data.table::fwrite(memory, file.path(config$paths[["logPath"]], "memoryUsed_summaries.csv"))
    qs::qsave(memory, file.path(config$paths[["logPath"]], "memoryUsed_summaries.qs"))
  }

  # archive and upload --------------------------------------------------------------------------
  #source("R/upload.R") ## TODO: not working correctly yet

  # end-of-sim notifications --------------------------------------------------------------------

  if (requireNamespace("slackr") & file.exists("~/.slackr")) {
    slackr::slackr_setup()
    slackr::slackr_msg(
      paste0("Post-processing for `", config$context[["runName"]], "` completed on host `", Sys.info()[["nodename"]], "`."),
      channel = config$args[["notifications"]][["slackChannel"]], preformatted = FALSE
    )
  }
}

relOutputPath <- SpaDES.config:::.getRelativePath(paths[["outputPath"]], prjDir)
rrFile <- file.path(relOutputPath, "INFO.md")
cat(SpaDES.config::printRunInfo(config$context), file = rrFile, sep = "")
cat(SpaDES.project::reproducibilityReceipt(), file = rrFile, sep = "\n", append = TRUE)

#source("11-post-sim.R")
