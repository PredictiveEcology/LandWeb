# environment variables -----------------------------------------------------------------------
if (file.exists("~/.Renviron")) readRenviron("~/.Renviron") ## GITHUB_PAT, etc.
if (file.exists("LandWeb.Renviron")) readRenviron("LandWeb.Renviron") ## database credentials

# use renv for package management -------------------------------------------------------------
if (!grepl("renv", .libPaths()[1])) {
  source("renv/activate.R")
}

# project setup (includes package installation etc.) ------------------------------------------

prjDir <- SpaDES.project::findProjectPath()

stopifnot(identical(prjDir, normalizePath(getwd())))

source("01a-globalvars.R")

## set new temp dir in scratch directory (existing /tmp too small for large callr ops in postprocessing)
## see https://github.com/r-lib/callr/issues/172
if (grepl("for-cast[.]ca", .nodename) && !grepl("larix", .nodename)) {
  newTmpDir <- file.path("/mnt/scratch", .user, basename(prjDir), "tmp")
  tmpdir::setTmpDir(newTmpDir, rmOldTempDir = TRUE)
}

options(
  Ncpus = .ncores,
  repos = c(CRAN = "https://cloud.r-project.org")
)

# source("01-setup.R") ## defunct package installation; now done using `renv`.
library("data.table")
library("plyr")
library("pryr")
library("reproducible")
library("SpaDES.core")
library("googledrive")
library("httr")
library("LandR")
library("LandWebUtils")
library("notifications")

source("R/cache_helpers.R") ## TODO: remove once reproducible updated to latest version

# configure project ---------------------------------------------------------------------------
source("02-configure.R")

# begin simulations ---------------------------------------------------------------------------

do.call(SpaDES.core::setPaths, paths)

if (config$args[["delayStart"]] > 0) {
  message(crayon::green("\nStaggered job start: delaying by", config$args[["delayStart"]], "minutes."))
  Sys.sleep(config$args[["delayStart"]]*60)
}

objects1 <- list()

config$params[["LandWeb_preamble"]][["mergeSlivers"]] <- FALSE ## TODO: awaiting decision re: merging slivers

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
                        userTags = c(config$context[["studyAreaName"]], config$context[["runName"]], "preamble"))

if (isUpdated(simOutPreamble) || isFALSE(config$args[["useCache"]])) {
  simOutPreamble@.xData[["._sessionInfo"]] <- SpaDES.project::projectSessionInfo(prjDir)
  saveRDS(simOutPreamble$ml, file.path(paths[["outputPath"]], "ml_preamble.rds")) ## TODO: use `qs::qsave()`
  saveSimList(simOutPreamble, preambleFile, fileBackend = 2)
}

# Species layers ------------------------------------------------------------------------------

config$params[[".globals"]][[".plots"]] <- c("png") ## TODO: change in config
config$params[["Biomass_speciesData"]][[".plots"]] <- c("png") ## TODO: change in config

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
                             userTags = c(config$context[["studyAreaName"]], config$context[["runName"]], "speciesLayers"))

if (isUpdated(simOutSpeciesLayers) || isFALSE(config$args[["useCache"]])) {
  simOutSpeciesLayers@.xData[["._sessionInfo"]] <- SpaDES.project::projectSessionInfo(prjDir)
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
                          userTags = c(config$context[["studyAreaName"]], config$context[["runName"]], "dataPrep"))
  ## TODO: enforce correct species table types (LandR#90)
  if (is(simOutDataPrep$species$postfireregen, "character")) {
    simOutDataPrep$species$postfireregen <- as.factor(simOutDataPrep$species$postfireregen)
  }

  if (isUpdated(simOutDataPrep) || isFALSE(config$args[["useCache"]])) {
    simOutDataPrep@.xData[["._sessionInfo"]] <- SpaDES.project::projectSessionInfo(prjDir)
    saveSimList(simOutDataPrep, dataPrepFile, fileBackend = 2)
  }

  source("10-main-sim.R")
} else {
  ## postprocessing --------------------------------------------------------------------------------
  if (grepl("Manning", config$context[["runName"]])) {
    config$params[["timeSeriesTimes"]] <- 450:500
  }

  ## TODO: use config
  modules4 <- list(
    "burnSummaries",
    "LandMine", ## using 'multi' mode
    "LandWeb_summary"
  )

  if (grepl("provMB", config$context[["studyAreaName"]])) {
    modules4 <- append(modules4, list("HSI_Caribou_MB"))
  }

  ## don't cache the init event
  config$params[["HSI_Caribou_MB"]][[".useCache"]] <- c(".inputObjects", "postprocess")
  config$params[["LandWeb_summary"]][[".useCache"]] <- c(".inputObjects", "animation", "postprocess")

  ## NOTE: previous .useParallel value is too low for this module
  config$params[[".globals"]][[".useParallel"]] <- getOption("map.useParallel")
  config$params[["LandWeb_summary"]][[".useParallel"]] <- getOption("map.useParallel")

  ## adjust N reps as needed:
  if (config$context[["studyAreaName"]] == "LandWeb_full") {
    config$params[["LandWeb_summary"]][["reps"]] <- 1L:50L
  }

  getOption("map.maxNumCores")
  options(map.maxNumCores = .ncores)

  parameters4 <- list(
    .globals = config$params[[".globals"]],
    burnSummaries = list( ## TODO: add to config
      reps = 1L:15L,
      simOutputPath = "outputs",
      .studyAreaName = config$context[["studyAreaName"]]
    ),
    HSI_caribou_MB = config$params[["HSI_Caribou_MB"]], ## TODO: add to config
    LandMine = list( ## TODO: add to config
      mode = "multi",
      reps = 1L:15L,
      simOutputPath = "outputs",
      .studyAreaName = config$context[["studyAreaName"]]
    ),
    LandWeb_summary = config$params[["LandWeb_summary"]]
  )

  objects4 <- list(
    flammableMap = simOutPreamble[["rstFlammable"]],
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
    if (requireNamespace("notifications") & file.exists("~/.rgooglespaces")) {
      notifications::notify_google(
        paste0("ERROR in post-processing `", config$context[["runName"]],
               "` on host `", config$context[["machine"]], "`.\n",
               "```\n", e$message, "\n```")
      )
      stop(e$message)
    }
  })

  if (isTRUE(attr(simOutSummaries, ".Cache")[["newCache"]])) {
    simOutSummaries@.xData[["._sessionInfo"]] <- SpaDES.project::projectSessionInfo(prjDir)
    message("Saving simulation to: ", fsim)
    saveSimList(sim = simOutSummaries, filename = fsim, fileBackend = 2)

    # save simulation info ------------------------------------------------------------------------
    relOutputPath <- SpaDES.config:::.getRelativePath(paths[["outputPath"]], prjDir)
    rrFile <- file.path(relOutputPath, "INFO.md")
    cat(SpaDES.config::printRunInfo(config$context), file = rrFile, sep = "")
    cat(SpaDES.project::reproducibilityReceipt(), file = rrFile, sep = "\n", append = TRUE)

    # save simulation stats -----------------------------------------------------------------------
    elapsed <- elapsedTime(simOutSummaries)
    data.table::fwrite(elapsed, file.path(config$paths[["logPath"]], "elapsedTime_summaries.csv"))
    qs::qsave(elapsed, file.path(config$paths[["logPath"]], "elapsedTime_summaries.qs"))

    if (!isFALSE(getOption("spades.memoryUseInterval"))) {
      memory <- memoryUse(simOutSummaries, max = TRUE)
      data.table::fwrite(memory, file.path(config$paths[["logPath"]], "memoryUsed_summaries.csv"))
      qs::qsave(memory, file.path(config$paths[["logPath"]], "memoryUsed_summaries.qs"))
    }
  }

  # archive and upload --------------------------------------------------------------------------
  if (isTRUE(.upload)) {
    source("R/upload.R")
  }

  # end-of-sim notifications --------------------------------------------------------------------
  if (requireNamespace("notifications") & file.exists("~/.rgooglespaces")) {
    notifications::notify_google(
      paste0("Post-processing for `", config$context[["runName"]],
             "` completed on host `", config$context[["machine"]], "`.")
    )
  }
}

#source("11-post-sim.R")
