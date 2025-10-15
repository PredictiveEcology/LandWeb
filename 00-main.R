# environment variables -----------------------------------------------------------------------

if (file.exists("~/.Renviron")) {
  readRenviron("~/.Renviron") ## GITHUB_PAT, etc.
}

if (file.exists("LandWeb.Renviron")) {
  readRenviron("LandWeb.Renviron") ## database credentials
}

# packages ------------------------------------------------------------------------------------

## use renv for package management
if (!grepl("renv", .libPaths()[1])) {
  source("renv/activate.R")
}

## load essential packgaes
library("data.table")
library("plyr")
library("pryr")
library("reproducible")
library("SpaDES.core")

library("googledrive")
library("httr")
# pkgload::load_all("~/GitHub/PredictiveEcology/LandR")
# pkgload::load_all("packages/LandWebUtils")
library("LandR")
library("LandWebUtils")
library("notifications")

# simulation setup ----------------------------------------------------------------------------

prjDir <- workflowtools::findProjectPath()

stopifnot(identical(prjDir, normalizePath(getwd(), winslash = "/")))

workflowtools::check_project_packages(prjDir)

source("01a-globalvars.R")

# source("01c-exptTbl.R") ## TODO

source("02-configure.R") ## will also run user config

# begin simulations ---------------------------------------------------------------------------

## paths
paths_shared <- config$paths |>
  modifyList(list(
    logPath = file.path(config$paths[["sharedOutputPath"]], "log") |> fs::dir_create(),
    outputPath = config$paths[["sharedOutputPath"]]
  ))

do.call(SpaDES.core::setPaths, SpaDES.config::paths4spades(config$paths))

if (config$args[["delayStart"]] > 0) {
  message(crayon::green(
    "\nStaggered job start: delaying by",
    config$args[["delayStart"]],
    "minutes."
  ))
  Sys.sleep(config$args[["delayStart"]] * 60)
}

## preamble ----------------------------------------------------------------------------------------

objects1 <- list()

parameters1 <- list(
  .globals = config$params[[".globals"]],
  LandWeb_preamble = config$params[["LandWeb_preamble"]]
)

preambleFile <- simFile(
  name = paste0("simOutPreamble_", config$context[["studyAreaName"]]),
  path = config$paths[["sharedOutputPath"]], ## use shared path
  ext = config$args[["fsimext"]]
)

tryCatch(
  {
    simOutPreamble <- Cache(
      simInitAndSpades,
      times = list(start = 0, end = 1),
      params = parameters1, ## TODO: use config$params
      modules = c("LandWeb_preamble"), ## TODO: use config$modules
      objects = objects1,
      paths = SpaDES.config::paths4spades(paths_shared),
      debug = list(
        file = list(
          file = file.path(paths_shared[["logPath"]], "01-preamble.log"),
          append = TRUE
        ),
        debug = 1
      ),
      omitArgs = c("debug", "paths", ".plotInitialTime"),
      useCache = config$args[["useCache"]],
      useCloud = config$args[["cloud"]][["useCloud"]],
      cloudFolderID = config$args[["cloud"]][["cacheDir"]],
      userTags = c(config$context[["studyAreaName"]], config$context[["runName"]], "preamble")
    )
  },
  error = function(e) {
    if (requireNamespace("notifications") && file.exists("~/.rgooglespaces")) {
      notifications::notify_google(
        paste0(
          "ERROR in preamble `",
          config$context[["runName"]],
          "` on host `",
          config$context[["machine"]],
          "`.\n",
          "```\n",
          e$message,
          "\n```"
        )
      )
      stop(e$message)
    }
  }
)

if (isUpdated(simOutPreamble) || isFALSE(config$args[["useCache"]])) {
  simOutPreamble@.xData[["._sessionInfo"]] <- workflowtools::projectSessionInfo(prjDir)
  ## TODO: save async using e.g., mirai or future
  saveSimList(
    simOutPreamble,
    preambleFile,
    inputs = FALSE,
    outputs = FALSE,
    cache = FALSE,
    files = FALSE
  )
}

## cleanup
gc()
terra::tmpFiles(remove = TRUE)

# Species layers ------------------------------------------------------------------------------

parameters2 <- list(
  .globals = config$params[[".globals"]],
  Biomass_speciesData = config$params[["Biomass_speciesData"]]
)

## TODO: confirm input objects
objects2 <- list(
  # nonTreePixels = simOutPreamble[["nonTreePixels"]], ## TODO: confirm no longer required
  rasterToMatch_biomassParam = simOutPreamble[["rasterToMatch_biomassParam"]],
  sppColorVect = simOutPreamble[["sppColorVect"]],
  sppEquiv = simOutPreamble[["sppEquiv"]],
  studyArea_biomassParam = simOutPreamble[["studyArea_biomassParam"]],
  studyAreaReporting = simOutPreamble[["studyAreaReporting"]]
)

sppLayersFile <- simFile(
  name = paste0("simOutSpeciesLayers_", config$context[["studyAreaName"]]),
  path = config$paths[["sharedOutputPath"]], ## use shared path
  ext = config$args[["fsimext"]]
)

tryCatch(
  {
    simOutSpeciesLayers <- Cache(
      simInitAndSpades,
      times = list(start = 0, end = 1),
      params = parameters2, ## TODO: use config$params
      modules = c("Biomass_speciesData"), ## TODO: use config$modules
      objects = objects2,
      paths = SpaDES.config::paths4spades(paths_shared),
      debug = list(
        file = list(
          file = file.path(paths_shared[["logPath"]], "02-speciesLayers.log"),
          append = TRUE
        ),
        debug = 1
      ),
      omitArgs = c("debug", "paths", ".plotInitialTime"),
      useCache = config$args[["useCache"]],
      useCloud = config$args[["cloud"]][["useCloud"]],
      cloudFolderID = config$args[["cloud"]][["cacheDir"]],
      userTags = c(config$context[["studyAreaName"]], config$context[["runName"]], "speciesLayers")
    )
  },
  error = function(e) {
    if (requireNamespace("notifications") && file.exists("~/.rgooglespaces")) {
      notifications::notify_google(
        paste0(
          "ERROR in species layers `",
          config$context[["runName"]],
          "` on host `",
          config$context[["machine"]],
          "`.\n",
          "```\n",
          e$message,
          "\n```"
        )
      )
      stop(e$message)
    }
  }
)

if (isUpdated(simOutSpeciesLayers) || isFALSE(config$args[["useCache"]])) {
  simOutSpeciesLayers@.xData[["._sessionInfo"]] <- workflowtools::projectSessionInfo(prjDir)
  ## TODO: save async using e.g., mirai or future
  saveSimList(
    simOutSpeciesLayers,
    sppLayersFile,
    inputs = FALSE,
    outputs = FALSE,
    cache = FALSE,
    files = FALSE
  )
}

## cleanup
gc()
terra::tmpFiles(remove = TRUE)

if (config$context[["mode"]] != "postprocess") {
  ## data prep -------------------------------------------------------------------------------------

  myMinRelativeB <- function(pixelCohortData) {
    pixelData <- unique(pixelCohortData, by = "pixelIndex")
    pixelData[, ecoregionGroup := factor(as.character(ecoregionGroup))] ## resorts them in order
    minRelativeB <- data.frame(
      ecoregionGroup = as.factor(levels(pixelData$ecoregionGroup)),
      data.frame(
        X1 = 0.15, ## 0.15
        X2 = 0.25, ## 0.25
        X3 = 0.35, ## 0.50
        X4 = 0.45, ## 0.75
        X5 = 0.55  ## 0.85
      )
    )

    return(minRelativeB)
  }

  config$params[["Biomass_borealDataPrep"]] <- list(
    minRelativeBFunction = quote(myMinRelativeB(pixelCohortData))
  )

  ## we don't want to rerun the factorial for each rep
  factorial_files <- list(
    cohortDataFactorial = fs::dir_ls(
      path = config$paths[["sharedOutputPath"]],
      recurse = FALSE,
      regexp = "cohortDataFactorial_.*[.]df"
    ),
    speciesTableFactorial = fs::dir_ls(
      path = config$paths[["sharedOutputPath"]],
      recurse = FALSE,
      regexp = "speciesTableFactorial_.*[.]df"
    )
  )

  n_cdf_files <- length(factorial_files$cohortDataFactorial)
  n_stf_files <- length(factorial_files$speciesTableFactorial)

  stopifnot(n_cdf_files <= 1, n_stf_files <= 1)

  run_factorial <- n_cdf_files == 0 && n_stf_files == 0

  if (run_factorial) {
    modules2a <- c(
      "Biomass_speciesFactorial",
      "Biomass_borealDataPrep",
      "Biomass_speciesParameters"
    ) ## TODO: use config$modules

    parameters2a <- list(
      .globals = config$params[[".globals"]],
      Biomass_borealDataPrep = config$params[["Biomass_borealDataPrep"]],
      Biomass_speciesFactorial = config$params[["Biomass_speciesFactorial"]],
      Biomass_speciesParameters = config$params[["Biomass_speciesParameters"]]
    )
  } else {
    modules2a <- c(
      "Biomass_borealDataPrep",
      "Biomass_speciesParameters"
    ) ## TODO: use config$modules

    parameters2a <- list(
      .globals = config$params[[".globals"]],
      Biomass_borealDataPrep = config$params[["Biomass_borealDataPrep"]],
      Biomass_speciesParameters = config$params[["Biomass_speciesParameters"]]
    )
  }

  objects2a <- list(
    cloudFolderID = config$args[["cloud"]][["cacheDir"]],
    rstLCC = simOutPreamble[["LCC"]],
    rasterToMatch = simOutPreamble[["rasterToMatch"]],
    rasterToMatch_biomassParam = simOutPreamble[["rasterToMatchLarge"]],
    speciesLayers = simOutSpeciesLayers[["speciesLayers"]],
    speciesParams = simOutPreamble[["speciesParams"]],
    speciesTable = simOutPreamble[["speciesTable"]],
    sppColorVect = simOutPreamble[["sppColorVect"]],
    sppEquiv = simOutPreamble[["sppEquiv"]],
    standAgeMap = simOutPreamble[["CC_TSF"]],

    ## study area polygons now need to be SpatVectors downstream in LandR Biomass???
    studyArea = simOutPreamble[["studyArea"]] |> terra::vect(),
    studyAreaANPP = simOutPreamble[["studyAreaANPP"]] |> terra::vect(),
    studyArea_biomassParam = simOutPreamble[["studyArea_biomassParam"]] |> terra::vect(),
    studyAreaReporting = simOutPreamble[["studyAreaReporting"]] |> terra::vect()
  )

  if (!run_factorial) {
    objects2a <- list(
      cohortDataFactorial_path = factorial_files$cohortDataFactorial,
      speciesTableFactorial_path = factorial_files$speciesTableFactorial
    ) |>
      append(objects2a)
  }

  ### data prep outputs ----------------------------------------------------------------------------

  dataPrepFile <- simFile(
    name = paste0("simOutDataPrep_", config$context[["studyAreaName"]]),
    path = config$paths[["sharedOutputPath"]], ## use shared path
    ext = config$args[["fsimext"]]
  )

  outputs2a <- data.frame(
    objectName = c(
      "ecoregionMap",
      "speciesEcoregion",
      "species" ## adjusted species traits table
    ),
    saveTime = c(1, 1, 1),
    fun = c("writeRaster", "write.csv", "write.csv"),
    package = c("terra", "base", "base"),
    file = c(
      "ecoregionMap_year0000.tif",
      "speciesEcoregion_year0000.csv",
      "speciesTraits_adjusted.csv"
    ),
    stringsAsFactors = FALSE
  )
  outputs2a$arguments <- I(list(
    list(overwrite = TRUE, progress = FALSE),
    list(row.names = FALSE),
    list(row.names = FALSE)
  ))

  ### run data prep simulation ---------------------------------------------------------------------

  tryCatch(
    {
      simOutDataPrep <- Cache(
        simInitAndSpades,
        times = list(start = 0, end = 1),
        params = parameters2a, ## TODO: use config$params
        modules = modules2a,
        objects = objects2a,
        outputs = outputs2a,
        paths = SpaDES.config::paths4spades(paths_shared),
        ## TODO: debug list is being used as `verbose` option in inputObject caching
        # debug = list(
        #   file = list(
        #     file = file.path(paths_shared[["logPath"]], "02a-dataPrep.log"),
        #     append = TRUE
        #   ),
        #   debug = 1
        # ),
        omitArgs = c("debug", "paths", ".plotInitialTime"),
        useCache = config$args[["useCache"]],
        useCloud = config$args[["cloud"]][["useCloud"]],
        cloudFolderID = config$args[["cloud"]][["cacheDir"]],
        userTags = c(config$context[["studyAreaName"]], config$context[["runName"]], "dataPrep")
      )
    },
    error = function(e) {
      if (requireNamespace("notifications") && file.exists("~/.rgooglespaces")) {
        notifications::notify_google(
          paste0(
            "ERROR in data prep `",
            config$context[["runName"]],
            "` on host `",
            config$context[["machine"]],
            "`.\n",
            "```\n",
            e$message,
            "\n```"
          )
        )
        stop(e$message)
      }
    }
  )

  if (isUpdated(simOutDataPrep) || isFALSE(config$args[["useCache"]])) {
    simOutDataPrep@.xData[["._sessionInfo"]] <- workflowtools::projectSessionInfo(prjDir)
    ## TODO: save async using e.g., mirai or future
    saveSimList(
      simOutDataPrep,
      dataPrepFile,
      inputs = FALSE,
      outputs = FALSE,
      cache = FALSE,
      files = FALSE
    )
  }

  ## cleanup
  gc()
  terra::tmpFiles(remove = TRUE)

  ## main simulation -------------------------------------------------------------------------------
  source("03-main-sim.R")
} else {
  ## postprocessing --------------------------------------------------------------------------------

  ## TODO: use config
  modules4 <- list(
    "burnSummaries",
    "LandMine", ## using 'multi' mode
    "LandWeb_summary"
  )

  if (grepl("provMB", config$context[["studyAreaName"]])) {
    modules4 <- append(modules4, list("HSI_Caribou_MB"))
  }

  ## TODO: don't use map option
  ## NOTE: previous .useParallel value is too low for this module
  options(map.maxNumCores = min(.ncores, getOption("map.maxNumCores")))
  config$params[[".globals"]][[".useParallel"]] <- getOption("map.maxNumCores")
  config$params[["LandWeb_summary"]][[".useParallel"]] <- getOption("map.maxNumCores")

  ## adjust N reps as needed:
  config$params[[".globals"]][["reps"]] <- 1L:15L ## TODO: more reps?
  config$params[["burnSummaries"]][["reps"]] <- config$params[[".globals"]][["reps"]]
  config$params[["LandMine"]][["reps"]] <- config$params[[".globals"]][["reps"]]
  config$params[["LandWeb_summary"]][["reps"]] <- config$params[[".globals"]][["reps"]]

  # config$params[["LandWeb_summary"]][["standAgeMapFromCohorts"]] <- FALSE

  parameters4 <- list(
    .globals = config$params[[".globals"]],
    burnSummaries = config$params[["burnSummaries"]], ## TODO: exclude for old runs
    HSI_caribou_MB = config$params[["HSI_Caribou_MB"]],
    LandMine = config$params[["LandMine"]],
    LandWeb_summary = config$params[["LandWeb_summary"]]
  )

  objects4 <- list(
    flammableMap = simOutPreamble[["rstFlammable"]],
    ml = simOutPreamble[["ml"]],
    speciesLayers = simOutSpeciesLayers[["speciesLayers"]],
    sppColorVect = simOutPreamble[["sppColorVect"]],
    sppEquiv = simOutPreamble[["sppEquiv"]],
    studyArea = simOutPreamble[["studyArea"]],
    studyAreaReporting = simOutPreamble[["studyAreaReporting"]]
  )

  outputs4 <- NULL

  summariesFile <- simFile(
    name = "simOutSummaries",
    path = config$paths[["outputPath"]],
    ext = config$args[["fsimext"]]
  )

  tryCatch(
    {
      simOutSummaries <- Cache(
        simInitAndSpades,
        times = list(start = 0, end = 1),
        params = parameters4, ## TODO: use config$params
        modules = modules4, ## TODO: use config$modules
        # outputs = outputs4,
        objects = objects4,
        paths = SpaDES.config::paths4spades(config$paths),
        loadOrder = unlist(modules4), ## TODO: use config$modules
        # cl = cl, ## TODO: get parallel processing working !!!
        debug = list(
          file = list(
            file = file.path(config$paths[["logPath"]], "04-summaries.log"),
            append = TRUE
          ),
          debug = 1
        ),
        useCache = config$args[["useCache"]],
        useCloud = FALSE, ## TODO param useCloud??
        cloudFolderID = config$args[["cloud"]][["cacheDir"]],
        omitArgs = c("debug", "paths"),
        userTags = c(config$context[["runName"]], "postprocess")
      )
      cat(
        capture.output(warnings()),
        file = file.path(config$paths[["logPath"]], "warnings_postprocess.txt"),
        sep = "\n"
      )
    },
    error = function(e) {
      if (requireNamespace("notifications") && file.exists("~/.rgooglespaces")) {
        notifications::notify_google(
          paste0(
            "ERROR in post-processing `",
            config$context[["runName"]],
            "` on host `",
            config$context[["machine"]],
            "`.\n",
            "```\n",
            e$message,
            "\n```"
          )
        )
        stop(e$message)
      }
    }
  )

  if (isTRUE(attr(simOutSummaries, ".Cache")[["newCache"]])) {
    simOutSummaries@.xData[["._sessionInfo"]] <- workflowtools::projectSessionInfo(prjDir)
    message("Saving simulation to: ", summariesFile)
    ## TODO: save async using e.g., mirai or future
    saveSimList(
      simOutSummaries,
      summariesFile,
      inputs = FALSE,
      outputs = FALSE,
      cache = FALSE,
      files = FALSE
    )

    # save simulation info ------------------------------------------------------------------------
    relOutputPath <- SpaDES.config:::.getRelativePath(paths[["outputPath"]], prjDir)
    rrFile <- file.path(relOutputPath, "INFO.md")
    cat(SpaDES.config::printRunInfo(config$context), file = rrFile, sep = "")
    cat(SpaDES.project::reproducibilityReceipt(), file = rrFile, sep = "\n", append = TRUE)

    # save simulation stats -----------------------------------------------------------------------
    elapsed <- elapsedTime(simOutSummaries)
    data.table::fwrite(elapsed, file.path(config$paths[["logPath"]], "elapsedTime_summaries.csv"))
    qs2::qs_save(elapsed, file.path(config$paths[["logPath"]], "elapsedTime_summaries.qs2"))

    if (!isFALSE(getOption("spades.memoryUseInterval"))) {
      memory <- memoryUse(simOutSummaries, max = TRUE)
      data.table::fwrite(memory, file.path(config$paths[["logPath"]], "memoryUsed_summaries.csv"))
      qs2::qs_save(memory, file.path(config$paths[["logPath"]], "memoryUsed_summaries.qs2"))
    }
  }

  # archive and upload --------------------------------------------------------------------------
  if (isTRUE(.upload)) {
    source("R/upload.R")
  }

  # end-of-sim notifications --------------------------------------------------------------------
  if (requireNamespace("notifications") && file.exists("~/.rgooglespaces")) {
    notifications::notify_google(
      paste0(
        "Post-processing for `",
        config$context[["runName"]],
        "` completed on host `",
        config$context[["machine"]],
        "`."
      )
    )
  }
}

## cleanup
gc()
terra::tmpFiles(remove = TRUE)

# source("04-post-sim.R")
