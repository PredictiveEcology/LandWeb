################################################################################
## main simulation
################################################################################

do.call(SpaDES.core::setPaths, paths$paths3)

times3 <- list(start = 0, end = config.get(config, c("params", "endTime")))
modules3 <- if (isTRUE(config.get(config, c("runInfo", "succession")))) {
  list("Biomass_core", "LandMine", "Biomass_regeneration", "LandWeb_output", "timeSinceFire")
} else {
  list("LandMine", "LandWeb_output", "timeSinceFire")
}

## check pixel resolution
# stopifnot(unique(res(simOutSpeciesLayers[["speciesLayers"]])) %==% 250 / config.get(config, c("runInfo", "mapResFact")))

parameters3 <- list(
  Biomass_core = list(
    initialBiomassSource = "cohortData", # can be 'biomassMap' or "spinup" too
    seedingAlgorithm = if (grepl("noDispersal", config.get(config, c("runInfo", "runName")))) "noDispersal" else "wardDispersal",
    sppEquivCol = simOutPreamble[["sppEquivCol"]],
    successionTimestep = config.get(config, c("params", "successionTimestep")),
    .maxMemory = if (format(pemisc::availableMemory(), units = "GiB") > 130) 5 else 2, ## GB
    .plots = config.get(config, c("params", ".plots")),
    .sslVerify = config.get(config, c("params", ".sslVerify")),
    .useCache = config.get(config, c("params", "eventCaching"))[1], # seems slower to use Cache for both
    .useParallel = config.get(config, c("useParallel"))
  ),
  Biomass_regeneration = list(
    fireInitialTime = config.get(config, c("params", "fireTimestep")),
    fireTimestep = config.get(config, c("params", "fireTimestep")),
    successionTimestep = config.get(config, c("params", "successionTimestep"))
  ),
  LandMine = list(
    biggestPossibleFireSizeHa = 5e5,
    burnInitialTime = config.get(config, c("params", "fireTimestep")),
    fireTimestep = config.get(config, c("params", "fireTimestep")),
    maxReburns = config.get(config, c("params", "fireTimestep")),
    maxRetriesPerID = config.get(config, c("params", "maxFireRetries")),
    minPropBurn = 0.90,
    ROSother = if (grepl("equalROS", config.get(config, c("runInfo", "runName")))) {
      1L
    } else if (grepl("logROS", config.get(config, c("runInfo", "runName")))) {
      log(30L)
    } else {
      30L
    },
    sppEquivCol = simOutPreamble[["sppEquivCol"]],
    useSeed = NULL, ## NULL to avoid setting a seed, which makes all simulation identical!
    .useCache = config.get(config, c("params", "eventCaching")),
    .useParallel = max(2, config.get(config, "useParallel")) ## doesn't benefit from more DT threads
  ),
  LandWeb_output = list(
    sppEquivCol = simOutPreamble[["sppEquivCol"]],
    summaryInterval = config.get(config, c("params", "summaryInterval")),
    vegLeadingProportion = config.get(config, c("params", "vegLeadingProportion")),
    # .plotInitialTime = config.get(config, c("params", ".plotInitialTime")),
    .plotInterval = 1
  ),
  timeSinceFire = list(
    startTime = config.get(config, c("params", "fireTimestep")),
    .useCache = config.get(config, c("params", "eventCaching"))[1] ## faster without caching for "init"
  )
)

objects3 <- list(
  biomassMap = simOutDataPrep[["biomassMap"]],
  cohortData = simOutDataPrep[["cohortData"]],
  ecoDistrict = simOutDataPrep[["ecoDistrict"]],
  ecoregion = simOutDataPrep[["ecoregion"]],
  ecoregionMap = simOutDataPrep[["ecoregionMap"]],
  fireReturnInterval = simOutPreamble[["fireReturnInterval"]],
  minRelativeB = simOutDataPrep[["minRelativeB"]],
  pixelGroupMap = simOutDataPrep[["pixelGroupMap"]],
  rawBiomassMap = simOutDataPrep[["rawBiomassMap"]],
  rstLCC = simOutPreamble[["LCC"]],
  rasterToMatch = simOutPreamble[["rasterToMatch"]],
  rasterToMatchLarge = simOutPreamble[["rasterToMatchLarge"]],
  rasterToMatchReporting = simOutPreamble[["rasterToMatchReporting"]],
  ROSTable = simOutPreamble[["LandMineROStable"]],
  rstFlammable = simOutPreamble[["rstFlammable"]],
  rstTimeSinceFire = crop(simOutPreamble[["CC TSF"]], simOutPreamble[["rasterToMatch"]]),
  species = simOutDataPrep[["species"]],
  speciesEcoregion = simOutDataPrep[["speciesEcoregion"]],
  speciesLayers = simOutSpeciesLayers[["speciesLayers"]],
  speciesParams = simOutDataPrep[["speciesParams"]],
  speciesTable = simOutDataPrep[["speciesTable"]],
  sppColorVect = simOutPreamble[["sppColorVect"]],
  sppEquiv = simOutPreamble[["sppEquiv"]],
  standAgeMap = simOutPreamble[["CC TSF"]], ## same as rstTimeSinceFire; TODO: use synonym?
  studyArea = simOutPreamble[["studyArea"]],
  studyAreaLarge = simOutPreamble[["studyAreaLarge"]],
  studyAreaReporting = simOutPreamble[["studyAreaReporting"]],
  sufficientLight = simOutDataPrep[["sufficientLight"]],
  summaryPeriod = config.get(config, c("params", "summaryPeriod")),
  useParallel = config.get(config, "useParallel")
)

objectNamesToSave <- c("rstTimeSinceFire", "vegTypeMap")
outputs3a <- data.frame(stringsAsFactors = FALSE,
                        expand.grid(
                          objectName = objectNamesToSave,
                          saveTime = c(config.get(config, c("params", "timeSeriesTimes")),
                                       config.get(config, c("params", "analysesOutputsTimes")))
                        ),
                        fun = "writeRaster", package = "raster",
                        file = paste0(objectNamesToSave, c(".tif", ".grd")))

outputs3b <- data.frame(expand.grid(objectName = c("simulationOutput"),
                                    saveTime = times3$end),
                        fun = "saveRDS",
                        package = "base",
                        stringsAsFactors = FALSE)

outputs3a$arguments <- I(rep(list(list(overwrite = TRUE, progress = FALSE,
                                       datatype = "INT2U", format = "GTiff"),
                                  list(overwrite = TRUE, progress = FALSE,
                                       datatype = "INT1U", format = "raster")),
                             times = NROW(outputs3a) / length(objectNamesToSave)))

outputs3c <- data.frame(stringsAsFactors = FALSE,
                        objectName = "rstFlammable",
                        saveTime = times3$end,
                        fun = "writeRaster", package = "raster",
                        arguments = I(list(list(overwrite = TRUE, progress = FALSE,
                                                datatype = "INT2U", format = "raster"))))

outputs3 <- as.data.frame(data.table::rbindlist(list(outputs3a, outputs3b, outputs3c), fill = TRUE))

fseed <- file.path(Paths$outputPath, "seed.rds")
fseed2 <- extension(fseed, "txt")
if (file.exists(fseed)) {
  seed <- readRDS(fseed)
} else {
  seed <- sample(1e4, 1)
  saveRDS(seed, fseed)
}
print(paste("random seed:", seed))
cat(paste("Setting seed in 09-pre-sim.R:", seed), file = fseed2, sep = "\n")
set.seed(seed)
writeRNGInfo(fseed2, append = TRUE)
