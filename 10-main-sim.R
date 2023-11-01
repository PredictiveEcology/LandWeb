################################################################################
## main simulation
################################################################################

times3 <- list(start = 0, end = config$args[["endTime"]])

modules3 <- if (isTRUE(config$context[["succession"]])) {
  list("Biomass_core", "LandMine", "Biomass_regeneration", "LandWeb_output", "timeSinceFire")
} else {
  list("LandMine", "LandWeb_output", "timeSinceFire")
}

config$params[["LandMine"]] <- list(
  biggestPossibleFireSizeHa = 3e5, ## for MB
  maxReburns = c(1L, 20L),
  maxRetriesPerID = 9L,
  .useCache = FALSE
) ## TODO: add these to config -- MB struggling to reach fire sizes

parameters3 <- list(
  .globals = config$params[[".globals"]],
  Biomass_core = config$params[["Biomass_core"]],
  Biomass_regeneration = config$params[["Biomass_regeneration"]],
  LandMine = config$params[["LandMine"]],
  LandWeb_output = config$params[["LandWeb_output"]],
  timeSinceFire = config$params[["timeSinceFire"]]
)

## check pixel resolution
# stopifnot(unique(res(simOutSpeciesLayers[["speciesLayers"]])) %==% config$params[["LandWeb_preamble"]][["pixelSize"]])

objects3 <- list(
  biomassMap = simOutDataPrep[["biomassMap"]],
  cohortData = simOutDataPrep[["cohortData"]],
  #ecoDistrict = simOutDataPrep[["ecoDistrict"]], ## TODO: unused??
  ecoregion = simOutDataPrep[["ecoregion"]],
  ecoregionMap = simOutDataPrep[["ecoregionMap"]],
  fireReturnInterval = simOutPreamble[["fireReturnInterval"]],
  minRelativeB = simOutDataPrep[["minRelativeB"]],
  pixelGroupMap = simOutDataPrep[["pixelGroupMap"]],
  rawBiomassMap = simOutDataPrep[["rawBiomassMap"]],
  rasterToMatch = simOutDataPrep[["rasterToMatch"]],
  rasterToMatchLarge = simOutDataPrep[["rasterToMatchLarge"]],
  ROSTable = simOutPreamble[["ROSTable"]],
  rstFlammable = simOutPreamble[["rstFlammable"]],
  rstLCC = simOutDataPrep[["rstLCC"]],
  rstTimeSinceFire = raster::crop(simOutPreamble[["CC TSF"]], simOutPreamble[["rasterToMatch"]]), ## TODO: fix
  species = simOutDataPrep[["species"]],
  speciesEcoregion = simOutDataPrep[["speciesEcoregion"]],
  speciesLayers = simOutDataPrep[["speciesLayers"]],
  speciesParams = simOutDataPrep[["speciesParams"]],
  speciesTable = simOutDataPrep[["speciesTable"]],
  sppColorVect = simOutDataPrep[["sppColorVect"]],
  sppEquiv = simOutDataPrep[["sppEquiv"]],
  standAgeMap = simOutPreamble[["CC TSF"]], ## TODO: fix
  #standAgeMap = simOutDataPrep[["standAgeMap"]],
  studyArea = simOutDataPrep[["studyArea"]],
  studyAreaLarge = simOutDataPrep[["studyAreaLarge"]],
  studyAreaReporting = simOutPreamble[["studyAreaReporting"]], ## TODO: use sAR from simOutDataPrep
  sufficientLight = simOutDataPrep[["sufficientLight"]],
  summaryPeriod = config$params[[".globals"]][["summaryPeriod"]]
)

analysesOutputsTimes <- LandWebUtils::analysesOutputsTimes(
  config$params[[".globals"]][["summaryPeriod"]], config$params[[".globals"]][["summaryInterval"]]
)

objectNamesToSave <- c("cohortData", "pixelGroupMap", "standAgeMap", "rstTimeSinceFire", "vegTypeMap")

outputs3a <- data.frame(
  expand.grid(
    objectName = objectNamesToSave,
    saveTime = c(config$args[["timeSeriesTimes"]], analysesOutputsTimes)
  ),
  fun = c("qsave", "writeRaster", "writeRaster", "writeRaster", "writeRaster"),
  package = c("qs", "raster", "raster", "raster", "raster"),
  file = paste0(objectNamesToSave, c(".qs", ".tif", ".tif", ".tif", ".grd")),
  stringsAsFactors = FALSE
)
outputs3a$arguments <- I(rep(list(
  list(nthreads = 1),
  list(overwrite = TRUE, progress = FALSE, datatype = "INT2U", format = "GTiff"),
  list(overwrite = TRUE, progress = FALSE, datatype = "INT2U", format = "GTiff"),
  list(overwrite = TRUE, progress = FALSE, datatype = "INT2U", format = "GTiff"),
  list(overwrite = TRUE, progress = FALSE, datatype = "INT1U", format = "raster")
), times = NROW(outputs3a) / length(objectNamesToSave)))

outputs3b <- data.frame(
  expand.grid(objectName = c("simulationOutput"), saveTime = times3$end),
  fun = c("saveRDS"),
  package = c("base"),
  stringsAsFactors = FALSE
)

outputs3c <- data.frame(
  expand.grid(objectName = c("rstCurrentBurnCumulative", "rstFlammable"), saveTime = times3$end),
  fun = c("writeRaster", "writeRaster"),
  package = c("raster", "raster"),
  arguments = I(list(list(overwrite = TRUE, progress = FALSE,
                          datatype = "INT2U", format = "GTiff"),
                     list(overwrite = TRUE, progress = FALSE,
                          datatype = "INT1U", format = "GTiff"))),
  stringsAsFactors = FALSE
)

outputs3 <- as.data.frame(data.table::rbindlist(list(outputs3a, outputs3b, outputs3c), fill = TRUE))

fseed <- file.path(Paths$outputPath, "seed.rds")
fseed2 <- raster::extension(fseed, "txt")
if (file.exists(fseed)) {
  seed <- readRDS(fseed)
} else {
  seed <- sample(1e4, 1)
  saveRDS(seed, fseed)
}
print(paste("random seed:", seed))
cat(paste("Setting seed in 10-main-sim.R:", seed), file = fseed2, sep = "\n")
set.seed(seed)
writeRNGInfo(fseed2, append = TRUE)

if ("screen" %in% config$params[[".globals"]][[".plots"]]) {
  quickPlot::dev(4, width = 18, height = 10)
  grid::grid.rect(0.90, 0.03, width = 0.2, height = 0.06, gp = gpar(fill = "white", col = "white"))
  grid::grid.text(label = config$context[["studyAreaName"]], x = 0.90, y = 0.03)
}

data.table::setDTthreads(config$params[[".globals"]][[".useParallel"]])

tryCatch({
  mySimOut <- simInitAndSpades(
    times = times3,
    params = parameters3, ## TODO: use config$params
    modules = modules3, ## TODO: use config$modules
    outputs = outputs3,
    objects = objects3,
    paths = paths,
    loadOrder = unlist(modules3), ## TODO: use config$modules
    debug = list(file = list(file = file.path(config$paths[["logPath"]], "sim.log"),
                             append = TRUE), debug = 1)
  )
  capture.output(warnings(), file = file.path(config$paths[["logPath"]], "warnings.txt"), split = TRUE)
}, error = function(e) {
  capture.output(traceback(), file = file.path(config$paths[["logPath"]], "traceback_mainSim.txt"), split = TRUE)

  if (requireNamespace("notifications") & file.exists("~/.rgooglespaces")) {
    notifications::notify_google(
      paste0("ERROR in simulation `", config$context[["runName"]],
             "` on host `", config$context[["machine"]], "`.\n",
             "```\n", e$message, "\n```")
    )

    stop(e$message)
  }
})

if (isUpdated(mySimOut) || isFALSE(config$args[["useCache"]])) {
  mySimOut@.xData[["._sessionInfo"]] <- SpaDES.project::projectSessionInfo(prjDir)

  fsim <- simFile("mySimOut", paths[["outputPath"]], config$args[["endTime"]], "qs")
  message("Saving simulation to: ", fsim)
  saveSimList(sim = mySimOut, filename = fsim, fileBackend = 2)

  # save simulation stats -----------------------------------------------------------------------
  elapsed <- elapsedTime(mySimOut)
  data.table::fwrite(elapsed, file.path(paths[["outputPath"]], "elapsedTime.csv"))
  qs::qsave(elapsed, file.path(paths[["outputPath"]], "elapsedTime.qs"))

  if (!isFALSE(getOption("spades.memoryUseInterval"))) {
    memory <- memoryUse(mySimOut, max = TRUE)
    data.table::fwrite(memory, file.path(paths[["outputPath"]], "memoryUsed.csv"))
    qs::qsave(memory, file.path(paths[["outputPath"]], "memoryUsed.qs"))
  }
}

# end-of-sim notifications --------------------------------------------------------------------

if (requireNamespace("notifications") & file.exists("~/.rgooglespaces")) {
  notifications::notify_google(
    paste0("Simulation `", config$context[["runName"]],
           "` completed on host `", config$context[["machine"]], "`",
           if (nzchar(Sys.getenv("STY"))) paste0(" (screen `", Sys.getenv("STY"), "`)"), ".")
  )
}
