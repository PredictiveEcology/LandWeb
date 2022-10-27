################################################################################
## main simulation
################################################################################

times3 <- list(start = 0, end = config$args[["endTime"]])

modules3 <- if (isTRUE(config$context[["succession"]])) {
  list("Biomass_core", "LandMine", "Biomass_regeneration", "LandWeb_output", "timeSinceFire")
} else {
  list("LandMine", "LandWeb_output", "timeSinceFire")
}

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
  rstLCC = simOutDataPrep[["rstLCC"]],
  rasterToMatch = simOutDataPrep[["rasterToMatch"]],
  rasterToMatchLarge = simOutDataPrep[["rasterToMatchLarge"]],
  ROSTable = simOutPreamble[["LandMineROStable"]],
  rstFlammable = simOutPreamble[["rstFlammable"]],
  rstTimeSinceFire = raster::crop(simOutPreamble[["CC TSF"]], simOutPreamble[["rasterToMatch"]]), ## TODO: fix
  #rstTimeSinceFire = simOutDataPrep[["standAgeMap"]],
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
  sufficientLight = simOutDataPrep[["sufficientLight"]],
  summaryPeriod = config$params[[".globals"]][["summaryPeriod"]]
)

analysesOutputsTimes <- LandWebUtils::analysesOutputsTimes(
  config$params[[".globals"]][["summaryPeriod"]], config$params[[".globals"]][["summaryInterval"]]
)

objectNamesToSave <- c("rstTimeSinceFire", "vegTypeMap")

outputs3a <- data.frame(stringsAsFactors = FALSE,
                        expand.grid(
                          objectName = objectNamesToSave,
                          saveTime = c(config$args[["timeSeriesTimes"]], analysesOutputsTimes)
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
  mySimOut <- Cache(simInitAndSpades,
                    times = times3,
                    params = parameters3, ## TODO: use config$params
                    modules = modules3, ## TODO: use config$modules
                    outputs = outputs3,
                    objects = objects3,
                    paths = paths,
                    loadOrder = unlist(modules3), ## TODO: use config$modules
                    debug = list(file = list(file = file.path(checkPath(config$paths[["logPath"]], create = TRUE), "sim.log"),
                                             append = TRUE), debug = 1),
                    useCloud = FALSE, ## TODO param useCloud??
                    cloudFolderID = config$args[["cloud"]][["cacheDir"]],
                    omitArgs = c("debug", "paths"),
                    userTags = c(config$context$runName, "mainSim"))
  mySimOut@.xData[["._sessionInfo"]] <- projectSessionInfo(prjDir)
}, error = function(e) {
  if (requireNamespace("slackr") & file.exists("~/.slackr")) {
    slackr::slackr_setup()
    slackr::slackr_msg(
      paste0("ERROR in simulation `", config$context[["runName"]], "` on host `", .nodename, "`.\n",
             "```\n", e$message, "\n```"),
      channel = config$args[["notifications"]][["slackChannel"]], preformatted = FALSE
    )

    capture.output(traceback(), file = file.path(paths[["outputPath"]], "traceback_mainSim.txt"), split = TRUE)

    stop(e$message)
  }
})

capture.output(warnings(), file = file.path(paths[["outputPath"]], "warnings.txt"), split = TRUE)

fsim <- simFile("mySimOut", paths[["outputPath"]], SpaDES.core::end(mySimOut), "qs")
message("Saving simulation to: ", fsim)
saveSimList(sim = mySimOut, filename = fsim, fileBackend = 2)

# save simulation stats -----------------------------------------------------------------------

elapsed <- elapsedTime(mySimOut)
data.table::fwrite(elapsed, file.path(paths[["outputPath"]], "elapsedTime.csv"))
qs::qsave(elapsed, file.path(paths[["outputPath"]], "elapsedTime.qs"))

memory <- memoryUse(mySimOut, max = TRUE)
data.table::fwrite(memory, file.path(paths[["outputPath"]], "memoryUsed.csv"))
qs::qsave(memory, file.path(paths[["outputPath"]], "memoryUsed.qs"))

# end-of-sim notifications --------------------------------------------------------------------

SpaDES.project::notify_slack(config$context[["runName"]], config$args[["notifications"]][["slackChannel"]])
