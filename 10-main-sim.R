################################################################################
## main simulation
################################################################################

times3 <- list(start = 0, end = config$args$endTime)

modules3 <- if (isTRUE(context$succession)) {
  list("Biomass_core", "LandMine", "Biomass_regeneration", "LandWeb_output", "timeSinceFire")
} else {
  list("LandMine", "LandWeb_output", "timeSinceFire")
}

parameters3 <- list(
  .globals = config$params$.globals,
  Biomass_core = config$params$Biomass_core,
  Biomass_regeneration = config$params$Biomass_regeneration,
  LandMine = config$params$LandMine,
  LandWeb_output = config$params$LandWeb_output,
  timeSinceFire = config$params$timeSinceFire
)

## check pixel resolution
# stopifnot(unique(res(simOutSpeciesLayers[["speciesLayers"]])) %==% 250 / config.get(config, c("runInfo", "mapResFact")))

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
  summaryPeriod = config$params[[".globals"]][["summaryPeriod"]]
)

## TODO: find better way of doing this (i.e., define once so not repetaed here and in summary module)
analysesOutputsTimes <- seq(config$params$LandWeb_summary$summaryPeriod[1],
                            config$params$LandWeb_summary$summaryPeriod[2],
                            by = config$params$LandWeb_summary$summaryInterval)


objectNamesToSave <- c("rstTimeSinceFire", "vegTypeMap")
outputs3a <- data.frame(stringsAsFactors = FALSE,
                        expand.grid(
                          objectName = objectNamesToSave,
                          saveTime = c(config$args$timeSeriesTimes, analysesOutputsTimes)
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
cat(paste("Setting seed in 10-main-sim.R:", seed), file = fseed2, sep = "\n")
set.seed(seed)
writeRNGInfo(fseed2, append = TRUE)

if ("screen" %in% config$params$.globals$.plots) {
  quickPlot::dev(4, width = 18, height = 10)
  grid::grid.rect(0.90, 0.03, width = 0.2, height = 0.06, gp = gpar(fill = "white", col = "white"))
  grid::grid.text(label = context$studyAreaName, x = 0.90, y = 0.03)
}

data.table::setDTthreads(config$params[[".globals"]][[".useParallel"]])

tryCatch({
  mySimOut <- Cache(simInitAndSpades,
                    times = times3, #cl = cl,
                    params = parameters3, ## TODO: use config$params
                    modules = modules3, ## TODO: use config$modules
                    outputs = outputs3,
                    objects = objects3,
                    paths = paths,
                    loadOrder = unlist(modules3), ## TODO: use config$modules
                    debug = list(file = list(file = file.path(paths$outputPath, "sim.log"),
                                             append = TRUE), debug = 1),
                    useCloud = FALSE, ## TODO param useCloud??
                    cloudFolderID = config$args$cloud$cacheDir,
                    omitArgs = c("debug", "paths"))
  mySimOut@.xData[["._sessionInfo"]] <- projectSessionInfo(prjDir)
}, error = function(e) {
  if (requireNamespace("slackr") & file.exists("~/.slackr")) {
    slackr::slackr_setup()
    slackr::slackr_msg(
      paste0("ERROR in simulation `", context$runName, "` on host `", .nodename, "`.\n",
             "```\n", e$message, "\n```"),
      channel = config$args$notifications$slackChannel, preformatted = FALSE
    )
    stop(e$message)
  }
})

cat(capture.output(warnings()), file = file.path(paths$outputPath, "warnings.txt"), sep = "\n")

fsim <- simFile("mySimOut", paths$outputPath, SpaDES.core::end(mySimOut), "qs")
message("Saving simulation to: ", fsim)
saveSimList(sim = mySimOut, filename = fsim, fileBackend = 2)

# save simulation stats -----------------------------------------------------------------------

elapsed <- elapsedTime(mySimOut)
data.table::fwrite(elapsed, file.path(paths$outputPath, "elapsedTime.csv"))
qs::qsave(elapsed, file.path(paths$outputPath, "elapsedTime.qs"))

memory <- memoryUse(mySimOut, max = TRUE)
data.table::fwrite(memory, file.path(paths$outputPath, "memoryUsed.csv"))
qs::qsave(memory, file.path(paths$outputPath, "memoryUsed.qs"))

# end-of-sim notifications --------------------------------------------------------------------

SpaDES.project::notify_slack(context$runName, config$args$notifications$slackChannel)
