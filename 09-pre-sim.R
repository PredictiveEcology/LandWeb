################################################################################
## main simulation
################################################################################

do.call(SpaDES.core::setPaths, paths$paths3)

times3 <- list(start = 0, end = config.get(config, c("params", "endTime")))

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
  summaryPeriod = config.get(config, c("params", "summaryPeriod"))
)

## TODO
analysesOutputsTimes <- seq(config$params$summaryPeriod[1], config$params$summaryPeriod[2],
                            by = config$params$summaryInterval)


objectNamesToSave <- c("rstTimeSinceFire", "vegTypeMap")
outputs3a <- data.frame(stringsAsFactors = FALSE,
                        expand.grid(
                          objectName = objectNamesToSave,
                          saveTime = c(config.get(config, c("params", "timeSeriesTimes")),
                                       analysesOutputsTimes)
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
