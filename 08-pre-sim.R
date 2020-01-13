################################################################################
## main simulation
################################################################################

do.call(SpaDES.core::setPaths, paths3) # Set them here so that we don't have to specify at each call to Cache

times3 <- list(start = 0, end = endTime)
modules3 <- list("Biomass_borealDataPrep",
                 "Biomass_core",
                 "LandMine",
                 "Biomass_regeneration",
                 "LandWeb_output",
                 "timeSinceFire")

## check pixel resolution
#stopifnot(unique(res(simOutSpeciesLayers[["speciesLayers"]])) %==% 250 / mapResFact)

objects3 <- list(
  "fireReturnInterval" = simOutPreamble[["fireReturnInterval"]],
  "rstLCC" = simOutPreamble[["LCC"]],
  "rasterToMatch" = simOutPreamble[["rasterToMatch"]],
  "rasterToMatchLarge" = simOutPreamble[["rasterToMatchLarge"]],
  "rasterToMatchReporting" = simOutPreamble[["rasterToMatchReporting"]],
  "ROSTable" = LandMineROStable,
  "rstFlammable" = simOutPreamble[["rstFlammable"]],
  "rstTimeSinceFire" = simOutPreamble[["CC TSF"]],
  "sppColorVect" = sppColorVect,
  "sppEquiv" = sppEquivalencies_CA,
  "speciesLayers" = simOutSpeciesLayers[["speciesLayers"]],
  "speciesParams" = speciesParams,
  "speciesTable" = speciesTable,
  "standAgeMap" = simOutPreamble[["CC TSF"]], ## same as rstTimeSinceFire; TODO: use synonym?
  "studyArea" = simOutPreamble[["studyArea"]],
  "studyAreaLarge" = simOutPreamble[["studyAreaLarge"]],
  "studyAreaReporting" = simOutPreamble[["studyAreaReporting"]],
  "summaryPeriod" = summaryPeriod, ## defined in params file
  "useParallel" = 2
)

parameters3 <- list(
  .restartR = if (isTRUE(useRestartR)) list(.restartRInterval = restartInterval) else NULL,
  Biomass_borealDataPrep = list(
    ## fastLM is ~35% faster than the default lmer but needs 820GB RAM !!
    ## also, fastLM cannot deal with rank-deficient models
    #"biomassModel" = quote(RcppArmadillo::fastLm(formula = B ~ logAge * speciesCode * ecoregionGroup +
    #                                               cover * speciesCode * ecoregionGroup)),
    "biomassModel" = quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode +
                                        (logAge + cover + speciesCode | ecoregionGroup))),
    "cloudFolderID" = cloudCacheFolderID,
    "LCCClassesToReplaceNN" = 34:36,
    # next two are used when assigning pixelGroup membership; what resolution for
    #   age and biomass
    "runName" = runName,
    "pixelGroupAgeClass" = successionTimestep * 2, # can be coarse because initial conditions are irrelevant
    "pixelGroupBiomassClass" = 1000, # can be coarse because initial conditions are irrelevant
    "sppEquivCol" = sppEquivCol,
    "subsetDataAgeModel" = 100, ## TODO: test with `NULL` and `50`
    "subsetDataBiomassModel" = 50, ## TODO: test with `NULL` and `50`
    "speciesUpdateFunction" = list(
      quote(LandR::speciesTableUpdate(sim$species, sim$speciesTable, sim$sppEquiv, P(sim)$sppEquivCol)),
      quote(LandWebUtils::updateSpeciesTable(sim$species, P(sim)$runName, sim$speciesParams))
    ),
    "useCloudCacheForStats" = useCloudCache, #TRUE,
    ".plotInitialTime" = .plotInitialTime,
    ".useCache" = eventCaching
  ),
  Biomass_core = list(
    "initialBiomassSource" = "cohortData", # can be 'biomassMap' or "spinup" too
    "seedingAlgorithm" = if (grepl("noDispersal", runName)) "noDispersal" else "wardDispersal",
    "sppEquivCol" = sppEquivCol,
    "successionTimestep" = successionTimestep,
    ".maxMemory" = if (format(pemisc::availableMemory(), units = "GiB") > 130) 5 else 2, ## GB
    ".plotInitialTime" = .plotInitialTime,
    ".useCache" = eventCaching[1], # seems slower to use Cache for both
    ".useParallel" = useParallel
  ),
  Biomass_regeneration = list(
    "fireInitialTime" = fireTimestep,
    "fireTimestep" = fireTimestep,
    "successionTimestep" = successionTimestep
  ),
  LandMine = list(
    "biggestPossibleFireSizeHa" = 5e5,
    "burnInitialTime" = fireTimestep,
    "fireTimestep" = fireTimestep,
    "maxRetriesPerID" = 4,
    "minPropBurn" = 0.90,
    "sppEquivCol" = sppEquivCol,
    "useSeed" = NULL, ## NULL to avoid setting a seed, which makes all simulation identical!
    ".useCache" = eventCaching,
    ".useParallel" = max(2, useParallel)
  ),
  LandWeb_output = list(
    "sppEquivCol" = sppEquivCol,
    "summaryInterval" = summaryInterval,
    "vegLeadingProportion" = vegLeadingProportion,
    #".plotInitialTime" = .plotInitialTime,
    ".plotInterval" = 1
  ),
  timeSinceFire = list(
    "startTime" = fireTimestep,
    ".useCache" = eventCaching[1] # way faster without caching for "init"
  )
)

if (grepl("scfm", runName)) {
  source(file.path(activeDir, "params", "scfm_params.R"))
  modules3 <- append(modules3[-which(modules3 == "LandMine")], scfmModules)
  objects3 <- append(objects3, scfmObjects)
  parameters3 <- append(parameters3, scfmParams)
}

objectNamesToSave <- c("rstTimeSinceFire", "vegTypeMap")
analysesOutputsTimes <- seq(objects3$summaryPeriod[1], objects3$summaryPeriod[2],
                            by = parameters3$LandWeb_output$summaryInterval)

outputs3a <- data.frame(stringsAsFactors = FALSE,
                        expand.grid(
                          objectName = objectNamesToSave,
                          saveTime = c(timeSeriesTimes, analysesOutputsTimes)
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
cat(paste("Setting seed in newStart.R:", seed), file = fseed2, sep = "\n")
set.seed(seed)
writeRNGInfo(fseed2, append = TRUE)
