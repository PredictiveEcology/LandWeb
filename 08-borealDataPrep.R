################################################################################
## species layers
################################################################################

do.call(SpaDES.core::setPaths, paths2a)

objects2a <- list(
  "cloudFolderID" = cloudCacheFolderID,
  "rstLCC" = simOutPreamble[["LCC"]],
  "rasterToMatch" = simOutPreamble[["rasterToMatch"]],
  "rasterToMatchLarge" = simOutPreamble[["rasterToMatchLarge"]],
  "rstFlammable" = simOutPreamble[["rstFlammable"]],
  "rstTimeSinceFire" = simOutPreamble[["CC TSF"]],
  "speciesLayers" = simOutSpeciesLayers[["speciesLayers"]],
  "speciesParams" = speciesParams,
  "speciesTable" = speciesTable,
  "sppColorVect" = sppColorVect,
  "sppEquiv" = sppEquivalencies_CA,
  "standAgeMap" = simOutPreamble[["CC TSF"]], ## same as rstTimeSinceFire; TODO: use synonym?
  "studyArea" = simOutPreamble[["studyArea"]],
  "studyAreaLarge" = simOutPreamble[["studyAreaLarge"]]
)

parameters2a <- list(
  Biomass_borealDataPrep = list(
    ## fastLM is ~35% faster than the default lmer but needs 820GB RAM !!
    ## also, fastLM cannot deal with rank-deficient models
    #"biomassModel" = quote(RcppArmadillo::fastLm(formula = B ~ logAge * speciesCode * ecoregionGroup +
    #                                               cover * speciesCode * ecoregionGroup)),
    "biomassModel" = quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode +
                                        (logAge + cover + speciesCode | ecoregionGroup))),
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
  )
)

dataPrepFile <- file.path(Paths$inputPath, paste0("simOutDataPrep_", substr(runName, 1, 8), ".rds"))
if (isTRUE(rerunDataPrep) || !file.exists("dataPrepFile")) {
  ## (re)run boreal data prep
  simOutDataPrep <- Cache(simInitAndSpades,
                          times = list(start = 0, end = 1),
                          params = parameters2a,
                          modules = c("Biomass_borealDataPrep"),
                          objects = objects2a,
                          omitArgs = c("debug", "paths", ".plotInitialTime"),
                          #useCache = "overwrite", ## TODO: remove this workaround
                          useCloud = useCloudCache,
                          cloudFolderID = cloudCacheFolderID,
                          ## make .plotInitialTime an argument, not a parameter:
                          ##  - Cache will see them as unchanged regardless of value
                          .plotInitialTime = .plotInitialTime,
                          paths = paths2a,
                          debug = 1)

  # saveRDS(Copy(simOutDataPrep), dataPrepFile, version = 3)
} else {
  if (runName == "random___res250_test") {
    dl <- downloadFile(url = "https://drive.google.com/file/d/1nL7KM33BSWh2n5P7SEEiAnpOzhb_xu81/view?usp=sharing",
                       targetFile = basename(dataPrepFile),
                       destinationPath = dirname(dataPrepFile),
                       neededFiles = basename(dataPrepFile),
                       archive = NULL,
                       checkSums = Checksums(dirname(dataPrepFile), write = TRUE), needChecksums = 0)
  }
  stopifnot(file.exists(dataPrepFile))
  simOutDataPrep <- readRDS(dataPrepFile)
  rm(dl)
}
