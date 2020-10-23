################################################################################
## species layers
################################################################################

do.call(SpaDES.core::setPaths, paths2a)

objects2a <- list(
  "cloudFolderID" = cloudCacheFolderID,
  "rstLCC" = simOutPreamble[["LCC"]],
  "rasterToMatch" = simOutPreamble[["rasterToMatch"]],
  "rasterToMatchLarge" = simOutPreamble[["rasterToMatchLarge"]],
  "speciesLayers" = simOutSpeciesLayers[["speciesLayers"]],
  "speciesParams" = speciesParams,
  "speciesTable" = speciesTable,
  "sppColorVect" = sppColorVect,
  "sppEquiv" = sppEquivalencies_CA,
  "standAgeMap" = simOutPreamble[["CC TSF"]],
  "studyArea" = simOutPreamble[["studyArea"]],
  "studyAreaLarge" = simOutPreamble[["studyAreaLarge"]]
)

parameters2a <- list(
  Biomass_borealDataPrep = list(
    ## fastLM is ~35% faster than the default lmer but needs 820GB RAM !!
    ## also, fastLM cannot deal with rank-deficient models
    #"biomassModel" = quote(RcppArmadillo::fastLm(formula = B ~ logAge * speciesCode * ecoregionGroup +
    #                                               cover * speciesCode * ecoregionGroup)),
    "biomassModel" = if (grepl("FMU", runName)) {
      quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode + (1 | ecoregionGroup)))
    } else {
      quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode +
                         (logAge + cover + speciesCode | ecoregionGroup)))
    },
    "ecoregionLayerField" = "ECOREGION", # "ECODISTRIC"
    "forestedLCCClasses" = forestedLCCClasses,
    "LCCClassesToReplaceNN" = 34:36,
    # next two are used when assigning pixelGroup membership; what resolution for
    #   age and biomass
    "runName" = runName,
    "pixelGroupAgeClass" = successionTimestep * 2,  ## can be coarse because initial conditions are irrelevant
    "pixelGroupBiomassClass" = 1000 / mapResFact^2, ## can be coarse because initial conditions are irrelevant
    "sppEquivCol" = sppEquivCol,
    "subsetDataAgeModel" = 100,
    "subsetDataBiomassModel" = 100,
    "speciesUpdateFunction" = list(
      quote(LandR::speciesTableUpdate(sim$species, sim$speciesTable, sim$sppEquiv, P(sim)$sppEquivCol)),
      quote(LandR::updateSpeciesTable(sim$species, sim$speciesParams))
    ),
    "useCloudCacheForStats" = useCloudCache, #TRUE,
    ".plotInitialTime" = .plotInitialTime,
    ".studyAreaName" = studyAreaName,
    ".useCache" = eventCaching
  )
)

dataPrepFile <- file.path(Paths$inputPath, paste0("simOutDataPrep_", substr(runName, 1, 8), ".qs"))
## (re)run boreal data prep
simOutDataPrep <- Cache(simInitAndSpades,
                        times = list(start = 0, end = 1),
                        params = parameters2a,
                        modules = c("Biomass_borealDataPrep"),
                        objects = objects2a,
                        omitArgs = c("debug", "paths", ".plotInitialTime"),
                        useCache = if (isTRUE(rerunDataPrep)) "overwrite" else TRUE,
                        useCloud = useCloudCache,
                        cloudFolderID = cloudCacheFolderID,
                        ## make .plotInitialTime an argument, not a parameter:
                        ##  - Cache will see them as unchanged regardless of value
                        .plotInitialTime = .plotInitialTime,
                        paths = paths2a,
                        debug = 1)
saveSimList(simOutDataPrep, dataPrepFile) ## TODO: fix issue loading simList
