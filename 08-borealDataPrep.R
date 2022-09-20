################################################################################
## species layers
################################################################################

do.call(SpaDES.core::setPaths, paths$paths2a)

parameters2a <- list(
  Biomass_borealDataPrep = list(
    ## fastLM is ~35% faster than the default lmer but needs 820GB RAM !!
    ## also, fastLM cannot deal with rank-deficient models
    #"biomassModel" = quote(RcppArmadillo::fastLm(formula = B ~ logAge * speciesCode * ecoregionGroup +
    #                                               cover * speciesCode * ecoregionGroup)),
    biomassModel = if (grepl("FMU", config.get(config, c("runInfo", "runName")))) {
      quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode + (1 | ecoregionGroup)))
    } else {
      quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode +
                         (logAge + cover | ecoregionGroup)))
    },
    ecoregionLayerField = "ECOREGION", # "ECODISTRIC"
    forestedLCCClasses = config.get(config, c("params", "forestedLCCClasses")),
    LCCClassesToReplaceNN = 34:36,
    # next two are used when assigning pixelGroup membership; what resolution for
    #   age and biomass
    runName = config.get(config, c("runInfo", "runName")),
    pixelGroupAgeClass = config.get(config, c("params", "successionTimestep")) * 2,  ## can be coarse because initial conditions are irrelevant
    pixelGroupBiomassClass = 1000 / config.get(config, c("runInfo", "mapResFact"))^2, ## can be coarse because initial conditions are irrelevant
    sppEquivCol = simOutPreamble[["sppEquivCol"]],
    subsetDataAgeModel = 100,
    subsetDataBiomassModel = 100,
    speciesUpdateFunction = list(
      quote(LandR::speciesTableUpdate(sim$species, sim$speciesTable, sim$sppEquiv, P(sim)$sppEquivCol)),
      quote(LandR::updateSpeciesTable(sim$species, sim$speciesParams))
    ),
    useCloudForStats = config.get(config, c("cloud", "useCloud")),
    .plots = config.get(config, c("params", ".plots")),
    .sslVerify = config.get(config, c("params", ".sslVerify")),
    .studyAreaName = config.get(config, c("runInfo", "studyAreaName")),
    .useCache = config.get(config, c("params", "eventCaching"))
  )
)

objects2a <- list(
  cloudFolderID = config.get(config, c("cloud", "cacheDir")),
  rstLCC = simOutPreamble[["LCC"]],
  rasterToMatch = simOutPreamble[["rasterToMatch"]],
  rasterToMatchLarge = simOutPreamble[["rasterToMatchLarge"]],
  speciesLayers = simOutSpeciesLayers[["speciesLayers"]],
  speciesParams = simOutPreamble[["speciesParams"]],
  speciesTable = simOutPreamble[["speciesTable"]],
  speciesTableAreas = c("BSW", "BP", "MC"), ## TODO: should we remove BP? MC?
  sppColorVect = simOutPreamble[["sppColorVect"]],
  sppEquiv = simOutPreamble[["sppEquiv"]],
  standAgeMap = simOutPreamble[["CC TSF"]],
  studyArea = simOutPreamble[["studyArea"]],
  studyAreaLarge = simOutPreamble[["studyAreaLarge"]]
)

dataPrepFile <- file.path(Paths$inputPath, paste0("simOutDataPrep_", substr(config.get(config, c("runInfo", "runName")), 1, 8), ".qs"))
debug(LandR::statsModel) ## TODO: filter out terms in singles that are not in formula
simOutDataPrep <- Cache(simInitAndSpades,
                        times = list(start = 0, end = 1),
                        params = parameters2a,
                        modules = c("Biomass_borealDataPrep"),
                        objects = objects2a,
                        omitArgs = c("debug", "paths", ".plotInitialTime"),
                        useCache = if (isTRUE(config.get(config, "rerunDataPrep"))) "overwrite" else TRUE,
                        useCloud = config.get(config, c("cloud", "useCloud")),
                        cloudFolderID = config.get(config, c("cloud", "cacheDir")),
                        ## make .plotInitialTime an argument, not a parameter:
                        ##  - Cache will see them as unchanged regardless of value
                        .plots = config.get(config, c("params", ".plots")),
                        paths = paths$paths2a,
                        debug = 1)
simOutDataPrep@.xData[["._sessionInfo"]] <- projectSessionInfo(prjDir)
saveSimList(simOutDataPrep, dataPrepFile, fileBackend = 2)
