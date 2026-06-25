# _targets.R -- LandWeb pipeline (Phase-0 spike: 1 study area, n_reps = 1).
#
# Proves the component-extraction-and-pass pattern (Part J of the refactor plan):
# each stage runs simInitAndSpades in-process and emits its components as
# targets -- plain objects as ordinary targets, spatial (terra) objects as
# `format = "file"` targets -- so no simList crosses a target boundary.
#
# Module params are ported from 00-main.R / 03-main-sim.R + box/landweb.R for a
# single small FMA. Phase-0 mirrors the CURRENT module set (5-module mainSim);
# the NRV_summary consolidation that retires timeSinceFire/LandWeb_output is a
# later phase (Part G #7).

source("_local.R") # per-user/host knobs, BEFORE tar_source()

library(targets)
library(SpaDES.targets)

tar_option_set(
  packages = "SpaDES.targets", # for unqualified read_spatial() in target commands
  format = "rds"
  # controller = ... # crew / crew.ssh added in Phase 6
)

res <- local$res

## ---- shared (.globals) + per-module parameters --------------------------------
globals <- list(
  dataYear = 2020L,
  fireTimestep = 1L,
  initialB = 10,
  sppEquivCol = "LandWeb",
  successionTimestep = 10L,
  summaryInterval = 50L,
  summaryPeriod = c(700, 1000),
  vegLeadingProportion = 0.8,
  .plotInitialTime = 0,
  .plots = "png",
  .sslVerify = 0L,
  .studyAreaName = local$study_areas,
  .useParallel = local$dt_threads
)

p_preamble <- list(
  .globals = globals,
  LandWeb_preamble = list(
    bufferDist = 20000, bufferDistLarge = 50000, dispersalType = "default",
    friMultiple = 1L, pixelSize = res, mergeSlivers = FALSE, minFRI = 25L,
    ROStype = "default", treeClassesLCC = c(81, 210, 220, 230, 240),
    .plotInitialTime = 0, .useCache = FALSE
  )
)

p_speciesData <- list(
  .globals = globals,
  Biomass_speciesData = list(types = "SCANFI", .plots = "png", .useCache = FALSE)
)

p_dataPrep <- list(
  .globals = globals,
  Biomass_borealDataPrep = list(
    adjustAgeAndLongevity = TRUE,
    biomassModel = quote(lme4::lmer(
      B ~ logAge * speciesCode + cover * speciesCode + (logAge + cover | ecoregionGroup)
    )),
    dataSource = "SCANFI", earliestFireYear = 1950L, ecoregionLayerField = "ECOREGION",
    exportModels = "none", fixModelBiomass = TRUE,
    forestedLCCClasses = c(81, 210, 220, 230, 240), LCCClassesToReplaceNN = 240,
    pixelGroupAgeClass = 20L, pixelGroupBiomassClass = 1000 / (250 / res)^2,
    speciesTableAreas = c("BSW", "BP", "MC"),
    subsetDataAgeModel = 100L, subsetDataBiomassModel = 100L,
    useCloudCacheForStats = FALSE, .plotInitialTime = 0, .useCache = FALSE
    ## TODO: speciesUpdateFunction (2 quotes) + minRelativeBFunction =
    ## quote(myMinRelativeB(pixelCohortData)) -- port verbatim from box/landweb.R
    ## + 00-main.R:299-301 when wiring the real run.
  ),
  Biomass_speciesFactorial = list(factorialSize = "large"),
  Biomass_speciesParameters = list(
    PSPdataTypes = "NFI", quantileAgeSubset = 98L, speciesFittingApproach = "focal"
  )
)

p_mainSim <- list(
  .globals = globals,
  Biomass_core = list(
    growthInitialTime = 0, initialBiomassSource = "cohortData", mixedType = 2L,
    seedingAlgorithm = "wardDispersal", .plotInitialTime = 0, .plotInterval = 100L,
    .useCache = FALSE
  ),
  Biomass_regeneration = list(
    calibrate = FALSE, fireInitialTime = 1, .plotInitialTime = 0, .useCache = FALSE
  ),
  LandMine = list(
    biggestPossibleFireSizeHa = 3e5, burnInitialTime = 1L, maxReburns = c(1L, 20L),
    maxRetriesPerID = 9L, minPropBurn = 0.90, mode = "single", ROSother = 30L,
    ROStype = "default", useSeed = NULL, .plotInitialTime = 1, .plotInterval = 100,
    .studyAreaName = local$study_areas, .unitTest = FALSE, .useCache = FALSE
  ),
  LandWeb_output = list(
    summaryInterval = 50L, summaryPeriod = c(700, 1000), .plotInitialTime = 0,
    .useCache = FALSE
  ),
  timeSinceFire = list(startTime = 1, .useCache = FALSE)
)

## ---- pipeline -----------------------------------------------------------------
list(
  ## branch sources
  tar_target(study_areas, local$study_areas), # Phase-0: one area
  tar_target(rep_index, seq_len(local$n_reps), iteration = "vector"),

  ## Stage 1: preamble
  tar_simspades(
    "preamble",
    modules = "LandWeb_preamble",
    params = p_preamble,
    paths = local$paths,
    plain = c("sppEquiv", "sppColorVect", "speciesParams", "speciesTable", "ROSTable"),
    spatial = c(
      "rasterToMatch", "rasterToMatch_biomassParam",
      "studyArea", "studyArea_biomassParam", "studyAreaReporting", "studyAreaANPP",
      "rstLCC", "standAgeMap", "rstFlammable", "fireReturnInterval"
    )
  ),

  ## Stage 2: speciesData
  tar_simspades(
    "speciesData",
    modules = "Biomass_speciesData",
    params = p_speciesData,
    inputs = quote(list(
      rasterToMatch = read_spatial(preamble_rasterToMatch),
      rasterToMatch_biomassParam = read_spatial(preamble_rasterToMatch_biomassParam),
      studyArea = read_spatial(preamble_studyArea),
      studyArea_biomassParam = read_spatial(preamble_studyArea_biomassParam),
      studyAreaReporting = read_spatial(preamble_studyAreaReporting),
      sppEquiv = preamble$sppEquiv,
      sppColorVect = preamble$sppColorVect
    )),
    paths = local$paths,
    spatial = "speciesLayers"
  ),

  ## Stage 3: dataPrep
  tar_simspades(
    "dataPrep",
    modules = c("Biomass_speciesFactorial", "Biomass_borealDataPrep", "Biomass_speciesParameters"),
    params = p_dataPrep,
    inputs = quote(list(
      rstLCC = read_spatial(preamble_rstLCC),
      rasterToMatch = read_spatial(preamble_rasterToMatch),
      rasterToMatch_biomassParam = read_spatial(preamble_rasterToMatch_biomassParam),
      speciesLayers = read_spatial(speciesData_speciesLayers),
      standAgeMap = read_spatial(preamble_standAgeMap),
      studyArea = read_spatial(preamble_studyArea),
      studyAreaANPP = read_spatial(preamble_studyAreaANPP),
      studyArea_biomassParam = read_spatial(preamble_studyArea_biomassParam),
      studyAreaReporting = read_spatial(preamble_studyAreaReporting),
      speciesParams = preamble$speciesParams,
      speciesTable = preamble$speciesTable,
      sppColorVect = preamble$sppColorVect,
      sppEquiv = preamble$sppEquiv
    )),
    paths = local$paths,
    plain = c(
      "cohortData", "species", "speciesEcoregion", "ecoregion", "minRelativeB",
      "sufficientLight", "sppEquiv", "sppColorVect", "speciesParams", "speciesTable"
    ),
    spatial = c(
      "biomassMap", "rawBiomassMap", "ecoregionMap", "pixelGroupMap", "rstLCC",
      "standAgeMap", "speciesLayers", "rasterToMatch", "rasterToMatch_biomassParam",
      "studyArea", "studyArea_biomassParam"
    )
  ),

  ## Stage 4: mainSim (current 5-module set for parity; NRV_summary retires
  ## timeSinceFire/LandWeb_output in a later phase)
  tar_simspades(
    "mainSim",
    modules = c("Biomass_core", "LandMine", "Biomass_regeneration", "LandWeb_output", "timeSinceFire"),
    params = p_mainSim,
    times = list(start = 0, end = local$sim_end),
    inputs = quote(list(
      biomassMap = read_spatial(dataPrep_biomassMap),
      rawBiomassMap = read_spatial(dataPrep_rawBiomassMap),
      ecoregionMap = read_spatial(dataPrep_ecoregionMap),
      pixelGroupMap = read_spatial(dataPrep_pixelGroupMap),
      rstLCC = read_spatial(dataPrep_rstLCC),
      standAgeMap = read_spatial(dataPrep_standAgeMap),
      speciesLayers = read_spatial(dataPrep_speciesLayers),
      rasterToMatch = read_spatial(dataPrep_rasterToMatch),
      rasterToMatch_biomassParam = read_spatial(dataPrep_rasterToMatch_biomassParam),
      studyArea = read_spatial(dataPrep_studyArea),
      studyArea_biomassParam = read_spatial(dataPrep_studyArea_biomassParam),
      rstFlammable = read_spatial(preamble_rstFlammable),
      fireReturnInterval = read_spatial(preamble_fireReturnInterval),
      studyAreaReporting = read_spatial(preamble_studyAreaReporting),
      cohortData = dataPrep$cohortData,
      species = dataPrep$species,
      speciesEcoregion = dataPrep$speciesEcoregion,
      ecoregion = dataPrep$ecoregion,
      minRelativeB = dataPrep$minRelativeB,
      sufficientLight = dataPrep$sufficientLight,
      sppEquiv = dataPrep$sppEquiv,
      sppColorVect = dataPrep$sppColorVect,
      speciesParams = dataPrep$speciesParams,
      speciesTable = dataPrep$speciesTable,
      ROSTable = preamble$ROSTable
    )),
    seed = 1L, # Phase-0 single rep; later: per-rep seed via cross(rep_index)
    paths = local$paths,
    plain = c("cohortData", "simulationOutput"),
    spatial = c(
      "pixelGroupMap", "standAgeMap", "rstTimeSinceFire", "vegTypeMap",
      "rstCurrentBurnCumulative", "rstFlammable"
    )
  )

  ## Stage 5: summaries + report -- added once stage 4 runs (Part J / G#7)
)
