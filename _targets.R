# _targets.R -- LandWeb pipeline (Phase-0 spike: 1 study area, n_reps = 1).
#
# Proves the component-extraction-and-pass pattern (Part J of the refactor plan):
# each stage runs simInitAndSpades in-process and emits its components as
# targets -- plain objects as ordinary targets, spatial (terra) objects as
# `format = "file"` targets -- so no simList crosses a target boundary.

source("_local.R") # per-user/host knobs, BEFORE tar_source()

library(targets)
library(SpaDES.targets)

tar_option_set(
  packages = "SpaDES.targets", # for unqualified read_spatial() in target commands
  format = "rds"
  # controller = ... # crew / crew.ssh added in Phase 6
)

list(
  ## ---- branch sources ------------------------------------------------------
  tar_target(study_areas, local$study_areas), # Phase-0: one area
  tar_target(rep_index, seq_len(local$n_reps), iteration = "vector"),

  ## ---- Stage 1: preamble ---------------------------------------------------
  tar_simspades(
    "preamble",
    modules = "LandWeb_preamble",
    params = list(), # TODO: port parameters1 from 00-main.R
    paths = local$paths,
    plain = c("sppEquiv", "sppColorVect", "speciesParams", "speciesTable", "ROSTable"),
    spatial = c(
      "rasterToMatch", "rasterToMatch_biomassParam",
      "studyArea", "studyArea_biomassParam", "studyAreaReporting", "studyAreaANPP",
      "rstLCC", "standAgeMap", "rstFlammable", "fireReturnInterval"
    )
  ),

  ## ---- Stage 2: speciesData ------------------------------------------------
  tar_simspades(
    "speciesData",
    modules = "Biomass_speciesData",
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

  ## ---- Stage 3: dataPrep ---------------------------------------------------
  tar_simspades(
    "dataPrep",
    modules = c("Biomass_speciesFactorial", "Biomass_borealDataPrep", "Biomass_speciesParameters"),
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

  ## ---- Stage 4: mainSim ----------------------------------------------------
  ## NB: rstTimeSinceFire is derived in 03-main-sim.R (terra::crop of standAgeMap);
  ## TODO decide whether the module derives it or it's passed.
  tar_simspades(
    "mainSim",
    modules = c("Biomass_core", "LandMine", "Biomass_regeneration"),
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

  ## ---- Stage 5: summaries + report -- added once stage 4 runs (Part J / G#7)
)
