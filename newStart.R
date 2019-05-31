quickPlot::dev.useRSGD(useRSGD = quickPlot::isRstudioServer()) ## TODO: temporary for Alex's testing

activeDir <- if (pemisc::user("rstudio")) "~/LandWeb" else "~/GitHub/LandWeb"
ageClasses <- c("Young", "Immature", "Mature", "Old")
ageClassCutOffs <- c(0, 40, 80, 120)
batchMode <- if (pemisc::user("achubaty")) TRUE else FALSE ## NOTE: runName must be defined
cloudCacheFolderID <- "/folders/1ry2ukXeVwj5CKEmBW1SZVS_W8d-KtmIj"
eventCaching <- c(".inputObjects", "init")
fireTimestep <- 1
mapParallel <- TRUE #getOption("Ncpus", parallel::detectCores() / 2)
maxAge <- 400
minFRI <- 25
postProcessOnly <- FALSE
rerunSpeciesLayers <- FALSE ## TODO: use this as workaround for speciesLayers cache problems
sppEquivCol <- "LandWeb"
useCloudCache <- FALSE # only for simInitAndSpades
useDEoptim <- FALSE
usePOM <- if (pemisc::user("achubaty")) FALSE else FALSE ## NOTE: TO and FROM indices must be defined
useParallel <- 2 ## values > 2 use WAY too much RAM for very little speed increase (too much overhead!)
useSpades <- if (pemisc::user("emcintir")) TRUE else TRUE
vegLeadingProportion <- 0.8 # indicates what proportion the stand must be in one species group for it to be leading.
                            # If all are below this, then it is a "mixed" stand

################################################################################
reproducible::checkPath(activeDir, create = TRUE)
setwd(activeDir)

##############################################################
## set run name
##############################################################

if (pemisc::user("emcintir"))
  runName <- "LandWeb_aspenDispersal_logROS"

if (isTRUE(batchMode)) {
  stopifnot(exists("runName", envir = .GlobalEnv)) ## run name should be set in batch_mode.R
} else {
  if (pemisc::user("achubaty") || pemisc::user("emcintir"))
    runName <- "tolko_SK_aspenDispersal_logROS_test01"

  ## running locally
  #runName <- "ANC"
  #runName <- "ANC_aspenDispersal_logROS"
  #runName <- "ANC_doubleFRI"
  #runName <- "ANC_equalROS"
  #runName <- "ANC_logROS"
  #runName <- "ANC_noDispersal"

  ## running locally
  #runName <- "DMI"
  #runName <- "DMI_aspenDispersal_logROS"
  #runName <- "DMI_doubleFRI"
  #runName <- "DMI_equalROS"
  #runName <- "DMI_logROS"
  #runName <- "DMI_noDispersal"

  ## running locally
  #runName <- "LP_MB"
  #runName <- "LP_MB_aspenDispersal_logROS"
  #runName <- "LP_MB_doubleFRI"
  #runName <- "LP_MB_equalROS"
  #runName <- "LP_MB_logROS"
  #runName <- "LP_MB_noDispersal"

  ## running locally
  #runName <- "tolko_AB_N"
  #runName <- "tolko_AB_S"
  #runName <- "tolko_SK"

  ## running locally
  #runName <- "tolko_AB_N_doubleFRI"
  #runName <- "tolko_AB_S_doubleFRI"
  #runName <- "tolko_SK_doubleFRI"

  ## running locally
  #runName <- "tolko_AB_N_equalROS"
  #runName <- "tolko_AB_S_equalROS"
  #runName <- "tolko_SK_equalROS"

  ## running locally
  #runName <- "tolko_AB_N_logROS"
  #runName <- "tolko_AB_S_logROS"
  #if (pemisc::user("emcintir")) runName <- "tolko_SK_logROS"

  ## running locally
  #runName <- "tolko_AB_N_noDispersal"
  #runName <- "tolko_AB_S_noDispersal"
  #runName <- "tolko_SK_noDispersal"

  ## running locally
  #runName <- "tolko_AB_N_aspenDispersal_logROS"
  #runName <- "tolko_AB_S_aspenDispersal_logROS"
  #runName <- "tolko_SK_aspenDispersal_logROS"

  ## running locally
  #runName <- "tolko_AB_N_aspen80"
  #runName <- "tolko_AB_S_aspen80"
  #runName <- "tolko_SK_aspen80"

  ## running locally
  #runName <- "LandWeb_aspenDispersal_logROS"
}
message(crayon::red(runName))

if (grepl("LandWeb", runName)) {
  source(file.path("params", "LandWeb_parameters.R")) ## same as FMA_parameters; i.e., no cache ids
} else if (grepl("ANC|DMI|LP|TOLKO", toupper(runName))) {
  source(file.path("params", "FMA_parameters.R"))
}

if (grepl("test", tolower(runName))) {
  source(file.path("params", "testing_parameters.R"))
}

##########################################################
# Packages for global.R -- don't need to load packages for modules -- happens automatically
##########################################################
library(data.table)
library(magrittr)
library(parallel)

library(raster)
library(SpaDES.core)
library(pemisc)
library(map)
library(LandR) # load_all("~/GitHub/PredictiveEcology/LandR")

#devtools::install_github("achubaty/amc@development")
library(amc)

packageLoadStartTime <- Sys.time()
SpaDESPkgs <- c(
  "PredictiveEcology/quickPlot@development",
  "PredictiveEcology/SpaDES.core@development",
  "PredictiveEcology/SpaDES.tools@development",
  "PredictiveEcology/map@development",
  "PredictiveEcology/LandR@development",
  "PredictiveEcology/LandWebUtils@development",
  #"PredictiveEcology/SpaDES.shiny@generalize-modules", ## do this after running the model, before app
  "raster"
)
shinyPkgs <- c("gdalUtils", "leaflet", "leaflet.extras", "parallel", "raster", "reactlog", "rgeos",
               "shiny", "shinyBS", "shinycssloaders", "shinydashboard", "shinyjs", "shinyWidgets")
googleAuthPkgs <- c("googleAuthR", "googledrive", "googleID")
moduleRqdPkgs <- c("crayon", "data.table", "dplyr", "fasterize", "fpCompare",
                   "gdalUtils", "ggplot2", "grDevices", "grid", "LandR",
                   "magrittr", "map", "parallel", "pemisc", "pryr", "purrr", "quickPlot",
                   "R.utils", "raster", "RColorBrewer", "Rcpp", "reproducible", "rgeos",
                   "scales", "sp", "SpaDES.core", "SpaDES.tools", "tidyr", "VGAM")

##########################################################
# Paths
##########################################################
paths1 <- list(
  ## use same cachePath for all data-prep steps before dynamic simulation
  cachePath = file.path("cache", "dataPrepGIS", "preamble"),
  modulePath = "m", # short name because shinyapps.io can't handle longer than 100 characters
  inputPath = "inputs",
  outputPath = file.path("outputs", runName)
)
do.call(SpaDES.core::setPaths, paths1) # Set them here so that we don't have to specify at each call to Cache
tilePath <- file.path(Paths$outputPath, "tiles")

## Options
.plotInitialTime <- if (user("emcintir")) NA else if (user("achubaty")) NA else 0

maxMemory <- if (grepl("LandWeb", runName)) 5e+12 else 5e+9
scratchDir <- file.path("/tmp/scratch/LandWeb")

rasterOptions(default = TRUE)
options(rasterMaxMemory = maxMemory, rasterTmpDir = scratchDir)

opts <- options(
  "future.globals.maxSize" = 1000*1024^2,
  "LandR.assertions" = if (user("emcintir")) FALSE else FALSE,
  "LandR.verbose" = if (user("emcintir")) 1 else 1,
  "map.dataPath" = Paths$inputPath, # not used yet
  "map.overwrite" = TRUE,
  "map.tilePath" = tilePath,
  "map.useParallel" = mapParallel,
  "reproducible.destinationPath" = normPath(Paths$inputPath),
  #"reproducible.devMode" = if (user("emcintir")) TRUE else FALSE,
  "reproducible.futurePlan" = if (.Platform$OS.type != "windows" && user("emcintir")) FALSE else FALSE,
  "reproducible.inputPaths" = if (user("emcintir")) path.expand("~/data") else NULL,
  "reproducible.overwrite" = TRUE,
  "reproducible.quick" = FALSE,
  "reproducible.showSimilar" = TRUE,
  "reproducible.useCache" = if (pemisc::user("emcintir")) TRUE else TRUE,
  "reproducible.useCloud" = TRUE,
  "reproducible.useGDAL" = FALSE, ## NOTE: gdal is faster, but mixing gdal with raster causes inconsistencies
  "reproducible.useMemoise" = ifelse(isTRUE(batchMode), FALSE, TRUE),
  "reproducible.useGDAL" = FALSE,
  "reproducible.useNewDigestAlgorithm" = TRUE,
  "spades.moduleCodeChecks" = FALSE,
  "spades.useRequire" = FALSE # Don't use Require... meaning assume all pkgs installed
)

httr::set_config(httr::config(http_version = 0))

#################################################
# Set up sppEquiv
#################################################
data("sppEquivalencies_CA", package = "LandR")
sppEquivalencies_CA[grep("Pin", LandR), `:=`(EN_generic_short = "Pine",
                                             EN_generic_full = "Pine",
                                             Leading = "Pine leading")]

# Make LandWeb spp equivalencies
sppEquivalencies_CA[, LandWeb := c(Pice_mar = "Pice_mar", Pice_gla = "Pice_gla",
                                   Pinu_con = "Pinu_sp", Pinu_ban = "Pinu_sp",
                                   Popu_tre = "Popu_sp", Betu_pap = "Popu_sp",
                                   Abie_bal = "Abie_sp", Abie_las = "Abie_sp", Abie_sp = "Abie_sp")[LandR]]

sppEquivalencies_CA[LandWeb == "Abie_sp", EN_generic_full := "Fir"]
sppEquivalencies_CA[LandWeb == "Abie_sp", EN_generic_short := "Fir"]
sppEquivalencies_CA[LandWeb == "Abie_sp", Leading := "Fir leading"]

sppEquivalencies_CA[LandWeb == "Popu_sp", EN_generic_full := "Deciduous"]
sppEquivalencies_CA[LandWeb == "Popu_sp", EN_generic_short := "Decid"]
sppEquivalencies_CA[LandWeb == "Popu_sp", Leading := "Deciduous leading"]

sppEquivalencies_CA <- sppEquivalencies_CA[!is.na(LandWeb),]

#################################################
## create color palette for species used in model
#################################################
sppColorVect <- sppColors(sppEquivalencies_CA, sppEquivCol, newVals = "Mixed", palette = "Accent")

#################################################
# Set up spades call for preamble -- studyArea stuff goes there
#################################################
objects1 <- list(
  #"sppEquiv" = sppEquivalencies_CA
)

parameters1 <- list(
  LandWeb_preamble = list(
    "minFRI" = minFRI,
    "runName" = runName
  )
)

simOutPreamble <- cloudCache(simInitAndSpades,
                             times = list(start = 0, end = 1),
                             params = parameters1,
                             modules = c("LandWeb_preamble"),
                             objects = objects1,
                             paths = paths1,
                             debug = 1,
                             useCloud = useCloudCache, #!isFALSE(getOption("reproducible.futurePlan")),
                             cloudFolderID = cloudCacheFolderID)

if (!is.na(.plotInitialTime)) {
  lapply(dev.list(), function(x) {
    try(quickPlot::clearPlot(force = TRUE))
    try(dev.off())
  })
  quickPlot::dev(2, width = 18, height = 10)
  grid::grid.rect(0.90, 0.03, width = 0.2, height = 0.06, gp = gpar(fill = "white", col = "white"))
  grid::grid.text(label = runName, x = 0.90, y = 0.03)

  Plot(simOutPreamble$studyAreaReporting, simOutPreamble$studyArea, simOutPreamble$studyAreaLarge)
  Plot(simOutPreamble$rasterToMatchReporting)
  Plot(simOutPreamble$rasterToMatch) # some bug in quickPlot that makes these 2 not plot together
}

#################################################
# Second spades call -- creates speciesLayers
#################################################

paths2 <- list(
  ## use same cachePath for all data-prep steps before dynamic simulation
  cachePath = file.path("cache", "dataPrepGIS", "speciesLayers"),
  modulePath = "m", # short name because shinyapps.io can't handle longer than 100 characters
  inputPath = "inputs",
  outputPath = file.path("outputs", runName)
)
do.call(SpaDES.core::setPaths, paths2)
tilePath <- file.path(Paths$outputPath, "tiles")

objects2 <- list(
  "nonTreePixels" = simOutPreamble$nonTreePixels,
  "rasterToMatch" = simOutPreamble$rasterToMatch,
  "rasterToMatchReporting" = simOutPreamble$rasterToMatchReporting,
  "sppColorVect" = sppColorVect,
  "sppEquiv" = sppEquivalencies_CA,
  "studyArea" = simOutPreamble$studyArea,
  "studyAreaLarge" = simOutPreamble$studyAreaLarge,
  "studyAreaReporting" = simOutPreamble$studyAreaReporting
)

parameters2 <- list(
  BiomassSpeciesData = list(
    "omitNonVegPixels" = TRUE,
    "types" = c("KNN", "CASFRI", "Pickell", "ForestInventory"),
    "sppEquivCol" = sppEquivCol,
    ".useCache" = FALSE
  )
)

if (isTRUE(rerunSpeciesLayers)) {
  ## delete existing species layers data and cache
  if (pemisc::user("achubaty")) {
    exts <- c(".tif", ".tif.vat.dbf", ".tif.vat.cpg", ".tif.ovr", ".tif.aux.xml", ".tfw")
    forInvFiles <- vapply(c("BlackSpruce1", "Deciduous1", "Fir1", "Pine1", "WhiteSpruce1"),
                          function(f) {
                            paste0(f, exts)
                          }, character(length(exts))) %>%
      c(., "CurrentCondition.zip", paste0(c("Abie_sp", "Pice_gla", "Pice_mar", "Pinu_sp", "Popu_sp"), "_overlay.tif")) %>%
      file.path(paths2$inputPath, .)
    vapply(forInvFiles, function(f) if (file.exists(f)) file.remove(f) else FALSE, logical(1))

    unlink(paths2$cachePath, recursive = TRUE)
  }

  ## (re)create species layers
  simOutSpeciesLayers <- Cache(simInitAndSpades,
                               times = list(start = 0, end = 1),
                               params = parameters2,
                               modules = c("BiomassSpeciesData"),
                               objects = objects2,
                               ## make .plotInitialTime an argument, not a parameter:
                               ##  - Cache will see them as unchanged regardless of value
                               .plotInitialTime = .plotInitialTime,
                               paths = paths2,
                               debug = 1)
  saveRDS(simOutSpeciesLayers, file.path(Paths$inputPath, "simOutSpeciesLayers.rds"), version = 3)
} else {
  simOutSpeciesLayers <- readRDS(file.path(Paths$inputPath, "simOutSpeciesLayers.rds"))
}

if (!is.na(.plotInitialTime)) {
  lapply(dev.list(), function(x) {
    try(quickPlot::clearPlot(force = TRUE))
    try(dev.off())
  })
  quickPlot::dev(3, width = 18, height = 10)
  grid::grid.rect(0.90, 0.03, width = 0.2, height = 0.06, gp = gpar(fill = "white", col = "white"))
  grid::grid.text(label = runName, x = 0.90, y = 0.03)

  Plot(simOutSpeciesLayers$speciesLayers)
}

######################################################
# Dynamic Simulation
######################################################
paths3 <- list(
  ## NOTE: use separate cachePath for each dynamic simulation
  cachePath = file.path("cache", runName),
  modulePath = "m", # short name because shinyapps.io can't handle longer than 100 characters
  inputPath = "inputs",
  outputPath = file.path("outputs", runName)
)
do.call(SpaDES.core::setPaths, paths3) # Set them here so that we don't have to specify at each call to Cache
tilePath <- file.path(Paths$outputPath, "tiles")

LandMineROStable <- rbindlist(list(
  list("mature", "decid", 9L),
  list("immature_young", "decid", 6L),
  list("immature_young", "mixed", 12L),
  list("mature", "mixed", 17L),
  list("immature", "pine", 14L),
  list("mature", "pine", 21L),
  list("young", "pine", 22L),
  list("immature_young", "softwood", 18L),
  list("mature", "softwood", 27L),
  list("immature_young", "spruce", 20L),
  list("mature", "spruce", 30L)
))
setnames(LandMineROStable, old = 1:3, new = c("age", "leading", "ros"))

if (grepl("equalROS", runName)) {
  LandMineROStable$ros <- 1L
} else if (grepl("logROS", runName)) {
  LandMineROStable$ros <- log(LandMineROStable$ros)
}

if (isFALSE(postProcessOnly)) {
  times <- list(start = 0, end = endTime)
  modules <- list("Boreal_LBMRDataPrep", #"LandR_BiomassGMOrig",
                  "LBMR",
                  "LandMine", "Biomass_regeneration",
                  "LandWeb_output",
                  "timeSinceFire")

  speciesTable <- getSpeciesTable(dPath = Paths$inputPath) ## uses default URL
  if (getOption("LandR.verbose") > 0) {
    message("Adjusting species-level traits for LandWeb")
  }

  objects <- list(
    "fireReturnInterval" = simOutPreamble$fireReturnInterval,
    "rstLCC" = simOutPreamble$LCC,
    "rasterToMatch" = simOutPreamble$rasterToMatch,
    "rasterToMatchReporting" = simOutPreamble$rasterToMatchReporting,
    "ROSTable" = LandMineROStable,
    "rstFlammable" = simOutPreamble$rstFlammable,
    "rstTimeSinceFire" = simOutPreamble$`CC TSF`,
    "sppColorVect" = sppColorVect,
    "sppEquiv" = sppEquivalencies_CA,
    "speciesLayers" = simOutSpeciesLayers$speciesLayers,
    "speciesTable" = speciesTable,
    "standAgeMap" = simOutPreamble$`CC TSF`, ## same as rstTimeSinceFire; TODO: use synonym?
    "studyArea" = simOutPreamble$studyArea,
    "studyAreaLarge" = simOutPreamble$studyAreaLarge,
    "studyAreaReporting" = simOutPreamble$studyAreaReporting,
    "summaryPeriod" = summaryPeriod, ## defined in params file
    "useParallel" = 2
  )

  parameters <- list(
    Boreal_LBMRDataPrep = list(
      ## fastLM is ~35% faster than the default lmer but needs 820GB RAM !!
      ## also, fastLM cannot deal with rank-deficient models
      #"biomassModel" = quote(RcppArmadillo::fastLm(formula = B ~ logAge * speciesCode * ecoregionGroup +
      #                                               cover * speciesCode * ecoregionGroup)),
      "biomassModel" = quote(lme4::lmer(B ~ logAge * speciesCode +
                                          cover * speciesCode +
                                          (logAge + cover + speciesCode | ecoregionGroup))),
      "cloudFolderID" = NA, #cloudCacheFolderID,
      "LCCClassesToReplaceNN" = 34:36,
      # next two are used when assigning pixelGroup membership; what resolution for
      #   age and biomass
      "pixelGroupAgeClass" = successionTimestep,
      "pixelGroupBiomassClass" = 100,
      "runName" = runName,
      "sppEquivCol" = sppEquivCol,
      "subsetDataAgeModel" = 100, ## TODO: test with `NULL` and `50`
      "subsetDataBiomassModel" = 50, ## TODO: test with `NULL` and `50`
      "useCloudCacheForStats" = FALSE, #TRUE,
      ".plotInitialTime" = .plotInitialTime,
      ".useCache" = eventCaching
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
    LBMR = list(
      "initialBiomassSource" = "cohortData", # can be 'biomassMap' or "spinup" too
      "seedingAlgorithm" = if (grepl("noDispersal", runName)) "noDispersal" else "wardDispersal",
      "sppEquivCol" = sppEquivCol,
      "successionTimestep" = successionTimestep,
      ".plotInitialTime" = .plotInitialTime,
      ".useCache" = eventCaching[1], # seems slower to use Cache for both
      ".useParallel" = useParallel
    ),
    # LandR_BiomassGMOrig = list(
    #   ".useParallel" = useParallel
    # ),
    Biomass_regeneration = list(
      "fireInitialTime" = fireTimestep,
      "fireTimestep" = fireTimestep,
      "successionTimestep" = successionTimestep
    ),
    timeSinceFire = list(
      "startTime" = fireTimestep,
      ".useCache" = eventCaching[1] # way faster without caching for "init"
    )
  )

  if (grepl("scfm", runName)) {
    source(file.path("params", "scfm_params.R"))
    modules <- append(modules[-which(modules == "LandMine")], scfmModules)
    objects <- append(objects, scfmObjects)
    parameters <- append(parameters, scfmParams)
  }

  objectNamesToSave <- c("rstTimeSinceFire", "vegTypeMap")
  analysesOutputsTimes <- seq(objects$summaryPeriod[1], objects$summaryPeriod[2],
                              by = parameters$LandWeb_output$summaryInterval)

  outputs <- data.frame(stringsAsFactors = FALSE,
                        expand.grid(
                          objectName = objectNamesToSave,
                          saveTime = c(timeSeriesTimes, analysesOutputsTimes)
                        ),
                        fun = "writeRaster", package = "raster",
                        file = paste0(objectNamesToSave, c(".tif", ".grd")))

  outputs2 <- data.frame(stringsAsFactors = FALSE,
                         expand.grid(objectName = c("simulationOutput"), saveTime = times$end),
                         fun = "saveRDS",
                         package = "base")

  outputs$arguments <- I(rep(list(list(overwrite = TRUE, progress = FALSE,
                                       datatype = "INT2U", format = "GTiff"),
                                  list(overwrite = TRUE, progress = FALSE,
                                       datatype = "INT1U", format = "raster")),
                             times = NROW(outputs) / length(objectNamesToSave)))

  outputs3 <- data.frame(stringsAsFactors = FALSE,
                         objectName = "rstFlammable",
                         saveTime = times$end, fun = "writeRaster", package = "raster",
                         arguments = I(list(list(overwrite = TRUE, progress = FALSE,
                                                 datatype = "INT2U", format = "raster"))))

  outputs <- as.data.frame(data.table::rbindlist(list(outputs, outputs2, outputs3), fill = TRUE))

  ######## set seed for RNG
  fseed <- file.path(Paths$outputPath, "seed.rds")
  fseed2 <- extension(fseed, "txt")
  if (file.exists(fseed)) {
    seed <- readRDS(fseed)
  } else {
    seed <- sample(1e4, 1)
    saveRDS(seed, fseed)
  }
  print(seed)
  cat(paste("Setting seed in newStart.R:", seed), file = fseed2, sep = "\n")
  set.seed(seed)
  writeRNGInfo(fseed2, append = TRUE)

  message(crayon::red(runName))

  ######## parameter estimation using POM (LandWeb#111)
  if (isTRUE(usePOM)) {
    data.table::setDTthreads(useParallel)
    runName <- "tolko_SK_logROS_POM"

    testFn <- function(params, sim) {
      sim2 <- reproducible::Copy(sim)

      params(sim2)$Boreal_LBMRDataPrep$establishProbAdjFacResprout <- params[1]
      params(sim2)$Boreal_LBMRDataPrep$establishProbAdjFacNonResprout <- params[2]
      params(sim2)$Boreal_LBMRDataPrep$growthCurveDecid <- params[3]
      params(sim2)$Boreal_LBMRDataPrep$growthCurveNonDecid <- params[4]
      params(sim2)$Boreal_LBMRDataPrep$mortalityShapeDecid <- params[5]
      params(sim2)$Boreal_LBMRDataPrep$mortalityShapeNonDecid <- params[6]

      sum(params) - 25 ## sum of param lower bounds is 25
    }

    objectiveFunction <- function(params, sim) {
      sim2 <- Copy(sim)

      params(sim2)$Boreal_LBMRDataPrep$establishProbAdjFacResprout <- params[1]
      params(sim2)$Boreal_LBMRDataPrep$establishProbAdjFacNonResprout <- params[2]
      params(sim2)$Boreal_LBMRDataPrep$growthCurveDecid <- params[3]
      params(sim2)$Boreal_LBMRDataPrep$growthCurveNonDecid <- params[4]
      params(sim2)$Boreal_LBMRDataPrep$mortalityShapeDecid <- params[5]
      params(sim2)$Boreal_LBMRDataPrep$mortalityShapeNonDecid <- params[6]

      httr::set_config(httr::config(http_version = 0)) ## worakorund 'HTTP2 framing layer' error

      mySimOut <- spades(sim2, .plotInitialTime = NA)

      summaryTable <- mySimOut$summaryBySpecies1[, totalPixels := sum(counts), by = year]
      summaryTable[, proportion := counts / totalPixels]

      initial <- summaryTable[year == 0, ]
      final <- summaryTable[year == end(sim), ]

      species <- unique(initial$leadingType)

      val <- 0
      for (x in species) {
        p_i <- initial[leadingType == x,]$proportion
        p_f <- final[leadingType == x,]$proportion

        # deal with missing species
        if (identical(p_i, numeric(0))) p_i <- 0
        if (identical(p_f, numeric(0))) p_f <- 0

        val <<- val + (p_f - p_i)^2
      }

      return(val)
    }

    parametersPOM <- parameters
    lapply(names(parametersPOM), function(x) {
      parametersPOM[[x]]$.plotInitialTime <<- NA
      parametersPOM[[x]]$.useParallel <<- useParallel
    })

    opts2 <- options("LandR.assertions" = FALSE, "LandR.verbose" = 0)
    mySim <- simInit(times = list(start = 0, end = 250),
                     params = parametersPOM,
                     modules = modules,
                     outputs = outputs,
                     objects = objects,
                     paths = paths3,
                     loadOrder = unlist(modules)
    )

    params4POM <- data.frame(
      name = c("establishProbAdjFacResprout", "establishProbAdjFacNonResprout",
               "growthCurveDecid", "growthCurveNonDecid",
               "mortalityShapeDecid", "mortalityShapeNonDecid"),
      lower = c(0, 1, 0, 0, 15, 15),
      upper = c(1, 2, 1, 1, 25, 25),
      stringsAsFactors = FALSE
    )

    packages4POM <- unique(c("lme4", "LandR", "map", "quickPlot", "reproducible",
                             "SpaDES.core", "SpaDES.tools", moduleRqdPkgs, googleAuthPkgs))

    if (isTRUE(useDEoptim)) {
      ## NOTE: bug in DEoptim prevents using our own cluster (ArdiaD/DEoptim#3)
      N <- 10 * nrow(params4POM) ## need 10 populations per parameter
      #cl <- parallel::makeCluster(N, type = "SOCK") ## forking doesn't work with data.table

      outPOM <- DEoptim::DEoptim(fn = objectiveFunction, #testFn,
                                 sim = mySim,
                                 control = DEoptim::DEoptim.control(
                                   #cluster = cl, ## see ArdiaD/DEoptim#3
                                   initialpop = matrix(c(
                                     runif(N, params4POM[1,]$lower, params4POM[1,]$upper),
                                     runif(N, params4POM[2,]$lower, params4POM[2,]$upper),
                                     runif(N, params4POM[3,]$lower, params4POM[3,]$upper),
                                     runif(N, params4POM[4,]$lower, params4POM[4,]$upper),
                                     runif(N, params4POM[5,]$lower, params4POM[5,]$upper),
                                     runif(N, params4POM[6,]$lower, params4POM[6,]$upper)
                                   ), ncol = nrow(params4POM)),
                                   itermax = 30,
                                   packages = packages4POM,
                                   parallelType = 1,
                                   parVar = list("objectiveFunction", "mySim"),
                                   VTR = 0
                                 ),
                                 lower = params4POM$lower,
                                 upper = params4POM$upper
      )
      #parallel::stopCluster(cl) ## see ArdiaD/DEoptim#3
    } else {
      n <- 3 ## number of values per parameter to use

      tableOfRuns <- expand.grid(
        establishProbAdjFacResprout = seq(params4POM[1,]$lower, params4POM[1,]$upper, length.out = n),
        establishProbAdjFacNonResprout = seq(params4POM[2,]$lower, params4POM[2,]$upper, length.out = n),
        growthCurveDecid = seq(params4POM[3,]$lower, params4POM[3,]$upper, length.out = n),
        growthCurveNonDecid = seq(params4POM[4,]$lower, params4POM[4,]$upper, length.out = n),
        mortalityShapeDecid = seq(params4POM[5,]$lower, params4POM[5,]$upper, length.out = n),
        mortalityShapeNonDecid = seq(params4POM[6,]$lower, params4POM[6,]$upper, length.out = n)
      )
      tableOfRuns$objFnReturn <- rep(NA_real_, NROW(tableOfRuns))

      cl <- parallel::makePSOCKcluster(5 * nrow(params4POM)) ## forking doesn't work with data.table
      parallel::clusterExport(cl, list("objectiveFunction"))

      out <- parallel::parLapplyLB(cl = cl,
                                   purrr::transpose(tableOfRuns),
                                   function(x, sim) {
                                     #testFn(unlist(x[1:6]), sim)
                                     objectiveFunction(unlist(x[1:6]), sim)
                                   }, sim = mySim)
      tableOfRuns$objFnReturn <- unlist(out)

      # FROM = 1; TO = 1;
      # FROM = 1; TO = nrow(tableOfRuns);
      # ids <- seq(FROM, TO, by = 1)
      # out <- lapply(purrr::transpose(tableOfRuns[ids,]),
      #               function(x, sim) {
      #                 #testFn(unlist(x[1:6]), sim)
      #                 objectiveFunction(unlist(x[1:6]), sim)
      #               }, sim = mySim)
      # tableOfRuns$objFnReturn[ids] <- unlist(out)

      parallel::stopCluster(cl)
    }

    options(opts2)
  }

  ######## SimInit and Experiment
  if (!useSpades) {
    data.table::setDTthreads(useParallel) # 4
    # cl <- pemisc::makeOptimalCluster(useParallel = TRUE, MBper = 5e3, maxNumClusters = 12,
    #                                  outfile = file.path(Paths$outputPath, "_parallel.log"))
    cl <- NULL
    #cl <- makeForkCluster(12); clusterSetRNGStream(cl, iseed = NULL)
    mySimOuts <- Cache(simInitAndExperiment,
                       times = times, cl = cl,
                       params = parameters,
                       modules = modules,
                       outputs = outputs,
                       debug = 1,
                       objects = objects,
                       paths = paths3,
                       loadOrder = unlist(modules),
                       clearSimEnv = TRUE,
                       .plotInitialTime = NA,
                       cache = TRUE, ## this caches each simulation rep (with all data!)
                       replicates = 12, ## TODO: can increase this later for additional runs
                       seed = seed
    )
    try(parallel::stopCluster(cl), silent = TRUE)
    saveRDS(mySimOuts, file.path(Paths$outputPath, "mySimOuts.rds"))
  } else {
    if (!is.na(.plotInitialTime)) {
      quickPlot::dev(4, width = 18, height = 10)
      grid::grid.rect(0.90, 0.03, width = 0.2, height = 0.06, gp = gpar(fill = "white", col = "white"))
      grid::grid.text(label = runName, x = 0.90, y = 0.03)
    }

    data.table::setDTthreads(useParallel) # 4

    mySimOut <- simInitAndSpades(times = times, #cl = cl,
                                 params = parameters,
                                 modules = modules,
                                 outputs = outputs,
                                 objects = objects,
                                 paths = paths3,
                                 loadOrder = unlist(modules),
                                 debug = 1,
                                 #debug = 'message(paste(unname(current(sim)), collapse = " "), try(print(sim$cohortData[pixelGroup %in% sim$pixelGroupMap[418136]])))',
                                 .plotInitialTime = .plotInitialTime
    )
    #mySimOut <- spades(mySim, debug = 1)

    saveRDS(mySimOut, file.path(Paths$outputPath, "mySimOut.rds"))
  }
} else {
  ##########################################################
  # Simulation Post-processing
  ##########################################################

  #devtools::install_github("PredictiveEcology/LandWebUtils@development")
  library(LandWebUtils) ## load_all("~/GitHub/PredictiveEcology/LandWebUtils")

  #allouts <- unlist(lapply(mySimOuts, function(sim) outputs(sim)$file))
  allouts <- dir(Paths$outputPath, full.names = TRUE, recursive = TRUE)
  allouts <- grep("vegType|TimeSince", allouts, value = TRUE)
  allouts <- grep("gri|png|txt|xml", allouts, value = TRUE, invert = TRUE) ## TODO: need to rm the non-rep files too!!!
  layerName <- gsub(allouts, pattern = paste0(".*", Paths$outputPath), replacement = "")
  layerName <- gsub(layerName, pattern = "[/\\]", replacement = "_")
  layerName <- gsub(layerName, pattern = "^_", replacement = "")
  ag1 <- gsub(layerName, pattern = "(.*)_.*_(.*)\\..*", replacement = "\\1_\\2") %>%
    grep(paste(analysesOutputsTimes, collapse = "|"), ., value = TRUE)
  destinationPath <- dirname(allouts)
  tsf <- gsub(".*vegTypeMap.*", NA, allouts) %>%
    grep(paste(analysesOutputsTimes, collapse = "|"), ., value = TRUE)
  vtm <- gsub(".*TimeSinceFire.*", NA, allouts) %>%
    grep(paste(analysesOutputsTimes, collapse = "|"), ., value = TRUE)

  ml <- simOutPreamble$ml

  parameters2a <- list(
    BiomassSpeciesData = list(
      "omitNonVegPixels" = TRUE,
      "types" = c("ForestInventory"),
      "sppEquivCol" = sppEquivCol,
      ".useCache" = FALSE
    )
  )

  simOutSpeciesLayers2a <- cloudCache(simInitAndSpades,
                                      times = list(start = 0, end = 1),
                                      params = parameters2a,
                                      modules = c("BiomassSpeciesData"),
                                      objects = objects2,
                                      ## make .plotInitialTime an argument, not a parameter:
                                      ##  - Cache will see them as unchanged regardless of value
                                      .plotInitialTime = .plotInitialTime,
                                      paths = paths2,
                                      debug = 1,
                                      cloudFolderID = cloudCacheFolderID)

  vtmCC <- makeVegTypeMap(simOutSpeciesLayers2a$speciesLayers, vegLeadingProportion, mixed = TRUE)
  fname <- file.path(Paths$outputPath, "CurrentConditionVTM.tif")
  writeRaster(vtmCC, fname, overwrite = TRUE)

  fname2 <- file.path(Paths$outputPath, "CurrentConditionTSF.tif")
  writeRaster(ml$`CC TSF`, fname2, overwrite = TRUE)

  ml <- mapAdd(map = ml, layerName = "CC VTM", analysisGroup1 = "CC",
               targetFile = asPath(fname),
               destinationPath = asPath(Paths$outputPath),
               filename2 = NULL,
               tsf = asPath(fname2),
               vtm = asPath(fname),
               CC = TRUE,
               overwrite = TRUE,
               #useCache = "overwrite",
               leaflet = asPath(tilePath))

  options(map.useParallel = FALSE)
  ml <- mapAdd(map = ml, layerName = layerName, analysisGroup1 = ag1,
               targetFile = asPath(allouts),
               destinationPath = asPath(destinationPath),
               filename2 = NULL, tsf = asPath(tsf), vtm = asPath(vtm),
               overwrite = TRUE,
               #useCache = "overwrite",
               leaflet = asPath(tilePath))
  options(map.useParallel = mapParallel)

  saveRDS(ml, file.path(Paths$outputPath, "ml.rds"))
  #ml <- readRDS(file.path(Paths$outputPath, "ml.rds"))

  ######################################################################
  # create vtm and tsf stacks for animation
  ######################################################################

  tsfTimeSeries <- gsub(".*vegTypeMap.*", NA, allouts) %>%
    grep(paste(timeSeriesTimes, collapse = "|"), ., value = TRUE)
  vtmTimeSeries <- gsub(".*TimeSinceFire.*", NA, allouts) %>%
    grep(paste(timeSeriesTimes, collapse = "|"), ., value = TRUE)

  tsfStack <- raster::stack(tsfTimeSeries)# %>% writeRaster(file.path(Paths$outputPath, "stack_tsf.tif"))
  gifName <- file.path(normPath(Paths$outputPath), "animation_tsf.gif")
  animation::saveGIF(ani.height = 1200, ani.width = 1200, interval = 1.0,
                     movie.name = gifName, expr = {
                       brks <- c(0, 1, 40, 80, 120, 1000)
                       cols <- RColorBrewer::brewer.pal(5, "RdYlGn")
                       for (i in seq(numLayers(tsfStack))) {
                         plot(mask(tsfStack[[i]], studyArea(ml, 2)), breaks = brks, col = cols)
                       }
  })
  rm(tsfStack)

  #vtmStack <- raster::stack(vtmTimeSeries)# %>% writeRaster(file.path(Paths$outputPath, "stack_vtm.tif"))
  #gifName <- file.path(normPath(Paths$outputPath), "animation_vtm.gif")
  #animation::saveGIF(ani.height = 1200, ani.width = 1200, interval = 1.0,
  #                   movie.name = gifName, expr = {
  #                     for (i in seq(numLayers(vtmStack)))
  #                       plot(mask(vtmStack[[i]], studyArea(ml, 2))) # TODO: this animation isn't great!
  #})
  #rm(vtmStack)

  ######################################################################
  # Leading Veg Type By Age Class
  ######################################################################

  options(map.useParallel = FALSE)
  ml <- mapAddAnalysis(ml, functionName = "LeadingVegTypeByAgeClass",
                       #purgeAnalyses = "LeadingVegTypeByAgeClass",
                       ageClasses = ageClasses, ageClassCutOffs = ageClassCutOffs)
  options(map.useParallel = mapParallel)

  # add an analysis -- this will trigger analyses because there are already objects in the map
  #    This will trigger 2 more analyses ... largePatches on each raster x polygon combo
  #    so there is 1 raster group, 2 polygon groups, 2 analyses - Total 4, only 2 run now
  options(map.useParallel = FALSE)
  ml <- mapAddAnalysis(ml, functionName = "LargePatches",
                       id = "1", labelColumn = "shinyLabel",
                       #purgeAnalyses = "LargePatches",
                       ageClasses = ageClasses, ageClassCutOffs = ageClassCutOffs,
                       sppEquivCol = "EN_generic_short", sppEquiv = sppEquivalencies_CA)
  options(map.useParallel = mapParallel)

  saveRDS(ml, file.path(Paths$outputPath, "ml_partial.rds"))

  ############################################################
  # Post hoc analyses -- specifically making the data.tables for histograms & boxplots
  ############################################################
  # This analysisGroupReportingPolygon MUST be the same as one of ones already
  #   analysed.
  ml <- mapAddPostHocAnalysis(map = ml, functionName = "rbindlistAG",
                              postHocAnalysisGroups = "analysisGroupReportingPolygon",
                              #purgeAnalyses = "rbindlistAG",
                              postHocAnalyses = "all")
  ml <- mapAddPostHocAnalysis(map = ml, functionName = "runBoxPlotsVegCover",
                              postHocAnalysisGroups = "analysisGroupReportingPolygon",
                              postHocAnalyses = "rbindlistAG",
                              #purgeAnalyses = "runBoxPlotsVegCover",
                              dPath = file.path(Paths$outputPath, "boxplots"))
  ml <- mapAddPostHocAnalysis(map = ml, functionName = "runHistsLargePatches",
                              postHocAnalysisGroups = "analysisGroupReportingPolygon",
                              postHocAnalyses = "rbindlistAG",
                              #purgeAnalyses = "runBoxPlotsVegCover",
                              dPath = file.path(Paths$outputPath, "boxplots"))

  saveRDS(ml, file.path(Paths$outputPath, "ml_done.rds"))
  print(runName)
}

################################################################
###   WORKS UP TO HERE
################################################################

if (FALSE) {
  ##########################################################
  # Reporting Polygons
  ##########################################################
  ml2 <- mapAdd(map = ml, layerName = "AB Natural Sub Regions",
                url = "https://drive.google.com/file/d/1mCEynahKnFkStJUJC8ho5ndRD41olz9F/view?usp=sharing",
                columnNameForLabels = "Name")

  ##########################################################
  # Load other maps
  ##########################################################

  ml <- mapAdd(map = ml,
               destinationPath = "~/GitHub/LandWeb/inputs/FMA_Boundaries/DMI/",
               targetCRS = targetCRS,
               targetFile = "DMI_Full.shp", #studyArea = studyArea(ml, 1),
               layerName = "DMI Full", overwrite = TRUE, isStudyArea = TRUE,
               columnNameForLabels = "Name", administrative = TRUE)

  ml <- mapAdd(map = ml, layerName = "AB Natural Sub Regions", overwrite = TRUE,
               url = "https://drive.google.com/file/d/1mCEynahKnFkStJUJC8ho5ndRD41olz9F/view?usp=sharing",
               columnNameForLabels = "Name", filename2 = NULL)

  ml <- mapAdd(url = "https://drive.google.com/open?id=1JnKeXrw0U9LmrZpixCDooIm62qiv4_G1",
               map = ml, leaflet = TRUE, #studyArea = studyArea(ml, 2),
               #targetFile = "age1.tif", overwrite = TRUE,
               filename2 = NULL,
               layerName = "Age") # dots include things like method = "ngb" for projectRaster

  ################################
  # set some options
  #################################
  source("appInfo.R")

  # Options
  originalOpts <- options("spades.moduleCodeChecks" = FALSE, "reproducible.quick" = FALSE,
                          reproducible.verbose = FALSE, reproducible.useMemoise = TRUE,
                          spades.browserOnError = FALSE)

  # Google Authentication setup
  options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/userinfo.email",
                                          "https://www.googleapis.com/auth/userinfo.profile"))

  if (Sys.info()["nodename"] == "landweb.ca") {
    ## LandWeb.ca (live version)
    options(googleAuthR.webapp.client_id = "680957910261-kmlslu6vu0fo9129oj1fckksapg94gja.apps.googleusercontent.com")
    options(googleAuthR.webapp.client_secret = "Qe0TE327wRf9DYM-BEhDxe4a")
  } else {
    ## LandWeb.org (Alex's development version)
    options(googleAuthR.webapp.client_id = "869088473060-a7o2bc7oit2vn11gj3ieh128eh8orb04.apps.googleusercontent.com")
    options(googleAuthR.webapp.client_secret = "FR-4jL12j_ynAtsl-1Yk_cEL")
  }
  options(httr_oob_default = TRUE)

  appURL <- "http://landweb.ca"

  ##########################################################
  # Set paths
  ##########################################################
  paths <- list(
    cachePath = "cache",
    modulePath = "m", # short name because shinyapps.io can't handle longer than 100 characters
    inputPath = "inputs",
    outputPath = "outputs"
  )
  do.call(setPaths, paths) # Set them here so that we don't have to specify at each call to Cache

  ##########################################################
  # source auxiliary functions
  ##########################################################
  source("R/functions.R")
  ## source additional shiny modules
  vapply(list.files("shiny-modules", "[.]R", full.names = TRUE), source, vector("list", 2))

  # This needs simInit call to be run already
  # a few map details for shiny app
  message("Preparing polygon maps for reporting histograms")
  source(file.path("R", "colorPaletteForShiny.R"))
  labelColumn <- "shinyLabel"




  # leaflet parameters
  leafletZoomInit <- 5

  # Some shinycssloaders options
  options("spinner.type" = 5)

  # This will search for gdal utilities. If it finds nothing, and you are on Windows,
  #   you should install the GDAL that comes with QGIS -- use OSGeo4W Network Installer 64 bit
  #   may be still here: http://www.qgis.org/en/site/forusers/download.html
  options(gdalUtils_gdalPath = Cache(gdalSet, cacheRepo = paths$cachePath))

  ########################################
  # simInit
  ########################################
  # Time steps
  fireTimestep <- 1
  successionTimestep <- 10 # was 2

  ## spades module variables -- creates
  # eventCaching, maxAge, vegLeadingProportion
  # ageClasses, ageClassCutOffs, ageClas0s0Zones
  source("R/LandWeb user parameters.R")
  landisInputs <- readRDS(file.path(paths$inputPath, "landisInputs.rds"))
  spEcoReg <- readRDS(file.path(paths$inputPath, "SpEcoReg.rds"))

  # The CRS for the Study -- spTransform converts this first one to the second one, they are identical geographically
  # crsStudyArea <- CRS(paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0",
  #                         "+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  crsStudyArea <- CRS(paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0",
                              "+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))


  ######################################################
  ml <- mapAdd(map = ml,
               url = "https://drive.google.com/file/d/1Oz2vSor3oIKf2uGv3KRtLoLRWEfX5Mas/view?usp=sharing",
               layerName = "Mountain Northern Caribou Ranges",
               columnNameForLabels = "Name")

  ml <- mapAdd(map = ml, layerName = "Provincial Parks",
               url = "https://drive.google.com/file/d/1GHgTI4JY-YhAXvWkgV20vugbvLNqEEGH/view?usp=sharing",
               columnNameForLabels = "Name")
  ml <- mapAdd(map = ml, layerName = "NWT Ecoregions",
               url = "https://drive.google.com/file/d/1iRAQfARkmS6-XVHFnTkB-iltzMNPAczC/view?usp=sharing",
               columnNameForLabels = "Name")
  ml <- mapAdd(map = ml, layerName = "National Parks",
               url = "https://drive.google.com/file/d/1B3VUU8PDn4NPveAyF76OBPY0vZkxScEt/view?usp=sharing",
               columnNameForLabels = "Name")
  ml <- mapAdd(map = ml, layerName = "AB Natural Sub Regions",
               url = "https://drive.google.com/file/d/1mCEynahKnFkStJUJC8ho5ndRD41olz9F/view?usp=sharing",
               columnNameForLabels = "Name")
  # "LP MASTERFILE June62012",
  #   url = "https://drive.google.com/file/d/1J38DKQQavjBV9F3z2gGzHNuNE0s2rmhh/view?usp=sharing",
  #   columnNameForLabels = "Name"),
  ml <- mapAdd(map = ml, layerName = "BC Bio Geoclimatic Zones",
               url = "https://drive.google.com/file/d/1VAwsax63l2akOM2j_O4Je9p0ZiYg8Hl-/view?usp=sharing",
               columnNameForLabels = "ZONE_NAME")
  ml <- mapAdd(map = ml, layerName = "FMU Alberta 2015-11",
               url = "https://drive.google.com/file/d/1JiCLcHh5fsBAy8yAx8NgtK7fxaZ4Tetl/view?usp=sharing",
               columnNameForLabels = "FMU_NAME")
  ml <- mapAdd(map = ml, layerName = "FMA Boundary Updated",
               url = "https://drive.google.com/file/d/1nTFOcrdMf1hIsxd_yNCSTr8RrYNHHwuc/view?usp=sharing",
               columnNameForLabels = "Name")
  ml <- mapAdd(map = ml, layerName = "Boreal Caribou Ranges",
               url = "https://drive.google.com/file/d/1PYLou8J1wcrme7Z2tx1wtA4GvaWnU1Jy/view?usp=sharing",
               columnNameForLabels = "Name")


  ### RASTERS
  # STOPPED HERE
  # Current Condition
  preProcess(url = "https://drive.google.com/file/d/1JnKeXrw0U9LmrZpixCDooIm62qiv4_G1/view?usp=sharing")

  ml <- mapAdd(map = ml, url = "https://drive.google.com/file/d/1Oz2vSor3oIKf2uGv3KRtLoLRWEfX5Mas/view?usp=sharing",
               layerName = "Mountain Northern Caribou Ranges",
               columnNameForLabels = "Name")
}
