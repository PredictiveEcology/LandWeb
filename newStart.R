quickPlot::dev.useRSGD(useRSGD = FALSE) ## TODO: temporary for Alex's testing

usePOM <- FALSE
useDEoptim <- FALSE
useParallel <- if (isTRUE(usePOM)) 1 else 8

useSpades <- TRUE
minFRI <- 25
activeDir <- if (pemisc::user("rstudio")) "~/LandWeb" else "~/GitHub/LandWeb"
reproducible::checkPath(activeDir, create = TRUE)
setwd(activeDir)

sppEquivCol <- "LandWeb"

eventCaching <- c(".inputObjects", "init")
maxAge <- 400
vegLeadingProportion <- 0.8 # indicates what proportion the stand must be in one species group for it to be leading.
                            # If all are below this, then it is a "mixed" stand

ageClasses <- c("Young", "Immature", "Mature", "Old")
ageClassCutOffs <- c(0, 40, 80, 120)
fireTimestep <- 1

##############################################################
## set run name
##############################################################

#runName <- "testing"

## running locally
#runName <- "ANC"
#runName <- "ANC_doubleFRI"
#runName <- "ANC_equalROS"
#runName <- "ANC_logROS"
#runName <- "ANC_noDispersal"

## running locally
#runName <- "DMI"
#runName <- "DMI_doubleFRI"
#runName <- "DMI_equalROS"
#runName <- "DMI_logROS"
#runName <- "DMI_noDispersal"

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
if (pemisc::user("emcintir")) runName <- "tolko_AB_N_logROS"
#runName <- "tolko_AB_S_logROS"
# runName <- "tolko_SK_logROS"

## running locally
#runName <- "tolko_AB_N_noDispersal"
#runName <- "tolko_AB_S_noDispersal"
#runName <- "tolko_SK_noDispersal"

## running locally
#runName <- "tolko_AB_N_aspenDispersal"
#runName <- "tolko_AB_S_aspenDispersal"
#runName <- "tolko_SK_aspenDispersal"

## running locally
#runName <- "tolko_AB_N_aspen80"
#runName <- "tolko_AB_S_aspen80"
#runName <- "tolko_SK_aspen80"

## running locally
#runName <- "LP_MB"
#runName <- "LP_MB_doubleFRI"
#runName <- "LP_MB_equalROS"
#runName <- "LP_MB_logROS"
#runName <- "LP_MB_noDispersal"

message(crayon::red(runName))

if (grepl("LandWeb", runName)) {
  source(file.path("params", "LandWeb_parameters.R"))
} else if (grepl("ANC|DMI|LP|TOLKO", toupper(runName))) {
  source(file.path("params", "FMA_parameters.R"))
} else if (grepl("testing", tolower(runName))) {
  source(file.path("params", "testing_parameters.R"))
} else {
  stop("runName not associated with a parameter file. Please check the name and try again.")
}

##########################################################
# Packages for global.R -- don't need to load packages for modules -- happens automatically
##########################################################
library(data.table)
library(raster)
library(SpaDES.core)
library(pemisc)
library(map)
library(LandR)

#devtools::install_github("achubaty/amc@development")
library(amc)

packageLoadStartTime <- Sys.time()
SpaDESPkgs <- c(
  "PredictiveEcology/quickPlot@development",
  "PredictiveEcology/SpaDES.core@development",
  "PredictiveEcology/map@master",
  "PredictiveEcology/SpaDES.tools@development",
  #"PredictiveEcology/SpaDES.shiny@generalize-modules", ## do this after running the model, before app
  "raster"
)
shinyPkgs <- c("gdalUtils", "leaflet", "leaflet.extras", "parallel", "raster", "rgeos",
               "shiny", "shinyBS", "shinycssloaders", "shinydashboard", "shinyjs", "shinyWidgets")
googleAuthPkgs <- c("googleAuthR", "googledrive", "googleID")
moduleRqdPkgs <- c("crayon", "data.table", "dplyr", "fasterize", "fpCompare",
                   "gdalUtils", "ggplot2", "grDevices", "grid", "LandR",
                   "magrittr", "pemisc", "pryr", "purrr", "quickPlot",
                   "R.utils", "raster", "RColorBrewer", "Rcpp", "reproducible", "rgeos",
                   "scales", "sp", "SpaDES.core", "SpaDES.tools", "tidyr", "VGAM")

##########################################################
# Paths
##########################################################
paths <- list(
  cachePath = file.path("cache", runName),
  modulePath = "m", # short name because shinyapps.io can't handle longer than 100 characters
  inputPath = "inputs",
  outputPath = file.path("outputs", runName)
)
do.call(SpaDES.core::setPaths, paths) # Set them here so that we don't have to specify at each call to Cache
tilePath <- file.path(Paths$outputPath, "tiles")

## Options
.plotInitialTime <- if (user("emcintir")) NA else 0
opts <- options(
  "LandR.assertions" = if (user("emcintir")) TRUE else TRUE,
  "LandR.verbose" = if (user("emcintir")) 2 else 1,
  "map.dataPath" = Paths$inputPath, # not used yet
  "map.overwrite" = TRUE,
  "map.tilePath" = tilePath,
  "map.useParallel" = TRUE, #!identical("windows", .Platform$OS.type),
  "reproducible.destinationPath" = normPath(Paths$inputPath),
  "reproducible.inputPaths" = if (user("emcintir")) path.expand("~/data") else NULL,
  #"reproducible.devMode" = if (user("emcintir")) TRUE else FALSE,
  "reproducible.overwrite" = TRUE,
  "reproducible.useMemoise" = TRUE,
  "reproducible.useNewDigestAlgorithm" = TRUE,
  "reproducible.quick" = FALSE,
  "reproducible.useCache" = if (pemisc::user("emcintir")) "devMode" else TRUE,
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

#################################################
## create color palette for species used in model
#################################################
sppColors <- sppColors(sppEquivalencies_CA, sppEquivCol, newVals = "Mixed", palette = "Accent")

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

simOutPreamble <- Cache(simInitAndSpades,
                        times = list(start = 0, end = 1),
                        params = parameters1,
                        modules = c("LandWeb_preamble"),
                        objects1,
                        paths = paths,
                        debug = 1)

if (!is.na(.plotInitialTime)) {
  lapply(dev.list(), function(x) {
    try(quickPlot::clearPlot(force = TRUE))
    try(dev.off())
  })
  quickPlot::dev(2, width = 18, height = 10)
  Plot(simOutPreamble$studyAreaReporting, simOutPreamble$studyArea, simOutPreamble$studyAreaLarge)
  Plot(simOutPreamble$rasterToMatchReporting)
  Plot(simOutPreamble$rasterToMatch) # some bug in quickPlot that makes these 2 not plot together
}

#################################################
# Second spades call -- creates speciesLayers
#################################################

objects2 <- list(
  "nonTreePixels" = simOutPreamble$nonTreePixels,
  "rasterToMatch" = simOutPreamble$rasterToMatch,
  "rasterToMatchReporting" = simOutPreamble$rasterToMatchReporting,
  "sppColors" = sppColors,
  "sppEquiv" = sppEquivalencies_CA,
  "studyArea" = simOutPreamble$studyArea,
  "studyAreaLarge" = simOutPreamble$studyAreaLarge,
  "studyAreaReporting" = simOutPreamble$studyAreaReporting
)

parameters2 <- list(
  BiomassSpeciesData = list(
    "types" = c("KNN", "CASFRI", "Pickell", "ForestInventory"),
    "sppEquivCol" = sppEquivCol,
    "omitNonVegPixels" = TRUE,
    ".plotInitialTime" = .plotInitialTime
  )
)

if (!is.na(.plotInitialTime)) {
  quickPlot::dev(3, width = 18, height = 10)
}

simOutSpeciesLayers <- Cache(simInitAndSpades,
                             times = list(start = 0, end = 1),
                             params = parameters2,
                             modules = c("BiomassSpeciesData"),
                             objects2,
                             # make .plotInitialTime an argument, not a parameter -- Cache will see them as unchanged regardless of value
                             .plotInitialTime = .plotInitialTime,
                             paths = paths,
                             debug = 1)

######################################################
# Dynamic Simulation
######################################################
times <- list(start = 0, end = endTime)
modules <- list("Boreal_LBMRDataPrep", "LandR_BiomassGMOrig", "LBMR",
                "LandMine", "Biomass_regeneration",
                "LandWeb_output",
                "timeSinceFire")

speciesTable <- getSpeciesTable(dPath = Paths$inputPath) ## uses default URL
if (getOption("LandR.verbose") > 0) {
  message("Adjusting species-level traits for LandWeb")
}

objects <- list(
  "fireReturnInterval" = simOutPreamble$fireReturnInterval,
  "LCC2005" = simOutPreamble$LCC2005,
  "rasterToMatch" = simOutPreamble$rasterToMatch,
  "rstFlammable" = simOutPreamble$rstFlammable,
  "rstTimeSinceFire" = simOutPreamble$`CC TSF`,
  "sppColors" = sppColors,
  "sppEquiv" = sppEquivalencies_CA,
  "speciesLayers" = simOutSpeciesLayers$speciesLayers,
  "speciesTable" = speciesTable,
  "standAgeMap" = simOutPreamble$`CC TSF`, ## same as rstTimeSinceFire; TODO: use synonym?
  "rasterToMatchReporting" = simOutPreamble$rasterToMatchReporting,
  "studyArea" = simOutPreamble$studyArea,
  "studyAreaLarge" = simOutPreamble$studyAreaLarge,
  "studyAreaReporting" = simOutPreamble$studyAreaReporting,
  "summaryPeriod" = summaryPeriod,
  "useParallel" = 2
)

parameters <- list(
  Boreal_LBMRDataPrep = list(
    "sppEquivCol" = sppEquivCol,
    # next two are used when assigning pixelGroup membership; what resolution for
    #   age and biomass
    "pixelGroupAgeClass" = successionTimestep,
    "pixelGroupBiomassClass" = 100,
    "establishProbAdjFacResprout" = if (grepl("noDispersal|aspenDispersal", runName)) 1e4 else 0.5,
    "establishProbAdjFacNonResprout" = if (grepl("noDispersal|aspenDispersal", runName)) 1e4 else 2,
    "runName" = runName,
    ".useCache" = eventCaching
  ),
  LandMine = list(
    "biggestPossibleFireSizeHa" = 5e5,
    "burnInitialTime" = fireTimestep,
    "fireTimestep" = fireTimestep,
    "minPropBurn" = 0.90,
    "ROStype" = if (grepl("equalROS", runName)) "equal" else if (grepl("logROS", runName)) "log" else "original",
    ".useCache" = eventCaching,
    ".useParallel" = useParallel
  ),
  LandWeb_output = list(
    "sppEquivCol" = sppEquivCol,
    "summaryInterval" = summaryInterval,
    "vegLeadingProportion" = vegLeadingProportion,
    ".plotInitialTime" = .plotInitialTime,
    ".plotInterval" = 1
  ),
  LBMR = list(
    "initialBiomassSource" = "cohortData", # can be 'biomassMap' or "spinup" too
    "seedingAlgorithm" = if (grepl("noDispersal", runName)) "noDispersal" else "wardDispersal",
    "sppEquivCol" = sppEquivCol,
    "successionTimestep" = successionTimestep,
    ".useCache" = eventCaching[1], # seems slower to use Cache for both
    ".useParallel" = useParallel
  ),
  LandR_BiomassGMOrig = list(
    ".useParallel" = useParallel
  ),
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

outputs <- data.frame(stringsAsFactors = FALSE,
                      expand.grid(
                        objectName = objectNamesToSave,#, "oldBigPatch"),
                        saveTime = seq(objects$summaryPeriod[1], objects$summaryPeriod[2],
                                       by = parameters$LandWeb_output$summaryInterval)
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
if (file.exists(fseed)) {
  seed <- readRDS(fseed)
} else {
  seed <- sample(1e8, 1)
  saveRDS(seed, fseed)
}
set.seed(seed)
print(seed)

message(crayon::red(runName))

######## parameter estimation using POM (LandWeb#111)
if (isTRUE(usePOM)) {
  runName <- "tolko_SK_logROS"

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
    parametersPOM[[x]]$.useParallel <<- 2
  })

  opts2 <- options("LandR.assertions" = FALSE, "LandR.verbose" = 0)
  mySim <- simInit(times = list(start = 0, end = 250),
                   params = parametersPOM,
                   modules = modules,
                   outputs = outputs,
                   objects, # do not name this argument -- collides with Cache -- leave it unnamed
                   paths = paths,
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
    #cl <- parallel::makeCluster(N, type = "FORK")

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

    cl <- parallel::makeForkCluster(5 * nrow(params4POM))
    parallel::clusterExport(cl, list("objectiveFunction"))

    out <- parallel::parLapplyLB(cl = cl, X = purrr::transpose(tableOfRuns),
                                 function(x, sim) {
                                   #testFn(unlist(x[1:6]), sim)
                                   objectiveFunction(unlist(x[1:6]), sim)
                                 }, sim = mySim)
    tableOfRuns$objFnReturn <- unlist(out)

    parallel::stopCluster(cl)
  }

  options(opts2)
}

######## SimInit and Experiment
if (!useSpades) {
  cl <- map::makeOptimalCluster(MBper = 1e3, maxNumClusters = 10,
                                outfile = file.path(Paths$outputPath, "_parallel.log"))
  mySimOuts <- Cache(simInitAndExperiment,
                     times = times, cl = cl,
                     params = parameters,
                     modules = modules,
                     outputs = outputs,
                     debug = 1,
                     objects, # do not name this argument -- collides with
                     paths = paths,
                     loadOrder = unlist(modules),
                     clearSimEnv = TRUE,
                     .plotInitialTime = NA,
                     cache = TRUE, ## this caches each simulation rep (with all data!)
                     replicates = 1 ## TODO: can increase this later for additional runs
  )
  try(stopCluster(cl), silent = TRUE)

  saveRDS(mySimOuts, file.path(Paths$outputPath, "mySimOuts.rds"))
} else {
  if (!is.na(.plotInitialTime)) {
    quickPlot::dev(4, width = 18, height = 10)
    grid::grid.rect(0.93, 0.03, width = 0.2, height = 0.06, gp = gpar(fill = "white", col = "white"))
    grid::grid.text(label = runName, x = 0.93, y = 0.03)
  }

  mySimOut <- simInitAndSpades(times = times, #cl = cl,
                               params = parameters,
                               modules = modules,
                               outputs = outputs,
                               objects, # do not name this argument -- collides with Cache -- leave it unnamed
                               paths = paths,
                               loadOrder = unlist(modules),
                               debug = 1,
                               #debug = 'message(paste(unname(current(sim)), collapse = " "), try(print(sim$cohortData[pixelGroup %in% sim$pixelGroupMap[418136]])))',
                               .plotInitialTime = .plotInitialTime
  )
  #mySimOut <- spades(mySim, debug = 1)

  saveRDS(mySimOut, file.path(Paths$outputPath, "mySimOut.rds"))
}

if (FALSE) {
#ml <- readRDS(file.path(Paths$outputPath, "ml.rds"))
#ml <- readRDS(file.path(Paths$outputPath, "ml_done.rds"))
#mySimOuts <- readRDS(file.path(Paths$outputPath, "mySimOuts.rds"))

##########################################################
# Dynamic Raster Layers from Simulation
##########################################################

allouts <- unlist(lapply(mySimOuts, function(sim) outputs(sim)$file))
#allouts <- dir(Paths$outputPath, full.names = TRUE, recursive = TRUE)
allouts <- grep("vegType|TimeSince", allouts, value = TRUE)
allouts <- grep("gri|png|txt|xml", allouts, value = TRUE, invert = TRUE) ## TODO: need to rm the non-rep files too!!!
layerName <- gsub(allouts, pattern = paste0(".*", Paths$outputPath), replacement = "")
layerName <- gsub(layerName, pattern = "[/\\]", replacement = "_")
layerName <- gsub(layerName, pattern = "^_", replacement = "")
ag1 <- gsub(layerName, pattern = "(.*)_.*_(.*)\\..*", replacement = "\\1_\\2")
destinationPath <- dirname(allouts)
tsf <- gsub(".*vegTypeMap.*", NA, allouts)
vtm <- gsub(".*TimeSinceFire.*", NA, allouts)

options(map.useParallel = FALSE)
ml <- mapAdd(map = ml, layerName = layerName, analysisGroup1 = ag1,
             targetFile = asPath(allouts),
             destinationPath = asPath(destinationPath),
             filename2 = NULL, tsf = asPath(tsf), vtm = asPath(vtm),
             overwrite = TRUE,
             #useCache = "overwrite",
             leaflet = asPath(tilePath))
options(map.useParallel = TRUE)

######################################################################
# Add reporting polygons
######################################################################

## For Tolko runs, they are added above!

######################################################################
# Leading Veg Type By Age Class
######################################################################
options(map.useParallel = FALSE)
ml <- mapAddAnalysis(ml, functionName = "LeadingVegTypeByAgeClass",
                     #purgeAnalyses = "LeadingVegTypeByAgeClass",
                     ageClasses = ageClasses, ageClassCutOffs = ageClassCutOffs)
options(map.useParallel = TRUE)

# add an analysis -- this will trigger analyses because there are already objects in the map
#    This will trigger 2 more analyses ... largePatches on each raster x polygon combo (only 1 currently)
#    so there is 1 raster group, 2 polygon groups, 2 analyses - Total 4, only 2 run now
options(map.useParallel = FALSE)
ml <- mapAddAnalysis(ml, functionName = "LargePatches", ageClasses = ageClasses,
                     id = "1", labelColumn = "shinyLabel",
                     ageClassCutOffs = ageClassCutOffs)
options(map.useParallel = TRUE)

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
                            dPath = file.path(Paths$outputPath, "boxplots"))

saveRDS(ml, file.path(Paths$outputPath, "ml_done.rds"))
print(runName)

################################################################
###   WORKS UP TO HERE
################################################################

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
