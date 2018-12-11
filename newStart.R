quickPlot::dev.useRSGD(useRSGD = FALSE) ## TODO: temporary for Alex's testing

useSpades <- TRUE
minFRI <- 40
activeDir <- "~/GitHub/LandWeb"
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

runName <- "testing"

#runName <- "tolko_AB_N"  ## original
#runName <- "tolko_AB_S"  ## original
#runName <- "tolko_SK"  ## original

## running locally
#runName <- "tolko_AB_N_doubleFRI" ## DONE
#runName <- "tolko_AB_S_doubleFRI" ## DONE
#runName <- "tolko_SK_doubleFRI" ## DONE

## running locally
#runName <- "tolko_AB_N_equalROS" ## DONE
#runName <- "tolko_AB_S_equalROS" ## DONE
#runName <- "tolko_SK_equalROS" ## DONE

## running locally
#runName <- "tolko_AB_N_logROS" ## DONE
#runName <- "tolko_AB_S_logROS" ## DONE
runName <- "tolko_SK_logROS" ## DONE

## running locally
#runName <- "tolko_AB_N_noDispersal" ## running
#runName <- "tolko_AB_S_noDispersal" ## running
#runName <- "tolko_SK_noDispersal" ## running

## running locally
#runName <- "tolko_AB_N_doubleDispersal" ## running
#runName <- "tolko_AB_S_doubleDispersal" ## running
#runName <- "tolko_SK_doubleDispersal" ## running

## running locally
#runName <- "tolko_AB_N_aspen80" ## not run
#runName <- "tolko_AB_S_aspen80" ## not run
#runName <- "tolko_SK_aspen80" ## running

## running locally
#runName <- "LP_MB" ## DONE
#runName <- "LP_MB_logROS" ## DONE

## running by Eliot on 343 Oct 27, 2018 -- These have Current Conditions as Initial Conditions,
#       more variable fire num per year
#runName <- "tolko_AB_N_logROS_new_Eliot" ## running

print(runName)
source(file.path("params", paste0("Development_Parameters_", runName, ".R")))

##########################################################
# Packages for global.R -- don't need to load packages for modules -- happens automatically
##########################################################
library(data.table)
library(raster)
library(SpaDES.core)
library(pemisc)
library(map)

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
moduleRqdPkgs <- c("data.table", "dplyr", "fasterize", "fpCompare",
                   "gdalUtils", "ggplot2", "grDevices", "grid", "magrittr",
                   "pryr", "purrr", "quickPlot",
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
opts <- options(
  "map.dataPath" = Paths$inputPath, # not used yet
  "map.overwrite" = TRUE,
  "map.tilePath" = tilePath,
  "map.useParallel" = TRUE, #!identical("windows", .Platform$OS.type),
  "reproducible.destinationPath" = normPath(Paths$inputPath),
  "reproducible.overwrite" = TRUE,
  "reproducible.quick" = FALSE,
  "reproducible.useCache" = TRUE,
  "spades.moduleCodeChecks" = FALSE,
  "spades.useRequire" = FALSE # Don't use Require... meaning assume all pkgs installed
)

#################################################
# Set up sppEquiv
#################################################
data("sppEquivalencies_CA", package = "pemisc")
sppEquivalencies_CA[grep("Pin", LandR), `:=`(EN_generic_short = "Pine",
                                             EN_generic_full = "Pine",
                                             Leading = "Pine leading")]

# Make LandWeb spp equivalencies
sppEquivalencies_CA[, LandWeb := c(Pice_mar = "Pice_mar", Pice_gla = "Pice_gla",
                                   Pinu_con = "Pinu_sp", Pinu_ban = "Pinu_sp",
                                   Popu_tre = "Popu_sp", Betu_pap = "Popu_sp",
                                   Abie_bal = "Abie_sp", Abie_las = "Abie_sp")[LandR]]

sppEquivalencies_CA[LandWeb == "Abie_sp", EN_generic_full := "Fir"]
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
  "sppEquiv" = sppEquivalencies_CA
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

#################################################
# Second spades call -- creates speciesLayers
#################################################

objects2 <- list(
  "rasterToMatch" = simOutPreamble$rasterToMatch,
  "sppColors" = sppColors,
  "sppEquiv" = sppEquivalencies_CA,
  "studyArea" = simOutPreamble$studyArea,
  "studyAreaLarge" = simOutPreamble$studyAreaLarge,
  "nonVegPixels" = simOutPreamble$nonVegPixels
)

parameters2 <- list(
  BiomassSpeciesData = list(
    "types" = c("KNN", "CASFRI", "Pickell", "ForestInventory"),
    "sppEquivCol" = sppEquivCol,
    "omitNonVegPixels" = TRUE,
    ".plotInitialTime" = 0
  )
)

quickPlot::dev(width = 18, height = 12)
quickPlot::clearPlot()

simOutSpeciesLayers <- Cache(simInitAndSpades,
                             times = list(start = 0, end = 1),
                             params = parameters2,
                             modules = c("BiomassSpeciesData"),
                             objects2,
                             paths = paths,
                             debug = 1)

######################################################
# Dynamic Simulation
######################################################
times <- list(start = 0, end = endTime)
modules <- list("Boreal_LBMRDataPrep", "LandR_BiomassGMOrig", "Biomass_regeneration", "LBMR",
                "LandMine",
                "LandWeb_output",
                "timeSinceFire")

speciesTable <- getSpeciesTable(dPath = Paths$inputPath) ## uses default URL
speciesTable[LandisCode == "PICE.GLA", SeedMaxDist := 2000] ## (see LandWeb#96)

if (grepl("aspen80", runName)) {
  speciesTable[LandisCode == "POPU.TRE", Longevity := 80] ## (see LandWeb#67)
}

objects <- list(
  "fireReturnInterval" = simOutPreamble$fireReturnInterval,
  "LCC2005" = simOutPreamble$LCC2005,
  "rasterToMatch" = simOutPreamble$rasterToMatch,
  "rstFlammable" = simOutPreamble$rstFlammable,
  "rstTimeSinceFire" = simOutPreamble$`CC TSF`,
  "sppEquiv" = sppEquivalencies_CA,
  "speciesLayers" = simOutSpeciesLayers$speciesLayers,
  "speciesTable" = speciesTable,
  "standAgeMap" = simOutPreamble$`CC TSF`, ## same as rstTimeSinceFire; TODO: use synonym?
  "studyArea" = simOutPreamble$studyArea,
  "studyAreaLarge" = simOutPreamble$studyArea,
  "summaryPeriod" = summaryPeriod,
  "useParallel" = 2
)

parameters <- list(
  Boreal_LBMRDataPrep = list(
    "sppEquivCol" = sppEquivCol,
    ".useCache" = eventCaching
  ),
  LandMine = list(
    "biggestPossibleFireSizeHa" = 5e5,
    "burnInitialTime" = fireTimestep,
    "fireTimestep" = fireTimestep,
    "ROStype" = if (grepl("equalROS", runName)) "equal" else if (grepl("logROS", runName)) "log" else "original",
    ".useCache" = eventCaching
  ),
  LandWeb_output = list(
    "sppEquivCol" = sppEquivCol,
    "summaryInterval" = summaryInterval,
    "vegLeadingProportion" = vegLeadingProportion,
    ".plotInitialTime" = 0
  ),
  LBMR = list(
    "seedingAlgorithm" = if (grepl("noDispersal", runName)) "noDispersal" else "wardDispersal",
    "successionTimestep" = successionTimestep,
    ".useCache" = eventCaching,
    ".useParallel" = 8 ## TODO: need
  ),
  LandR_BiomassGMOrig = list(
    ".useParallel" = 8 ## TODO: need
  ),
  # Biomass_regeneration = list(
  #
  # ),
  timeSinceFire = list(
    "startTime" = fireTimestep,
    ".useCache" = eventCaching
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

######## SimInit and Experiment
if (file.exists(file.path(Paths$outputPath, "seed.rds"))) {
  seed <- readRDS(file.path(Paths$outputPath, "seed.rds"))
} else {
  seed <- sample(1e8, 1)
  saveRDS(seed, file.path(Paths$outputPath, "seed.rds"))
}
set.seed(seed)
print(seed)

print(runName)

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
  quickPlot::dev(width = 18, height = 12)
  quickPlot::clearPlot()

  mySimOut <- simInitAndSpades(times = times, #cl = cl,
                   params = parameters,
                   modules = modules,
                   outputs = outputs,
                   objects, # do not name this argument -- collides with Cache -- leave it unnamed
                   paths = paths,
                   loadOrder = unlist(modules),
                   debug = 1
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
