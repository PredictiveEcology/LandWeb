emptyList <- list(Free = NULL)
emptyList <- list(Proprietary = NULL)
# Packages for global.R -- don't need to load packages for modules -- happens automatically
SpaDESPkgs <- c(
  "PredictiveEcology/SpaDES.core@development",
  "PredictiveEcology/SpaDES.tools@development",
  #"PredictiveEcology/SpaDES.shiny@develop",
  "PredictiveEcology/SpaDES.shiny@generalize-modules",
  "raster"
)
shinyPkgs <- c("leaflet", "gdalUtils", "rgeos", "raster", "parallel",
               "shiny", "shinydashboard", "shinyBS", "shinyjs", "shinycssloaders")
googleAuthPkgs <- c("googleAuthR", "googledrive", "googleID")
moduleRqdPkgs <- c("data.table", "dplyr", "fasterize", "fpCompare",
                   "gdalUtils", "ggplot2", "grDevices", "grid", "magrittr", "PredictiveEcology/quickPlot@development",
                   "PredictiveEcology/SpaDES.tools@development", "PredictiveEcology/SpaDES.tools@prepInputs",
                   "purrr", "R.utils", "raster", "RColorBrewer", "Rcpp", "reproducible",
                   "rgeos", "scales", "sp", "SpaDES.core", "SpaDES.tools", "tidyr",
                   "VGAM")

reproducible::Require(unique(c(
  SpaDESPkgs,
  shinyPkgs,
  googleAuthPkgs,
  if (Sys.info()["sysname"] != "Windows") "Cairo",
  # `snow` required internally by `parallel` for Windows SOCK clusters
  if (Sys.info()["sysname"] == "Windows") "snow",
  moduleRqdPkgs
  # shiny app
)))

# Options
options(reproducible.verbose = FALSE)
options(reproducible.useMemoise = TRUE)
options(spades.browserOnError = FALSE)

# Google Authentication setup
options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/drive.readonly",
                                        "https://www.googleapis.com/auth/userinfo.email",
                                        "https://www.googleapis.com/auth/userinfo.profile"))
options(googleAuthR.webapp.client_id = "869088473060-a7o2bc7oit2vn11gj3ieh128eh8orb04.apps.googleusercontent.com")
options(googleAuthR.webapp.client_secret = "FR-4jL12j_ynAtsl-1Yk_cEL")
options(httr_oob_default = TRUE)

appURL <- "http://landweb.predictiveecology.org/Demo/"
authFile <- "https://drive.google.com/file/d/1sJoZajgHtsrOTNOE3LL8MtnTASzY0mo7/view?usp=sharing"

# Spatial stuff -- determines the size of the area that will be "run" in the simulations
subStudyRegionName <- "SMALL"  #other options: "FULL", "EXTRALARGE", "LARGE", "MEDIUM", "NWT", "SMALL" , "RIA"
#subStudyRegionName <- c("AB")  #other options: "BC", "AB", "SK", "MB" or combinations, please specify in West-East order

## paths -- NOTE: these are the 'default' paths for app setup;
##                however, in-app, the paths need to be set as reactive values for authentication!
subStudyRegionNameCollapsed <- paste(subStudyRegionName, collapse = "_")
paths <- list(
  cachePath = file.path("cache", paste0(subStudyRegionNameCollapsed)),
  modulePath = "m", # short name because shinyapps.io can't handle longer than 100 characters
  inputPath = "inputs",
  outputPath = file.path("outputs", paste0(subStudyRegionNameCollapsed))
)
do.call(SpaDES.core::setPaths, paths) # Set them here so that we don't have to specify at each call to Cache

# This is a separate cache ONLY used for saving snapshots of working LandWeb runs
# It needs to be separate because it is an overarching one, regardless of scale
reproducibleCache <- "reproducibleCache"

if (any(c("achubaty", "emcintir") %in% Sys.info()["user"])) {
  opts <- options("spades.moduleCodeChecks" = FALSE, "reproducible.quick" = TRUE)
}

## get additonal helper functions used throughout this shiny app
source("functions.R")
source("largePatchesFn.R")

# This is for rerunning apps -- Will not do anything if not on one of named computers
reloadPreviousWorking <- FALSE#c("SMALL","50") # This can be:
# FALSE -- standard -- just run present conditions
# TRUE (most recent one) or
# character string (most recent one with that character string) or
# character vector (most recent one with AND search)
# numeric -- counting backwards from 1 previous, 2 previous etc.
.reloadPreviousWorking <- reloadPreviousWorkingFn(reloadPreviousWorking)

# App - variables
appStartTime <- st <- Sys.time() - 1
message("Started at ", appStartTime)
rsyncToAWS <- FALSE
useGdal2Tiles <- TRUE

# leaflet parameters
leafletZoomInit <- 5

# Some shinycssloaders options
options("spinner.type" = 5)

# This will search for gdal utilities. If it finds nothing, and you are on Windows,
#   you should install the GDAL that comes with QGIS -- use OSGeo4W Network Installer 64 bit
#   may be still here: http://www.qgis.org/en/site/forusers/download.html
options(gdalUtils_gdalPath = Cache(gdalSet, cacheRepo = paths$cachePath))
#options(spinner.color="blue")

## spades module variables
eventCaching <- c(".inputObjects", "init")
maxAge <- 400
ageClasses <- c("Young", "Immature", "Mature", "Old")
ageClassCutOffs <- c(0, 40, 80, 120)
ageClassZones <- lapply(seq_along(ageClassCutOffs), function(x) {
  if (x < length(ageClassCutOffs)) {
    paste0(ageClassCutOffs[x], "-", ageClassCutOffs[x + 1])
  } else {
    paste0(">", ageClassCutOffs[x])
  }
})

# Time steps
fireTimestep <- 1
successionTimestep <- 10 # was 2

# Overall model times # start is default at 0
endTime <- 4
summaryInterval <- 2
summaryPeriod <- c(2, endTime)

# Import and build 2 polygons -- one for whole study area, one for demonstration area
# "shpStudyRegion"     "shpStudyRegion"
source("inputMaps.R") # source some functions

# LANDIS-II params that are used
landisInputs <- readRDS(file.path(paths$inputPath, "landisInputs.rds"))
spEcoReg <- readRDS(file.path(paths$inputPath, "SpEcoReg.rds"))

# The CRS for the Study -- spTransform converts this first one to the second one, they are identical geographically
# crsStudyRegion <- CRS(paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0",
#                         "+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
crsStudyRegion <- CRS(paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0",
                            "+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

studyRegionFilePath <- {
  studyRegionFilename <- if ("RIA" %in% subStudyRegionName) {
    "RIA_SE_ResourceDistricts_Clip.shp"
  } else {
    "studyarea-correct.shp"
  }
  file.path(paths$inputPath, studyRegionFilename)
}

studyRegionsShps <- Cache(loadStudyRegions,
                          asPath(studyRegionFilePath),
                          fireReturnIntervalMap = asPath(file.path(paths$inputPath, "ltfcmap correct.shp")),
                          subStudyRegionName = subStudyRegionName,
                          crsStudyRegion = crsStudyRegion, cacheRepo = paths$cachePath)
list2env(studyRegionsShps, envir = environment()) # shpStudyRegion & shpStudyRegion

## source additional shiny modules
vapply(list.files("shiny-modules", "[.]R", full.names = TRUE), source, vector("list", 2))

# This needs simInit call to be run already
# a few map details for shiny app
message("Preparing polygon maps for reporting histograms")
labelColumn <- "shinyLabel"

source("colorPaletteForShiny.R")
labelColumn <- "shinyLabel"

########################################################
########################################################
# formerly in mapsForShiny.R
# Reporting polygons
message("Loading Reporting Polygons")
reportingPolygons <- list()
reportingPolygons$Free <- Cache(createReportingPolygons,
                                c("Alberta Ecozones", "National Ecozones", "National Ecodistricts"),
                                shpStudyRegion = shpStudyRegion,
                                shpSubStudyRegion = sSubSRXYXY)


tmpProprietary <- Cache(createReportingPolygons,
                        c("Forest Management Areas", "Alberta FMUs", "Caribou Herds"),
                        shpStudyRegion = shpStudyRegion,
                        shpSubStudyRegion = sSubSRXYXY)
reportingPolygons$Proprietary <- reportingPolygons$Free
reportingPolygons$Proprietary[names(tmpProprietary)] <- tmpProprietary
rm(tmpProprietary)


### CURRENT CONDITION ##################################
message("Loading Current Condition Rasters")
dPath <- file.path(paths$inputPath, "CurrentCondition")
CCspeciesNames <- c("Pine", "Age", "BlackSpruce", "Deciduous", "Fir", "LandType", "WhiteSpruce")
rstCurrentConditionList <- Cache(loadCCSpecies, CCspeciesNames,
                                 url = "https://drive.google.com/open?id=1JnKeXrw0U9LmrZpixCDooIm62qiv4_G1",
                                 dPath = dPath)

#############################

##### SERVER FILE.R
# THIS IS DANGEROUS, BUT NECESSARY FOR GUARANTEED RUNNING --
#    THIS MEANS that any values of objects will be OK and will trigger a cached return
#    Only shpStudySubRegion and non-object arguments to simInit will make a new run
guaranteedRun <- ifelse(any(c("emcintir") %in% Sys.info()["user"]), TRUE, FALSE)
guaranteedRun <- FALSE

experimentReps <- emptyList
experimentReps <- lapply(experimentReps, function(x) 1)

# simInit objects
times4sim <- emptyList
times4sim <- lapply(times4sim, function(x) list(start = 0, end = endTime))

modules4sim <- emptyList
modules4sim$Free <- list("landWebDataPrep", "initBaseMaps", "fireDataPrep", "LandMine",
                         "Boreal_LBMRDataPrep", "LBMR", "timeSinceFire", "LandWebOutput")
modules4sim$Proprietary <- c(modules4sim$Free[1:4], "landWebProprietaryData", 
                             modules4sim$Free[5:length(modules4sim$Free)])
modules4sim <- modules4sim[names(emptyList)]

objects4sim <- emptyList
objects4sim <- lapply(objects4sim, function(x) 
  list("shpStudyRegionFull" = shpStudyRegion,
       "shpStudySubRegion" = sSubSRXYXY,
       "summaryPeriod" = summaryPeriod,
       "useParallel" = 2) 
)


parameters4sim <- emptyList
parameters4sim <- lapply(parameters4sim, function(x) {
  list(
    LandWebOutput = list(summaryInterval = summaryInterval),
    landWebDataPrep = list(.useCache = eventCaching),
    landWebProprietaryData = list(.useCache = eventCaching),
    Boreal_LBMRDataPrep = list(.useCache = eventCaching),
    LandMine = list(biggestPossibleFireSizeHa = 5e5,
                    fireTimestep = fireTimestep,
                    burnInitialTime = fireTimestep,
                    .plotInitialTime = NA,
                    .useCache = eventCaching),
    LBMR = list(successionTimestep = successionTimestep,
                .plotInitialTime = times4sim$start,
                .saveInitialTime = NA,
                .useCache = eventCaching),
    initBaseMaps = list(.useCache = eventCaching),
    timeSinceFire = list(startTime = fireTimestep,
                         .useCache = eventCaching),
    fireDataPrep = list(.useCache = eventCaching)
  )
})


outputs4simFn <- function(objects4sim, parameters4sim, times4sim, 
                          objectNamesToSave) {
  outputs <- data.frame(stringsAsFactors = FALSE,
                        expand.grid(
                          objectName = objectNamesToSave,#, "oldBigPatch"),
                          saveTime = seq(objects4sim$summaryPeriod[1], objects4sim$summaryPeriod[2],
                                         by = parameters4sim$LandWebOutput$summaryInterval)),
                        fun = "writeRaster", package = "raster",
                        file = paste0(objectNamesToSave, c(".tif", ".grd")))
  
  outputs2 <- data.frame(stringsAsFactors = FALSE,
                         expand.grid(objectName = c("simulationOutput"), saveTime = times4sim$end),
                         fun = "saveRDS",
                         package = "base")
  
  outputs$arguments <- I(rep(list(list(overwrite = TRUE, progress = FALSE, datatype = "INT2U", format = "GTiff"),
                                  list(overwrite = TRUE, progress = FALSE, datatype = "INT1U", format = "raster")),
                             times = NROW(outputs) / length(objectNamesToSave)))
  
  outputs3 <- data.frame(stringsAsFactors = FALSE,
                         objectName = "rstFlammable",
                         saveTime = times4sim$end, fun = "writeRaster", package = "raster",
                         arguments = I(list(list(overwrite = TRUE, progress = FALSE,
                                                 datatype = "INT2U", format = "raster"))))
  
  as.data.frame(data.table::rbindlist(list(outputs, outputs2, outputs3), fill = TRUE))
}
objectNamesToSave <- emptyList
objectNamesToSave <- lapply(objectNamesToSave, function(x) {
  c("rstTimeSinceFire", "vegTypeMap")
})

outputs4sim <- mapply(objects4sim = objects4sim,
                      parameters4sim = parameters4sim,
                      times4sim = times4sim,
                      objectNamesToSave = objectNamesToSave,
                      outputs4simFn, SIMPLIFY = FALSE
)

## paths for sim
pathFn <- function(pathType, basename, suffix) {
  file.path(pathType, paste0(basename, "_", suffix))
}

cPaths <- emptyList
cPaths <- mapply(pathFn, suffix = names(cPaths),  
                 MoreArgs = list(basename = subStudyRegionName, pathType = "cache"), 
                 SIMPLIFY = FALSE)

oPaths <- emptyList
oPaths <- mapply(pathFn, suffix = names(oPaths),  
                 MoreArgs = list(basename = subStudyRegionName, pathType = "outputs"), 
                 SIMPLIFY = FALSE)

paths4sim <- emptyList
paths4sim <- mapply(cPath = cPaths, oPath = oPaths,  
                    function(cPath, oPath) {
                      list(
                        cachePath = cPath,
                        modulePath = "m",
                        inputPath = "inputs",
                        outputPath = oPath
                      )
                    }, SIMPLIFY = FALSE)

seed <- sample(1e8, 1)

######## SimInit


mySims <- emptyList
#numCl <- min(length(emptyList), parallel::detectCores()/2)
#cl <- parallel::makeForkCluster(numCl, 
#                                outfile = paste0("outputs/log_cluster", 
#                                                 paste(sample(LETTERS,5), collapse = ""),
#                                                 ".txt"))
# parallel::clusterMap
mySims <- mapply(simInit, times = times4sim, params = parameters4sim, 
                 modules = modules4sim, 
                 outputs = outputs4sim, 
                 objects = objects4sim, 
                 paths = paths4sim, loadOrder = lapply(modules4sim, unlist))


##### PRE experiment
debugCache <- "complete"

objectsToHash <- emptyList
objectsToHash <- mapply(mySim = mySims, objectsToHash = objectsToHash, 
                        MoreArgs = list(guaranteedRun = guaranteedRun),
                        FUN = function(mySim, objectsToHash, guaranteedRun) {
                          if (guaranteedRun) {
                            "shpStudySubRegion" # basically only cache on non-.envir objects plus study area
                          } else {
                            grep("useParallel", ls(mySim@.envir, all.names = TRUE), value = TRUE, invert = TRUE)
                          }
                          
                        }, SIMPLIFY = FALSE
) 

#########################################
# run the simulation experiment
runExperiment <- function(sim, nReps, objectsToHash = "") {
  args <- list(experiment, sim, replicates = nReps,
               objects = objectsToHash,
               debug = "paste(Sys.time(), format(Sys.time() - appStartTime, digits = 2),
               paste(unname(current(sim)), collapse = ' '))",
               .plotInitialTime = NA,
               clearSimEnv = TRUE,
               omitArgs = c("debug", ".plotInitialTime"))
  args <- args[!unlist(lapply(args, is.null))]
  simOut <- do.call(Cache, args)
  message(attr(simOut, "tags"))
  
  for (simNum in seq_along(simOut)) {
    simOut[[simNum]]@outputs$file <- lapply(
      strsplit(outputs(simOut[[simNum]])$file,
               split = paste0(outputPath(simOut[[simNum]]), "[\\/]+")),
      function(f) {
        f[[2]]
      }) %>%
      unlist() %>%
      file.path(outputPath(simOut[[simNum]]), .)
  }
  simOut
}

message("  Starting Experiment...")

set.seed(seed)
message("    current seed is: ", seed)

mySimOuts <- emptyList
# parallel::clusterMap, cl = cl, 
mySimOuts <- Cache(mapply, runExperiment, sim = mySims, 
                   nReps = experimentReps, objectsToHash = objectsToHash)
message("  Finished Experiment.")
##### POST Experiment

rastersFromOutputs <- emptyList
rastersFromOutputs <- lapply(mySimOuts, function(mySimOut) {
  lapply(seq_along(mySimOut), function(x) {
    grep(pattern = ".grd$|.tif$", outputs(mySimOut[[x]])$file, value = TRUE)
  }) %>% unlist()
})

extractFilepaths <- function(filename, rastersFromOutput) {
  grep(pattern = filename, rastersFromOutput, value = TRUE)
}
tsfs <- lapply(rastersFromOutputs, function(rastersFromOutput) {
  asPath(extractFilepaths("rstTimeSinceFire", rastersFromOutput))
})

vtms <- lapply(rastersFromOutputs, function(rastersFromOutput) {
  asPath(extractFilepaths("vegTypeMap", rastersFromOutput))
})

tsfLFLTFilenames <- lapply(tsfs, function(tsf) SpaDES.core::.suffix(tsf, "LFLT") )
vtmLFLTFilenames <- lapply(vtms, function(vtm) SpaDES.core::.suffix(vtm, "LFLT") )

rasterResolutions <- lapply(tsfs, function(x) raster(x[1]) %>% res(.))

flammableFiles <- lapply(mySimOuts, function(mySimOut) {
  asPath(file.path(outputPath(mySimOut[[1]]), "rstFlammable.grd"))
})

#tsfRasters <- emptyList
tsfRasters <- Cache(parallel::clusterMap, cl = cl, tsf = tsfs, 
                    lfltFN = tsfLFLTFilenames, flammableFile = flammableFiles,
                    reprojectRasts, MoreArgs = list(crs = sp::CRS(SpaDES.shiny::proj4stringLFLT)),
                    SIMPLIFY = FALSE)

tsfRasterTilePaths <- Cache(mapply, rst = tsfRasters, modelType = names(tsfRasters), 
       MoreArgs = list(zoomRange = 1:10, colorTableFile = asPath(colorTableFile)),
       function(rst, modelType, zoomRange, colorTableFile) {
         outputPath <- file.path("www", modelType, subStudyRegionNameCollapsed)
         filenames <- gdal2Tiles(rst, outputPath = outputPath, 
                      zoomRange = zoomRange, colorTableFile = colorTableFile)  
         return(filenames)
       }, SIMPLIFY = FALSE)



if (FALSE) { # This is to have vegetation type maps -- TODO: they are .grd, need to be .tif & color table
  vtmRasters <- Cache(parallel::clusterMap, cl = cl, tsf = vtms, 
                      lfltFN = vtmLFLTFilenames, flammableFile = flammableFiles,
                      reprojectRasts, MoreArgs = list(crs = sp::CRS(SpaDES.shiny::proj4stringLFLT)),
                      SIMPLIFY = FALSE)
  vtmRasterTilePaths <- mapply(rst = vtmRasters, modelType = names(vtmRasters), 
                               MoreArgs = list(zoomRange = 1:10, colorTableFile = asPath(colorTableFile)),
                               function(rst, modelType, zoomRange, colorTableFile) {
                                 outputPath <- file.path("www", modelType, subStudyRegionNameCollapsed, "map-tiles")
                                 filenames <- gdal2Tiles(rst, outputPath = outputPath, 
                                                         zoomRange = zoomRange, colorTableFile = colorTableFile)  
                                 return(filenames)
                               })
}

# leading <- function(tsf, vtm, poly, cl = NULL) {
#   
#   args <- list(leadingByStage, tsf, vtm,
#                polygonToSummarizeBy = poly, 
#                cl = TRUE,
#                omitArgs = "cl",
#                ageClasses = ageClasses)
#   args <- args[!unlist(lapply(args, is.null))]
#   out <- do.call(Cache, args)
#   rm(args)
#   out
# }

leadingMultiPolygons <- function(reportingPolygon, tsf, vtm, cl, ageClasses, ageClassCutOffs) {
  reportingPolysWOStudyArea <- reportingPolygon[-which(names(reportingPolygon)=="LandWeb Study Area")]
  lapply(lapply(reportingPolysWOStudyArea, function(p) p$crsSR), function(poly) {
    message("  Determine leading species by age class, by polygon (loading 2 rasters, summarize by polygon)")
    Cache(leadingByStage, timeSinceFireFiles = asPath(tsf), 
          vegTypeMapFiles = asPath(vtm), 
          polygonToSummarizeBy = poly$shpSubStudyRegion,
          cl = TRUE, omitArgs = "cl", ageClasses = ageClasses, ageClassCutOffs = ageClassCutOffs)
  })
}


leading <- Cache(mapply, #cl = cl, 
  reportingPolygon = reportingPolygons, tsf = tsfs, vtm = vtms, 
  MoreArgs = list(cl = TRUE, ageClasses = ageClasses, ageClassCutOffs = ageClassCutOffs),
  leadingMultiPolygons)

#########################

if (FALSE) {
  polygonsWithData <- lapply(leading, function(polyWData) {
    lapply(polyWData, function(dt) {
      dt[, unique(polygonNum[!is.na(proportion)]), by = ageClass]
    })
  })
  
  #  vegLeadingTypes NEEDED?
  
  vegLeadingTypesWithAllSpecies <- lapply(leading, function(polyWData) {
    lapply(polyWData, function(dt) {
      c(unique(dt$vegType), "All species")
    })
  })
}
#############################

