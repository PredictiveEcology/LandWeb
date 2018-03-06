reproducible::Require(googleAuthR)
reproducible::Require(googledrive)
reproducible::Require(googleID)

options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/drive.readonly",
                                        "https://www.googleapis.com/auth/userinfo.email",
                                        "https://www.googleapis.com/auth/userinfo.profile"))
options(googleAuthR.webapp.client_id = "869088473060-a7o2bc7oit2vn11gj3ieh128eh8orb04.apps.googleusercontent.com")
options(googleAuthR.webapp.client_secret = "FR-4jL12j_ynAtsl-1Yk_cEL")

appURL <- "http://landweb.predictiveecology.org/Demo/"
authFile <- "https://drive.google.com/file/d/1sJoZajgHtsrOTNOE3LL8MtnTASzY0mo7/view?usp=sharing"

# List modules first, so we can get all their dependencies
modules <- list("landWebDataPrep", "initBaseMaps", "fireDataPrep", "LandMine",
                "Boreal_LBMRDataPrep", "LBMR", "timeSinceFire", "LandWebOutput")#, "makeLeafletTiles")
# Spatial stuff -- determines the size of the area that will be "run" in the simulations
studyArea <- "RIA"  #other options: "FULL", "EXTRALARGE", "LARGE", "MEDIUM", "NWT", "SMALL" , "RIA"
studyArea <- "VERYSMALL"  #other options: "FULL", "EXTRALARGE", "LARGE", "MEDIUM", "NWT", "SMALL" , "RIA", "VERYSMALL"

## paths -- NOTE: these are the 'default' paths for app setup;
##                however, in-app, the paths need to be set as reactive values for authentication!
paths <- list(
  cachePath = ifelse(paste0("appCache", studyArea)),
  modulePath = "m", # short name because shinyapps.io can't handle longer than 100 characters
  inputPath = "inputs",
  outputPath = paste0("outputs", studyArea)
)
do.call(SpaDES.core::setPaths, paths) # Set them here so that we don't have to specify at each call to Cache

# This is a separate cache ONLY used for saving snapshots of working LandWeb runs
# It needs to be separate because it is an overarching one, regardless of scale
reproducibleCache <- "reproducibleCache"

source("loadPackages.R") # load & install (if not available) package dependencies, with specific versioning
source("functions.R") # get functions used throughout this shiny app

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
  leafletZoomInit = 5
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
  if (!exists("globalRasters")) globalRasters <- list()

  # Computation stuff
  experimentReps <- 1 # Currently, only using 1 -- more than 1 may not work

  # Time steps
  fireTimestep <- 1
  successionTimestep <- 10 # was 2

  # Overall model times # start is default at 0
  endTime <- 20
  summaryInterval <- 10
  summaryPeriod <- c(10, endTime)

# Import and build 2 polygons -- one for whole study area, one for demonstration area
  # "shpStudyRegion"     "shpStudyRegionFull"
  source("inputMaps.R") # source some functions
  loadLandisParams(path = paths$inputPath, envir = .GlobalEnv) # assigns 2 Landis objects to .GlobalEnv
  if (studyArea == "RIA") {
    shpStudyRegion <- Cache(shapefile, file.path(paths$inputPath, "RIA_SE_ResourceDistricts_Clip.shp"))
    loadAndBuffer <- function(shapefile) {
      a <- shapefile(shapefile)
      b <- buffer(a, 0, dissolve = FALSE)
      SpatialPolygonsDataFrame(b, data = as.data.frame(a))
    }
    fireReturnIntervalTemp <- 400
    shpStudyRegion[["LTHRC"]] <- fireReturnIntervalTemp # Fire return interval
    shpStudyRegion[["fireReturnInterval"]] <- shpStudyRegion$LTHRC # Fire return interval

    shpStudyRegionFull <- Cache(loadAndBuffer, file.path(paths$inputPath, "RIA_StudyArea.shp"),
                                cacheRepo = paths$cachePath)
    shpStudyRegionFull[["LTHRC"]] <- fireReturnIntervalTemp # Fire return interval
    shpStudyRegionFull$fireReturnInterval <- shpStudyRegionFull$LTHRC
    #shpStudyRegion <- shpStudyRegion[1,]
    shpStudyRegionFull <- shpStudyRegion

  } else {
    shpStudyRegions <- Cache(loadStudyRegion,
                             asPath(file.path(paths$inputPath, "shpLandWEB.shp")),
                             studyArea = studyArea,
                             crsKNNMaps = crsKNNMaps, cacheRepo = paths$cachePath)
    list2env(shpStudyRegions, envir = environment())
  }

# simInit objects
  times <- list(start = 0, end = endTime)
  .quickChecking <- TRUE
  objects <- list("shpStudyRegionFull" = shpStudyRegionFull,
                  "shpStudySubRegion" = shpStudyRegion,
                  "successionTimestep" = successionTimestep,
                  "summaryPeriod" = summaryPeriod,
                  "useParallel" = FALSE)
  parameters <- list(LandWebOutput = list(summaryInterval = summaryInterval,
                                          .useCache = eventCaching),
                     landWebDataPrep = list(.useCache = eventCaching),
                     Boreal_LBMRDataPrep = list(.useCache = eventCaching),
                     LandMine = list(biggestPossibleFireSizeHa = 5e5, fireTimestep = fireTimestep,
                                     burnInitialTime = fireTimestep,
                                     .plotInitialTime = NA
                                     , .useCache = eventCaching
                     ),
                     LBMR = list(.plotInitialTime = times$start,
                                 .saveInitialTime = NA
                                 , .useCache = eventCaching
                     ),
                     initBaseMaps = list(.useCache = eventCaching),
                     timeSinceFire = list(startTime = fireTimestep,
                                          .useCache = eventCaching),
                     fireDataPrep = list(.useCache = eventCaching))
  objectNamesToSave <- c("rstTimeSinceFire", "vegTypeMap")
  outputs <- data.frame(stringsAsFactors = FALSE,
                        expand.grid(
                          objectName = objectNamesToSave,#, "oldBigPatch"),
                          saveTime = seq(objects$summaryPeriod[1], objects$summaryPeriod[2],
                                         by = parameters$LandWebOutput$summaryInterval)),
                        fun = "writeRaster", package = "raster",
                        file = paste0(objectNamesToSave, c(".tif", ".grd")))
  outputs2 <- data.frame(stringsAsFactors = FALSE,
                         expand.grid(
                           objectName = c("simulationOutput"),
                           saveTime = times$end), fun = "saveRDS", package = "base" )
  outputs$arguments <- I(rep(list(list(overwrite = TRUE, progress = FALSE, datatype = "INT2U", format = "GTiff"),
                                  list(overwrite = TRUE, progress = FALSE, datatype = "INT1U", format = "raster")),
                             times = NROW(outputs) / length(objectNamesToSave)))
  outputs <- as.data.frame(data.table::rbindlist(list(outputs, outputs2), fill = TRUE))

# i = i + 1; a[[i]] <- .robustDigest(mySim); b[[i]] <- mySim
# This needs simInit call to be run already
# a few map details for shiny app
source("mapsForShiny.R")

# Run Experiment
source("runExperiment.R")

seed <- sample(1e8, 1)
set.seed(seed)
message("Current seed is: ", seed)

objectsToHash <- grep("useParallel", ls(mySim@.envir, all.names = TRUE), value = TRUE, invert = TRUE)

# THIS IS THE MAIN "SIMULATION FUNCTION"
# THE FOLLOWING OBJECT IS A LIST OF 1 simList,
# A simList is a rich data structure that comes with the SpaDES.core package
mySimOut <<- Cache(runExperiment, mySim, experimentReps,
                   debugCache = "complete",
                   objects = objectsToHash)#,
#sideEffect = TRUE)

message("  Finished Experiment")

message("  Identify which files were created during simulation")
# outputs() function reports on any files that were created during the simulation
filesFromOutputs <- lapply(seq_along(mySimOut), function(x) {
  outputs(mySimOut[[x]])$file
})

for (simNum in seq_along(mySimOut)) {
  mySimOut[[simNum]]@outputs$file <- lapply(
    strsplit(outputs(mySimOut[[simNum]])$file,
             split = paste0(outputPath(mySimOut[[simNum]]),"[\\/]+")),
    function(f) {
      f[[2]]
    }) %>%
    unlist() %>%
    file.path(paths$outputPath, .)
}

message("  Load rasters from disk, reproject them to leaflet projection")
rastersFromOutputs <- lapply(seq_along(mySimOut), function(x) {
  grep(pattern = ".grd$|.tif$", outputs(mySimOut[[x]])$file, value = TRUE)
}) %>% unlist()

# Look for all files named rstTimeSinceFire -- these are several rasters each with a filename
#   that represents the simulation "time" when it was created, e.g., 10, 20, 30 years
tsf <- grep(pattern = "rstTimeSinceFire", rastersFromOutputs, value = TRUE)
# These are several rasters indicating vegetation type, again each one coming from a specific
#   simulation time ... a time series of rasters...
vtm <- grep(pattern = "vegTypeMap", rastersFromOutputs, value = TRUE)
lenTSF <- length(tsf)
rasterResolution <<- raster(tsf[1]) %>% res()

if (FALSE) {
  message("  Identify which files were created during simulation")
  # outputs() function reports on any files that were created during the simulation
  filesFromOutputs <- lapply(seq_along(mySimOut), function(x) {
    outputs(mySimOut[[x]])$file
  })

  for (simNum in seq_along(mySimOut)) {
    mySimOut[[simNum]]@outputs$file <- lapply(
      strsplit(outputs(mySimOut[[simNum]])$file,
               split = paste0(outputPath(mySimOut[[simNum]]),"[\\/]+")),
      function(f) {
        f[[2]]
      }) %>%
      unlist() %>%
      file.path(paths$outputPath, .)
  }

  message("  Load rasters from disk, reproject them to leaflet projection")
  rastersFromOutputs <- lapply(seq_along(mySimOut), function(x) {
    grep(pattern = ".grd$|.tif$", outputs(mySimOut[[x]])$file, value = TRUE)
  }) %>% unlist()

  # Look for all files named rstTimeSinceFire -- these are several rasters each with a filename
  #   that represents the simulation "time" when it was created, e.g., 10, 20, 30 years
  tsf <- grep(pattern = "rstTimeSinceFire", rastersFromOutputs, value = TRUE)
  # These are several rasters indicating vegetation type, again each one coming from a specific
  #   simulation time ... a time series of rasters...
  vtm <- grep(pattern = "vegTypeMap", rastersFromOutputs, value = TRUE)
  lenTSF <- length(tsf)
  rasterResolution <<- raster(tsf[1]) %>% res()
}

## WORKAROUND: was part of the previous if(FALSE) block, but we need this
if (TRUE) {
  lfltFN <- gsub(tsf, pattern = ".grd$|.tif$", replacement = "LFLT.tif")

  globalRasters <<- Cache(reprojectRasts, lapply(tsf, asPath), digestPathContent = .quickCheck,
                          lfltFN, sp::CRS(lflt), end(mySim), cacheRepo = paths$cachePath,
                          flammableFile = asPath(file.path(paths$outputPath, "rstFlammable.grd")))
}

message("  Determine leading species by age class, by polygon (loading 2 rasters, summarize by polygon)")
args <- list(leadingByStage, tsf, vtm,
             polygonToSummarizeBy = ecodistricts,
             cl = if (exists("cl")) cl,
             omitArgs = "cl", digestPathContent = .quickCheck,
             ageClasses = ageClasses, cacheRepo = paths$cachePath)
args <- args[!unlist(lapply(args, is.null))]
leading <- do.call(Cache, args)
rm(args)


message("  Determine number of large patches, by polygon (loading 2 rasters, summarize by polygon)")
# Large patches
polygonsWithData <- leading[, unique(polygonNum[!is.na(proportion)]), by = ageClass]
vegLeadingTypes <- c(unique(leading$vegType))
vegLeadingTypesWithAllSpecies <- c(vegLeadingTypes, "All species")

source("shiny-modules/inputTables.R")

clumpMod2Args <- list(
  currentPolygon = polygons[[1 + length(polygons)/4]],
  tsf = tsf, vtm = vtm,
  cl = if (exists("cl")) cl,
  ageClasses = ageClasses, cacheRepo = paths$cachePath,
  largePatchesFn = largePatchesFn, countNumPatches = countNumPatches)
clumpMod2Args <- clumpMod2Args[!unlist(lapply(clumpMod2Args, is.null))]

message("  Finished global.R")
