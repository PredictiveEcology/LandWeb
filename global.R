source("loadPackages.R") # load & install (if not available) package dependencies
source("functions.R") # get functions used throughout this shiny app
source("shinyModules.R") # shiny modules
source("footers.R") # minor footer stuff for app

# This is for rerunning apps -- Will not do anything if not on one of named computers
reloadPreviousWorking <- FALSE#c("SMALL","50") # This can be:
     # FALSE -- standard -- just run present conditions
     # TRUE (most recent one) or 
     # character string (most recent one with that character string) or 
     # character vector (most recent one with AND search)
     # numeric -- counting backwards from 1 previous, 2 previous etc.
.reloadPreviousWorking <- reloadPreviousWorkingFn(reloadPreviousWorking)
reproducibleCache <- "reproducibleCache" # this is a separate cache ONLY used for saving snapshots of working LandWeb runs
                                         # It needs to be separate because it is an overarching one, regardless of scale

# Spatial stuff -- determines the size of the area that will be "run" in the simulations
studyArea <- "MEDIUM"  #other options: "FULL", "EXTRALARGE", "LARGE", "MEDIUM", "NWT", "SMALL" 

## paths
paths <- list(
  cachePath = paste0("appCache", studyArea),
  modulePath = "m", # short name because shinyapps.io can't handle longer than 100 characters
  inputPath = "inputs",
  outputPath = paste0("outputs", studyArea)
)


# App - variables 
  appStartTime <- st <- Sys.time() - 1
  message("Started at ", appStartTime)
  rsyncToAWS <- FALSE
  useGdal2Tiles <- TRUE
  # leaflet parameters
  leafletZoomInit = 5 
  # Some shinycssloaders options
  options("spinner.type" = 5)
  options(gdalUtils_gdalPath = Cache(gdalSet, cacheRepo = paths$cachePath))
  #options(spinner.color="blue")  

## spades module variables
  eventCaching <- "init" 
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
  endTime <- 300
  summaryInterval <- 10
  summaryPeriod <- c(100, endTime)

### Package stuff that should not be run automatically
if (FALSE) {
  SpaDESDeps <- miniCRAN::pkgDep("SpaDES.core")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
  if (length(new.packages)) install.packages(new.packages)
  
}

# Import and build 2 polygons -- one for whole study area, one for demonstration area
  source("inputMaps.R") # source some functions
  loadLandisParams(path = paths$inputPath, envir = .GlobalEnv) # assigns 2 Landis objects to .GlobalEnv
  shpStudyRegions <- Cache(loadStudyRegion, asPath(file.path(paths$inputPath,"shpLandWEB.shp")), 
                           studyArea = studyArea,
                           crsKNNMaps=crsKNNMaps, cacheRepo=paths$cachePath)
  list2env(shpStudyRegions, envir = environment())


# simInit objects
  modules <- list("landWebDataPrep", "initBaseMaps", "fireDataPrep", "LandMine",
                  "LW_LBMRDataPrep", "LBMR", "timeSinceFire", "LandWebOutput")
  times <- list(start = 0, end = endTime)
  objects <- list("shpStudyRegionFull" = shpStudyRegionFull,
                  "shpStudySubRegion" = shpStudyRegion,
                  "successionTimestep" = successionTimestep,
                  "summaryPeriod" = summaryPeriod,
                  "useParallel" = FALSE)
  parameters <- list(LandWebOutput = list(summaryInterval = summaryInterval,
                                          .useCache = eventCaching),
                     LW_LBMRDataPrep = list(.useCache = eventCaching),
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
                             times = NROW(outputs)/length(objectNamesToSave)))
  outputs <- as.data.frame(rbindlist(list(outputs, outputs2), fill = TRUE))
  
# Main simInit function call -- loads all data
mySim <<- simInit(times = times, params = parameters, modules = modules, 
                  objects = objects, paths = paths, outputs = outputs, loadOrder = unlist(modules))

# This needs simInit call to be run alread
source("mapsForShiny.R") # a few map details for shiny app
