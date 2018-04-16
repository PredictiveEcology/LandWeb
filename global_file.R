## LandWeb app information
appInfo <- list(
  name = "LandWeb",
  version = numeric_version("1.0.0"),
  authors = c(
    person("Alex M", "Chubaty", email = "alex.chubaty@gmail.com", role = "aut"),
    person("Eliot J B", "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre"))
  ),
  copyright = paste(icon("copyright"), format(Sys.time(), "%Y"),
                    "Her Majesty the Queen in Right of Canada,",
                    "as represented by the Minister of Natural Resources Canada.")
)

# Packages for global.R -- don't need to load packages for modules -- happens automatically
packageLoadStartTime <- Sys.time()
SpaDESPkgs <- c(
  "PredictiveEcology/SpaDES.core@development",
  "PredictiveEcology/SpaDES.tools@development",
  "PredictiveEcology/SpaDES.shiny@generalize-modules",
  "raster"
)
shinyPkgs <- c("leaflet", "gdalUtils", "rgeos", "raster", "parallel", "shinyWidgets",
               "shiny", "shinydashboard", "shinyBS", "shinyjs", "shinycssloaders")
googleAuthPkgs <- c("googleAuthR", "googledrive", "googleID")
moduleRqdPkgs <- c("data.table", "dplyr", "fasterize", "fpCompare",
                   "gdalUtils", "ggplot2", "grDevices", "grid", "magrittr",
                   "PredictiveEcology/quickPlot@development",
                   "PredictiveEcology/SpaDES.tools@development",
                   "PredictiveEcology/SpaDES.tools@prepInputs",
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
)))
packageLoadEndTime <- Sys.time()

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
  opts <- options("spades.moduleCodeChecks" = FALSE, "reproducible.quick" = FALSE)
}

## get additonal helper functions used throughout this shiny app
source("functions.R")

# This is for rerunning apps -- Will not do anything if not on one of named computers

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
vegLeadingPercent <- 0.8 # indicates what proportion the stand must be in one species group for it to be leading.
# If all are below this, then it is a "mixed" stand
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

# Import and build 2 polygons -- one for whole study area, one for demonstration area
# "shpStudyRegion"     "shpStudyRegion"
source("inputMaps.R") # source some functions

# These are used in inputTables.R for filling the tables of parameters in
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

studyRegionsShps <- Cache(loadStudyRegions, shpStudyRegionCreateFn = shpStudyRegionCreate,
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
source("colorPaletteForShiny.R")
labelColumn <- "shinyLabel"


#############################

##### SERVER FILE.R
names(authenticationType) <- authenticationType
authenticationTypePossibilities <- c("Free", "Proprietary", "All")
if (!all(authenticationType %in% authenticationTypePossibilities)) {
  stop("authenticationType must be one or both of ", authenticationTypePossibilities)
}
emptyList <- Map(function(y) y, authenticationTypePossibilities) # creates named list
emptyListAll <- emptyList["All"] # creates named list
emptyListFree <- emptyList["Free"] # creates named list
emptyListFreeProprietary <- emptyList[c("Free", "Proprietary")] # creates named list

experimentReps <- emptyListAll
experimentReps <- lapply(experimentReps, function(x) 1)

# simInit objects
times4sim <- emptyListAll
times4sim <- lapply(times4sim, function(x) list(start = 0, end = endTime))

modules4sim <- emptyListAll
modules4sim$All <- list("landWebDataPrep", "initBaseMaps", "fireDataPrep", "LandMine",
                         "landWebProprietaryData",
                         "Boreal_LBMRDataPrep", "LBMR", "timeSinceFire", "LandWebOutput")

objects4sim <- emptyListAll
objects4sim <- lapply(objects4sim, function(x)
  list("shpStudyRegionFull" = shpStudyRegion,
       "shpStudySubRegion" = shpSubStudyRegion,
       "summaryPeriod" = summaryPeriod,
       "useParallel" = 2,
       "vegLeadingPercent" = vegLeadingPercent)
)

parameters4sim <- emptyListAll
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
objectNamesToSave <- emptyListAll
objectNamesToSave <- lapply(objectNamesToSave, function(x) {
  c("rstTimeSinceFire", "vegTypeMap")
})

outputs4sim <- Map(objects4sim = objects4sim,
                   parameters4sim = parameters4sim,
                   times4sim = times4sim,
                   objectNamesToSave = objectNamesToSave,
                   outputs4simFn)

## paths for sim
pathFn <- function(pathType, basename, suffix) {
  file.path(pathType, paste0(basename, "_", suffix))
}

cPaths <- emptyListAll
cPaths <- Map(pathFn, suffix = names(cPaths),
              MoreArgs = list(basename = subStudyRegionName, pathType = "cache"))

oPaths <- emptyListAll
oPaths <- Map(pathFn, suffix = names(oPaths),
              MoreArgs = list(basename = subStudyRegionName, pathType = "outputs"))

paths4sim <- emptyListAll
paths4sim <- Map(cPath = cPaths, oPath = oPaths,
                 function(cPath, oPath) {
                   list(
                     cachePath = cPath,
                     modulePath = "m",
                     inputPath = "inputs",
                     outputPath = oPath
                   )
                 })

seed <- sample(1e8, 1)


######## SimInit and Experiment
mySimOuts <- Cache(simInitAndExperiment, times = times4sim, params = parameters4sim,
                   modules = modules4sim,
                   cacheId = if (exists("cacheId4Experiment")) cacheId4Experiment else NULL,
                   outputs = outputs4sim,
                   cacheIds4Experiment = if (exists("cacheIds4Experiment")) cacheIds4Experiment else NULL,
                   objects4sim = objects4sim, # study area -- cache will respect this
                   paths = paths4sim, loadOrder = lapply(modules4sim, unlist),
                   emptyList = emptyListAll)

message("  Finished simInit and Experiment.")

message("  Running LandWebShiny module")
sim2 <- Cache(simInitAndSpades, times = list(start = 0, end = 1), params = list(),
              modules = list("LandWebShiny"),
              list(mySimOuts = mySimOuts,  # can't name "objects" arg in simInit because same as Cache
                   paths = paths4sim$All),
              paths = paths4sim$All)

globalEndTime <- Sys.time()

onStop(function() {
  appStopTime <<- Sys.time()

  cat("App took", format(appStopTime - appStartTime), "\n")
  cat("Package loading took", format(packageLoadEndTime - packageLoadStartTime), "\n")
  cat("Global.R took", format(globalEndTime - appStartTime), "\n")
  cat("Server took", format(appStopTime - serverStartTime), "\n")
})

