# Put all loading in one call, makes versioning easier whenever we need that.
reproducible::Require(c("googleAuthR", "googledrive", "googleID"))

options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/drive.readonly",
                                        "https://www.googleapis.com/auth/userinfo.email",
                                        "https://www.googleapis.com/auth/userinfo.profile"))
options(googleAuthR.webapp.client_id = "869088473060-a7o2bc7oit2vn11gj3ieh128eh8orb04.apps.googleusercontent.com")
options(googleAuthR.webapp.client_secret = "FR-4jL12j_ynAtsl-1Yk_cEL")

appURL <- "http://landweb.predictiveecology.org/Demo/"
authFile <- "https://drive.google.com/file/d/1sJoZajgHtsrOTNOE3LL8MtnTASzY0mo7/view?usp=sharing"

# Spatial stuff -- determines the size of the area that will be "run" in the simulations
studyArea <- "VERYSMALL"  #other options: "FULL", "EXTRALARGE", "LARGE", "MEDIUM", "NWT", "SMALL" , "RIA", "VERYSMALL"

## paths -- NOTE: these are the 'default' paths for app setup;
##                however, in-app, the paths need to be set as reactive values for authentication!
paths <- list(
  cachePath = paste0("appCache", studyArea),
  modulePath = "m", # short name because shinyapps.io can't handle longer than 100 characters
  inputPath = "inputs",
  outputPath = paste0("outputs", studyArea)
)
do.call(SpaDES.core::setPaths, paths) # Set them here so that we don't have to specify at each call to Cache

# This is a separate cache ONLY used for saving snapshots of working LandWeb runs
# It needs to be separate because it is an overarching one, regardless of scale
reproducibleCache <- "reproducibleCache"

source("loadPackages.R") # load & install (if not available) package dependencies, with specific versioning

if (any(c("emcintir") %in% Sys.info()["user"])) {
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

## source additional shiny modules
vapply(list.files("shiny-modules", "[.]R", full.names = TRUE), source, vector("list", 2))

# This needs simInit call to be run already
# a few map details for shiny app
source("mapsForShiny.R")

message("  Finished global.R")
