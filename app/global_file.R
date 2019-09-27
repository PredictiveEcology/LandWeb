#install_github("PredictiveEcology/SpaDES.shiny@development")
#setwd("~/GitHub/LandWeb/app")

packageLoadStartTime <<- Sys.time()
googleAuthPkgs <- c("googleAuthR", "googledrive", "googleID")
otherPkgs <- c("future", "magrittr", "promises", "reproducible")
spatialPkgs <- c("gdalUtils", "map", "parallel", "rgeos", "raster", "sp")
shinyPkgs <- c("leaflet", "leaflet.extras",
               "shiny", "shinydashboard", "shinyBS", "shinyjs", "shinycssloaders", "shinyWidgets")

reproducible::Require(c(googleAuthPkgs, otherPkgs, shinyPkgs, spatialPkgs,
                        "PredictiveEcology/SpaDES.shiny@generalize-modules"))
packageLoadEndTime <<- Sys.time()

## LandWeb app information
source("appInfo.R")

## package options and Google setup
future::plan("multiprocess")
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

paths <- list(
  cachePath = file.path("cache", "LandWeb_App"),
  modulePath = "m", # short name because shinyapps.io can't handle longer than 100 characters
  inputPath = "inputs",
  outputPath = "outputs"
)

## get additonal helper functions used throughout this shiny app
source(file.path("R", "helpers.R"))

# This is for rerunning apps -- Will not do anything if not on one of named computers

# App - variables
appStartTime <- st <- Sys.time()
message("Started at ", appStartTime)

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
maxAge <- 400
vegLeadingProportion <- 0.8
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
successionTimestep <- 10

## map object with study areas and reporting polygons
runName <- "LandWeb_highDispersal_logROS"  ## TODO: don't use runName?
ml <- readRDS(file.path(appDir, paths$outputPath, runName, "ml_preamble.rds")) ## TODO: don't use runName?

# These are used in inputTables.R for filling the tables of parameters in
#landisInputs <- readRDS(file.path(appDir, paths$inputPath, "landisInputs.rds")) ## TODO: remove
speciesTraits <- readRDS(file.path(appDir, paths$inputPath, "speciesTraitsTable.rds")) ## TODO: use this instead of landisTraits
spEcoReg <- readRDS(file.path(appDir, paths$inputPath, "SpEcoReg.rds")) ## TODO: use updated values

# The CRS for the Study -- spTransform converts this first one to the second one, they are identical geographically
# crsStudyRegion <- CRS(paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0",
#                         "+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
crsStudyRegion <- CRS(paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0",
                            "+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

studyRegionFilename <- "landweb_ltfc_v6.shp" ## TODO: verify this
studyRegionFilePath <- file.path(appDir, paths$inputPath, studyRegionFilename)

FMA_names <- sort(unique(ml[["FMA Boundaries Updated"]][["Name"]]))

## source additional shiny modules
vapply(list.files("shiny-modules", "[.]R", full.names = TRUE), source, vector("list", 2))

## a few map details for shiny app
message("Preparing polygon maps for reporting histograms")
source(file.path("R", "colorPaletteForShiny.R"))
labelColumn <- "shinyLabel"

####################################################################################################

globalEndTime <<- Sys.time()

onStop(function() {
  appStopTime <<- Sys.time()

  cat("App took", format(appStopTime - appStartTime), "\n")
  cat("Package loading took", format(packageLoadEndTime - packageLoadStartTime), "\n")
  cat("Global.R took", format(globalEndTime - appStartTime), "\n")
  cat("Server took", format(appStopTime - serverStartTime), "\n")
})
