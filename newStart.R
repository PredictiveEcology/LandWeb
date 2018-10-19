runName = "testing"
##########################################################
# Packages for global.R -- don't need to load packages for modules -- happens automatically
##########################################################
library(raster)
library(SpaDES.core)
library(map)
#devtools::load_all("~/GitHub/map")

packageLoadStartTime <- Sys.time()
SpaDESPkgs <- c(
  "PredictiveEcology/quickPlot@development",
  "PredictiveEcology/SpaDES.core@development",
  "PredictiveEcology/map@master",
  "PredictiveEcology/SpaDES.tools@development",
  #"PredictiveEcology/SpaDES.shiny@generalize-modules", ## do this after running the model, before app
  "raster"
)
shinyPkgs <- c("leaflet", "leaflet.extras", "gdalUtils", "rgeos", "raster", "parallel",
               "shiny", "shinydashboard", "shinyBS", "shinyjs", "shinycssloaders", "shinyWidgets")
googleAuthPkgs <- c("googleAuthR", "googledrive", "googleID")
moduleRqdPkgs <- c("data.table", "dplyr", "fasterize", "fpCompare",
                   "gdalUtils", "ggplot2", "grDevices", "grid", "magrittr",
                   "PredictiveEcology/quickPlot@development",
                   "PredictiveEcology/SpaDES.tools@development",
                   "purrr", "R.utils", "raster", "RColorBrewer", "Rcpp", "reproducible",
                   "rgeos", "scales", "sp", "SpaDES.core", "SpaDES.tools", "tidyr", "VGAM")

##########################################################
# Paths
##########################################################
paths <- list(
  cachePath = "cache",
  modulePath = "m", # short name because shinyapps.io can't handle longer than 100 characters
  inputPath = "inputs",
  outputPath = file.path("outputs", runName)
)
do.call(SpaDES.core::setPaths, paths) # Set them here so that we don't have to specify at each call to Cache
tilePath <- file.path(runName, "tiles")


## Options
# options(reproducible.inputPaths = NULL)
options(reproducible.destinationPath = Paths$inputPath)
opts <- options("spades.moduleCodeChecks" = FALSE,
                "reproducible.quick" = FALSE,
                "reproducible.overwrite" = TRUE,
                "map.overwrite" = TRUE,
                "reproducible.useCache" = TRUE,
                "spades.useRequire" = FALSE, # Don't use Require... meaning assume all pkgs installed
                map.tilePath = tilePath,
                map.dataPath = Paths$inputPath, # not used yet
                map.useParallel = TRUE, #!identical("windows", .Platform$OS.type),
                reproducible.destinationPath = Paths$inputs
)

##########################################################
# Load Study Area
##########################################################

activeDir <- "~/GitHub/LandWeb"
checkPath(activeDir, create = TRUE)
targetCRS <- CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
setwd(activeDir)
ml <- mapAdd(layerName = "LandWeb Study Area",
             targetCRS = targetCRS, overwrite = TRUE,
             url = "https://drive.google.com/open?id=1JptU0R7qsHOEAEkxybx5MGg650KC98c6", # This is landweb_ltfc_v6.shp
             columnNameForLabels = "Name", isStudyArea = FALSE, filename2 = NULL
)

# Make a random small study area
seed <- 863
set.seed(seed)
sp2 <- SpaDES.tools::randomPolygon(ml$`LandWeb Study Area`, 4e5)
ml <- mapAdd(obj = sp2, map = ml, filename2 = FALSE,
             targetCRS = targetCRS,
             layerName = "Small Study Area",
             columnNameForLabels = "Name", isStudyArea = TRUE,
             filename1 = NULL, poly = TRUE,
             analysisGroupReportingPolygon = "Small Study Area"
)
aaa <- 1
# re-add the LandWeb polygon, but this time crop it to the Small Study Area
ml <- mapAdd(layerName = "Small Study Area", map = ml,
             #studyArea = studyArea(ml),
             overwrite = TRUE, useSAcrs = TRUE, poly = TRUE,
             analysisGroupReportingPolygon = "Small Study Area",
             url = "https://drive.google.com/open?id=1JptU0R7qsHOEAEkxybx5MGg650KC98c6",
             columnNameForLabels = "NSN", isStudyArea = TRUE, filename2 = NULL
)
rm(aaa)

######
#
######
endTime <- 3
successionTimestep <- 10
summaryPeriod <- c(2,3)
summaryInterval <- 1
## spades module variables
eventCaching <- c(".inputObjects", "init")
maxAge <- 400
vegLeadingPercent <- 0.8 # indicates what proportion the stand must be in one species group for it to be leading.
# If all are below this, then it is a "mixed" stand
ageClasses <- c("Young", "Immature", "Mature", "Old")
ageClassCutOffs <- c(0, 40, 80, 120)
fireTimestep <- 1

times <- list(start = 0, end = endTime)
modules <- list("LandWeb_dataPrep", "initBaseMaps", "fireDataPrep",
                "LandMine",
                "LandWebProprietaryData",
                "Boreal_LBMRDataPrep", "LBMR", "timeSinceFire", "LandWeb_output")
objects <- list("shpStudyRegionFull" = ml$`LandWeb Study Area`,
                "shpStudySubRegion" = studyArea(ml, 1),
                "summaryPeriod" = summaryPeriod,
                "useParallel" = 2,
                "vegLeadingPercent" = vegLeadingPercent)
parameters <- list(
  Boreal_LBMRDataPrep = list(.useCache = eventCaching, .crsUsed = crs(studyArea(ml))),
  fireDataPrep = list(.useCache = eventCaching),
  initBaseMaps = list(.useCache = eventCaching),
  LandMine = list(biggestPossibleFireSizeHa = 5e5,
                  fireTimestep = fireTimestep,
                  burnInitialTime = fireTimestep,
                  .useCache = eventCaching),
  LandWeb_dataPrep = list(.useCache = eventCaching),
  LandWeb_output = list(summaryInterval = summaryInterval),
  LandWebProprietaryData = list(.useCache = eventCaching),
  LBMR = list(successionTimestep = successionTimestep,
              .useCache = eventCaching),
  timeSinceFire = list(startTime = fireTimestep,
                       .useCache = eventCaching)
)

objectNamesToSave <- c("rstTimeSinceFire", "vegTypeMap")

outputs <- data.frame(stringsAsFactors = FALSE,
                      expand.grid(
                        objectName = objectNamesToSave,#, "oldBigPatch"),
                        saveTime = seq(objects$summaryPeriod[1], objects$summaryPeriod[2],
                                       by = parameters$LandWeb_output$summaryInterval)),
                      fun = "writeRaster", package = "raster",
                      file = paste0(objectNamesToSave, c(".tif", ".grd")))

outputs2 <- data.frame(stringsAsFactors = FALSE,
                       expand.grid(objectName = c("simulationOutput"), saveTime = times$end),
                       fun = "saveRDS",
                       package = "base")

outputs$arguments <- I(rep(list(list(overwrite = TRUE, progress = FALSE, datatype = "INT2U", format = "GTiff"),
                                list(overwrite = TRUE, progress = FALSE, datatype = "INT1U", format = "raster")),
                           times = NROW(outputs) / length(objectNamesToSave)))

outputs3 <- data.frame(stringsAsFactors = FALSE,
                       objectName = "rstFlammable",
                       saveTime = times$end, fun = "writeRaster", package = "raster",
                       arguments = I(list(list(overwrite = TRUE, progress = FALSE,
                                               datatype = "INT2U", format = "raster"))))

outputs <- as.data.frame(data.table::rbindlist(list(outputs, outputs2, outputs3), fill = TRUE))


######## SimInit and Experiment
seed <- sample(1e8, 1)
set.seed(seed)
print(seed)

######## SimInit and Experiment
useParallel <- getOption("map.useParallel", !identical("windows", .Platform$OS.type))
cl <- map:::makeOptimalCluster(useParallel = useParallel,
                         MBper = 1e3,
                         maxNumClusters = 10)

mySimOuts <- Cache(simInitAndExperiment, times = times, cl = cl,
                     params = parameters,
                     modules = modules,
                     outputs = outputs,
                     objects, # do not name this argument -- collides with
                     paths = paths,
                     loadOrder = unlist(modules),
                     clearSimEnv = TRUE, .plotInitialTime = NA,
                     replicates = 2
)
try(stopCluster(cl), silent = TRUE)

##########################################################
# Current Condition
##########################################################
ccURL <- "https://drive.google.com/file/d/1JnKeXrw0U9LmrZpixCDooIm62qiv4_G1/view?usp=sharing"

layerNames <- c("Pine", "Age", "Black Spruce", "Deciduous", "Fir", "LandType", "White Spruce")
layerNamesFiles <- paste0(gsub(" ", "", layerNames), "1.tif")
ml <- mapAdd(map = ml, url = ccURL, layerName = layerNames,
             targetFile = layerNamesFiles, filename2 = NULL, #useCache = FALSE,
             alsoExtract = "similar",  leaflet = FALSE)

##########################################################
# Dynamic Raster Layers from Simulation
##########################################################
allouts <- unlist(lapply(mySimOuts, function(sim) outputs(sim)$file))
allouts <- grep("vegType|TimeSince", allouts, value = TRUE)
layerName <- gsub(allouts, pattern = paste0(".*", Paths$outputPath), replacement = "")
layerName <- gsub(layerName, pattern = "[/\\]", replacement = "_")
layerName <- gsub(layerName, pattern = "^_", replacement = "")
ag1 = gsub(layerName, pattern = "(.*)_.*_(.*)\\..*", replacement = "\\1_\\2")
destinationPath <- dirname(allouts)
tsf <- gsub(".*vegTypeMap.*", NA, allouts)
vtm <- gsub(".*TimeSinceFire.*", NA, allouts)

ml <- mapAdd(map = ml, layerName = layerName, analysisGroup1 = ag1,
             targetFile = asPath(allouts),
             destinationPath = asPath(destinationPath),
             filename2 = NULL, tsf = asPath(tsf), vtm = asPath(vtm),
             overwrite = TRUE, leaflet = tilePath)

######################################################################
# Leading Veg Type By Age Class
######################################################################
ml <- mapAddAnalysis(ml, functionName ="LeadingVegTypeByAgeClass",
                     ageClasses = ageClasses, ageClassCutOffs = ageClassCutOffs)

# add an analysis -- this will trigger analyses because there are already objects in the map
#    This will trigger 2 more analyses ... largePatches on each raster x polygon combo (only 1 currently)
#    so there is 1 raster group, 2 polygon groups, 2 analyses - Total 4, only 2 run now
ml <- mapAddAnalysis(ml, functionName = "LargePatches", ageClasses = ageClasses,
                     id = "1", labelColumn = "shinyLabel",
                     ageClassCutOffs = ageClassCutOffs)

if (FALSE) {
  ################################################################
  ################################################################
  ################################################################
  ###   WORKS UP TO HERE
  ################################################################
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
  # eventCaching, maxAge, vegLeadingPercent
  # ageClasses, ageClassCutOffs, ageClas0s0Zones
  source("R/LandWeb user parameters.R")
  landisInputs <- readRDS(file.path(paths$inputPath, "landisInputs.rds"))
  spEcoReg <- readRDS(file.path(paths$inputPath, "SpEcoReg.rds"))

  # The CRS for the Study -- spTransform converts this first one to the second one, they are identical geographically
  # crsStudyRegion <- CRS(paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0",
  #                         "+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  crsStudyRegion <- CRS(paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0",
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
