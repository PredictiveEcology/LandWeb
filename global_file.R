
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
authenticationTypePossibilities <- c("Free", "Proprietary")
if (!all(authenticationType %in% authenticationTypePossibilities)) {
  stop("authenticationType must be one or both of ", authenticationTypePossibilities)
}
emptyList <- lapply(authenticationType, function(x) NULL)

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
       "shpStudySubRegion" = shpSubStudyRegion,
       "summaryPeriod" = summaryPeriod,
       "useParallel" = 2,
       "vegLeadingPercent" = vegLeadingPercent)
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

outputs4sim <- Map(objects4sim = objects4sim,
                   parameters4sim = parameters4sim,
                   times4sim = times4sim,
                   objectNamesToSave = objectNamesToSave,
                   outputs4simFn)

## paths for sim
pathFn <- function(pathType, basename, suffix) {
  file.path(pathType, paste0(basename, "_", suffix))
}

cPaths <- emptyList
cPaths <- Map(pathFn, suffix = names(cPaths),
              MoreArgs = list(basename = subStudyRegionName, pathType = "cache"))

oPaths <- emptyList
oPaths <- Map(pathFn, suffix = names(oPaths),
              MoreArgs = list(basename = subStudyRegionName, pathType = "outputs"))

paths4sim <- emptyList
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

if (!exists("cacheIds4Experiment")) {
  cacheIds4Experiment <- emptyList  
}

######## SimInit and Experiment
mySimOuts <- Cache(simInitAndExperiment, times = times4sim, params = parameters4sim,
                   modules = modules4sim, 
                   cacheId = if (exists("cacheId4Experiment")) cacheId4Experiment else NULL,
                   outputs = outputs4sim, cacheIds4Experiment = cacheIds4Experiment,
                   objects4sim = objects4sim, # study area -- cache will respect this
                   paths = paths4sim, loadOrder = lapply(modules4sim, unlist),
                   emptyList = emptyList)

message("  Finished simInit and Experiment.")

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
  fps <- extractFilepaths("rstTimeSinceFire", rastersFromOutput)
  fps <- convertPath(fps, old = "outputsFULL", new = "outputs/FULL_Proprietary")
  if (Sys.info()["sysname"]=="Windows" && Sys.info()["user"]=="emcintir") {
    fps <- convertPath(fps, old = "/home/emcintir/Documents/", new = pathToLandWebApp)
  }
  asPath(fps)
})

vtms <- lapply(rastersFromOutputs, function(rastersFromOutput) {
  fps <- extractFilepaths("vegTypeMap", rastersFromOutput)
  fps <- convertPath(fps, old = "outputsFULL", new = "outputs/FULL_Proprietary")
  if (Sys.info()["sysname"]=="Windows" && Sys.info()["user"]=="emcintir") {
    fps <- convertPath(fps, old = "/home/emcintir/Documents/", new = pathToLandWebApp)
  }
  asPath(fps)
})

tsfLFLTFilenames <- lapply(tsfs, function(tsf) SpaDES.core::.suffix(tsf, "LFLT") )

rasterResolutions <- lapply(tsfs, function(x) raster(x[1]) %>% res(.))
rasterResolutions <- lapply(tsfs, function(x) {
  raster(x[1]) %>% res(.)
}
)

flammableFiles <- lapply(mySimOuts, function(mySimOut) {
  fps <- file.path(outputPath(mySimOut[[1]]), "rstFlammable.grd")
  fps <- convertPath(fps, old = "outputsFULL", new = "outputs/FULL_Proprietary")
  if (Sys.info()["sysname"]=="Windows" && Sys.info()["user"]=="emcintir") {
    fps <- convertPath(fps, old = "/home/emcintir/Documents/", new = pathToLandWebApp)
  }
  asPath(fps)
})

tsfRasters <- Cache(Map, tsf = tsfs, 
                    userTags = c("reprojectRasts", "tsf", "tsfs"),
                    cacheId = if (exists("cacheIdTsfRasters")) cacheIdTsfRasters else NULL,
                    lfltFN = tsfLFLTFilenames, flammableFile = flammableFiles,
                    reprojectRasts, MoreArgs = list(crs = sp::CRS(SpaDES.shiny::proj4stringLFLT)))
if (Sys.info()["sysname"]=="Windows" && Sys.info()["user"]=="emcintir") {
  tsfRasters <- lapply(tsfRasters, function(authType) {
    lapply(authType, function(crsType) {
      lapply(crsType, function(ras) {
        
        fps <- ras@file@name
        fps <- convertPath(fps, old = "outputsFULL", new = "outputs/FULL_Proprietary")
        fps <- convertPath(fps, old = "/home/emcintir/Documents/", new = pathToLandWebApp)
        ras@file@name <- fps
        ras
      })
    })
    
  })
}

tsfRasterTilePaths <- Cache(Map, rst = tsfRasters, modelType = names(tsfRasters), 
                            userTags = c("gdal2TilesWrapper", "tsf", "tsfs"),
                            cacheId = if (exists("cacheIdTsfRasterTilePaths")) 
                              cacheIdTsfRasterTilePaths else NULL,
                            
                            MoreArgs = list(zoomRange = 1:10, colorTableFile = asPath(colorTableFile)),
                            function(rst, modelType, zoomRange, colorTableFile) {
                              outputPath <- file.path("www", modelType, subStudyRegionNameCollapsed, "map-tiles")
                              filenames <- unlist(lapply(rst$crsLFLT, function(ras) filename(ras)))
                              lfltDirNames <- gsub(file.path(outputPath, paste0("out", basename(filenames))), pattern = "\\.tif$", replacement = "")
                              filenames <- if (!(all(dir.exists(lfltDirNames))))  {
                                Cache(gdal2Tiles, rst$crsLFLT, outputPath = outputPath, 
                                      userTags = c("gdal2Tiles", "tsf", "tsfs"),
                                      zoomRange = zoomRange, colorTableFile = colorTableFile)
                              } else {
                                lfltDirNames
                              }
                              return(filenames)
                            })



vtmsTifs <- Cache(lapply, vtms, 
                  cacheId = if (exists("cacheIdVtmsTifs")) 
                    cacheIdVtmsTifs else NULL,
                  userTags = c("writeRaster", "tifs"),
                  function(vtmsInner) {
                    vtmTifs <- lapply(vtmsInner, function(vtm) {
                      vtmRas <- raster(vtm)
                      vtmRas <- writeRaster(vtmRas, file = gsub(".grd", ".tif", filename(vtmRas)), overwrite = TRUE)
                    })
                    return(unlist(lapply(vtmTifs, filename)))
                  })
vtmLFLTFilenames <- lapply(vtmsTifs, function(vtm) SpaDES.core::.suffix(vtm, "LFLT") )

vtmRasters <- Cache(Map, tsf = vtmsTifs, userTags = c("reprojectRasts", "vtms", "vtm"),
                    cacheId = if (exists("cacheIdVtmRasters")) cacheIdVtmRasters else NULL,
                    lfltFN = vtmLFLTFilenames, flammableFile = flammableFiles,
                    reprojectRasts, MoreArgs = list(crs = sp::CRS(SpaDES.shiny::proj4stringLFLT)))
if (Sys.info()["sysname"]=="Windows" && Sys.info()["user"]=="emcintir") {
  vtmRasters <- lapply(vtmRasters, function(authType) {
    lapply(authType, function(crsType) {
      lapply(crsType, function(ras) {
        
        fps <- ras@file@name
        fps <- convertPath(fps, old = "outputsFULL", new = "outputs/FULL_Proprietary")
        fps <- convertPath(fps, old = "/home/emcintir/Documents/", new = pathToLandWebApp)
        ras@file@name <- fps
        ras
      })
    })
    
  })
}


if (FALSE) { # This is to have vegetation type maps -- TODO: they are .grd, need to be .tif & color table
  vtmRasterTilePaths <- Cache(Map, rst = vtmRasters, modelType = names(vtmRasters),
                              userTags = c("gdal2Tiles", "vtm", "vtms"),
                              cacheId = if (exists("cacheIdVtmRasterTilePaths")) 
                                cacheIdVtmRasterTilePaths else NULL,
                              MoreArgs = list(zoomRange = 1:10, colorTableFile = asPath(colorTableFile)),
                              function(rst, modelType, zoomRange, colorTableFile) {
                                outputPath <- file.path("www", modelType, subStudyRegionNameCollapsed, "map-tiles")
                                filenames <- gdal2Tiles(rst$crsLFLT, outputPath = outputPath,
                                                        zoomRange = zoomRange, colorTableFile = colorTableFile)
                                return(filenames)
                              })
}


########################################################
# formerly in mapsForShiny.R
# Reporting polygons
if (isTRUE(useParallelCluster)) {
  library(parallel)
  message("  Closing existing cluster for raster::extract")
  raster::endCluster()
  message("  Starting ",numClusters, "  node cluster for raster::extract")
  raster::beginCluster(min(numClusters, parallel::detectCores() / 4))
  
  numClus <- 6
  message("  Also starting a cluster with ", numClus," threads")
  if (!exists("cl6")) {
    cl6 <- parallel::makeForkCluster(numClus)
  } 
} else {
  cl6 <- NULL
}


### CURRENT CONDITION ##################################
message("Loading Current Condition Rasters")
dPath <- file.path(paths$inputPath, "CurrentCondition")
CCspeciesNames <- list(Free = c(), 
                       Proprietary = c("Pine", "Age", "BlackSpruce", "Deciduous", "Fir", "LandType", "WhiteSpruce"))
CCspeciesNames <- CCspeciesNames[names(authenticationType)] # make sure it has the names in authenticationType
CurrentConditions <- Cache(Map, createCCfromVtmTsf, CCspeciesNames = CCspeciesNames, 
                           userTags = c("createCCfromVtmTsf", "CurrentConditions"),
                           cacheId = if (exists("cacheIdCurrentCondition")) 
                             cacheIdCurrentCondition else NULL,
                           MoreArgs = list(vtmRasters = vtmRasters, 
                                           dPath = dPath, 
                                           loadCCSpeciesFn = loadCCSpecies, 
                                           shpSubStudyRegion = shpSubStudyRegion, 
                                           tsfRasters = tsfRasters))
tsfsCC <- lapply(CurrentConditions, function(x) {if (!is.null(x)) {
  fps <- convertPath(filename(x$CCtsf), old = "outputsFULL", new = "outputs/FULL_Proprietary")
  if (Sys.info()["sysname"]=="Windows" && Sys.info()["user"]=="emcintir") {
    fps <- convertPath(fps, old = "/home/emcintir/Documents/", new = pathToLandWebApp)
  }
  asPath(fps)
}
})
vtmsCC <- lapply(CurrentConditions, function(x) {if (!is.null(x)) {
  fps <- convertPath(filename(x$CCvtm), old = "outputsFULL", new = "outputs/FULL_Proprietary")
  if (Sys.info()["sysname"]=="Windows" && Sys.info()["user"]=="emcintir") {
    fps <- convertPath(fps, old = "/home/emcintir/Documents/", new = pathToLandWebApp)
  }
  asPath(fps)
}
})


reportingAndLeading <- Cache(reportingAndLeadingFn,
                             createReportingPolygonsAllFn = createReportingPolygonsAll, # pass function in so Caching captures function
                             createReportingPolygonsFn = createReportingPolygons,
                             userTags = c("leading", "reportingPolygons"),
                             cacheId = if (exists("cachdId4ReportingAndLeadingFn")) cachdId4ReportingAndLeadingFn else NULL,
                             leadingByStageFn = leadingByStage,
                             intersectListShpsFn = intersectListShps,
                             shpStudyRegion = shpStudyRegion, shpSubStudyRegion = shpSubStudyRegion,
                             authenticationType = authenticationType,
                             ageClasses = ageClasses, ageClassCutOffs = ageClassCutOffs,
                             tsfs = tsfs, vtms = vtms, cl = cl6, lapplyFn = lapplyFn)
list2env(reportingAndLeading, envir = .GlobalEnv) # puts leading and reportingPolygons into .GlobalEnv

reportingAndLeadingCC <- Cache(reportingAndLeadingFn, #notOlderThan = Sys.time(),
                               createReportingPolygonsAllFn = createReportingPolygonsAll, # pass function in so Caching captures function
                               createReportingPolygonsFn = createReportingPolygons,
                               userTags = c("leading", "reportingPolygons"),
                               cacheId = if (exists("cachdId4ReportingAndLeadingFnCC")) cachdId4ReportingAndLeadingFnCC else NULL,
                               leadingByStageFn = leadingByStage,
                               intersectListShpsFn = intersectListShps,
                               shpStudyRegion = shpStudyRegion, shpSubStudyRegion = shpSubStudyRegion,
                               authenticationType = authenticationType,
                               ageClasses = ageClasses, ageClassCutOffs = ageClassCutOffs,
                               tsfs = tsfsCC, vtms = vtmsCC, cl = cl6, lapplyFn = lapplyFn)
reportingPolygonsCC <- reportingAndLeadingCC$reportingPolygons
leadingCC <- reportingAndLeadingCC$leading
leadingCC$Free <- NULL


#########################

message(paste("Running largePatchesFn"))
rp4LrgPatches <- lapply(reportingPolygons, function(rpAll) {
  lapply(rpAll, function(rp) {
    rp$crsSR$shpSubStudyRegion
  })
})
lrgPatches <- Cache(Map, largePatchesFn, 
                    timeSinceFireFiles = tsfs,
                    vegTypeMapFiles = vtms,
                    reportingPolygons = rp4LrgPatches,
                    authenticationType = authenticationType,
                    cacheId = if (exists("cacheIdLrgPatches")) 
                      cacheIdLrgPatches else NULL,
                    omitArgs = c("cl", "lapplyFn"),
                    MoreArgs = list(ageClasses = ageClasses,
                                    cl = cl6, lapplyFn = lapplyFn, # this is passed to lapply on timeSinceFireFiles 
                                    countNumPatchesFn = countNumPatches,
                                    ageCutoffs = ageClassCutOffs)
)
lrgPatchesCC <- Cache(Map, largePatchesFn,
                      timeSinceFireFiles = tsfsCC,
                      vegTypeMapFiles = vtmsCC,
                      cacheId = if (exists("cacheIdLrgPatchesCC")) 
                        cacheIdLrgPatchesCC else NULL,
                      reportingPolygons = rp4LrgPatches,
                      authenticationType = authenticationType,
                      omitArgs = c("cl", "lapplyFn"),
                      MoreArgs = list(ageClasses = ageClasses,
                                      cl = cl6, lapplyFn = lapplyFn, # this is passed to lapply on timeSinceFireFiles 
                                      countNumPatchesFn = countNumPatches,
                                      ageCutoffs = ageClassCutOffs)
)
message(paste("Finished largePatchesFn"))

##### TODO: remove this??
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
################################################################################
# Write all Proprietary input shapefiles to disk
polySubDir <- file.path(oPaths$Proprietary, "Polygons")
dir.create(polySubDir, showWarnings = FALSE)
out <- Cache(Map, polys = lapply(reportingPolygons$Proprietary, function(p) p$crsSR$shpSubStudyRegion), 
             namesPolys = names(reportingPolygons$Proprietary),
             cacheId = if (exists("cacheIdWriteShapefiles")) 
               cacheIdWriteShapefiles else NULL,
             function(polys, namesPolys) {
               tryCatch(raster::shapefile(polys, 
                                          filename = file.path(polySubDir, namesPolys),
                                          overwrite = TRUE), error = function(x) NULL)
             })


globalEndTime <- Sys.time()

onStop(function() {
  appStopTime <<- Sys.time()
  
  cat("App took", format(appStopTime - appStartTime), "\n")
  cat("Package loading took", format(packageLoadEndTime - packageLoadStartTime), "\n")
  cat("Global.R took", format(globalEndTime - appStartTime), "\n")
  cat("Server took", format(appStopTime - serverStartTime), "\n")
})
