useSpades <- TRUE
minFRI <- 40
activeDir <- "~/GitHub/LandWeb"
setwd(activeDir)

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

#runName <- "tolko_AB_N"  ## original
#runName <- "tolko_AB_S"  ## original
#runName <- "tolko_SK"  ## original

## running locally
# NOTE: these 'doubleFRI' runs never used double FRI so they are same as original!
#runName <- "tolko_AB_N_doubleFRI" ## DONE
#runName <- "tolko_AB_S_doubleFRI" ## DONE
#runName <- "tolko_SK_doubleFRI" ## DONE

## running locally
#runName <- "tolko_AB_N_equalROS" ## DONE
#runName <- "tolko_AB_S_equalROS" ## DONE
#runName <- "tolko_SK_equalROS" ## DONE

## running on 388; results saved locally
#runName <- "tolko_AB_N_logROS" ## DONE
#runName <- "tolko_AB_S_logROS" ## DONE
#runName <- "tolko_SK_logROS" ## DONE

## running locally
#runName <- "tolko_AB_N_logROS_new" ## running
#runName <- "tolko_AB_S_logROS_new" ## running
#runName <- "tolko_SK_logROS_new" ## running

## running locally
#runName <- "tolko_AB_N_noDispersal" ## running
runName <- "tolko_AB_S_noDispersal" ## running
#runName <- "tolko_SK_noDispersal" ## running

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
library(raster)
library(SpaDES.core)
library(map)
# if (Sys.info()[["user"]] == "achubaty") {
#   devtools::load_all("~/GitHub/PredictiveEcology/map")
# } else if (Sys.info()[["user"]] == "emcintir") {
#   devtools::load_all("~/GitHub/map")
# }
library(pemisc)

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
  "reproducible.inputPaths" = normPath(Paths$inputPath),
  "reproducible.overwrite" = TRUE,
  "reproducible.quick" = FALSE,
  "reproducible.useCache" = TRUE,
  "spades.moduleCodeChecks" = FALSE,
  "spades.useRequire" = FALSE # Don't use Require... meaning assume all pkgs installed
)

##########################################################
# Load Study Area
##########################################################

checkPath(activeDir, create = TRUE)
targetCRS <- CRS(paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                       "+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))

studyRegionName <- "LandWeb Study Area"
ml <- mapAdd(layerName = studyRegionName,
             targetCRS = targetCRS, overwrite = TRUE,
             url = "https://drive.google.com/open?id=1JptU0R7qsHOEAEkxybx5MGg650KC98c6", # This is landweb_ltfc_v6.shp
             columnNameForLabels = "NSN", isStudyArea = TRUE, filename2 = NULL
)

if (grepl("testing", runName)) {
  # Make a random small study area
  seed <- 863
  ranSeed <- .Random.seed
  set.seed(seed)
  sp2 <- Cache(SpaDES.tools::randomPolygon, ml[[studyRegionName]], 4e5) # was 4e5
  ml <- mapAdd(obj = sp2, map = ml, filename2 = FALSE,
               #targetCRS = targetCRS,
               layerName = "Small Study Area",
               columnNameForLabels = "Name", isStudyArea = TRUE,
               filename1 = NULL, poly = TRUE,
               analysisGroupReportingPolygon = "Small Study Area"
  )
  # re-add the LandWeb polygon, but this time crop it to the Small Study Area
  ml <- mapAdd(layerName = "Small Study Area", map = ml,
               #studyArea = studyArea(ml),
               overwrite = TRUE, useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Small Study Area",
               url = "https://drive.google.com/open?id=1JptU0R7qsHOEAEkxybx5MGg650KC98c6",
               columnNameForLabels = "NSN", isStudyArea = TRUE, filename2 = NULL
  )
  # create rasterToMatch from LCC layer
}

################################################################################
## COMPANY-SPECIFIC STUDY AREAS

dataDir <- file.path("inputs", "FMA_Boundaries")
dataDirDMI <- file.path(dataDir, "DMI")
dataDirLP <- file.path(dataDir, "LP")
dataDirTolko <- file.path(dataDir, "Tolko")

### ADMINISTRATIVE POLYGONS
if (grepl("tolko_AB_N", runName)) {
  studyRegionName <- "Tolko AB North SR"

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  tolko_ab_n_sr <- shapefile(file.path(dataDirTolko, "Tolko_AB_N_SR.shp"))
  ml <- mapAdd(tolko_ab_n_sr, ml, isStudyArea = TRUE, layerName = studyRegionName,
               useSAcrs = TRUE, poly = TRUE,
               columnNameForLabels = "NSN", filename2 = NULL)

  ## reportingPolygons
  tolko_ab_n <- shapefile(file.path(dataDirTolko, "Tolko_AB_N.shp"))
  tolko_ab_n.ansr <- shapefile(file.path(dataDirTolko, "Tolko_AB_N_ANSR.shp"))
  tolko_ab_n.caribou <- shapefile(file.path(dataDirTolko, "Tolko_AB_N_caribou.shp"))

  ml <- mapAdd(tolko_ab_n, ml, layerName = "Tolko AB North", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Tolko AB North",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(tolko_ab_n.ansr, ml, layerName = "Tolko AB North ANSR", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Tolko AB North ANSR",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(tolko_ab_n.caribou, ml, layerName = "Tolko AB North Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Tolko AB North Caribou",
               columnNameForLabels = "Name", filename2 = NULL)
} else if (grepl("tolko_AB_S", runName)) {
  studyRegionName <- "Tolko AB South SR"

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  tolko_ab_s_sr <- shapefile(file.path(dataDirTolko, "Tolko_AB_S_SR.shp"))
  ml <- mapAdd(tolko_ab_s_sr, ml, isStudyArea = TRUE, layerName = studyRegionName,
               useSAcrs = TRUE, poly = TRUE,
               columnNameForLabels = "NSN", filename2 = NULL)

  ## reportingPolygons
  tolko_ab_s <- shapefile(file.path(dataDirTolko, "Tolko_AB_S.shp"))
  tolko_ab_s.ansr <- shapefile(file.path(dataDirTolko, "Tolko_AB_S_ANSR.shp"))
  tolko_ab_s.caribou <- shapefile(file.path(dataDirTolko, "Tolko_AB_S_caribou.shp"))

  ml <- mapAdd(tolko_ab_s, ml, layerName = "Tolko AB South", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Tolko AB South",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(tolko_ab_s.ansr, ml, layerName = "Tolko AB South ANSR", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Tolko AB South ANSR",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(tolko_ab_s.caribou, ml, layerName = "Tolko AB South Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Tolko AB South Caribou",
               columnNameForLabels = "Name", filename2 = NULL)
} else if (grepl("tolko_SK", runName)) {
  studyRegionName <- "Tolko SK SR"
  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  tolko_sk_sr <- shapefile(file.path(dataDirTolko, "Tolko_SK_SR.shp"))
  ml <- mapAdd(tolko_sk_sr, ml, isStudyArea = TRUE, layerName = studyRegionName,
               useSAcrs = TRUE, poly = TRUE,
               columnNameForLabels = "NSN", filename2 = NULL)

  ## reportingPolygons
  tolko_sk <- shapefile(file.path(dataDirTolko, "Tolko_SK.shp"))
  tolko_sk.caribou <- shapefile(file.path(dataDirTolko, "Tolko_SK_caribou.shp"))

  ml <- mapAdd(tolko_sk, ml, layerName = "Tolko SK", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Tolko SK",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(tolko_sk.caribou, ml, layerName = "Tolko SK Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Tolko SK Caribou",
               columnNameForLabels = "Name", filename2 = NULL)
} else if (grepl("LP_MB", runName)) {
  studyRegionName <- "LP MB SR"
  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  lp_mb_sr <- shapefile(file.path(dataDirLP, "LP_MB_SR.shp"))
  ml <- mapAdd(lp_mb_sr, ml, isStudyArea = TRUE, layerName = studyRegionName,
               useSAcrs = TRUE, poly = TRUE,
               columnNameForLabels = "NSN", filename2 = NULL)

  ## reportingPolygons
  lp_mb <- shapefile(file.path(dataDirLP, "LP_MB.shp"))
  lp_mb.caribou <- shapefile(file.path(dataDirLP, "LP_MB_caribou.shp"))

  ml <- mapAdd(lp_mb, ml, layerName = "LP MB", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "LP MB",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(lp_mb.caribou, ml, layerName = "LP MB Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "LP MB Caribou",
               columnNameForLabels = "Name", filename2 = NULL)
}

##########################################################
# Current Conditions (Age Map)
##########################################################
ccURL <- "https://drive.google.com/file/d/1JnKeXrw0U9LmrZpixCDooIm62qiv4_G1/view?usp=sharing"

fname_age <- "Age1.tif"
ml <- mapAdd(map = ml, url = ccURL, layerName = "CC TSF", CC = TRUE,
             tsf = file.path(Paths$inputPath, fname_age), analysisGroup1 = "CC",
             targetFile = fname_age, filename2 = NULL, #file.path(Paths$inputPath, fname_age),
             useCache = TRUE, isRasterToMatch = TRUE,
             alsoExtract = "similar", leaflet = FALSE)

##########################################################
# LCC2005
##########################################################

LCC2005 <- pemisc::prepInputsLCC(studyArea = studyArea(ml), destinationPath = Paths$inputPath)
ml <- mapAdd(LCC2005, layerName = "LCC2005", map = ml, filename2 = NULL, leaflet = FALSE,
             isRasterToMatch = FALSE, method = "ngb")

##########################################################
# Clean up the study area
##########################################################

studyArea(ml) <- pemisc::polygonClean(studyArea(ml), type = runName, minFRI = minFRI)

##########################################################
# Flammability and Fire Return Interval maps
##########################################################

## flammability map shouldn't be masked using the age raster, so don't use the ml object!
LandTypeFile <- file.path(Paths$inputPath, "LandType1.tif")
#LandTypeFile <- file.path(Paths$inputPath, "LCC2005_V1_4a.tif")
rstFlammable <- prepInputs(LandTypeFile, studyArea = studyArea(ml),
                           rasterToMatch = rasterToMatch(ml), filename2 = NULL) %>%
  defineFlammable(., nonFlammClasses = c(1, 2, 5), mask = NULL, filename2 = NULL)
#  defineFlammable(., nonFlammClasses = c(36, 37, 38, 39), mask = NULL, filename2 = NULL)

## fireReturnInterval needs to be masked by rstFlammable
rstFireReturnInterval <- postProcess(rasterToMatch(studyArea(ml), rasterToMatch = rasterToMatch(ml)),
                                     maskvalue = 0,
                                     filename2 = NULL)# %>%
  #crop(., rstFlammable) %>%
  #mask(., mask = rstFlammable, maskvalue = 0L)
ml <- mapAdd(rstFireReturnInterval, layerName = "fireReturnInterval", filename2 = NULL,
             map = ml, leaflet = FALSE, maskWithRTM = TRUE)

# factorValues2 -- removes NAs -- so need to account for this
fireReturnInterval <- pemisc::factorValues2(ml$fireReturnInterval,
                                            ml$fireReturnInterval[], att = "fireReturnInterval")
theNAs <- !is.na(ml$fireReturnInterval[])
ml$fireReturnInterval <- raster(ml$fireReturnInterval) # blank out values for new, non-factor version
ml$fireReturnInterval[theNAs] <- fireReturnInterval
ml@metadata[layerName == "LCC2005", rasterToMatch := NA]

##########################################################
# Current Conditions (Species Layers)
##########################################################

CClayerNames <- c("Pine", "Black Spruce", "Deciduous", "Fir", "White Spruce")
CClayerNamesFiles <- paste0(gsub(" ", "", CClayerNames), "1.tif")

options(map.useParallel = FALSE)
ml <- mapAdd(map = ml, url = ccURL, layerName = CClayerNames, CC = TRUE,
             targetFile = CClayerNamesFiles, filename2 = NULL, ## TODO: check this for file creation sadness
             alsoExtract = "similar",  leaflet = FALSE, method = "ngb")
options(map.useParallel = TRUE)

ccs <- ml@metadata[CC == TRUE & !(layerName == "CC TSF"), ]
CCs <- maps(ml, layerName = ccs$layerName)
CCstack <- raster::stack(CCs)
CCstack[CCstack[] < 0] <- 0
CCstack[CCstack[] > 10] <- 10
CCstack <- CCstack * 10

CCvtm <- Cache(pemisc::makeVegTypeMap, CCstack, vegLeadingProportion)
CCvtmFilename <- file.path(Paths$outputPath, "currentConditionVTM")

ml <- mapAdd(map = ml, CCvtm, layerName = "CC VTM", filename2 = NULL,
             leaflet = FALSE, #isRasterToMatch = FALSE,
             analysisGroup1 = "CC",
             #tsf = file.path(Paths$inputPath, fname_age),
             vtm = CCvtmFilename,
             useCache = TRUE)

if (!file.exists(CCvtmFilename)) {
  CCvtm <- writeRaster(CCvtm, filename = CCvtmFilename, overwrite = TRUE)
}

saveRDS(ml, file.path(Paths$outputPath, "ml.rds"))

######################################################
# Dynamic Simulation
######################################################

## scfm stuff:
mapDim <- 200
defaultInterval <- NA
defaultPlotInterval <- NA
defaultInitialSaveTime <- NA

times <- list(start = 0, end = endTime)
modules <- list("LandWeb_output",
                "LandMine",
                "Boreal_LBMRDataPrep", "LBMR",
                "timeSinceFire")
scfmModules <- list("andisonDriver_dataPrep", "andisonDriver", "scfmLandcoverInit",
                    "scfmIgnition", "ageModule", "scfmRegime", "scfmEscape", "scfmSpread")


objects <- list("shpStudyAreaLarge" = studyArea(ml, 1),
                "shpStudyArea" = studyArea(ml, 2),
                "rasterToMatch" = rasterToMatch(ml),
                "fireReturnInterval" = ml$fireReturnInterval,
                "rstFlammable" = rstFlammable,
                "LCC2005" = ml$LCC2005,
                "rstTimeSinceFire" = ml$`CC TSF`,
                "specieslayers" = CCstack,
                "summaryPeriod" = summaryPeriod,
                "useParallel" = 2,
                "vegLeadingProportion" = vegLeadingProportion)
scfmObjects <- list("mapDim" = mapDim)

parameters <- list(
  Boreal_LBMRDataPrep = list(.useCache = eventCaching, .crsUsed = crs(studyArea(ml))),
  fireDataPrep = list(.useCache = eventCaching),
  initBaseMaps = list(.useCache = eventCaching),
  LandMine = list(biggestPossibleFireSizeHa = 5e5,
                  fireTimestep = fireTimestep,
                  burnInitialTime = fireTimestep,
                  .useCache = eventCaching),
  LandWeb_output = list(summaryInterval = summaryInterval),
  LandWebProprietaryData = list(.useCache = eventCaching),
  LBMR = list(
    seedingAlgorithm = if (grepl("noDispersal", runName)) "noDispersal" else "wardDispersal",
    successionTimestep = successionTimestep,
    .useCache = eventCaching
  ),
  timeSinceFire = list(startTime = fireTimestep,
                       .useCache = eventCaching)
)
scfmParams <- list(
  #.progress = list(type = "text", interval = 1),
  ageModule = list(
    initialAge = 100,
    maxAge = 200,
    returnInterval = defaultInterval,
    startTime = times$start,
    .plotInitialTime = times$start,
    .plotInterval = defaultPlotInterval,
    .saveInitialTime = defaultInitialSaveTime,
    .saveInterval = defaultInterval),
  scfmIgnition = list(
    pIgnition = 0.0001,
    returnInterval = defaultInterval,
    startTime = times$start,
    .plotInitialTime = NA,
    .plotInterval = defaultPlotInterval,
    .saveInitialTime = defaultInitialSaveTime,
    .saveInterval = defaultInterval),
  scfmEscape = list(
    p0 = 0.05,
    returnInterval = defaultInterval,
    startTime = times$start,
    .plotInitialTime = NA,
    .plotInterval = defaultPlotInterval,
    .saveInitialTime = defaultInitialSaveTime,
    .saveInterval = defaultInterval),
  scfmSpread = list(
    pSpread = 0.235,
    returnInterval = defaultInterval,
    startTime = times$start,
    .plotInitialTime = times$start,
    .plotInterval = defaultPlotInterval,
    .saveInitialTime = defaultInitialSaveTime,
    .saveInterval = defaultInterval)
)

if (grepl("scfm", runName)) {
  modules <- append(modules[-which(modules == "LandMine")], scfmModules)
  objects <- append(objects, scfmObjects)
  parameters <- append(parameters, scfmParams)
}

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
  mySimOuts <- Cache(simInitAndExperiment, times = times, cl = cl,
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
  quickPlot::dev()
  quickPlot::clearPlot()
  mySim <- simInit(times = times, #cl = cl,
                   params = parameters,
                   modules = modules,
                   outputs = outputs,
                   objects, # do not name this argument -- collides with Cache -- leave it unnamed
                   paths = paths,
                   loadOrder = unlist(modules)
  )
  mySimOut <- spades(mySim, debug = 1)

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
