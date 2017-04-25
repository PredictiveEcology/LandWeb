raster::rasterOptions(maxmemory=4e10, chunksize = 1e9)

#### Some variables

largePatchSizeOptions <- c(500, 1000, 2000)
largePatchesFnLoop <- length(largePatchSizeOptions) - 1 # The number is how many to run, e.g., 1 would be run just 1000
ageClasses <- c("Young", "Immature", "Mature", "Old")
ageClassCutOffs <- c(0, 40, 80, 120)
ageClassZones <- lapply(seq_along(ageClassCutOffs), function(x) {
  if(x < length(ageClassCutOffs)) {
    paste0(ageClassCutOffs[x],"-",ageClassCutOffs[x+1])
  } else {
    paste0(">",ageClassCutOffs[x])
  }
})
experimentReps <- 1 # was 4
maxNumClusters <- 5 # use 0 to turn off # otherwise detectCPUs() - 1
library(raster)
library(fpCompare)

fireTimestep <- 10
beginCluster(25, type = "FORK")
#print(raster::getCluster())
if(!exists("globalRasters")) globalRasters <- list()
studyArea <- "LARGE"
#studyArea <- "MEDIUM"
studyArea <- "FULL"
#studyArea <- "SMALL"
successionTimestep <- 10 # was 2
endTime <- 400 # was 4
summaryInterval <- 10#endTime/2 # was 2
summaryPeriod <- c(200, endTime)

try(rm(mySim), silent=TRUE)
useGGplot <- FALSE
##########
aaaa <- Sys.time()
message("Started at ", aaaa)

source("functions.R")
source("shinyModules.R")
source("footers.R")

### Package stuff that should not be run automatically
if (FALSE) {
  pkgNamespaces <- c("htmlwidgets", "shiny", "shinydashboard", "shinyBS", "leaflet",
                     "BH", "RCurl", "RandomFieldsUtils", "R.oo", "R.methodsS3", "SpaDES", "markdown",
                     "visNetwork", "rgexf", "influenceR", "DBI", "viridis", "bit", "parallel",
                     "devtools", "raster", "rgeos", "RSQLite", "magrittr", "raster", "sp",
                     "dplyr", "ggplot2", "maptools", "broom", "ggvis", "rgdal", "grid", "VGAM")
  lapply(pkgNamespaces, function(p) if (!require(p, quietly = TRUE, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE, lib="/usr/local/lib/R/site-library")
  })
  if (!require("RandomFieldsUtils", character.only = TRUE)) install.packages("RandomFieldsUtils")
  if (!require("RandomFields", character.only = TRUE)) install.packages("RandomFields")
}

## Make sure SpaDES is up to date
#if (tryCatch(packageVersion("SpaDES") < "1.3.1.9047", error = function(x) TRUE))
devtools::install_github("PredictiveEcology/SpaDES@development")  
#devtools::install_github("PredictiveEcology/SpaDES@spreadDT2")  
devtools::install_github("achubaty/amc@development")  
#devtools::install_github("YongLuo007/amc@development")  

## Actual loading here -- not as long as the list for shinyapps.io, which fails if only these are 
###  provided. But it is not necessary to library all of them for the app
pkgs <- c("shiny", "shinydashboard", "shinyBS", "leaflet", #"plotly", 
          "data.table",
          "broom", "rgeos", "raster", "rgdal", "grid", "ggplot2", "VGAM", "maptools",
          "dplyr", "data.table", "magrittr", "parallel", "SpaDES", "ggvis", "markdown",
          "amc"# fastRasterize and fastMask functions
)
lapply(pkgs, require, quietly = TRUE, character.only = TRUE)
setDTthreads(4) # data.table multi-threading

if (maxNumClusters > 0) {
  if (!exists("cl")) {
    library(parallel)
    # try(stopCluster(cl), silent = TRUE)
    ncores <- if (Sys.info()[["user"]] == "achubaty") {
      pmin(maxNumClusters, detectCores() / 2)
    } else {
      maxNumClusters
    } 
    
    ncores <-  pmin(ncores, detectCores() - 1) 
    
    message("Spawning ", ncores, " threads")
    if (Sys.info()[["sysname"]] == "Windows") {
      clusterType = "SOCK"
    } else {
      # machine1 <- "W-VIC-A105343.pfc.forestry.ca"
      # machine2 <- "W-VIC-A105342.pfc.forestry.ca"
      # machine3 <- "localhost"
      # NcoresOnEach <- 5 # can put this up to about 35
      # clNames <- c(rep(machine1, NcoresOnEach),
      #              rep(machine2, NcoresOnEach),
      #              rep(machine3, NcoresOnEach))
      # cl <- makeCluster(clNames, type = "PSOCK")
      
      clusterType = "FORK"
    }
    cl <- makeCluster(ncores, type = clusterType)
    if (Sys.info()[["sysname"]] == "Windows") {
      clusterExport(cl = cl, varlist = list("objects", "shpStudyRegion"))
    }
    message("  Finished Spawning multiple threads")
  }
}


## Create mySim
paths <- list(
  cachePath = paste0("appCache", studyArea),
  modulePath = "m", # short name because shinyapps.io can't handle longer than 100 characters
  inputPath = "inputs",
  outputPath = paste0("outputs", studyArea)
)

source("inputMaps.R")
modules <- list("landWebDataPrep", "initBaseMaps", "fireDataPrep", "LandMine",
                "LW_LBMRDataPrep", "LBMR", "timeSinceFire", "LandWebOutput")


fireInitialTime <- fireTimestep
times <- list(start = 0, end = endTime)
objects <- list("shpStudyRegionFull" = shpStudyRegionFull,
                "shpStudySubRegion" = shpStudyRegion,
                "successionTimestep" = successionTimestep,
                "summaryPeriod" = summaryPeriod,
                "useParallel" = if(maxNumClusters) cl else TRUE)
parameters <- list(fireNull = list(burnInitialTime = 1,
                                   returnInterval = 1,
                                   .statsInitialTime = 1),
                   LandWebOutput = list(summaryInterval = summaryInterval),
                   LandMine = list(biggestPossibleFireSizeHa = 5e5, fireTimestep = fireTimestep, 
                                   burnInitialTime = fireInitialTime,
                                   .plotInitialTime = NA),
                   LBMR = list(.plotInitialTime = times$start,
                               .saveInitialTime = NA),
                   initBaseMaps = list(.useCache = FALSE),
                   timeSinceFire = list(startTime = fireInitialTime))
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

outputs$arguments <- I(rep(list(list(overwrite = TRUE, progress = FALSE, datatype="INT2U", format = "GTiff"),
                                list(overwrite = TRUE, progress = FALSE, datatype="INT1U", format = "raster")), 
                           times=NROW(outputs)/length(objectNamesToSave)))

outputs <- as.data.frame(rbindlist(list(outputs, outputs2), fill = TRUE))

mySim <- simInit(times = times, params = parameters, modules = modules,
                 objects = objects, paths = paths, outputs = outputs)

source("mapsForShiny.R")
#devtools::load_all("~/Documents/GitHub/SpaDES/.")
