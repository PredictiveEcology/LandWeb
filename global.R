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
maxNumClusters <- 0#35 # use 0 to turn off # otherwise detectCPUs() - 1
if(!exists("globalRasters")) globalRasters <- list()
endTime <- 800 # was 4
studyArea <- "LARGE"
studyArea <- "MEDIUM"
#studyArea <- "SMALL"
#studyArea <- "VERYSMALL"
successionTimestep <- 10 # was 2
summaryInterval <- 10#endTime/2 # was 2
summaryPeriod <- c(600, endTime)


useGGplot <- FALSE
##########
message("Started at ", Sys.time())

source("functions.R")
source("shinyModules.R")
source("footers.R")

if(FALSE) { # THese are all "dangerous" for development only... 
  # in the sense that they should never be run inadvertently
  # To rerun the spades initial call, delete the mySim object in the .GlobalEnv ##
  SpaDES::clearCache(cacheRepo = file.path("appCache", studyArea))
  SpaDES::clearCache(cacheRepo = "appCache/studyRegion/")
  rm(mySim)
  rm(cl)
  file.remove(dir("outputs", recursive = TRUE, full.names = TRUE))
  unlink("outputs", force = TRUE)
  unlink(file.path("appCache", studyArea), force = TRUE, recursive = TRUE)
  file.remove("mySimDigestSaved.rds", "mySimSaved.rds")
}

if (FALSE) { # For pushing to shinyapps.io
  message("Started at: ",Sys.time())
  allFiles <- dir(recursive = TRUE)
  allFiles <- grep(allFiles, pattern = "^R-Portable", invert = TRUE, value = TRUE)
  allFiles <- grep(allFiles, pattern = "^appCache", invert = TRUE, value = TRUE)
  allFiles <- grep(allFiles, pattern = "^outputs", invert = TRUE, value = TRUE)
  print(paste("Total size:", sum(unlist(lapply(allFiles, function(x) file.info(x)[, "size"]))) / 1e6, "MB"))
  #rsconnect::deployApp(appName = "LandWebDemo", appFiles = allFiles, appTitle = "LandWeb Demo",
  #                     contentCategory = "application")  
  rsconnect::deployApp(appName = "LandWebDemoDev", appFiles = allFiles, 
                       appTitle = "LandWeb Demo",
                       contentCategory = "application")  
  
}

print(getwd())

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
#  devtools::install_github("PredictiveEcology/SpaDES@development")  
devtools::install_github("PredictiveEcology/SpaDES@EliotCacheMultiOS")  
if(FALSE) {
  library(devtools)
  library(withr);
  with_libpaths(new = "/usr/local/lib/R/site-library/", install_github("PredictiveEcology/SpaDES@EliotCacheMultiOS"))
  with_libpaths(new = "R-Portable/App/R-Portable/library/", install_github("PredictiveEcology/SpaDES@EliotCacheMultiOS"))
}

## Actual loading here -- not as long as the list for shinyapps.io, which fails if only these are 
###  provided. But it is not necessary to library all of them for the app
pkgs <- c("shiny", "shinydashboard", "shinyBS", "leaflet", #"plotly", 
          "broom", "rgeos", "raster", "rgdal", "grid", "ggplot2", "VGAM", "maptools",
          "dplyr", "data.table", "magrittr", "parallel", "SpaDES", "ggvis", "markdown")
lapply(pkgs, require, quietly = TRUE, character.only = TRUE)

## For shinyapps.io -- needs to see explicit require statements
if (FALSE) {
  require(shiny)
  require(shinydashboard)
  require(shinyBS)
  require(BH)
  require(RCurl)
  require(RandomFieldsUtils)
  require(R.oo)
  require(R.methodsS3)
  require(SpaDES)
  library(fastmatch)
  require(visNetwork)
  require(rgexf)
  require(influenceR)
  require(DBI)
  require(viridis)
  require(htmlwidgets)
  require(bit)
  require(devtools)
  require(raster)
  require(rgeos)
  require(RSQLite)
  require(magrittr)
  require(raster)
  require(sp)
  require(VGAM)
  require(dplyr)
  require(ggplot2)
  require(maptools)
  require(broom)
  require(ggvis)
  require(rgdal)
  require(grid)
  require(data.table)
  require(leaflet)
  require(parallel)
  require(markdown)
}

curDir <- getwd()
setwd(curDir)



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


times <- list(start = 0, end = endTime)
objects <- list("shpStudyRegionFull" = shpStudyRegionFull,
                "shpStudySubRegion" = shpStudyRegion,
                "successionTimestep" = successionTimestep,
                "summaryPeriod" = summaryPeriod)
parameters <- list(fireNull = list(burnInitialTime = 1,
                                   returnInterval = 1,
                                   .statsInitialTime = 1),
                   LandWebOutput = list(summaryInterval = summaryInterval),
                   LBMR = list(.plotInitialTime = times$start,
                               .saveInitialTime = NA),
                   initBaseMaps = list(.useCache = FALSE))
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

if (exists("mySim")) {
  if (readRDS(file = "mySimDigestSaved.rds") == digest::digest(mySim)) {
    needMySim <- FALSE
  } else {
    needMySim <- TRUE
  }
} else {
  needMySim <- TRUE
}
if (needMySim) {
  mySim <- simInit(times = times, params = parameters, modules = modules,
                   objects = objects, paths = paths, outputs = outputs)
  saveRDS(digest::digest(mySim), file = "mySimDigestSaved.rds")
} 

rm(objects)

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
      clusterType = "FORK"
    }
    cl <- makeCluster(ncores, type = clusterType)
    if (Sys.info()[["sysname"]] == "Windows") {
      clusterExport(cl = cl, varlist = list("objects", "shpStudyRegion"))
    }
    message("  Finished Spawning multiple threads")
  }
}


