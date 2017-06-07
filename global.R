#devtools::load_all("~/GitHub/SpaDES/.")
devmode <- FALSE # If TRUE, this will skip simInit call, if mySim exists (shave off 5 seconds)
## SpaDES & amc
if (FALSE) {
  devtools::install_github("PredictiveEcology/SpaDES@development")  
  #devtools::install_github("PredictiveEcology/SpaDES@moreCache")  
  devtools::install_github("achubaty/amc@development")  
  devtools::install("~/Documents/GitHub/amc/.")  
  devtools::install("~/Documents/GitHub/SpaDES/.")  
  #devtools::install_github("YongLuo007/amc@development")  
}
## Libraries
pkgs <- c("shiny", "shinydashboard", "shinyBS", "leaflet", "data.table", "fpCompare",
          "broom", "rgeos", "raster", "rgdal", "grid", "ggplot2", "VGAM", #"maptools",
          "dplyr", "data.table", "magrittr", "parallel", "SpaDES", #"ggvis", 
          "markdown",
          "amc"# fastRasterize and fastMask functions
)
lapply(pkgs, require, quietly = TRUE, character.only = TRUE)

#### Some variables
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
maxNumClusters <- 8 # use 0 to turn off
#machines <- c("localhost"=maxNumClusters, "132.156.148.91"=5, "132.156.149.7"=5)
machines <- c("localhost" = maxNumClusters) #, "132.156.148.91"=5, "132.156.149.7"=5)

if (Sys.info()["sysname"] != "Windows") beginCluster(25, type = "FORK")
setDTthreads(4) # data.table multi-threading

# Time steps
fireTimestep <- 1
successionTimestep <- 10 # was 2
endTime <- 800 # was 4
summaryInterval <- 10#endTime/2 # was 2
summaryPeriod <- c(500, endTime)

# Spatial stuff
studyArea <- "EXTRALARGE"
#studyArea <- "LARGE"
#studyArea <- "MEDIUM"
#studyArea <- "FULL"
#studyArea <- "SMALL"
raster::rasterOptions(maxmemory = 4e10, chunksize = 1e9)

# shiny variables
useGGplotForHists <- FALSE

# sourcing other files
source("functions.R")
source("shinyModules.R")
source("footers.R")

### Package stuff that should not be run automatically
if (FALSE) {
  SpaDESDeps <- miniCRAN::pkgDep("SpaDES")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
  if (length(new.packages)) install.packages(new.packages)
  
  pkgNamespaces <- c("htmlwidgets", "shiny", "shinydashboard", "shinyBS", "leaflet",
                     "BH", "RCurl", "RandomFieldsUtils", "R.oo", "R.methodsS3", "SpaDES", "markdown",
                     "visNetwork", "rgexf", "influenceR", "DBI", "viridis", "bit", "parallel",
                     "devtools", "raster", "rgeos", "RSQLite", "magrittr", "raster", "sp",
                     "dplyr", "ggplot2", "maptools", "broom", "ggvis", "rgdal", "grid", "VGAM")
  lapply(pkgNamespaces, function(p) if (!require(p, quietly = TRUE, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE, lib = "/usr/local/lib/R/site-library")
  })
  if (!require("RandomFieldsUtils", character.only = TRUE)) install.packages("RandomFieldsUtils")
  if (!require("RandomFields", character.only = TRUE)) install.packages("RandomFields")
}

if (maxNumClusters > 0) {
  # get current IP -- will be Master
  if (!exists("cl")) {
    library(parallel)
    # try(stopCluster(cl), silent = TRUE)
    ncores <- if (Sys.info()[["user"]] == "achubaty") {
      pmin(maxNumClusters, detectCores() / 2)
    } else {
      maxNumClusters
    } 
    
    ncores <-  pmin(ncores, detectCores() - 1) 
    
    clNames <- rep("localhost", ncores)
    if (length(machines) > 1) {
      currIP <- system("ifconfig", intern = TRUE) %>%
        split(cumsum(!nzchar(.))) %>%
        .[unlist(lapply(., function(y) any(grepl("eth1", y))))] %>%
        unlist(recursive = FALSE) %>%
        .[grep("inet addr", .)] %>%
        strsplit(., split = " {2,}") %>%
        unlist(recursive = FALSE) %>%
        grep("inet addr:", ., value = TRUE) %>%
        gsub("inet addr:", "\\1", .) %>% 
        unname()
      
        clNames <- rep(names(machines), machines)
        clusterType = "PSOCK"
        cl <- makeCluster(clNames, type = clusterType, master = currIP)
    } else {
      if (Sys.info()[["sysname"]] == "Windows") {
        clusterType = "SOCK"
      } else {
        clusterType = "FORK"
      }
      cl <- makeCluster(ncores, type = clusterType)
    }
    # if (!all(unlist(lapply(cl, function(x) is(x, "forknode"))))) {
    #   clusterExport(cl = cl, varlist = list("objects", "shpStudyRegion"))
    # }
    message("  Finished Spawning ",length(cl)," threads on ", paste(names(machines), collapse = ", "))
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
                "useParallel" = if (maxNumClusters) cl else TRUE)
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

outputs$arguments <- I(rep(list(list(overwrite = TRUE, progress = FALSE, datatype = "INT2U", format = "GTiff"),
                                list(overwrite = TRUE, progress = FALSE, datatype = "INT1U", format = "raster")),
                           times = NROW(outputs)/length(objectNamesToSave)))

outputs <- as.data.frame(rbindlist(list(outputs, outputs2), fill = TRUE))

# clean up previous runs -- really should always start with a fresh R session (Ctrl-Shft-10)
#try(rm(mySim), silent=TRUE)
skipSimInit <- FALSE
if (devmode) if (!exists("mySim", envir = .GlobalEnv)) skipSimInit <- TRUE

if (!skipSimInit)
  mySim <<- simInit(times = times, params = parameters, modules = modules,
                    objects = objects, paths = paths, outputs = outputs)

source("mapsForShiny.R")
#devtools::load_all("~/GitHub/SpaDES/.")
startTime <- st <- Sys.time()
