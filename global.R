#try(detach("package:SpaDES", unload=TRUE)); try(detach("package:reproducible", unload=TRUE)); 
#devtools::load_all("~/Documents/GitHub/reproducible/."); devtools::load_all("~/Documents/GitHub/SpaDES/.")
#devtools::install("~/Documents/GitHub/reproducible/."); devtools::install("~/Documents/GitHub/SpaDES.core/.");
#devtools::install("~/Documents/GitHub/SpaDES.tools/."); 
appStartTime <- st <- Sys.time() - 1
message("Started at ", appStartTime)
rsyncToAWS <- FALSE
useGdal2Tiles <- TRUE
eventCaching <- FALSE#"init" #Sys.time()
needWorking <- FALSE # this is the "latest working version of SpaDES, LandWeb, packages, modules")
if(needWorking) {
  LandWebVersion <- "2e3656bb957eb265daad638551c74bf1423ca287"
  spadesHash <- "88e4b394f466498a7aac92bda265a7acf818693c"
  amcHash <- "ca905fdd6847591d351e9bd3d64afdfb1be59684"
}
devmode <- FALSE # If TRUE, this will skip simInit call, if mySim exists (shave off 5 seconds)
## SpaDES & amc
if (FALSE) {
  devtools::install_github("PredictiveEcology/SpaDES.tools")
  devtools::install_github("PredictiveEcology/SpaDES.core")
  devtools::install_github(paste0("PredictiveEcology/SpaDES.core@", spadesHash))  
  devtools::install_github("PredictiveEcology/SpaDES.addins") 
  #devtools::install_github("PredictiveEcology/SpaDES@moreCache")  
  devtools::install_github("achubaty/amc@development")  
  devtools::install("~/Documents/GitHub/amc/.")  
  devtools::install("~/Documents/GitHub/SpaDES.core/.")  
  #devtools::install_github("YongLuo007/amc@development")  
}

#### Some variables
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
maxNumClusters <- 0 # use 0 to turn off
if( grepl("ip", Sys.info()["nodename"])) maxNumClusters <- 0 # on Amazon
machines <- c("localhost" = maxNumClusters) #, "132.156.148.91"=5, "132.156.149.7"=5)


# Time steps
fireTimestep <- 1
successionTimestep <- 10 # was 2
endTime <- 800 # was 4
summaryInterval <- 10#endTime/2 # was 2
summaryPeriod <- c(500, endTime)

# Spatial stuff
#studyArea <- "FULL"
studyArea <- "EXTRALARGE"
studyArea <- "LARGE"
#studyArea <- "MEDIUM"
#studyArea <- "SMALL"
#studyArea <- "NWT"

# leaflet parameters
leafletZoomInit = 5 

## Create mySim
paths <- list(
  cachePath = paste0("appCache", studyArea),
  modulePath = "m", # short name because shinyapps.io can't handle longer than 100 characters
  inputPath = "inputs",
  outputPath = paste0("outputs", studyArea)
)
if(needWorking) {
  # Need SpaDES.core and all packages
  dateWorking <- "2017-06-08"
  origLibPaths <- .libPaths()
  if(!dir.exists(".checkpoint")) dir.create(".checkpoint")
  if(!require(checkpoint)) install.packages("checkpoint")
  checkpoint(dateWorking, checkpointLocation = ".", scanForPackages = FALSE)
} 

source("packagesUsedFromCRAN.R")

if(needWorking) {
  library(devtools)
  library(git2r) # has git repo internally
  # git remote set-url origin https://github.com/eliotmcintire/LandWeb.git
  
  # Internal caching inside install_github doesn't seem to work for commit-based refs
  #  this function is in packagesUsedFromCRAN.R
  updatePkg("SpaDES.core", spadesHash, "PredictiveEcology")
  updatePkg("amc", amcHash, "achubaty")
  
  # LandWeb -- get correct version based on git hash
  browser()
  source("gitCheckout.R")
  checkoutCondition <- checkoutVersion(LandWebVersion)
  
  # get specific Cache version
  startCacheTime <- Sys.time()
  
} else {
  if(FALSE) {
    devtools::install_github(paste0("PredictiveEcology/reproducible@development"))
    devtools::install_github(paste0("PredictiveEcology/SpaDES.core@development"))
    devtools::install_github(paste0("achubaty/amc@development") )
  }
}


#####
# if (Sys.info()["sysname"] != "Windows") beginCluster(4, type = "FORK")
setDTthreads(10) # data.table multi-threading
raster::rasterOptions(maxmemory = 4e10, chunksize = 1e9)


# shiny variables
useGGplotForHists <- FALSE

# sourcing other files
source("functions.R")
source("shinyModules.R")
source("footers.R")

### Package stuff that should not be run automatically
if (FALSE) {
  SpaDESDeps <- miniCRAN::pkgDep("SpaDES.core")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
  if (length(new.packages)) install.packages(new.packages)
  
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
    message("  Finished Spawning ",length(cl)," threads on ", paste(names(machines), collapse = ", "))
  }
}

# Build shpStudyRegion -- Needs to be a polygon with inner polygons
source("inputMaps.R") # source some functions
loadLandisParams(path=paths$inputPath, envir=.GlobalEnv) # assigns 2 Landis objects to .GlobalEnv
shpStudyRegions <- Cache(loadStudyRegion, asPath(file.path(paths$inputPath,"shpLandWEB.shp")), 
                         studyArea = studyArea,
                         crsKNNMaps=crsKNNMaps, cacheRepo=paths$cachePath)
list2env(shpStudyRegions, envir = environment())

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
                   LW_LBMRDataPrep = list(.useCache = eventCaching),
                   LandMine = list(biggestPossibleFireSizeHa = 5e5, fireTimestep = fireTimestep, 
                                   burnInitialTime = fireInitialTime,
                                   .plotInitialTime = NA
                                   , .useCache = eventCaching
                                   ),
                   LBMR = list(.plotInitialTime = times$start,
                               .saveInitialTime = NA
                               , .useCache = eventCaching
                               ),
                   initBaseMaps = list(.useCache = eventCaching),
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
                    objects = objects, paths = paths, outputs = outputs, loadOrder = unlist(modules))

source("mapsForShiny.R")
#saveRDS(out, file = "out.rds")
#rm(out)


if(TRUE) {
  library(gdalUtils)  
  gdalSet <- function() {
    gdal_setInstallation()
    getOption("gdalUtils_gdalPath")
  }
  options(gdalUtils_gdalPath=Cache(gdalSet, cacheRepo = paths$cachePath))
}
if(FALSE){
  library(future)
  message("running future plan(multisession)")
  curFuture <- future::plan()
  if(!is(curFuture, "multisession"))
    plan(multisession)
  
  # TO get this to work
  #  https://stackoverflow.com/questions/5599872/python-windows-importerror-no-module-named-site
  #Cache(system, notOlderThan = Sys.time(),paste("python",
  #                                              file.path(getOption("gdalUtils_gdalPath")[[1]]$path,"gdal_polygonize.py"), 
  #                                              basename(newfilename), basename(shapeFile), "-f \"ESRI Shapefile\""))
  
}

