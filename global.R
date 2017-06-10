needWorking <- FALSE # this is the "latest working version of SpaDES, LandWeb, packages, modules")
if(needWorking) {
  LandWebVersion <- "7eff8f552a38a5a8121a4ea2ceca3efebfa3923c"
  spadesHash <- "8ca67c8bd7e2862fec21cc4402ebddf8b51ce4dd"
  #spadesHash <- "8cb69c383aaac356e547ede96bbda4d0bc6e5f9e"
  amcHash <- "ca905fdd6847591d351e9bd3d64afdfb1be59684"
}
devmode <- FALSE # If TRUE, this will skip simInit call, if mySim exists (shave off 5 seconds)
## SpaDES & amc
if (FALSE) {
  devtools::install_github("PredictiveEcology/SpaDES@development")  
  devtools::install_github("PredictiveEcology/SpaDES.addins") 
  #devtools::install_github("PredictiveEcology/SpaDES@moreCache")  
  devtools::install_github("achubaty/amc@development")  
  devtools::install("~/Documents/GitHub/amc/.")  
  devtools::install("~/Documents/GitHub/SpaDES/.")  
  #devtools::install_github("YongLuo007/amc@development")  
}

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
machines <- c("localhost" = maxNumClusters) #, "132.156.148.91"=5, "132.156.149.7"=5)


# Time steps
fireTimestep <- 1
successionTimestep <- 10 # was 2
endTime <- 2 # was 4
summaryInterval <- 1#endTime/2 # was 2
summaryPeriod <- c(1, endTime)

# Spatial stuff
studyArea <- "EXTRALARGE"
#studyArea <- "LARGE"
#studyArea <- "MEDIUM"
#studyArea <- "FULL"
studyArea <- "SMALL"

## Create mySim
paths <- list(
  cachePath = paste0("appCache", studyArea),
  modulePath = "m", # short name because shinyapps.io can't handle longer than 100 characters
  inputPath = "inputs",
  outputPath = paste0("outputs", studyArea)
)
if(needWorking) {
  # Need SpaDES and all packages
  dateWorking <- "2017-06-08"
  origLibPaths <- .libPaths()
  if(!file.exists(".checkpoint")) dir.create(".checkpoint")
  if(!require(checkpoint)) install.packages("checkpoint")
  checkpoint(dateWorking, checkpointLocation = ".")
} 

source("packagesUsedFromCRAN.R")

if(needWorking) {
  library(devtools)
  library(git2r) # has git repo internally
  # git remote set-url origin https://github.com/eliotmcintire/LandWeb.git
  
  # Internal caching inside install_github doesn't seem to work for commit-based refs
  updatePkg <- function(pkg, pkgHash, repo) {
    PkgDescr <- read.dcf(system.file(package = pkg, "DESCRIPTION"))
    needPkg <- TRUE
    if("GithubSHA1" %in% colnames(PkgDescr)) {
      if(grepl(paste0("^",pkgHash), PkgDescr[,"GithubSHA1"])) needPkg <- FALSE
    }
    if(needPkg) install_github(paste0(file.path(repo,pkg),"@", pkgHash))
  }
  updatePkg("SpaDES", spadesHash, "PredictiveEcology")
  updatePkg("amc", amcHash, "achubaty")
  
  
  # LandWeb -- get correct version based on git hash
  cred <- cred_token("GITHUB_PAT")
  repo <- git2r::init(".")
  httpsURL <- "https://github.com/eliotmcintire/LandWeb.git"
  sshURL <- "git@github.com:eliotmcintire/LandWeb.git"
  remoteWasHTTPS <- remote_url(repo)==httpsURL
  if(!remoteWasHTTPS)
    remote_set_url(repo, "origin", url=httpsURL)
  
  #remote_set_url(repo, "origin", "https://github.com/eliotmcintire/LandWeb.git")
  #config(repo, user.name="Eliot McIntire", user.email="eliotmcintire@gmail.com")
  #pull(repo, cred)
  
  # Get specific LandWeb version
  hasUncommittedFiles <- !any(grepl(pattern="working directory clean", 
                                    status(repo)))
  if(hasUncommittedFiles) {
    lastCommit <- revparse_single(repo, "HEAD")
    git2r::add(repo, unlist(status(repo)$unstaged))
    tempCommit <- commit(repo, "testing")
  }
  checkout(lookup(repo, LandWebVersion))
  
  # get specific Cache version
  # newCachePath <- "appCacheStable"
  # dir.create(newCachePath)
  # files <- dir(paths$cachePath, recursive = TRUE)
  # sapply(file.path(newCachePath, unique(dirname(files))[-1]), dir.create)
  # file.copy(from=file.path(paths$cachePath, files),
  #           to=file.path(newCachePath, files))
  # paths$cachePath <- newCachePath
  # keepCache(paths$cachePath, LandWebVersion)
  startCacheTime <- Sys.time()

} else {
  devtools::install_github(paste0("PredictiveEcology/SpaDES@", spadesTag) )
  devtools::install_github(paste0("achubaty/amc@", amcHash) )
  
} else {
  LandWebVersion <- "development"
  spadesHash <- "development"
  amcHash <- "development"
  
  devtools::install_github(paste0("PredictiveEcology/SpaDES@", spadesHash) )
  devtools::install_github(paste0("achubaty/amc@", amcHash) )
  
}

#####
if (Sys.info()["sysname"] != "Windows") beginCluster(25, type = "FORK")
setDTthreads(4) # data.table multi-threading
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
startTime <- st <- Sys.time()
