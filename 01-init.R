################################################################################
## Initialization parameters and settings
################################################################################

.starttime <- Sys.time()

if (file.exists(".Renviron")) readRenviron(".Renviron")

library(config)

quickPlot::dev.useRSGD(useRSGD = quickPlot::isRstudioServer()) ## TODO: temporary for Alex's testing

activeDir <- config::get("paths")[["activedir"]]
ageClasses <- c("Young", "Immature", "Mature", "Old") ## LandWebUtils:::.ageClasses
ageClassCutOffs <- c(0, 40, 80, 120)                  ## LandWebUtils:::.ageClassCutOffs
cloudCacheFolderID <- config::get("cloud")[["cachedir"]]
deleteSpeciesLayers <- FALSE
endTime <- config::get("params")[["endtime"]]
eventCaching <- c(".inputObjects", "init")
fireTimestep <- 1
gitPkgPath <- config::get("gitpkgpath")
mapParallel <- TRUE #getOption("Ncpus", parallel::detectCores() / 2)
mapResFact <- config::get("mapresfact")
maxAge <- 400
minFRI <- 25
postProcessOnly <- config::get("postprocess")
rerunSpeciesLayers <- if (grepl("LandWeb", runName)) FALSE else TRUE
restartInterval <- 100
sppEquivCol <- "LandWeb"
successionTimestep <- config::get("params")[["successiontimestep"]]
summaryPeriod <- config::get("params")[["summaryperiod"]]
summaryInterval <- config::get("params")[["summaryinterval"]]
timeSeriesTimes <- config::get("params")[["timeseriestimes"]]
useCloudCache <- config::get("cloud")[["usecloud"]] # only for simInitAndSpades
useDEoptim <- FALSE
usePOM <- FALSE ## NOTE: TO and FROM indices must be defined
useParallel <- 3 ## values > 2 use WAY too much RAM for very little speed increase (too much overhead!)
useRestartR <- config::get("restartr")
useSpades <- if (pemisc::user("emcintir")) TRUE else TRUE
vegLeadingProportion <- 0.8 # indicates what proportion the stand must be in one species group for it to be leading.
                            # If all are below this, then it is a "mixed" stand

################################################################################
reproducible::checkPath(activeDir, create = TRUE)
setwd(activeDir)
