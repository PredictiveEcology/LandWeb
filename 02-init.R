################################################################################
## Initialization parameters and settings
################################################################################

.starttime <- Sys.time()

if (file.exists(".Renviron")) readRenviron(".Renviron")

quickPlot::dev.useRSGD(useRSGD = quickPlot::isRstudioServer()) ## TODO: temporary for Alex's testing

getFRImultiple <- function(runName) {
  frim <- strsplit(runName, "_")[[1]] %>%
    grep("fri", ., value = TRUE) %>%
    substr(., 4, 6) %>%
    as.numeric(.)

  if (identical(frim, numeric(0))) frim <- 1 ## use 1 when not specified, e.g. for old runs

  frim
}

getMapResFact <- function(runName) {
  res <- strsplit(runName, "_")[[1]] %>%
    grep("res", ., value = TRUE) %>%
    substr(., 4, 6) %>%
    as.integer(.)

  if (identical(res, integer(0))) res <- 250 ## use 250 when not specified, e.g. for old runs

  if (res %in% c(50, 125, 250)) {
    250 / res
  } else {
    warning("res should be one of 250, 125, or 50. using value specified by config.yml.")
    config::get("mapresfact")
  }
}

activeDir <- config::get("paths")[["activedir"]]
ageClasses <- c("Young", "Immature", "Mature", "Old") ## LandWebUtils:::.ageClasses
ageClassCutOffs <- c(0, 40, 80, 120)                  ## LandWebUtils:::.ageClassCutOffs
cacheDir <- config::get("paths")[["cachedir"]]
cloudCacheFolderID <- config::get("cloud")[["cachedir"]]
delayStart <- config::get("delaystart")
deleteSpeciesLayers <- FALSE
endTime <- config::get("params")[["endtime"]]
eventCaching <- c(".inputObjects", "init")
fireTimestep <- 1
friMultiple <- getFRImultiple(runName)
forestedLCCClasses <- if (grepl("no2032", runName)) c(1:15, 34:36) else c(1:15, 20, 32, 34:36)
gitPkgPath <- config::get("gitpkgpath")
landwebVersion <- config::get("version")
mapParallel <- TRUE #getOption("Ncpus", parallel::detectCores() / 2)
mapResFact <- getMapResFact(runName)
maxAge <- 400
minFRI <- 25
postProcessOnly <- config::get("postprocess")
rerunDataPrep <- if (grepl("LandWeb", runName)) FALSE else TRUE
rerunSpeciesLayers <- if (grepl("LandWeb", runName)) FALSE else TRUE
restartInterval <- 100
rep <- config::get("rep")
scratchDir <- config::get("paths")[["scratchdir"]]
sppEquivCol <- "LandWeb"
studyAreaName <- if (grepl("FMU", runName)) {
  paste(strsplit(runName, "_")[[1]][1:2], collapse = "_")
} else {
  strsplit(runName, "_")[[1]][1]
}
succession <- !grepl("noSuccession", runName)
successionTimestep <- config::get("params")[["successiontimestep"]]
summaryPeriod <- config::get("params")[["summaryperiod"]]
summaryInterval <- config::get("params")[["summaryinterval"]]
timeSeriesTimes <- config::get("params")[["timeseriestimes"]]
useCloudCache <- config::get("cloud")[["usecloud"]] # only for simInitAndSpades
useDEoptim <- FALSE
usePOM <- FALSE ## NOTE: TO and FROM indices must be defined
useParallel <- 2 ## values > 2 use WAY too much RAM for very little speed increase (too much overhead!)
useRestartR <- config::get("restartr")
useSpades <- if (Sys.info()[["user"]] == "emcintir") TRUE else TRUE
vegLeadingProportion <- 0.8 # indicates what proportion the stand must be in one species group for it to be leading.
                            # If all are below this, then it is a "mixed" stand
.plotInitialTime <- if (isTRUE(config::get("plot"))) 0 else NA
