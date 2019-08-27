################################################################################
## Initialization parameters and settings
################################################################################

.starttime <- Sys.time()

quickPlot::dev.useRSGD(useRSGD = quickPlot::isRstudioServer()) ## TODO: temporary for Alex's testing

computeCanadaScratch <- file.path("~/scratch/LandWeb")

gitLocalPath <- if (pemisc::user("achubaty")) "~/GitHub/PredictiveEcology" else "~/GitHub"
activeDir <- if (dir.exists(computeCanadaScratch)) {
  file.path("~/LandWeb")
} else if (pemisc::user("rstudio")) {
  file.path("~/LandWeb")
} else {
  file.path("~/GitHub/LandWeb")
}
ageClasses <- c("Young", "Immature", "Mature", "Old")
ageClassCutOffs <- c(0, 40, 80, 120)
batchMode <- if (pemisc::user("achubaty")) TRUE else FALSE ## NOTE: runName must be defined
cloudCacheFolderID <- "/folders/1b2h5hJlarNMkxdUyJLO5pPUwrQV8m626"
eventCaching <- c(".inputObjects", "init")
fireTimestep <- 1
mapParallel <- TRUE #getOption("Ncpus", parallel::detectCores() / 2)
maxAge <- 400
minFRI <- 25
postProcessOnly <- FALSE
rerunSpeciesLayers <- if (grepl("LandWeb", runName)) FALSE else TRUE
restartInterval <- 100
sppEquivCol <- "LandWeb"
successionTimestep <- 10
useCloudCache <- if (pemisc::user("emcintir")) TRUE else TRUE # only for simInitAndSpades
useDEoptim <- FALSE
usePOM <- if (pemisc::user("achubaty")) FALSE else FALSE ## NOTE: TO and FROM indices must be defined
useParallel <- 3 ## values > 2 use WAY too much RAM for very little speed increase (too much overhead!)
useSpades <- if (pemisc::user("emcintir")) TRUE else TRUE
vegLeadingProportion <- 0.8 # indicates what proportion the stand must be in one species group for it to be leading.
                            # If all are below this, then it is a "mixed" stand

################################################################################
reproducible::checkPath(activeDir, create = TRUE)
setwd(activeDir)
