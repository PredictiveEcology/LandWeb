library(data.table)
library(magrittr)
library(raster)
library(SpaDES.core)
library(ggplot2)
library(patchwork)

outputDir <- "~/GitHub/LandWeb/outputs"
simAreas <- list.dirs(outputDir, recursive = FALSE, full.names = FALSE) %>%
  grep("E14|L11|LandWeb|SprayLake", ., invert = TRUE, value = TRUE) ## omit some runs

cleanAreaName <- Vectorize(function(area) {
  strsplit(area, "_")[[1]] %>%
    grep("Dispersal|ROS", ., invert = TRUE, value = TRUE) %>%
    paste(., collapse = "_")
})

nodes <- min(getOption("Ncpus", parallel::detectCores() / 2), length(simAreas))
cl <- parallel::makeForkCluster(nnodes = nodes)
parallel::clusterExport(cl, c("outputDir"))
parallel::clusterEvalQ(cl, {
  library(data.table)
  library(raster)
  setDTthreads(2)
})

burnMaps <- parallel::parLapplyLB(cl = cl, simAreas, function(area) {
  reps <- list.dirs(file.path(outputDir, area), recursive = FALSE, full.names = FALSE) %>%
    grep("boxplots|histograms|tiles", ., invert = TRUE, value = TRUE)

  burns <- lapply(reps, function(rep) {
    simFiles <- file.path(outputDir, area, rep, paste0("mySimOut_", seq(100, 1000, 100), ".rds"))
    simFiles <- simFiles[which(file.exists(simFiles))]

    ## burn maps from each interval
    burnMaps <- lapply(simFiles, function(f) {
        message("Loading file ", f, "...")
        mySimOut <- readRDS(f)
        message("... loaded file ", f, ".")

        mySimOut$rstCurrentBurn
    }) %>%
      raster::stack()
  }) %>%
    raster::stack()

  cumulBurns <- sum(burns)
  nYears <- nlayers(burns)

  ## calculate FRI for each FRI polygon within the study area
  f <- file.path(outputDir, area, reps[1], "mySimOut_1000.rds")
  mySimOut <- readRDS(f)

  compareRaster(cumulBurns, mySimOut$fireReturnInterval, mySimOut$rstFlammable, res = TRUE, orig = TRUE)

  toRm <- which(is.na(mySimOut$rstFlammable[]) | mySimOut$rstFlammable[] == 0) ## non-flammable
  cumulBurns[toRm] <- NA
  mySimOut$rstFlammable[toRm] <- NA
  friDT <- data.table(pixelID = 1:ncell(mySimOut$fireReturnInterval),
                      expArea = mySimOut$rstFlammable[],
                      expFRI = mySimOut$fireReturnInterval[],
                      nBurns = cumulBurns[]) %>%
    na.omit(., cols = c("nBurns"))
  areaDT <- friDT[, lapply(.SD, sum, na.rm = TRUE), by = expFRI, .SDcols = "expArea"]
  sumDT <- friDT[, lapply(.SD, sum, na.rm = TRUE), by = expFRI, .SDcols = "nBurns"]
  friDT2 <- merge(areaDT, sumDT)
  friDT2[, simFRI := nYears * expArea / nBurns]
  friDT2[, propFRI := simFRI / expFRI]
  friDT2[, simArea := cleanAreaName(area)]
  setcolorder(friDT2, c("simArea", "expFRI", "expArea", "nBurns", "simFRI", "propFRI"))
  friDT2
}) %>%
  rbindlist()

parallel::stopCluster(cl)

allFireSizes[, simArea := cleanAreaName(simArea)]
fwrite(allFireSizes, file.path(outputDir, "allFireSizes.csv"))
