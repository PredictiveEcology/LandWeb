library("Require")
Require("data.table")
Require("magrittr")
Require("raster")
Require("SpaDES.core")
Require("LandWebUtils")
Require("ggplot2")
Require("patchwork")
Require("kSamples")

outputDir <- "~/GitHub/LandWeb/outputs"
simAreas <- list.dirs(outputDir, recursive = FALSE, full.names = FALSE) %>%
  grep("E14|L11|LandWeb|prov|SprayLake|v3", ., invert = TRUE, value = TRUE) ## omit some runs

nodes <- min(getOption("Ncpus", parallel::detectCores() / 2), length(simAreas))
cl <- parallel::makeForkCluster(nnodes = nodes)
parallel::clusterExport(cl, c("outputDir"))
parallel::clusterEvalQ(cl, {
  library("Require")
  Require("data.table")
  setDTthreads(2)
})

allFireSizes <- parallel::parLapplyLB(cl = cl, simAreas, function(area) {
  reps <- list.dirs(file.path(outputDir, area), recursive = FALSE, full.names = FALSE) %>%
    grep("boxplots|histograms|tiles", ., invert = TRUE, value = TRUE)

  fireSizes <- NULL
  lapply(reps, function(rep) {
    simFile <- file.path(outputDir, area, rep, "mySimOut_1000.rds")

    if (!file.exists(simFile))
      simFile <- file.path(outputDir, area, rep, "mySimOut_year1000.rds") ## try alt/older name

    if (file.exists(simFile)) {
      message("Loading file ", simFile, "...")
      mySimOut <- readRDS(simFile)
      message("... loaded file ", simFile, ".")

      ## fire sizes
      if (!is.null(mySimOut$fireSizes)) {
        fs <- rbindlist(mySimOut$fireSizes, idcol = "year")
        fs[, `:=`(simArea = area, rep = rep)]
        setcolorder(fs, c("simArea", "rep", "year", "size", "maxSize"))
        setnames(fs, old = c("size", "maxSize"), new = c("simSize", "expSize"))
      } else {
        NULL
      }
    }
  }) %>%
    rbindlist()
}) %>%
  rbindlist()

parallel::stopCluster(cl)

setDTthreads(8)
allFireSizes[, simArea := cleanAreaName(simArea)]

fAllFireSizes <- file.path(outputDir, "allFireSizes.csv")
fwrite(allFireSizes, fAllFireSizes)

googledrive::drive_update(file = googledrive::as_id("1lJtPJ4IfzUsMqcEeYCDXvX2MbrtqAySK"),
                          media = fAllFireSizes)

# Plot fire size distributions ----------------------------------------------------------------

if (!exists(allFireSizes))
  allFireSizes <- fread(fAllFireSizes)

lapply(simAreas, function(area) {
  expSizes <- log(allFireSizes[simArea == cleanAreaName(area) & expSize > 0, ]$expSize)
  simSizes <- log(allFireSizes[simArea == cleanAreaName(area) & simSize > 0, ]$simSize)
  xlab <- "log[fireSize] (pixels)"
  ylab1 <- "Frequency"
  ylab2 <- "Median area burned (ha)"
  hsim <- hist(simSizes,
               main = paste("Simulated fire size distribution", cleanAreaName(area)),
               xlab = xlab, ylab = ylab1)
  hexp <- hist(expSizes,
               main = paste("Expected fire size distribution", cleanAreaName(area)),
               xlab = xlab, ylab = ylab1)

  ## TODO: second set of plots per Dave's email
  ## > What I would like is both the number of disturbances on the y axis,
  ## > and the area of disturbances on a second y-axis graph.
  ## Per Eliot: x-axis uses same bins as histogram, with y-axis of median area burned per bin

  allFireSizes[, binIDexp := cut(expSize, hexp$breaks)]
  allFireSizes[, binIDsim := cut(simSize, hsim$breaks)]

  medianFireSizesExp <- allFireSizes[, lapply(.SD, stats::median, na.rm = TRUE), by = expArea, .SDcols = binIDexp]
})

# Test fire size distributions ----------------------------------------------------------------

subsample <- 1:10000
kSamples::ad.test(allFireSizes[simArea == area, ]$simSize[subsample], allFireSizes[simArea == area, ]$expSize[subsample])
