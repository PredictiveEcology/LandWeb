library("data.table")
library("magrittr")
library("raster")
library("SpaDES.core")
library("LandWebUtils")
library("ggplot2")
library("patchwork")
library("kSamples")

outputDir <- file.path(SpaDES.project::findProjectPath(), "outputs")
simAreas <- list.dirs(outputDir, recursive = FALSE, full.names = FALSE) |>
  grep("E14|L11|LandWeb|prov|SprayLake|v3", x = _, invert = TRUE, value = TRUE) ## omit some runs

simAreas <- "provMB_highDispersal_burnyROS" ## TODO: remove post-testing

nodes <- min(parallelly::availableCores(constraints = "connections"), length(simAreas))
cl <- parallel::makeForkCluster(nnodes = nodes)
parallel::clusterExport(cl, c("outputDir"))
parallel::clusterEvalQ(cl, {
  library("data.table")
  setDTthreads(2)
})

# allFireSizes <- lapply(simAreas, function(area) {
allFireSizes <- parallel::parLapplyLB(cl = cl, simAreas, function(area) {
  reps <- list.dirs(file.path(outputDir, area), recursive = FALSE, full.names = FALSE) |>
    grep("boxplots|histograms|figures|log|tiles", x = _, invert = TRUE, value = TRUE)

  fireSizes <- NULL
  lapply(reps, function(rep) {
    simFile <- file.path(outputDir, area, rep, "mySimOut_1000.qs")

    ## try alt/older names
    if (!file.exists(simFile)) {
      simFile <- file.path(outputDir, area, rep, "mySimOut_1000.rds")
    }

    if (!file.exists(simFile)) {
      simFile <- file.path(outputDir, area, rep, "mySimOut_year1000.rds")
    }

    stopifnot(file.exists(simFile))

    message("Loading file ", simFile, "...")
    mySimOut <- loadSimList(simFile)
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
  }) |>
    rbindlist()
}) |>
  rbindlist()

parallel::stopCluster(cl)

setDTthreads(8)
allFireSizes[, simArea := cleanAreaName(simArea)]

fAllFireSizes <- file.path(outputDir, paste0("allFireSizes_", Sys.Date(), ".csv"))
fwrite(allFireSizes, fAllFireSizes)

googledrive::drive_put(fAllFireSizes, googledrive::as_id("1lJtPJ4IfzUsMqcEeYCDXvX2MbrtqAySK"))

# Plot fire size distributions ----------------------------------------------------------------

if (!exists(allFireSizes))
  allFireSizes <- fread(fAllFireSizes)

pixelRes <- c(250, 250) ## TODO: get from raster using res()
pixelSizeHa <- prod(pixelRes) / 10^4

lapply(simAreas, function(area) {
  area <- cleanAreaName(area)
  subsetDT <- allFireSizes[simArea == area & (expSize > 0 | simSize > 0), ]

  subsetDT[, expSizeHa := expSize * pixelSizeHa]
  subsetDT[, simSizeHa := simSize * pixelSizeHa]

  subsetDT[, logExpSize := log(expSize)]
  subsetDT[, logSimSize := log(simSize)]

  subsetDT[, logExpSizeHa := log(expSize*pixelSizeHa)]
  subsetDT[, logSimSizeHa := log(simSize*pixelSizeHa)]

  ## per Dave's original email:
  ## > What I would like is both the number of disturbances on the y axis,
  ## > and the area of disturbances on a second y-axis graph.
  ## Per Eliot: x-axis uses same bins as histogram, with y-axis of median area burned per bin

  maxLogExpSizeHa <- max(subsetDT$logExpSizeHa)
  maxLogSimSizeHa <- max(subsetDT$logSimSizeHa)

  breaks <- seq(1.0, ceiling(max(maxLogExpSizeHa, maxLogSimSizeHa) / 0.5) * 0.5, 0.5)

  hexp <- hist(subsetDT$logExpSizeHa, breaks = breaks, plot = FALSE)
  hsim <- hist(subsetDT$logSimSizeHa, breaks = breaks, plot = FALSE)

  countsExp <- hexp$counts ## correct counts?
  countsSim <- hsim$counts ## correct counts?

  subsetDT[, binIDexp := cut(logExpSizeHa, hexp$breaks)]
  subsetDT[, binIDsim := cut(logSimSizeHa, hsim$breaks)]

  summaryExpDT <- subsetDT[, lapply(.SD, stats::median, na.rm = TRUE), by = binIDexp, .SDcols = "expSizeHa"]
  summarySimDT <- subsetDT[, lapply(.SD, stats::median, na.rm = TRUE), by = binIDsim, .SDcols = "simSizeHa"]
  setnames(summaryExpDT, "expSizeHa", "medExpSizeHa")
  setnames(summarySimDT, "simSizeHa", "medSimSizeHa")

  summaryExpDT <- summaryExpDT[, medLogExpSizeHa := log(medExpSizeHa)]
  summarySimDT <- summarySimDT[, medLogSimSizeHa := log(medSimSizeHa)]

  midsExp <- cbind(
    as.numeric( sub("\\((.+),.*", "\\1", summaryExpDT$binIDexp) ),
    as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", summaryExpDT$binIDexp) )
  ) |>
    rowMeans()
  summaryExpDT <- summaryExpDT[, midsExp := midsExp]

  midsSim <- cbind(
    as.numeric( sub("\\((.+),.*", "\\1", summarySimDT$binIDsim) ),
    as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", summarySimDT$binIDsim) )
  ) |>
    rowMeans()
  summarySimDT <- summarySimDT[, midsSim := midsSim]

  scaleFactorExp <- max(countsExp) / maxLogExpSizeHa
  scaleFactorSim <- max(countsSim) / maxLogSimSizeHa

  y1col <- "grey20"
  y2col <- "darkred"
  y1lab <- "Total number of fires across all simulated years"
  y2lab <- "Median log[fireSize] (ha)"
  x_lab <- "log[fireSize] (ha)"

  ggHistExp <- ggplot(subsetDT, aes(x = logExpSizeHa)) +
    geom_histogram(breaks = breaks, alpha = 0.5, fill = y1col) +
    stat_summary_bin(data = summaryExpDT,
                     mapping = aes(x = midsExp, y = medLogExpSizeHa * scaleFactorExp),
                     fun = "identity", geom = "point", breaks = breaks, col = y2col) +
    scale_y_continuous(y1lab, sec.axis = sec_axis(~ . / scaleFactorExp , name = y2lab)) +
    xlab(x_lab) +
    ggtitle(paste("Total expected number and size of fires in", area, "study area")) +
    theme_bw() +
    theme(
      axis.title.y.left = element_text(color = y1col),
      axis.text.y.left = element_text(color = y1col),
      axis.title.y.right = element_text(color = y2col),
      axis.text.y.right = element_text(color = y2col)
    )

  ggHistSim <- ggplot(subsetDT, aes(x = logSimSizeHa)) +
    geom_histogram(breaks = breaks, alpha = 0.5, fill = y1col) +
    stat_summary_bin(data = summarySimDT,
                     mapping = aes(x = midsSim, y = medLogSimSizeHa * scaleFactorSim),
                     fun = "identity", geom = "point", breaks = breaks, col = y2col) +
    scale_y_continuous(y1lab, sec.axis = sec_axis(~ . / scaleFactorSim , name = y2lab)) +
    xlab(x_lab) +
    ggtitle(paste("Total simulated number and size of fires in", area, "study area")) +
    theme_bw() +
    theme(
      axis.title.y.left = element_text(color = y1col),
      axis.text.y.left = element_text(color = y1col),
      axis.title.y.right = element_text(color = y2col),
      axis.text.y.right = element_text(color = y2col)
    )

  ## TODO:
  # ggsave(ggHistExp, ...) ## use figPath(sim)
  # ggsave(ggHistSim, ...)

  ## exp vs sim fire sizes
  ggExpVsSimHex <- ggplot(subsetDT, aes(x = expSizeHa, y = simSizeHa)) +
    geom_hex(bins = 50) +
    xlab("Expected fire size (ha)") +
    ylab("Simulated fire size (ha)") +
    ggtitle(paste("Expected vs. simulated fire sizes in", area, "study area")) +
    theme_bw() +
    geom_abline(slope = 1, lty = "dotted")
  ggsave(filename = file.path(outputPath(sim), "figures", "burnSummaries_exp_vs_sim_hex.png"),
         plot = ggExpVsSimHex,
         height = 1000, width = 1000) ## NOTE: keep square aspect ratio

  ggExpVsSimSmooth <- ggplot(subsetDT, aes(x = expSizeHa, y = simSizeHa, group = rep)) +
    geom_smooth() +
    xlab("Expected fire size (ha)") +
    ylab("Simulated fire size (ha)") +
    ggtitle(paste("Expected vs. simulated fire sizes in", area, "study area")) +
    theme_bw() +
    scale_y_continuous(limits = c(0, NA)) +
    scale_x_continuous(limits = c(0, NA)) +
    geom_abline(slope = 1, lty = "dotted")
  ggsave(filename = file.path(outputPath(sim), "figures", "burnSummaries_exp_vs_sim_smooth.png"),
         plot = ggExpVsSimSmooth,
         height = 1000, width = 1000) ## NOTE: keep square aspect ratio

  ## test fire size distributions (very slow...)
  kSamples::ad.test(subsetDT$simSize, subsetDT$expSize) ## TODO: output this somewhere...
})


# compare cumulative burn maps ----------------------------------------------------------------


