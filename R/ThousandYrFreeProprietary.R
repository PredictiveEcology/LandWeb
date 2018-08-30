if (Sys.info()["sysname"] == "Linux" &&
    parallel::detectCores() > numClusters &&
    numClusters > 0) {
  useParallelCluster <- TRUE
  cl6 <- NULL
} else {
  useParallelCluster <- FALSE
  cl6 <- NULL
}
lapplyFn <- if (useParallelCluster) "parLapplyLB" else "lapply"

pathToLandWebApp <- "C:/Eliot/" ## not used anywhere in the code; just a helper for Eliot

# Overall model times # start is default at 0
endTime <- 1500
summaryInterval <- 20
summaryPeriod <- c(700, endTime)

# cacheId for 1000 years: 2e35699c4ade1b4bfa82e864558c7436, 7.3 days - on 342
authenticationType <- list("Free", "Proprietary") # Can do one or both of "Free" "Proprietary"
