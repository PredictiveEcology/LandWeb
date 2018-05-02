

if (Sys.info()["sysname"]=="Linux" && parallel::detectCores()>numClusters && numClusters > 0) {
  useParallelCluster <- FALSE
  lapplyFn <- "parLapplyLB"
} else {
  useParallelCluster <- FALSE
  cl6 <- NULL
  lapplyFn <- "lapply"
}

pathToLandWebApp <- "C:/Eliot/"

# Overall model times # start is default at 0
endTime <- 1500
summaryInterval <- 20
summaryPeriod <- c(700, endTime)

# cacheId for 1000 years: 2e35699c4ade1b4bfa82e864558c7436, 7.3 days - on 342
authenticationType <- list("Free", "Proprietary") # Can do one or both of "Free" "Proprietary"
# Spatial stuff -- determines the size of the area that will be "run" in the simulations
