# Overall model times # start is default at 0
endTime <- 1500
summaryInterval <- 20
summaryPeriod <- c(700, endTime)

useParallelCluster <- TRUE
cl6 <- NULL
lapplyFn <- "lapply"

# cacheId for 1000 years: 2e35699c4ade1b4bfa82e864558c7436, 7.3 days - on 342
authenticationType <- list("Free", "Proprietary") # Can do one or both of "Free" "Proprietary"
# Spatial stuff -- determines the size of the area that will be "run" in the simulations
subStudyRegionName <- "SMALL"  #other options: "FULL", "EXTRALARGE", "LARGE", "MEDIUM", "NWT", "SMALL" , "RIA"
                              #other options: "BC", "AB", "SK", "MB" or combinations, please specify in West-East order
cacheId <- list()
