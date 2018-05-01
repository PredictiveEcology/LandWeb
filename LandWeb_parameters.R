numClusters <- 3
source("ThousandYrFreeProprietary.R")
subStudyRegionName <- "FULL"
authenticationType <- list("Free", "Proprietary") # Can do one or both of "Free" "Proprietary"
cacheId <- list()

if (Sys.info()["nodename"] == "W-VIC-A105388") {
  oldStyle <- TRUE
  #cacheId$runExperiment <- list("53d4b5f8645c379326613b182c6b660d", "2e35699c4ade1b4bfa82e864558c7436")
  cacheId$simInitAndExperiment <- "76667135a58ee2327ff6f9e7477137ec"
  #cacheIdTsfRasters <- "cc0497d32c2828c81d0eab88d8a7de00"
  cacheId$simInitAndExperiment <- "bf6a8d8416c2975818f8074703ad9482" # 800 years .149.44
  #cacheId$simInitAndExperiment <- "0efd1482ddb59c68a5fe8441404295b0" # SMALL
  #cacheIds4Experiment <- list("53d4b5f8645c379326613b182c6b660d", "2e35699c4ade1b4bfa82e864558c7436")
  #cacheIdTsfRasters <- "cc0497d32c2828c81d0eab88d8a7de00"
  #cacheId4Experiment <- "bf6a8d8416c2975818f8074703ad9482"
  #cacheId$simInitAndExperiment <- "285d3cf6b5075e869e1e705900c02f6a" # 1500 yrs on 132.156.148.172
}

if (Sys.info()["nodename"] == "W-VIC-A105342") {
  cacheId$simInitAndExperiment <- "285d3cf6b5075e869e1e705900c02f6a" # 1500 yrs on 132.156.148.172
  cacheId$tsfRasters <- "0d7bac03ab37e9bfb2ac190c323fbf43"
  cacheId$tsfRasterTilePaths <- "d9bfadeabdb2abd4ca5867ebf8d77d85"
  cacheId$vtmsTifs <- "a2ca69575d8068a307e733663831c5ea"
  cacheId$vtmRasters <- "61e5e93584226088dfe27ac3cd344bf2"
  cacheId$vtmRasterTilePaths <- "5f3c7c2bb6330a1e29c398a538269f5e"
  cacheId$currentConditions <- "c5a1b5b295be3747eab1918786581ec2"
  cacheId$ReportingAndLeadingFn <- "9ec61e989ce7dc6160736eaecacb1346"
  cacheId$calculateLeadingVegType <- NULL
}