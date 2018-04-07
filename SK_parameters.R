numClusters <- 4
source("ThousandYrFreeProprietary.R")
subStudyRegionName <- "SK"
cacheIds4Experiment <- list("6bf0949aee93abab8dfb72bdd02d9fda", "642cd5d1026730c8fac6ca617236c9f1")
cacheIdTsfRasters <- "e1e5e790999a2ef9f6b344c68755a8f8"
cacheIdTsfRasterTilePaths <- "b5ac714617ee0393eadd63abf9238921"
# cacheIdVtmRasters <- NULL
# was N = 10 for both Free & Proprietary
cacheIdVtmRasterTilePaths <-   c("b5ac714617ee0393eadd63abf9238921") # 10th
cacheIdReportingAndLeadingFn <- c("0d7b0e333487549133ba021d11724c3b") # 2nd one

# cacheIdCurrentConditions <- c("e1e5e790999a2ef9f6b344c68755a8f8", "07ae053f596ed5552d9dfda8e5e2f3d5", 
#   "a71be687fd2c3f354759db14db677898", "2c92741a03ba0e92fa93b6413a41684b", 
#   "27c34918533ac1c6815dadba732b5fa6", "8b4dd8a1c5eedbc3b773ac8bf2cde116", 
#   "744d5997cf6cb3d2e8ee557a2ca6bc11", "b0a6b6aa07ef80020f5ecfd81ea8ad9f", 
#   "f4e1316dfd968b2d2fad29ab99e80d92", "b5ac714617ee0393eadd63abf9238921"
# )

if (FALSE) {
  cP <- paths$cachePath
  cacheIdVtmRasters <- showCache(cP, userTags = "Map")[tagKey=="cacheId"]$tagValue
}
