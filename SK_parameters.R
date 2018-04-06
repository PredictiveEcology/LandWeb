source("ThousandYrFreeProprietary.R")
subStudyRegionName <- "SK"
cacheIds4Experiment <- list("6bf0949aee93abab8dfb72bdd02d9fda", "642cd5d1026730c8fac6ca617236c9f1")
cacheIdTsfRasters <- "e1e5e790999a2ef9f6b344c68755a8f8"
cacheIdTsfRasterTilePaths <- "b5ac714617ee0393eadd63abf9238921"
# cacheIdVtmRasters <- NULL


if (FALSE) {
  cP <- paths$cachePath
  cacheIdVtmRasters <- showCache(cP, userTags = "Map")[tagKey=="cacheId"]$tagValue
}
