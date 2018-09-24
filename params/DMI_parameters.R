useCache342 <- FALSE

numClusters <- 3
source(file.path("R", "ThousandYrFreeProprietary.R"))
subStudyRegionName <- "DMI"
authenticationType <- list("Free", "Proprietary") # Can do one or both of "Free" "Proprietary"
cacheId <- list()

# cacheId$loadStudyRegions <- "a195b4a919b1dae9744fdc04306615d9"

## reproducible::showCache("cache/DMI", userTags = "simInitAndExperiment")[tagKey=="cacheId"]$tagValue
# cacheId$simInitAndExperiment <- "285d3cf6b5075e869e1e705900c02f6a"

# cacheId$tsfRasters <- "0d7bac03ab37e9bfb2ac190c323fbf43"
# cacheId$tsfRasterTilePaths <- "d9bfadeabdb2abd4ca5867ebf8d77d85"
# cacheId$vtmsTifs <- "a2ca69575d8068a307e733663831c5ea"
# cacheId$vtmRasters <- "61e5e93584226088dfe27ac3cd344bf2"
# cacheId$vtmRasterTilePaths <- "5f3c7c2bb6330a1e29c398a538269f5e"

## reproducible::showCache("cache/DMI", userTags = "currentConditions")[tagKey=="cacheId"]$tagValue
# cacheId$currentConditions <- "a7b2adc999108768c8a889c6ea8d08f6"

## reproducible::showCache("cache/DMI", userTags = "reportingAndLeadingFn")[tagKey=="cacheId"]$tagValue
# cacheId$ReportingAndLeadingFn <- "5acd6042606dddd6c7e5137827ae685b"

## reproducible::showCache("cache/DMI", userTags = "leadingByStage")[tagKey=="cacheId"]$tagValue
# cacheId$leadingByStage <- "6d1230eb2c7623fd86718f11b48fc7aa"

## reproducible::showCache("cache/DMI", userTags = "simInitAndSpades")[tagKey=="cacheId"]$tagValue
# cacheId$simInitAndSpades <- "1c10e2cae09f13951f444ad67875b4de"
