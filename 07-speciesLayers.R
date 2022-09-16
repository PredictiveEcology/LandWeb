################################################################################
## species layers
################################################################################

do.call(SpaDES.core::setPaths, paths$paths2)

parameters2 <- list(
  Biomass_speciesData = list(
    omitNonVegPixels = TRUE,
    sppEquivCol = simOutPreamble[["sppEquivCol"]],
    types = if (grepl("provMB", config.get(config, c("runInfo", "runName")))) {
      c("KNN", "CASFRI", "Pickell", "MBFRI")
    } else {
      c("KNN", "CASFRI", "Pickell", "ForestInventory")
    },
    .plots = config.get(config, c("params", ".plots")),
    .sslVerify = config.get(config, c("params", ".sslVerify")),
    .studyAreaName = config.get(config, c("runInfo", "studyAreaName")),
    .useCache = FALSE
  )
)

objects2 <- list(
  #nonTreePixels = simOutPreamble[["nonTreePixels"]], ## TODO: confirm no longer required
  rasterToMatchLarge = simOutPreamble[["rasterToMatchLarge"]],
  sppColorVect = simOutPreamble[["sppColorVect"]],
  sppEquiv = simOutPreamble[["sppEquiv"]],
  studyAreaLarge = simOutPreamble[["studyAreaLarge"]],
  studyAreaReporting = simOutPreamble[["studyAreaReporting"]]
)

sppLayersFile <- file.path(Paths$inputPath, paste0(
  "simOutSpeciesLayers_", config.get(config, c("runInfo", "studyAreaName")), ".qs"
))

simOutSpeciesLayers <- Cache(simInitAndSpades,
                             times = list(start = 0, end = 1),
                             params = parameters2,
                             modules = c("Biomass_speciesData"),
                             objects = objects2,
                             omitArgs = c("debug", "paths", ".plotInitialTime"),
                             useCache = TRUE,
                             useCloud = config$cloud$useCloud,
                             cloudFolderID = config$cloud$cacheDir,
                             ## make .plotInitialTime an argument, not a parameter:
                             ##  - Cache will see them as unchanged regardless of value
                             paths = paths$paths2,
                             debug = 1)
saveSimList(Copy(simOutSpeciesLayers), sppLayersFile, fileBackend = 2)

if ("screen" %in% config.get(config, c("params", ".plots"))) {
  lapply(dev.list(), function(x) {
    try(quickPlot::clearPlot(force = TRUE))
    try(dev.off())
  })
  quickPlot::dev(3, width = 18, height = 10)
  grid::grid.rect(0.90, 0.03, width = 0.2, height = 0.06, gp = gpar(fill = "white", col = "white"))
  grid::grid.text(label = config.get(config, c("runInfo", "runName")), x = 0.90, y = 0.03)

  Plot(simOutSpeciesLayers$speciesLayers)
}
