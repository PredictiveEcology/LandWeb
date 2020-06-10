################################################################################
## species layers
################################################################################

do.call(SpaDES.core::setPaths, paths2)

objects2 <- list(
  "nonTreePixels" = simOutPreamble[["nonTreePixels"]],
  "rasterToMatchLarge" = simOutPreamble[["rasterToMatchLarge"]],
  "sppColorVect" = sppColorVect,
  "sppEquiv" = sppEquivalencies_CA,
  "studyAreaLarge" = simOutPreamble[["studyAreaLarge"]],
  "studyAreaReporting" = simOutPreamble[["studyAreaReporting"]]
)

parameters2 <- list(
  Biomass_speciesData = list(
    "omitNonVegPixels" = TRUE,
    "sppEquivCol" = sppEquivCol,
    "types" = c("KNN", "CASFRI", "Pickell", "ForestInventory"),
    ".plotInitialTime" = .plotInitialTime,
    ".studyAreaName" = studyAreaName,
    ".useCache" = FALSE
  )
)

sppLayersFile <- file.path(Paths$inputPath, paste0("simOutSpeciesLayers_", studyAreaName, ".qs"))
if (isTRUE(rerunSpeciesLayers)) {
  ## delete existing species layers data and cache
  if (pemisc::user("achubaty") && isTRUE(deleteSpeciesLayers)) {
    exts <- c(".tif", ".tif.vat.dbf", ".tif.vat.cpg", ".tif.ovr", ".tif.aux.xml", ".tfw")
    forInvFiles <- vapply(c("BlackSpruce1", "Deciduous1", "Fir1", "Pine1", "WhiteSpruce1"),
                          function(f) {
                            paste0(f, exts)
                          }, character(length(exts))) %>%
      c(., "CurrentCondition.zip") %>%
      file.path(paths2$inputPath, .)
    vapply(forInvFiles, function(f) if (file.exists(f)) file.remove(f) else FALSE, logical(1))
  }

  ## (re)create species layers
  simOutSpeciesLayers <- Cache(simInitAndSpades,
                               times = list(start = 0, end = 1),
                               params = parameters2,
                               modules = c("Biomass_speciesData"),
                               objects = objects2,
                               omitArgs = c("debug", "paths", ".plotInitialTime"),
                               #useCache = "overwrite", ## TODO: remove this workaround
                               useCloud = useCloudCache,
                               cloudFolderID = cloudCacheFolderID,
                               ## make .plotInitialTime an argument, not a parameter:
                               ##  - Cache will see them as unchanged regardless of value
                               .plotInitialTime = .plotInitialTime,
                               paths = paths2,
                               debug = 1)

  saveSimList(Copy(simOutSpeciesLayers), sppLayersFile)
} else {
  dl <- downloadFile(url = "https://drive.google.com/file/d/19vJ8neNoi97nXHLFgTQp-lLepQ80CE7m/view?usp=sharing",
                     targetFile = basename(sppLayersFile),
                     destinationPath = dirname(sppLayersFile),
                     neededFiles = basename(sppLayersFile),
                     archive = NULL,
                     checkSums = Checksums(dirname(sppLayersFile), write = TRUE), needChecksums = 0)
  simOutSpeciesLayers <- loadSimList(sppLayersFile)
  rm(dl)
}

if (!is.na(.plotInitialTime)) {
  lapply(dev.list(), function(x) {
    try(quickPlot::clearPlot(force = TRUE))
    try(dev.off())
  })
  quickPlot::dev(3, width = 18, height = 10)
  grid::grid.rect(0.90, 0.03, width = 0.2, height = 0.06, gp = gpar(fill = "white", col = "white"))
  grid::grid.text(label = runName, x = 0.90, y = 0.03)

  Plot(simOutSpeciesLayers$speciesLayers)
}
