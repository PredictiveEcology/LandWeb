################################################################################
## species layers
################################################################################

do.call(SpaDES.core::setPaths, paths2)

objects2 <- list(
  "nonTreePixels" = simOutPreamble$nonTreePixels,
  "rasterToMatch" = simOutPreamble$rasterToMatch,
  "rasterToMatchReporting" = simOutPreamble$rasterToMatchReporting,
  "sppColorVect" = sppColorVect,
  "sppEquiv" = sppEquivalencies_CA,
  "studyArea" = simOutPreamble$studyArea,
  "studyAreaLarge" = simOutPreamble$studyAreaLarge,
  "studyAreaReporting" = simOutPreamble$studyAreaReporting
)

parameters2 <- list(
  BiomassSpeciesData = list(
    "omitNonVegPixels" = TRUE,
    "types" = c("KNN", "CASFRI", "Pickell", "ForestInventory"),
    "sppEquivCol" = sppEquivCol,
    ".useCache" = FALSE
  )
)

sppLayersFile <- file.path(Paths$inputPath, paste0("simOutSpeciesLayers_", substr(runName, 1, 8), ".rds"))
if (isTRUE(rerunSpeciesLayers)) {
  ## delete existing species layers data and cache
  if (pemisc::user("achubaty")) {
    exts <- c(".tif", ".tif.vat.dbf", ".tif.vat.cpg", ".tif.ovr", ".tif.aux.xml", ".tfw")
    forInvFiles <- vapply(c("BlackSpruce1", "Deciduous1", "Fir1", "Pine1", "WhiteSpruce1"),
                          function(f) {
                            paste0(f, exts)
                          }, character(length(exts))) %>%
      c(., "CurrentCondition.zip", paste0(c("Abie_sp", "Pice_gla", "Pice_mar", "Pinu_sp", "Popu_sp"), "_overlay.tif")) %>%
      file.path(paths2$inputPath, .)
    vapply(forInvFiles, function(f) if (file.exists(f)) file.remove(f) else FALSE, logical(1))

    unlink(paths2$cachePath, recursive = TRUE)
  }

  ## (re)create species layers
  simOutSpeciesLayers <- Cache(simInitAndSpades,
                               times = list(start = 0, end = 1),
                               params = parameters2,
                               modules = c("BiomassSpeciesData"),
                               objects = objects2,
                               omitArgs = c("debug", "paths", ".plotInitialTime"),
                               useCloud = useCloudCache,
                               cloudFolderID = cloudCacheFolderID,
                               ## make .plotInitialTime an argument, not a parameter:
                               ##  - Cache will see them as unchanged regardless of value
                               .plotInitialTime = .plotInitialTime,
                               paths = paths2,
                               debug = 1)

  saveRDS(Copy(simOutSpeciesLayers), sppLayersFile, version = 3)
} else {
  dl <- downloadFile(url = "https://drive.google.com/file/d/1oSUfp9HTx1eu4zq5ylWtXirZaoPas_9c/view?usp=sharing",
                     targetFile = basename(sppLayersFile),
                     destinationPath = dirname(sppLayersFile),
                     neededFiles = basename(sppLayersFile),
                     archive = NULL,
                     checkSums = Checksums(dirname(sppLayersFile), write = TRUE), needChecksums = 0)
  simOutSpeciesLayers <- readRDS(sppLayersFile)
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
