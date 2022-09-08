# Preamble (create study areas, etc.) ---------------------------------------------------------
do.call(SpaDES.core::setPaths, paths$paths1) # Set them here so that we don't have to specify at each call to Cache

objects1 <- list()

parameters1 <- list(
  LandWeb_preamble = list(
    bufferDist = 20000,        ## 20 km buffer
    bufferDistLarge = 50000,   ## 50 km buffer
    friMultiple = config$params$friMultiple,
    mapResFact = config$runInfo$mapResFact,
    minFRI = config$params$minFRI,
    runName = config$runInfo$runName,
    treeClassesLCC = config$params$forestedLCCClasses
  )
)

simOutPreamble <- Cache(simInitAndSpades,
                        times = list(start = 0, end = 1),
                        params = parameters1,
                        modules = c("LandWeb_preamble"),
                        objects = objects1,
                        paths = paths$paths1,
                        debug = 1,
                        omitArgs = c("debug", "paths"),
                        useCloud = config$cloud$useCloud,
                        cloudFolderID = config$cloud$cacheDir)

saveRDS(simOutPreamble$ml, file.path(Paths$outputPath, "ml_preamble.rds")) ## TODO: use `qs::qsave()`

if ("screen" %in% config$params$.plots) {
  lapply(dev.list(), function(x) {
    try(quickPlot::clearPlot(force = TRUE))
    try(dev.off())
  })
  quickPlot::dev(2, width = 18, height = 10)
  grid::grid.rect(0.90, 0.03, width = 0.2, height = 0.06, gp = gpar(fill = "white", col = "white"))
  grid::grid.text(label = runName, x = 0.90, y = 0.03)

  Plot(simOutPreamble$studyAreaReporting, simOutPreamble$studyArea, simOutPreamble$studyAreaLarge)
  Plot(simOutPreamble$rasterToMatchReporting) #bug in quickPlot that makes these not plot together
  Plot(simOutPreamble$rasterToMatch)
  Plot(simOutPreamble$rasterToMatchLarge)
}
