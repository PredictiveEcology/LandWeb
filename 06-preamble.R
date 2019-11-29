################################################################################
## Preamble (creates study areas, etc.)
################################################################################

do.call(SpaDES.core::setPaths, paths1) # Set them here so that we don't have to specify at each call to Cache

objects1 <- list()

parameters1 <- list(
  LandWeb_preamble = list(
    "minFRI" = minFRI,
    "runName" = runName
  )
)

simOutPreamble <- Cache(simInitAndSpades,
                        times = list(start = 0, end = 1),
                        params = parameters1,
                        modules = c("LandWeb_preamble"),
                        objects = objects1,
                        paths = paths1,
                        debug = 1,
                        omitArgs = c("debug", "paths"),
                        useCloud = useCloudCache, #!isFALSE(getOption("reproducible.futurePlan")),
                        cloudFolderID = cloudCacheFolderID)

saveRDS(simOutPreamble$ml, file.path(Paths$outputPath, "ml_preamble.rds"))

if (!is.na(.plotInitialTime)) {
  lapply(dev.list(), function(x) {
    try(quickPlot::clearPlot(force = TRUE))
    try(dev.off())
  })
  quickPlot::dev(2, width = 18, height = 10)
  grid::grid.rect(0.90, 0.03, width = 0.2, height = 0.06, gp = gpar(fill = "white", col = "white"))
  grid::grid.text(label = runName, x = 0.90, y = 0.03)

  Plot(simOutPreamble$studyAreaReporting, simOutPreamble$studyArea, simOutPreamble$studyAreaLarge)
  Plot(simOutPreamble$rasterToMatchReporting)
  Plot(simOutPreamble$rasterToMatch) # some bug in quickPlot that makes these 2 not plot together
}
