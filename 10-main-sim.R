################################################################################
## main simulation
################################################################################

if (!is.na(.plotInitialTime)) {
  quickPlot::dev(4, width = 18, height = 10)
  grid::grid.rect(0.90, 0.03, width = 0.2, height = 0.06, gp = gpar(fill = "white", col = "white"))
  grid::grid.text(label = runName, x = 0.90, y = 0.03)
}

data.table::setDTthreads(useParallel)
options("spades.recoveryMode" = TRUE)

saveRDS(NULL, simFile("mySimOut", Paths$outputPath, 0))
nRestarts <- ceiling(endTime / restartInterval)
restartIteration <- list.files(Paths$outputPath, pattern = "mySimOut_") %>%
  substr(., 10, 13) %>%
  as.numeric() %>%
  max() %>%
  `/`(., restartInterval)

if (restartIteration == 0) {
  times3$end <- restartInterval
  mySimOut <- Cache(simInitAndSpades, times = times3, #cl = cl,
                    params = parameters3,
                    modules = modules3,
                    outputs = outputs3,
                    objects = objects3,
                    paths = paths3,
                    loadOrder = unlist(modules3),
                    debug = 1,
                    useCloud = FALSE, #useCloudCache, #!isFALSE(getOption("reproducible.futurePlan")),
                    cloudFolderID = cloudCacheFolderID,
                    omitArgs = c("debug", "paths", ".plotInitialTime"),
                    #debug = 'message(paste(unname(current(sim)), collapse = " "), try(print(names(sim$cohortData))))',
                    .plotInitialTime = .plotInitialTime
  )
} else {
  mySimOut <- readRDS(simFile("mySimOut", Paths$outputPath, restartIteration * restartInterval))

  Require(packages(mySimOut))

  SpaDES.core::end(mySimOut) <- min((restartIteration + 1) * restartInterval, endTime)

  mySimOut <- spades(mySimOut)
}

fsim <- simFile("mySimOut", Paths$outputPath, SpaDES.core::end(mySimOut))
message("Saving simulation to: ", fsim)
saveRDS(mySimOut, fsim)
