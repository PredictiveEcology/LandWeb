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

if (isFALSE(useRestartR)) {
  qs::qsave(NULL, simFile("mySimOut", Paths$outputPath, 0, "qs"))
  nRestarts <- ceiling(endTime / restartInterval)
  restartIteration <- list.files(Paths$outputPath, pattern = "mySimOut_") %>%
   substr(., 10, 13) %>%
   as.numeric() %>%
   max() %>%
   `/`(., restartInterval)

  if (restartIteration == 0) {
    times3$end <- restartInterval
    tryCatch({
      mySimOut <- Cache(simInitAndSpades, times = times3, #cl = cl,
                        params = parameters3,
                        modules = modules3,
                        outputs = outputs3,
                        objects = objects3,
                        paths = paths3,
                        loadOrder = unlist(modules3),
                        debug = list(file = list(file = file.path(Paths$outputPath, "sim.log"),
                                                 append = TRUE), debug = 1),
                        useCloud = FALSE,
                        cloudFolderID = cloudCacheFolderID,
                        omitArgs = c("debug", "paths", ".plotInitialTime"),
                        .plotInitialTime = .plotInitialTime)
    }, error = function(e) {
      if (requireNamespace("slackr") & file.exists("~/.slackr")) {
        slackr::slackr_setup()
        slackr::text_slackr(
          paste0("ERROR in simulation `", runName, "` on host `", Sys.info()[["nodename"]], "`.\n",
                 "```\n", e$message, "\n```"),
          channel = config::get("slackchannel"), preformatted = FALSE
        )
        stop(e$message)
      }
    })
  } else {
   frds <- simFile("mySimOut", Paths$outputPath, restartIteration * restartInterval, "rds")
   fqs <- raster::extension(frds, "qs")

    if (file.exists(fqs)) {
      mySimOut <- loadSimList(fqs)
    } else if (file.exists(frds)) {
      mySimOut <- readRDS(frds)
    } else {
      stop("suitable simulation save file not found.")
    }

    Require(packages(mySimOut))

    SpaDES.core::end(mySimOut) <- min((restartIteration + 1) * restartInterval, endTime)

    tryCatch({
      mySimOut <- spades(mySimOut)
    }, error = function(e) {
      if (requireNamespace("slackr") & file.exists("~/.slackr")) {
        slackr::slackr_setup()
        slackr::text_slackr(
          paste0("ERROR in simulation `", runName, "` on host `", Sys.info()[["nodename"]], "`.\n",
                 "```\n", e$message, "\n```"),
          channel = config::get("slackchannel"), preformatted = FALSE
        )
        stop(e$message)
      }
    })
  }
} else {
  tryCatch({
    mySimOut <- Cache(simInitAndSpades, times = times3, #cl = cl,
                      params = parameters3,
                      modules = modules3,
                      outputs = outputs3,
                      objects = objects3,
                      paths = paths3,
                      loadOrder = unlist(modules3),
                      debug = list(file = list(file = file.path(Paths$outputPath, "sim.log"),
                                               append = TRUE), debug = 1),
                      useCloud = FALSE,
                      cloudFolderID = cloudCacheFolderID,
                      omitArgs = c("debug", "paths", ".plotInitialTime"),
                      .plotInitialTime = .plotInitialTime)
  }, error = function(e) {
    if (requireNamespace("slackr") & file.exists("~/.slackr")) {
      slackr::slackr_setup()
      slackr::text_slackr(
        paste0("ERROR in simulation `", runName, "` on host `", Sys.info()[["nodename"]], "`.\n",
               "```\n", e$message, "\n```"),
        channel = config::get("slackchannel"), preformatted = FALSE
      )
      stop(e$message)
    }
  })
}

fsim <- simFile("mySimOut", Paths$outputPath, SpaDES.core::end(mySimOut), "qs")
message("Saving simulation to: ", fsim)
saveSimList(sim = mySimOut, filename = fsim)

# if (restartIteration == (endTime / restartInterval)) {
  if (requireNamespace("slackr") & file.exists("~/.slackr")) {
    slackr::slackr_setup()
    slackr::text_slackr(
      paste0("Simulation `", runName, "` completed on host `", Sys.info()[["nodename"]], "`."),
      channel = config::get("slackchannel"), preformatted = FALSE
    )
  }
# }

unlink(tempdir())
