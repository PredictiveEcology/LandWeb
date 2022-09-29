################################################################################
## main simulation
################################################################################

if ("screen" %in% config.get(config, c("params", ".plots"))) {
  quickPlot::dev(4, width = 18, height = 10)
  grid::grid.rect(0.90, 0.03, width = 0.2, height = 0.06, gp = gpar(fill = "white", col = "white"))
  grid::grid.text(label = config.get(config, c("runInfo", "runName")), x = 0.90, y = 0.03)
}

data.table::setDTthreads(config.get(config, "useParallel"))
options(spades.recoveryMode = TRUE)

tryCatch({
  mySimOut <- Cache(simInitAndSpades,
                    times = times3, #cl = cl,
                    params = config$params,
                    modules = config$modules$main,
                    outputs = outputs3,
                    objects = objects3,
                    paths = paths$paths3,
                    loadOrder = unlist(config$modules$main),
                    debug = list(file = list(file = file.path(config$paths$paths3$outputPath, "sim.log"),
                                             append = TRUE), debug = 1),
                    useCloud = FALSE, ## TODO param useCloud??
                    cloudFolderID = config$args$cloud$cacheDir,
                    omitArgs = c("debug", "paths"))
  mySimOut@.xData[["._sessionInfo"]] <- projectSessionInfo(prjDir)
}, error = function(e) {
  if (requireNamespace("slackr") & file.exists("~/.slackr")) {
    slackr::slackr_setup()
    slackr::slackr_msg(
      paste0("ERROR in simulation `", config.get(config, c("runInfo", "runName")), "` on host `", .nodename, "`.\n",
             "```\n", e$message, "\n```"),
      channel = config.get(config, "slackChannel"), preformatted = FALSE
    )
    stop(e$message)
  }
})

cat(capture.output(warnings()), file = file.path(paths$paths3$outputPath, "warnings.txt"), sep = "\n")

fsim <- simFile("mySimOut", paths$paths3$outputPath, SpaDES.core::end(mySimOut), "qs")
message("Saving simulation to: ", fsim)
saveSimList(sim = mySimOut, filename = fsim, fileBackend = 2)

# save simulation stats -----------------------------------------------------------------------

elapsed <- elapsedTime(mySimOut)
data.table::fwrite(elapsed, file.path(paths$paths3$outputPath, "elapsedTime.csv"))
qs::qsave(elapsed, file.path(paths$paths3$outputPath, "elapsedTime.qs"))

memory <- memoryUse(mySimOut, max = TRUE)
data.table::fwrite(memory, file.path(paths$paths3$outputPath, "memoryUsed.csv"))
qs::qsave(memory, file.path(paths$paths3$outputPath, "memoryUsed.qs"))

# end-of-sim notifications --------------------------------------------------------------------

SpaDES.project::notify_slack(config.get(config, c("runInfo", "runName")), config.get(config, c("slackChannel")))
