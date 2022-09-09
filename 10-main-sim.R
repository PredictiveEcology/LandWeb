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
                    params = parameters3,
                    modules = modules3,
                    outputs = outputs3,
                    objects = objects3,
                    paths = paths$paths3,
                    loadOrder = unlist(modules3),
                    debug = list(file = list(file = file.path(paths$paths3$outputPath, "sim.log"),
                                             append = TRUE), debug = 1),
                    useCloud = FALSE,
                    cloudFolderID = config.get(config, c("cloud", "cacheDir")),
                    omitArgs = c("debug", "paths"))
}, error = function(e) {
  if (requireNamespace("slackr") & file.exists("~/.slackr")) {
    slackr::slackr_setup()
    slackr::slackr_msg(
      paste0("ERROR in simulation `", config.get(config, c("runInfo", "runName")), "` on host `", .nodename, "`.\n",
             "```\n", e$message, "\n```"),
      channel = config::get("slackchannel"), preformatted = FALSE
    )
    stop(e$message)
  }
})

cat(capture.output(warnings()), file = file.path(paths$paths3$outputPath, "warnings.txt"))

fsim <- simFile("mySimOut", paths$paths3$outputPath, SpaDES.core::end(mySimOut), "qs")
message("Saving simulation to: ", fsim)
saveSimList(sim = mySimOut, filename = fsim, fileBackend = 2)

elapsed <- elapsedTime(mySimOut)
data.table::fwrite(elapsed, file.path(paths$paths3$outputPath, "elapsedTime.csv"))
qs::qsave(elapsed, file.path(paths$paths3$outputPath, "elapsedTime.qs"))

SpaDES.project::notify_slack(config.get(config, c("runInfo", "runName")), config::get("slackchannel"))

#unlink(tempdir(), recursive = TRUE)
