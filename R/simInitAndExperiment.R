simInitAndExperiment <- function(times, params, modules, outputs, objects4sim,
                                  paths, loadOrder, cacheIds4Experiment = NULL,
                                  emptyList, cacheSpades = TRUE) {
  mySims <- Map(simInit, times = times, params = params,
                modules = modules,
                loadOrder = loadOrder,
                outputs = outputs,
                objects = objects4sim,
                paths = paths)

  ##### PRE experiment
  debugCache <- "complete"

  objectsToHash <- emptyList
  objectsToHash <- Map(mySim = mySims, function(mySim) {
                       grep("useParallel", ls(mySim@.envir, all.names = TRUE), value = TRUE, invert = TRUE)
  })

  #########################################

  message("  Starting Experiment...")

  set.seed(seed)
  message("    current seed is: ", seed)

  mySimOuts <- emptyList
  if (!exists("cacheIds4Experiment")) cacheIds4Experiment <- Map(function(y) NULL, names(mySimOuts))
  mySimOuts <- Cache(Map, runExperiment, sim = mySims,
                     cacheId = cacheIds4Experiment,
                     nReps = experimentReps, objectsToHash = objectsToHash,
                     cacheSpades = cacheSpades)
}


# run the simulation experiment
runExperiment <- function(sim, nReps, objectsToHash = "", cacheSpades, cacheId = NULL) {
  args <- list(experiment, sim, replicates = nReps,
               objects = objectsToHash,
               cache = cacheSpades, # cache each spades call
               debug = "paste(Sys.time(), format(Sys.time() - appStartTime, digits = 2),
                 paste(unname(current(sim)), collapse = ' '))",
               .plotInitialTime = NA, cacheId = cacheId,
               clearSimEnv = TRUE,
               omitArgs = c("debug", ".plotInitialTime"))
  args <- args[!unlist(lapply(args, is.null))]
  simOut <- do.call(Cache, args)
  message(attr(simOut, "tags"))

  for (simNum in seq_along(simOut)) {
    simOut[[simNum]]@outputs$file <- lapply(
      strsplit(outputs(simOut[[simNum]])$file,
               split = paste0(outputPath(simOut[[simNum]]), "[\\/]+")),
      function(f) {
        f[[2]]
      }) %>%
      unlist() %>%
      file.path(outputPath(simOut[[simNum]]), .)
  }
  simOut
}
