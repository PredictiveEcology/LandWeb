simInitAndExperiment <- function( times, params, 
                                  modules, 
                                  outputs, 
                                  objects4sim, 
                                  paths, loadOrder, cacheIds4Experiment = NULL,
                                  emptyList, cacheSpades = TRUE) {
  mySims <- Map(simInit, times = times, params = params, 
                modules = modules, 
                outputs = outputs, 
                objects = objects4sim, 
                paths = paths, loadOrder = lapply(modules, unlist))
  
  ##### PRE experiment
  debugCache <- "complete"
  
  objectsToHash <- emptyList
  objectsToHash <- Map(mySim = mySims, objectsToHash = objectsToHash,
                       MoreArgs = list(guaranteedRun = guaranteedRun),
                       function(mySim, objectsToHash, guaranteedRun) {
                         if (guaranteedRun) {
                           "shpStudySubRegion" # basically only cache on non-.envir objects plus study area
                         } else {
                           grep("useParallel", ls(mySim@.envir, all.names = TRUE), value = TRUE, invert = TRUE)
                         }
                         
                       }) 
  
  #########################################
  # run the simulation experiment
  runExperiment <- function(sim, nReps, objectsToHash = "", cacheSpades, cacheIds = NULL) {
    args <- list(experiment, sim, replicates = nReps,
                 objects = objectsToHash, 
                 cache = cacheSpades, # cache each spades call
                 debug = "paste(Sys.time(), format(Sys.time() - appStartTime, digits = 2),
                 paste(unname(current(sim)), collapse = ' '))",
                 .plotInitialTime = NA, cacheId = cacheIds,
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
  
  message("  Starting Experiment...")
  
  set.seed(seed)
  message("    current seed is: ", seed)
  
  mySimOuts <- emptyList
  # parallel::clusterMap, cl = cl, 
  mySimOuts <- Cache(Map, runExperiment, sim = mySims, cacheIds = cacheIds4Experiment, 
                     nReps = experimentReps, objectsToHash = objectsToHash, 
                     cacheSpades = cacheSpades)
}
