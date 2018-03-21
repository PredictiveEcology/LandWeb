runExperiment <- function(sim, nReps, objectsToHash = "") {
  if (TRUE) {
    # # Do an initial run for each given study area so that all the data prep can be done once only
    #initialRun1 <- spades(Copy(sim), debug = TRUE)
    # 5 minutes for 6e3 km2
    # 30 minutes for 6e4 km2
    simCopy <- Copy(sim)
    end(simCopy) <- 0
    message("Running Initial spades call")
    initialRun <<- Cache(spades, sim = simCopy, #notOlderThan = Sys.time(),
                         debug = "paste(Sys.time(), paste(unname(current(sim)), collapse = ' '))",
                         objects = "shpStudyRegion",
                         #cacheRepo = cachePath(sim),
                         .plotInitialTime = NA,
                         omitArgs = c("debug", ".plotInitialTime")#,
                         #debugCache = "complete"
                         )
  }

  ##########
  raster::endCluster()
  seed <- sample(1e8, 1)
  #seed <- 792282
  set.seed(seed)
  message("Current seed is: ", seed)
  #startTime <<- st <<- Sys.time()
  message("Running Experiment, starting at time: ", appStartTime)
  args <- list(experiment, sim, replicates = nReps,
               objects = objectsToHash,
               debug = "paste(Sys.time(), format(Sys.time() - appStartTime, digits = 2),
               paste(unname(current(sim)), collapse = ' '))",
               .plotInitialTime = NA,
               clearSimEnv = TRUE,
               #debugCache = "complete",
               omitArgs = c("debug", ".plotInitialTime"))
  args <- args[!unlist(lapply(args, is.null))]
  simOut <- do.call(Cache, args)
  message(attr(simOut, "tags"))
  simOut
}
