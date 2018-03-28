function(input, output, session) {
  session$userData$userLoggedIn <- reactiveVal(FALSE)
  session$userData$userAuthorized <- reactiveVal(FALSE)

  ## run additonal server code from server_file.R
  if (file.exists("server_file.R")) source("server_file.R", local = TRUE)

  # simulation initialization
  mySim <- callModule(spades_simInit, "sim_init",
                      times = times4sim(),
                      params = parameters4sim(),
                      modules = modules4sim(),
                      outputs = outputs4sim(),
                      objects = objects4sim(),
                      paths = paths4sim(),
                      loadOrder = unlist(modules4sim()))

  ## pre-experiment customizations
  if (file.exists("pre_experiment.R")) source("pre_experiment.R", local = TRUE)

  # run the simulation experiment
  mySimOut <- callModule(spades_expt, "sim_expt", sim = mySim,
                         reps = experimentReps, seed = seed,
                         objectsToHash = objectsToHash, cacheDebug = debugCache)

  ## post-experiment customizations
  if (file.exists("post_experiment.R")) source("post_experiment.R", local = TRUE)

  ## module calls
  callModule(authGoogle, "auth_google", authFile = authFile, appURL = appURL) ## TODO: write this with generator

  # TODO: update generator to handle this assignment
  chosenPoly <-  callModule(timeSeriesofRasters, "timeSinceFire", rasterList = globalRasters(),
                            polygonList = polygonList(),
                            subRegionName = "LandWeb Study Area",
                            shpStudyRegion = shpStudyRegion(), # full study region
                            colorTable = colorTableFile, palette = timeSinceFirePalette,
                            mapLegend = paste0("Time since fire", br(), "(years)"),
                            maxAge = maxAge, zoom = 5, studyArea = studyArea, sim = mySimOut()[[1]],
                            nPolygons = 1, nRasters = length(tsf()))

  callModule(largePatches, "largePatches", nSimTimes = length(tsf()), clumpMod2Args()) ## here
  callModule(simInfo, "simInfo", mySimOut()[[1]])
  callModule(moduleInfo, "moduleInfo", mySimOut()[[1]])
  callModule(inputTables, "inputTables")

  ## footers (see ?copyrightFooter)
  callModule(copyrightFooter, "copyright", "Her Majesty the Queen in Right of Canada, as represented by the Minister of Natural Resources Canada.")
  callModule(sidebarFooter, "sidebar", character(0))
}
