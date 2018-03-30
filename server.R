function(input, output, session) {
  session$userData$userLoggedIn <- reactiveVal(FALSE)
  session$userData$userAuthorized <- reactiveVal(FALSE)

  ## run additonal server code from server_file.R
  if (file.exists("server_file.R")) source("server_file.R", local = TRUE)

  # simulation initialization now in global.R (pre run all simulations)
  mySimOut <- reactive({
      if (session$userData$userAuthorized()) {
        mySimOuts$Proprietary
      } else {
        mySimOuts$Free
      }
    })
  
  ## post-experiment customizations
  if (file.exists("post_experiment.R")) source("post_experiment.R", local = TRUE)

  ## module calls
  callModule(authGoogle, "auth_google", authFile = authFile, appURL = appURL) ## TODO: write this with generator

  # TODO: update generator to handle this assignment
  chosenPolyName <-  callModule(timeSeriesofRasters, "timeSinceFire", rasterList = globalRasters(),
                                rctPolygonList = rctReportingPolygons,
                                shpStudyRegionName = "LandWeb Study Area",
                                defaultPolyName = NULL,
                                colorTable = colorTableFile, palette = timeSinceFirePalette,
                                mapLegend = paste0("Time since fire", br(), "(years)"),
                                maxAge = maxAge, zoom = 5, sim = mySimOut()[[1]],
                                nPolygons = 1, nRasters = length(tsf()))

  callModule(largePatches, "largePatches", polygonList = rctReportingPolygons(),   ## TODO: write this with generator
             chosenPolyName = chosenPolyName(), tsf = tsf(), vtm = vtm(), cl = NULL,
             ageClasses = ageClasses, cachePath = cachePath(mySim()),
             FUN = largePatchesFn, nPatchesFun = countNumPatches)
  callModule(simInfo, "simInfo", mySimOut()[[1]])
  callModule(moduleInfo, "moduleInfo", mySimOut()[[1]])
  callModule(inputTables, "inputTables")

  ## footers (see ?copyrightFooter)
  callModule(copyrightFooter, "copyright", "Her Majesty the Queen in Right of Canada, as represented by the Minister of Natural Resources Canada.")
  callModule(sidebarFooter, "sidebar", character(0))
}
