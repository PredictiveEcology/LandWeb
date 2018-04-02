function(input, output, session) {
  serverStartTime <<- Sys.time()
  session$userData$userLoggedIn <- reactiveVal(FALSE)
  session$userData$userAuthorized <- reactiveVal(FALSE)

  ## run additonal server code from server_file.R
  if (file.exists("server_file.R")) source("server_file.R", local = TRUE)

  ## module calls
  callModule(authGoogle, "auth_google", authFile = authFile, appURL = appURL) ## TODO: write this with generator

  # TODO: update generator to handle this assignment
  rctChosenPolyName <-  callModule(timeSeriesofRasters, "timeSinceFire",  ## TODO: write this with generator
                                   rctRasterList = rctRasterList,
                                   rctUrlTemplate = rctUrlTemplate,
                                   rctPolygonList = rctPolygonList,
                                   shpStudyRegionName = "LandWeb Study Area",
                                   defaultPolyName = NULL,
                                   colorTable = colorTableFile,
                                   palette = timeSinceFirePalette,
                                   mapLegend = paste0("Time since fire", br(), "(years)"),
                                   maxAge = maxAge, zoom = 5, nPolygons = 1,
                                   nRasters = length(rctTsf()))

  callModule(largePatches, "largePatches", polygonList = rctPolygonList(),   ## TODO: write this with generator
             chosenPolyName = rctChosenPolyName(), tsf = rctTsf(), vtm = rctVtm(), cl = NULL,
             ageClasses = ageClasses, FUN = largePatchesFn, nPatchesFun = countNumPatches)
  callModule(simInfo, "simInfo", rctSim())
  callModule(moduleInfo, "moduleInfo", rctSim())
  callModule(inputTables, "inputTables")

  ## footers (see ?copyrightFooter)
  callModule(copyrightFooter, "copyright", "Her Majesty the Queen in Right of Canada, as represented by the Minister of Natural Resources Canada.")
  callModule(sidebarFooter, "sidebar", character(0))
}
