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
                                   defaultPolyName = "National Ecozones",
                                   colorPalette = timeSinceFirePalette,
                                   mapTitle = "Time since fire",
                                   mapLegend = paste0("Time since fire", br(), "(years)"),
                                   maxAge = maxAge, zoom = 5, nPolygons = 1,
                                   nRasters = length(rctTsf()),
                                   rasterStepSize = summaryInterval)

  callModule(largePatches, "largePatches",
             rctPolygonList = rctPolygonList,   ## TODO: write this with generator
             rctChosenPolyName = rctChosenPolyName,
             rctLrgPatches = rctLrgPatches,
             rctLrgPatchesCC = rctLrgPatchesCC,
             rctTsf = rctTsf, rctVtm = rctVtm,
             outputPath = rctPaths4sim()$outputPath,
             ageClasses = ageClasses, FUN = largePatchesFn, nPatchesFun = countNumPatches)
  callModule(vegAgeMod, "vegArea",
             rctPolygonList = rctPolygonList,        ## TODO: write this with generator
             rctChosenPolyName = rctChosenPolyName,
             rctLeadingDTlist = rctLeadingDTlist,
             rctVtm = rctVtm,
             outputPath = rctPaths4sim()$outputPath,
             ageClasses = ageClasses)
  callModule(simInfo, "simInfo", rctSim())
  callModule(moduleInfo, "moduleInfo", rctSim())
  callModule(inputTables, "inputTables")
  callModule(downloadOutputs, "downloadOutputs") ## TODO: write this with generator

  ## footers (see ?copyrightFooter)
  callModule(copyrightFooter, "copyright", "Her Majesty the Queen in Right of Canada, as represented by the Minister of Natural Resources Canada.")
  callModule(sidebarFooter, "sidebar", character(0))
}
