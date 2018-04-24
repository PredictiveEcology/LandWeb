function(input, output, session) {
  serverStartTime <<- Sys.time()
  session$userData$userLoggedIn <- reactiveVal(FALSE)
  session$userData$userAuthorized <- reactiveVal(FALSE)

  ## run additonal server code from server_file.R
  if (file.exists("server_file.R")) source("server_file.R", local = TRUE)

  ## module calls
  # TODO: update generator to handle these assignments

  rctUserInfo <- callModule(authGoogle, "auth_google", authFile = authFile, appURL = appURL) ## TODO: write this with generator

  defaultPolyName <- "National Ecozones"  ## TODO: move to global.R ?

  userEmail <- reactive({
    rctUserInfo()$emails[1, "value"] %>%
      gsub("/", "_", .) ## `/` is valid for email addresses but not filenames
  })

  rctChosenPolyUser <-  callModule(timeSeriesofRasters, "timeSinceFire",  ## TODO: write this with generator
                                   rctRasterList = rctRasterList,
                                   rctUrlTemplate = rctUrlTemplate,
                                   rctPolygonList = rctPolygonListUser,
                                   shpStudyRegionName = "LandWeb Study Area",
                                   defaultPolyName = defaultPolyName,
                                   colorPalette = timeSinceFirePalette,
                                   mapTitle = "Time since fire",
                                   mapLegend = paste0("Time since fire", br(), "(years)"),
                                   maxAge = maxAge, zoom = 5, nPolygons = 1,
                                   nRasters = length(rctTsf()),
                                   rasterStepSize = summaryInterval,
                                   uploadOpts = list(
                                     auth = isTRUE(session$userData$userAuthorized()),
                                     path = "uploads",
                                     user = userEmail()
                                   ))

  rctPolygonListUser <- reactive(rctChosenPolyUser()$polygons)
  rctChosenPolyName <- reactive(rctChosenPolyUser()$selected)

  rctLargePatchesData <- callModule(largePatches, "largePatches",  ## TODO: write this with generator
                                    rctPolygonList = rctPolygonListUser,
                                    rctChosenPolyName = rctChosenPolyName,
                                    rctLrgPatches = rctLrgPatches,
                                    rctLrgPatchesCC = rctLrgPatchesCC,
                                    rctTsf = rctTsf, rctVtm = rctVtm,
                                    outputPath = rctPaths4sim()$outputPath,
                                    ageClasses = ageClasses, FUN = largePatchesFn, nPatchesFun = countNumPatches)

  rctVegData <- callModule(vegAgeMod, "vegArea",  ## TODO: write this with generator
                           rctPolygonList = rctPolygonListUser,
                           rctChosenPolyName = rctChosenPolyName,
                           rctLeadingDTlist = rctLeadingDTlist,
                           rctLeadingDTlistCC = rctLeadingDTlistCC,
                           rctVtm = rctVtm,
                           outputPath = rctPaths4sim()$outputPath,
                           ageClasses = ageClasses)

  callModule(landwebAppInfo, "appInfo", appInfo)
  callModule(simInfo, "simInfo", rctSim())
  callModule(moduleInfo, "moduleInfo", rctSim())
  callModule(inputTables, "inputTables")

  callModule(downloadOutputs, "downloadOutputs", ## TODO: write this with generator
             appInfo = appInfo, ## defined in global.R
             rctLargePatchesData = rctLargePatchesData,
             rctVegData = rctVegData,
             rctPolygonList = rctPolygonListUser,
             rctChosenPolyName = rctChosenPolyName,
             patchSize = inputs$patchSize)

  ## footers (see ?copyrightFooter)
  callModule(copyrightFooter, "copyright", "Her Majesty the Queen in Right of Canada, as represented by the Minister of Natural Resources Canada.")
  callModule(sidebarFooter, "sidebar", character(0))
}
