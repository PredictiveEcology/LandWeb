function(input, output, session) {
  serverStartTime <<- Sys.time()
  session$userData$userLoggedIn <- reactiveVal(FALSE)
  session$userData$userAuthorized <- reactiveVal(FALSE)

  ## run additonal server code from server_file.R
  if (file.exists("server_file.R")) source("server_file.R", local = TRUE)

  ## show the user the ToS when they start the app
  #toggleModal(session, "appToS_modal", toggle = "open")

  ## module calls
  # TODO: update generator to handle these assignments

  callModule(landwebAppInfo, "appInfo", appInfo)
  callModule(termsOfService, "appToS", "TERMS.md", "success")
  callModule(landwebAppSupport, "appSupport", appInfo)

  unsuspendModule("largePatches")
  unsuspendModule("vegArea")

  rctUserInfo <- callModule(authGoogle, "auth_google", appURL = appURL,
                            authUsers = appInfo$users, icon = NULL)

  defaultPolyName <- "National Ecozones"  ## TODO: move to global.R ?

  authStatus <- reactive(isTruthy(session$userData$userAuthorized()))
  userEmail <- reactive({
    rctUserInfo()$emails[1, "value"] %>%
      gsub("/", "_", .) ## `/` is valid for email addresses but not filenames
  })

  rctChosenPolyUser <-  callModule(timeSeriesofRasters, "timeSinceFire",  ## TODO: write this with generator
                                   rctRasterList = rctRasterList,
                                   rctUrlTemplate = rctUrlTemplate,
                                   rctPolygonList = rctPolygonList,
                                   shpStudyRegionName = "LandWeb Study Area",
                                   defaultPolyName = defaultPolyName,
                                   colorPalette = timeSinceFirePalette,
                                   mapTitle = "Time since fire",
                                   mapLegend = paste0("Time since fire", br(), "(years)"),
                                   maxAge = maxAge, zoom = 5, nPolygons = 1,
                                   nRasters = length(rctTsf()),
                                   rasterStepSize = summaryInterval,
                                   uploadOpts = list(
                                     auth = authStatus(),
                                     path = "uploads",
                                     user = userEmail()
                                   ),
                                   rctStudyArea = rctStudyArea,
                                   omitPolys = c("AB Natural Sub Regions",
                                                 "LandWeb Study Area")) ## TODO: remove this workaround

  rctPolygonListUser <- reactive({
    do.call(polygonList, append(rctChosenPolyUser()$polygons, list(studyArea = rctStudyArea())))
  })
  rctChosenPolyName <- reactive(rctChosenPolyUser()$selected)

  rctLargePatchesData <- callModule(largePatches, "largePatches",  ## TODO: write this with generator
                                    rctPolygonList = rctPolygonListUser,
                                    rctChosenPolyName = rctChosenPolyName,
                                    rctLrgPatches = rctLrgPatches,
                                    rctLrgPatchesCC = rctLrgPatchesCC,
                                    rctTsf = rctTsf, rctVtm = rctVtm,
                                    outputPath = rctPaths4sim()$outputPath,
                                    ageClasses = ageClasses,
                                    FUN = largePatchesFn,
                                    nPatchesFun = countNumPatches)

  rctVegData <- callModule(vegAgeMod, "vegArea",  ## TODO: write this with generator
                           rctPolygonList = rctPolygonListUser,
                           rctChosenPolyName = rctChosenPolyName,
                           rctLeadingDTlist = rctLeadingDTlist,
                           rctLeadingDTlistCC = rctLeadingDTlistCC,
                           rctVtm = rctVtm,
                           outputPath = rctPaths4sim()$outputPath,
                           ageClasses = ageClasses)

  callModule(simInfo, "simInfo", rctSim())
  callModule(moduleInfo, "moduleInfo", rctSim())
  callModule(inputTables, "inputTables")

  callModule(downloadOutputs, "downloadOutputs", ## TODO: write this with generator
             appInfo = appInfo, ## defined in global.R
             rctLargePatches = rctLargePatchesData,
             rctVegData = rctVegData,
             rctPolygonList = rctPolygonListUser,
             rctChosenPolyName = rctChosenPolyName)

  ## footers (see ?copyrightFooter)
  callModule(copyrightFooter, "copyright", "Her Majesty the Queen in Right of Canada, as represented by the Minister of Natural Resources Canada.")
  callModule(sidebarFooter, "sidebar", character(0))
}
