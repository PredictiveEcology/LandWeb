function(input, output, session) {
  serverStartTime <<- Sys.time()

  session$userData$userLoggedIn <- reactiveVal(FALSE)
  session$userData$userAuthorized <- reactiveVal(FALSE)

  polygonList <- lapply(FMA_names, function(i) ml[["FMA Boundaries Updated"]]["Name" == i]) %>%
    set_names(FMA_names)
  rctPolygonList <- reactive(polygonList)

  rctSim <- reactive({
    readRDS(file.path(appDir, "docs", "mySim_landweb_0000.rds")) ## TODO: check this path is OK in app
  })

  ## show the user the ToS when they start the app, but not after logging in
  observe({
    showModal(modalDialog(
      title = NULL, easyClose = FALSE, size = "l",
      # footer = modalButton("Accept"),
      footer = tags$button(type = "button",
                           class = "btn btn-default-green", # defined in www/style.css
                           `data-dismiss` = "modal",
                           "Accept"),
      wellPanel(includeMarkdown("TERMS.md"), style = "overflow-y:scroll; max-height: 600px")
    ))
    if (isTruthy(session$userData$userLoggedIn())) removeModal()
  })

  ## notifications menu
  output$notifications <- renderMenu({
    dropdownMenu(
      type = "notifications",
      notificationItem(
        text = paste0("App updated to v", appInfo$version, "."),
        icon = icon("info-circle"),
        status = "info"
      ),
      notificationItem(
        text = "No maintenence scheduled.",
        icon = icon("calendar"),
        status = "success"
      ),
      notificationItem(
        text = "NOTE: this app is no longer maintained.",
        icon = icon("calendar"),
        status = "warning"
      )
    )
  })

  ## module calls
  # TODO: update generator to handle these assignments

  callModule(landwebAppInfo, "appInfo", appInfo, readmeFile = "../README.md")
  callModule(appNews, "appNews", "../NEWS.md", "success")
  callModule(termsOfService, "appToS", "TERMS.md", "success")
  callModule(landwebAppSupport, "appSupport", appInfo)

  #unsuspendModule("largePatches")
  #unsuspendModule("vegArea")
  #unsuspendModule("vegArea2")

  rctUserInfo <- callModule(authGoogle, "auth_google", appURL = appURL,
                            authUsers = appInfo$users, icon = NULL)

  session$userData$userAuthorized(TRUE) ## TODO: remove this!
  authStatus <- reactive(isTruthy(session$userData$userAuthorized()))
  userEmail <- reactive({
    rctUserInfo()$emails[1, "value"] %>%
      gsub("/", "_", .) ## `/` is valid for email addresses but not filenames
  })

  rctPolySubList <- reactive({
    rctPolygonList() ## TODO: limit each user to seeing only their own FMAs
  })

  rctUploadOptions <- reactive({
    list(
      auth = authStatus(),
      path = "uploads",
      user = userEmail()
    )
  })

  defaultPolyName <- reactive({
    FMA_names[sample(seq_along(FMA_names), 1)] ## TODO: currently random; could be user-specific
  })

  rctChosenPolyUser <- callModule(polygonChooser, "polyDropdown",
                                  rctPolygonList = rctPolySubList,
                                  selectedPoly = defaultPolyName(),
                                  uploadOpts = rctUploadOptions(),
                                  studyArea = rctStudyArea())

  rctChosenPolyName <- reactive(rctChosenPolyUser()$selected)

  rctPolygonListUser <- reactive({
    ## polygonList containing ONLY the user's chosen polygon
    allPolys <- rctChosenPolyUser()$polygons
    userPoly <- rctChosenPolyName()
    do.call(polygonList, append(allPolys[userPoly], list(studyArea = rctStudyArea())))
  })

  # callModule(timeSeriesofRasters, "timeSinceFire",  ## TODO: write this with generator
  #            rctRasterList = rctRasterList,
  #            rctUrlTemplate = rctUrlTemplate,
  #            rctPolygonList = rctPolygonListUser,
  #            rctChosenPoly = rctChosenPolyUser,
  #            shpStudyRegionName = "LandWeb Study Area",
  #            shpStudyRegionLFLT = isolate(rctPolygonList()[["LandWeb Study Area"]][["crsLFLT"]]), ## won't change
  #            defaultPolyName = defaultPolyName,
  #            colorPalette = timeSinceFirePalette,
  #            mapTilesDir = tilePath,
  #            mapTitle = "Time since fire",
  #            mapLegend = paste0("Time since fire", br(), "(years)"),
  #            maxAge = maxAge, zoom = 5, nPolygons = 1,
  #            nRasters = length(rctTsf()),
  #            rasterStepSize = summaryInterval,
  #            sliderTitle = "Sampled simulation years (does not correspond to calendar years)",
  #            uploadOpts = rctUploadOptions(),
  #            rctStudyArea = rctStudyArea,
  #            thinKeep = 0.01
  # )

  # ## large patches histograms
  # rctLargePatchesData <- callModule(largePatches, "largePatches",  ## TODO: write this with generator
  #                                   rctPolygonList = rctPolygonListUser,
  #                                   rctChosenPolyName = rctChosenPolyName,
  #                                   rctLrgPatches = rctLrgPatchesUser,
  #                                   rctLrgPatchesCC = rctLrgPatchesUserCC,
  #                                   rctTsf = rctTsf, rctVtm = rctVtm,
  #                                   outputPath = rctPaths4sim()$outputPath,
  #                                   ageClasses = ageClasses,
  #                                   FUN = largePatchesFn)
  #
  # ## veg cover histograms
  # rctVegData <- callModule(vegAgeMod, "vegArea",  ## TODO: write this with generator
  #                          rctAuthenticationType = rctAuthenticationType,
  #                          rctPolygonList = rctPolygonListUser,
  #                          rctChosenPolyName = rctChosenPolyName,
  #                          rctLeadingDTlist = rctLeadingDTlistUser,
  #                          rctLeadingDTlistCC = rctLeadingDTlistUserCC,
  #                          rctVtm = rctVtm,
  #                          outputPath = rctPaths4sim()$outputPath,
  #                          ageClasses = ageClasses)
  #
  # ## veg cover boxplots
  # rctVegData2 <- callModule(vegAgeMod2, "vegArea2",  ## TODO: write this with generator
  #                           rctAuthenticationType = rctAuthenticationType,
  #                           rctPolygonList = rctPolygonListUser,
  #                           rctChosenPolyName = rctChosenPolyName,
  #                           rctLeadingDTlist = rctLeadingDTlistUser,
  #                           rctLeadingDTlistCC = rctLeadingDTlistUserCC,
  #                           rctVtm = rctVtm,
  #                           outputPath = rctPaths4sim()$outputPath,
  #                           ageClasses = ageClasses)

  callModule(simInfo, "simInfo", rctSim(), elements = c("modules", "objects")) ## no events
  callModule(moduleInfo, "moduleInfo", rctSim())
  callModule(inputTables, "inputTables") ## TODO: use updated tables

  callModule(downloadOutputs, "downloadOutputs", ## TODO: write this with generator
             appInfo = appInfo, ## defined in global.R
             rctLargePatches = rctLargePatchesData,
             rctVegData = rctVegData,
             rctPolygonList = rctPolygonListUser,
             rctChosenPolyName = rctChosenPolyName)

  ## footers (see ?copyrightFooter)
  callModule(copyrightFooter, "copyright", paste("Her Majesty the Queen in Right of Canada,",
                                                 "as represented by the Minister of Natural Resources Canada."))
  callModule(sidebarFooter, "sidebar", character(0))
}
