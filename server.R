function(input, output, session) {
  serverStartTime <<- Sys.time()

  session$userData$userLoggedIn <- reactiveVal(FALSE)
  session$userData$userAuthorized <- reactiveVal(FALSE)

  ## run additonal server code from server_file.R
  if (file.exists("server_file.R")) source("server_file.R", local = TRUE)

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
        text = paste0("App updated to v", appInfo$version),
        icon = icon("info-circle"),
        status = "info"
      ),
      notificationItem(
        #text = "Scheduled maintenance: Aug 16, 2018",
        text = "No maintenence scheduled",
        icon = icon("calendar"),
        #status = "warning",
        status = "success"
      )
    )
  })

  ## module calls
  # TODO: update generator to handle these assignments

  callModule(landwebAppInfo, "appInfo", appInfo)
  callModule(appNews, "appNews", "NEWS.md", "success")
  callModule(termsOfService, "appToS", "TERMS.md", "success")
  callModule(landwebAppSupport, "appSupport", appInfo)

  unsuspendModule("largePatches")
  unsuspendModule("vegArea")
  unsuspendModule("vegArea2")

  rctUserInfo <- callModule(authGoogle, "auth_google", appURL = appURL,
                            authUsers = appInfo$users, icon = NULL)

  authStatus <- reactive(isTruthy(session$userData$userAuthorized()))
  userEmail <- reactive({
    rctUserInfo()$emails[1, "value"] %>%
      gsub("/", "_", .) ## `/` is valid for email addresses but not filenames
  })

  rctPolySubList <- reactive({
    sublist <- lapply(rctPolygonList(), function(x) x$crsSR)
    omitPolys <- c("AB Natural Sub Regions", "LandWeb Study Area") ## TODO: remove this workaround
    lapply(omitPolys, function(x) sublist[[x]] <<- NULL)
    sublist
  })

  defaultPolyName <- "National Ecozones"  ## TODO: move to global.R ?
  rctUploadOptions <- reactive({
    list(
      auth = authStatus(),
      path = "uploads",
      user = userEmail()
    )
  })

  rctChosenPolyUser <- callModule(polygonChooser, "polyDropdown", rctPolySubList,
                                  defaultPolyName, uploadOpts = rctUploadOptions(),
                                  studyArea = rctStudyArea())
  rctPolygonListUser <- reactive({
    do.call(polygonList, append(rctChosenPolyUser()$polygons, list(studyArea = rctStudyArea())))
  })
  rctChosenPolyName <- reactive(rctChosenPolyUser()$selected)

  callModule(timeSeriesofRasters, "timeSinceFire",  ## TODO: write this with generator
             rctRasterList = rctRasterList,
             rctUrlTemplate = rctUrlTemplate,
             rctPolygonList = rctPolygonList,
             rctChosenPoly = rctChosenPolyUser,
             shpStudyRegionName = "LandWeb Study Area",
             defaultPolyName = defaultPolyName,
             colorPalette = timeSinceFirePalette,
             mapTilesDir = "www/All/FULL/map-tiles",
             mapTitle = "Time since fire",
             mapLegend = paste0("Time since fire", br(), "(years)"),
             maxAge = maxAge, zoom = 5, nPolygons = 1,
             nRasters = length(rctTsf()),
             rasterStepSize = summaryInterval,
             sliderTitle = "Sampled simulation years (does not correspond to calendar years)",
             uploadOpts = rctUploadOptions(),
             rctStudyArea = rctStudyArea,
             thinKeep = 0.01
  )

  ## recalculate large patches for new polygons
  lrgPatches <- reactiveValues()
  lrgPatchesCC <- reactiveValues()
  largePatchesFn <- sim2$LandWebShiny$largePatchesCalc
  .largePatchesCalcFn <- sim2$LandWebShiny$.largePatchesCalc

  observe({
    lapply(names(sim2$lrgPatches[[rctAuthenticationType()]]), function(x) {
      lrgPatches[[x]] <- sim2$lrgPatches[[rctAuthenticationType()]][[x]]
    })

    lapply(names(sim2$lrgPatchesCC[[rctAuthenticationType()]]), function(x) {
      lrgPatchesCC[[x]] <- sim2$lrgPatchesCC[[rctAuthenticationType()]][[x]]
    })

    if (is.null(lrgPatches[[rctChosenPolyName()]])) {
      newPoly <- rctPolygonListUser()[[rctChosenPolyName()]]$crsSR
      lrgPatches[[rctChosenPolyName()]] <- Cache(
        largePatchesFn,
        byPoly = newPoly,
        tsfFile = rctTsf(),
        vtmFile = rctVtm(),
        ageClasses = ageClasses,
        ageClassCutOffs = ageClassCutOffs,
        labelColumn = sim2$labelColumn, ## shinyLabel
        useParallelCluster = useParallelCluster,
        .largePatchesCalc = .largePatchesCalcFn # need to Cache the internals
      )
    }

    if (authStatus() && is.null(lrgPatchesCC[[rctChosenPolyName()]])) {
      newPoly <- rctPolygonListUser()[[rctChosenPolyName()]]$crsSR
      lrgPatchesCC[[rctChosenPolyName()]] <- Cache(
        largePatchesFn,
        byPoly = newPoly,
        tsfFile = rctTsf(),
        vtmFile = rctVtm(),
        ageClasses = ageClasses,
        ageClassCutOffs = ageClassCutOffs,
        labelColumn = sim2$labelColumn, ## shinyLabel
        .largePatchesCalc = .largePatchesCalcFn, # need to Cache the internals
        omitArgs = "useParallelCluster"
      )
    }
  })

  ## large patches histograms
  rctLargePatchesData <- callModule(largePatches, "largePatches",  ## TODO: write this with generator
                                    rctPolygonList = rctPolygonListUser,
                                    rctChosenPolyName = rctChosenPolyName,
                                    lrgPatches = lrgPatches,
                                    lrgPatchesCC = lrgPatchesCC,
                                    rctTsf = rctTsf, rctVtm = rctVtm,
                                    outputPath = rctPaths4sim()$outputPath,
                                    ageClasses = ageClasses,
                                    FUN = largePatchesFn,
                                    nPatchesFun = countNumPatches)

  ## recalculate leading vegetation classes for new polygons
  leadingDTlist <- reactiveValues()
  leadingDTlistCC <- reactiveValues()
  leadingByStageFn <- sim2$LandWebShiny$leadingByStage

  observe({
    lapply(names(sim2$leading[[rctAuthenticationType()]]), function(x) {
      leadingDTlist[[x]] <- sim2$leading[[rctAuthenticationType()]][[x]]
    })

    lapply(names(sim2$leadingCC[[rctAuthenticationType()]]), function(x) {
      leadingDTlistCC[[x]] <- sim2$leadingCC[[rctAuthenticationType()]][[x]]
    })

    if (is.null(leadingDTlist[[rctChosenPolyName()]])) {
      newPoly <- rctPolygonListUser()[[rctChosenPolyName()]]$crsSR
      leadingDTlist[[rctChosenPolyName()]] <- Cache(leadingByStageFn,
                                                    tsf = list(rctTsf()),
                                                    vtm = list(rctVtm()),
                                                    polygonToSummarizeBy = newPoly,
                                                    ageClassCutOffs = ageClassCutOffs,
                                                    ageClasses = ageClasses)
    }

    if (authStatus() && is.null(leadingDTlistCC[[rctChosenPolyName()]])) {
      newPoly <- rctPolygonListUser()[[rctChosenPolyName()]]$crsSR
      leadingDTlistCC[[rctChosenPolyName()]] <- Cache(leadingByStageFn,
                                                      tsf = list(rctTsf()),
                                                      vtm = list(rctVtm()),
                                                      polygonToSummarizeBy = newPoly,
                                                      ageClassCutOffs = ageClassCutOffs,
                                                      ageClasses = ageClasses)
    }
  })

  ## veg cover histograms
  rctVegData <- callModule(vegAgeMod, "vegArea",  ## TODO: write this with generator
                           rctAuthenticationType = rctAuthenticationType,
                           rctPolygonList = rctPolygonListUser,
                           rctChosenPolyName = rctChosenPolyName,
                           leadingDTlist = leadingDTlist,
                           leadingDTlistCC = leadingDTlistCC,
                           rctVtm = rctVtm,
                           outputPath = rctPaths4sim()$outputPath,
                           ageClasses = ageClasses)

  ## veg cover boxplots
  rctVegData2 <- callModule(vegAgeMod2, "vegArea2",  ## TODO: write this with generator
                            rctAuthenticationType = rctAuthenticationType,
                            rctPolygonList = rctPolygonListUser,
                            rctChosenPolyName = rctChosenPolyName,
                            leadingDTlist = leadingDTlist,
                            leadingDTlistCC = leadingDTlistCC,
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
