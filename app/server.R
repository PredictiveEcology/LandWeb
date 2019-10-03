function(input, output, session) {
  serverStartTime <<- Sys.time()

  session$userData$userLoggedIn <- reactiveVal(FALSE)
  session$userData$userAuthorized <- reactiveVal(FALSE)

  rctPolygonList <- reactive(polygonList)
  rctStudyArea <- reactive({
    ml[["LandWeb Study Area"]] %>% spTransform(., CRS("+init=epsg:4326"))
  })

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
      )
    )
  })

  ## module calls
  # TODO: update generator to handle these assignments

  callModule(landwebAppInfo, "appInfo", appInfo, readmeFile = "../README.md")
  callModule(appNews, "appNews", "../NEWS.md", "success")
  callModule(termsOfService, "appToS", "TERMS.md", "success")
  callModule(landwebAppSupport, "appSupport", appInfo)

  unsuspendModule("largePatches")
  unsuspendModule("vegArea")
  unsuspendModule("vegArea2")

  rctUserInfo <- callModule(authGoogle, "auth_google", appURL = appURL,
                            authUsers = appInfo$users, icon = NULL)

  session$userData$userAuthorized(TRUE) ## TODO: remove this!
  authStatus <- reactive(isTruthy(session$userData$userAuthorized()))
  userEmail <- reactive({
    rctUserInfo()$emails[1, "value"] %>%
      gsub("/", "_", .) ## `/` is valid for email addresses but not filenames
  })

  rctPolySubList <- reactive({
    ## TODO: limit each user to seeing only their own FMAs
    rctPolygonList()
  })

  rctUploadOptions <- reactive({
    list(
      auth = FALSE, #authStatus(), # TODO: re-enable uploads
      path = "uploads",
      user = userEmail()
    )
  })

  defaultPolyName <- reactive({
    ids <- which(!is.na(animationsInfo[["FILE"]]))
    animationsInfo[["FMA"]][sample(ids, 1)] ## TODO: currently random; could be user-specific
  })

  rctChosenPolyUser <- callModule(polygonChooser, "polyDropdown",
                                  rctPolygonList = rctPolySubList,
                                  selectedPoly = defaultPolyName(),
                                  uploadOpts = rctUploadOptions(),
                                  studyArea = rctStudyArea())

  rctChosenPolyName <- reactive(rctChosenPolyUser()$selected)
  rctPolySubType <- reactive({
    switch(input$polySubType,
           "Alberta Natural Sub-Regions" = "ANSR",
           "Caribou Ranges" = "Caribou",
           "FMA" = "None")
  })

  callModule(landwebMap, "mainMap",
             rctStudyArea = rctStudyArea,
             polygonList = polygonList,
             rctChosenPolyName = rctChosenPolyName)
  callModule(rasterAnimation, "movie",
             appDir = appDir,
             animationsInfo = animationsInfo,
             rctChosenPolyName = rctChosenPolyName)

  rctLrgPatchesDT <- reactive({
    if (rctPolySubType() == "ANSR") {
      ids <- which(grepl(rctChosenPolyName(), largePatchesDT.ANSR[["polygonName"]], fixed = TRUE))
      largePatchesDT.ANSR[ids, ]
    } else if (rctPolySubType() == "Caribou") {
      ids <- which(grepl(rctChosenPolyName(), largePatchesDT.Caribou[["polygonName"]], fixed = TRUE))
      largePatchesDT.Caribou[ids, ]
    } else {
      ids <- which(grepl(rctChosenPolyName(), largePatchesDT[["polygonName"]], fixed = TRUE))
      largePatchesDT[ids, ]
    }
  })

  ## large patches histograms
  rctLargePatchesData <- callModule(largePatches, "largePatches",  ## TODO: write this with generator
                                    rctPolygonList = rctPolygonList,
                                    rctChosenPolyName = rctChosenPolyName,
                                    rctLrgPatches = rctLrgPatchesDT,
                                    ageClasses = ageClasses,
                                    FUN = largePatchesFn)

  rctLeadingDT <- reactive({
    if (rctPolySubType() == "ANSR") {
      ids <- which(grepl(rctChosenPolyName(), leadingDT.ANSR[["zone"]], fixed = TRUE))
      leadingDT.ANSR[ids, ]
    } else if (rctPolySubType() == "Caribou") {
      ids <- which(grepl(rctChosenPolyName(), leadingDT.Caribou[["zone"]], fixed = TRUE))
      leadingDT.Caribou[ids, ]
    } else {
      ids <- which(grepl(rctChosenPolyName(), leadingDT[["zone"]], fixed = TRUE))
      leadingDT[ids, ]
    }
  })

  ## veg cover histograms
  rctVegData <- callModule(vegAgeMod, "vegArea",  ## TODO: write this with generator
                           rctPolygonList = rctPolygonList,
                           rctChosenPolyName = rctChosenPolyName,
                           rctLeading = rctLeadingDT,
                           ageClasses = ageClasses)

  ## veg cover boxplots
  rctVegData2 <- callModule(vegAgeMod2, "vegArea2",  ## TODO: write this with generator
                            rctPolygonList = rctPolygonList,
                            rctChosenPolyName = rctChosenPolyName,
                            rctLeading = rctLeadingDT,
                            ageClasses = ageClasses)

  callModule(simInfo, "simInfo", rctSim(), elements = c("modules", "objects")) ## no events
  callModule(moduleInfo, "moduleInfo", rctSim())
  callModule(inputTables, "inputTables") ## TODO: use updated tables

  callModule(downloadOutputs, "downloadOutputs", ## TODO: write this with generator
             appInfo = appInfo, ## defined in global.R
             rctLargePatches = rctLargePatchesData,
             rctVegData = rctVegData,
             rctPolygonList = rctPolygonList,
             rctChosenPolyName = rctChosenPolyName)

  ## footers (see ?copyrightFooter)
  callModule(copyrightFooter, "copyright", paste("Her Majesty the Queen in Right of Canada,",
                                                 "as represented by the Minister of Natural Resources Canada."))
  callModule(sidebarFooter, "sidebar", character(0))
}
