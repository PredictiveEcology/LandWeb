downloadOutputsUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("downloadModel"))
}

#' only authorized users can download model outputs
downloadOutputs <- function(input, output, session, appInfo,
                            rctLargePatchesData, rctVegData, rctPolygonList,
                            rctChosenPolyName, patchSize) {

  output$downloadModel <- renderUI({
    ns <- session$ns

    if (TRUE) {
    #if (isTRUE(session$userData$userAuthorized())) {
      tagList(
        tags$hr(),
        h4(HTML("&nbsp;"), "Download model data (.zip):"),
        actionButton(ns("showDownloadOptions"), "Download Options")
      )
    }
  })

  observeEvent(input$showDownloadOptions, {
    ns <- session$ns

    showModal(modalDialog(
      title = "Download options",

      h5("Inputs:"), ## TODO: rename this subsection
      checkboxInput(ns("dlPolygon"), "Currently selected reporting polygon (.shp)", TRUE),
      ###
      h5("Current Condition:"),
      checkboxInput(ns("dlCC"), "Map of current condition (.tif)", TRUE),
      ###
      h5("Outputs:"),
      h6("Large Patches Data for study region"),
      checkboxInput(ns("dlLargePatchesData"), "Large Patches Data (.csv)", TRUE),
      checkboxInput(ns("dlLargePatchesHists"), "Large patches histograms (.png)", TRUE),

      h6("Leading Vegetation Cover Data for study region"),
      checkboxInput(ns("dlVegData"), "Leading Vegetation Cover Data (.csv)", TRUE),
      checkboxInput(ns("dlVegAgeHists"), "Leading Vegetaiton Cover histograms (.png)", TRUE),

      h6("Simulation Rasters (cropped to study reagion)"),
      checkboxInput(ns("dlTimeSinceFireMaps"), "Time Since Fire maps (.tif)", TRUE),
      checkboxInput(ns("dlVegTypeMaps"), "Vegetation type maps (.grd, .tif)", TRUE),
      ###
      h5("Additional R Data Files"), ## all of these should be false by default
      checkboxInput(ns("dlSimOutputs"), "Simulation data files (.RData, .rds)", FALSE),
      #checkboxInput(ns("dlInitCommMap"), "Inital communities map (.tif)", FALSE),
      ###
      radioButtons(ns("usePrefix"), "Prefix all filenames with app version info:",
                   choices = list(yes = TRUE, no = FALSE), inline = TRUE, selected = FALSE), ## false by default
      ###
      downloadButton(ns("downloadData"), "Download"),
      modalButton("Cancel", icon("ban")),
      footer = h6(paste(appInfo$name, "version", appInfo$version)),
      size = "m", # medium
      easyClose = TRUE
    ))
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      filePrefix <- paste0(appInfo$name, "_v", appInfo$version, "_")
      paste(filePrefix, Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      fileList <- list()

      ### "inputs"
      if (isTRUE(input$dlPolygon)) {
        polygonFiles <- list.files(
          file.path("outputs", paste0(subStudyRegionName, "_Proprietary"), "Polygons"),
          pattern = rctChosenPolyName(), recursive = TRUE, full.names = TRUE
        )
        fileList <- append(fileList, polygonFiles)
      }

      ### Current condition
      if (isTRUE(input$dlCC)) {
        ccFile <- file.path("cache", paste0(subStudyRegionName, "_Proprietary"),
                             "rasters", "CurrentCondition.tif")
        fileList <- append(fileList, ccFile)
      }

      ### Large Patches Data
      if (isTRUE(input$dlLargePatchesData)) {
        largePatchesDataFile <- file.path("outputs", paste0(subStudyRegionName, "_Proprietary"),
                                          "largePatches.csv")
        write.csv(rctLargePatchesData(), largePatchesDataFile)
        fileList <- append(fileList, largePatchesDataFile)
      }

      if (isTRUE(input$dlLargePatchesHists)) {
        histFiles1 <- list.files(
          file.path("outputs", paste0(subStudyRegionName, "_Proprietary"), ## currently in _All not _Proprietary
                    "histograms", gsub(" ", "_", rctChosenPolyName()),
                    "largePatches", patchSize),
          recursive = TRUE, full.names = TRUE
        )
        fileList <- append(fileList, histFiles1)
      }

      ### Veg Class Data
      if (isTRUE(input$dlVegData)) {
        vegDataFile <- file.path("outputs", paste0(subStudyRegionName, "_Proprietary"),
                                 "vegArea.csv")
        write.csv(rctVegData(), vegDataFile)
        fileList <- append(fileList, vegDataFile)
      }

      if (isTRUE(input$dlVegAgeHists)) {
        histFiles2 <- list.files(
          file.path("outputs", paste0(subStudyRegionName, "_Proprietary"), ## currently in _All not _Proprietary
                    "histograms", gsub(" ", "_", rctChosenPolyName()),
                    "vegAgeMod"),
          recursive = TRUE, full.names = TRUE
        )
        fileList <- append(fileList, histFiles2)
      }

      ### Simulation rasters
      if (isTRUE(input$dlTimeSinceFireMaps)) {
        tsfMapFiles <- list.files(
          file.path("outputs", paste0(subStudyRegionName, "_Proprietary")), ## currently in _All not _Proprietary
          recursive = TRUE, full.names = TRUE, pattern = "rstTimeSinceFire"
        )
        fileList <- append(fileList, tsfMapFiles)
      }

      if (isTRUE(input$dlVegTypeMaps)) {
        vegTypeMapFiles <- list.files(
          file.path("outputs", paste0(subStudyRegionName, "_Proprietary")), ## currently in _All not _Proprietary
          recursive = TRUE, full.names = TRUE, pattern = "vegTypeMap.+[0-9]\\.tif"
        )
        fileList <- append(fileList, vegTypeMapFiles)
      }

      ### other simulation data files
      if (isTRUE(input$dlSimOutputs)) {
        simOutputFiles <- list.files(
          file.path("outputs", paste0(subStudyRegionName, "_Proprietary")), ## currently in _All not _Proprietary
          recursive = TRUE, full.names = TRUE, pattern = "[.]RData|[.]rds"
        )
        fileList <- append(fileList, simOutputFiles)
      }

      ### append filename prefix if selected
      fileListRenamed <- if (input$usePrefix == "TRUE") {
        renamedFiles <- lapply(fileList, function(fname) {
          filePrefix <- paste0(appInfo$name, "_v", appInfo$version, "_")
          file.path(dirname(fname), .prefix(basename(fname), filePrefix)) %>%
            gsub("/\\./", "/", .)
        })
        mapply(file.copy, fileList, renamedFiles)
        on.exit(lapply(fileListRenamed, unlink), add = TRUE)
        renamedFiles
      } else {
        fileList
      }

      ## TODO: improve info/metadata in this file
      readmeFile <- "README.md"

      ## create the zip file containing the selected files
      zip(file, files = c(readmeFile, unlist(fileListRenamed)))
    },
    contentType = "application/zip"
  )
}
