downloadOutputsUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("downloadModel"))
}

downloadOutputs <- function(input, output, session, appInfo,
                            rctLargePatchesData, rctVegData, rctPolygonList, rctChosenPolyName) {

  ## only authorized users can download model outputs

  output$downloadModel <- renderUI({
    ns <- session$ns

    if (isTRUE(session$userData$userAuthorized())) {
      tagList(
        h4(HTML("&nbsp;"), "Download model data (.zip):"),
        actionButton(ns("showDownloadOptions"), "Download Options")
      )
    } else {
      NULL
    }
  })

  observeEvent(input$showDownloadOptions, {
    ns <- session$ns

    showModal(modalDialog(
      title = "Download options",

      h5("Inputs:"), ## TODO: rename this subsection
      checkboxInput(ns("dlPolygon"), "Currently selected reporting polygon (.shp)", TRUE),
      checkboxInput(ns("dlCachedRasters"), "Cached raster input files (.tif)", TRUE),
      checkboxInput(ns("dlInitCommMap"), "Inital communities map (.tif)", TRUE),
      ###
      h5("Outputs:"),
      h6("Large Patches Data for study region"),
      checkboxInput(ns("dlLargePatchesData"), "Large Patches Data (.csv)", TRUE),
      checkboxInput(ns("dlLargePatchesHists"), "Large patches histograms (.png)", TRUE),

      h6("Leading Vegetation Cover Data for study region"),
      checkboxInput(ns("dlVegData"), "Leading Vegetation Cover Data (.csv)", TRUE),
      checkboxInput(ns("dlVegAgeHists"), "Leading Vegetaiton Cover histograms (.png)", TRUE),

      h6("Simulation Rasters (cropped to study reagion)"),
      checkboxInput(ns("dlFlammableMaps"), "Flammability maps (.grd)", TRUE),
      checkboxInput(ns("dlTimeSinceFireMaps"), "Time Since Fire maps (.tif)", TRUE),
      checkboxInput(ns("dlVegTypeMaps"), "Vegetation type maps (.grd, .tif)", TRUE),

      h6("Additional R Data Files"),
      checkboxInput(ns("dlSimOutputs"), "Simulation data files (.RData, .rds)", FALSE), ## false by default
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

      if (isTRUE(input$dlInitCommMap)) {
        initCommMapFile <- file.path("outputs", paste0(subStudyRegionName, "_Proprietary"),
                                     "initialCommunitiesMap.tif")
        fileList <- append(fileList, initCommMapFile)
      }

      if (isTRUE(input$dlCachedRasters)) {
        cachedRasterFiles <- list.files(
          file.path("cache", paste0(subStudyRegionName, "_Proprietary"), "rasters"),
          recursive = TRUE, full.names = TRUE
        )
        fileList <- append(fileList, cachedRasterFiles)
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
          file.path("outputs", paste0(subStudyRegionName, "_Proprietary"),
                    gsub(" ", "_", rctChosenPolyName()),
                    "histograms", "largePatches"),
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
          file.path("outputs", paste0(subStudyRegionName, "_Proprietary"),
                    gsub(" ", "_", rctChosenPolyName()),
                    "histograms", "vegAgeMod"),
          recursive = TRUE, full.names = TRUE
        )
        fileList <- append(fileList, histFiles2)
      }

      ### Simulation rasters
      if (isTRUE(input$dlFlammableMaps)) {
        flammableMapFiles <- list.files(
          file.path("outputs", paste0(subStudyRegionName, "_Proprietary")),
          recursive = TRUE, full.names = TRUE, pattern = "rstFlammable"
        )
        fileList <- append(fileList, flammableMapFiles)
      }

      if (isTRUE(input$dlTimeSinceFireMaps)) {
        tsfMapFiles <- list.files(
          file.path("outputs", paste0(subStudyRegionName, "_Proprietary")),
          recursive = TRUE, full.names = TRUE, pattern = "rstTimeSinceFire"
        )
        fileList <- append(fileList, tsfMapFiles)
      }

      if (isTRUE(input$dlVegTypeMaps)) {
        vegTypeMapFiles <- list.files(
          file.path("outputs", paste0(subStudyRegionName, "_Proprietary")),
          recursive = TRUE, full.names = TRUE, pattern = "vegTypeMap"
        )
        fileList <- append(fileList, vegTypeMapFiles)
      }

      ### other simulation data files
      if (isTRUE(input$dlSimOutputs)) {
        simOutputFiles <- list.files(
          file.path("outputs", paste0(subStudyRegionName, "_Proprietary")),
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
      readmeFile <- "README.Rmd"

      ## create the zip file containing the selected files
      zip(file, files = c(readmeFile, unlist(fileListRenamed)))
    },
    contentType = "application/zip"
  )
}
