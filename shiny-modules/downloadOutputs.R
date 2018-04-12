downloadOutputsUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("downloadBtn"))
}

downloadOutputs <- function(input, output, session, appInfo,
                            rctLargePatchesData, rctVegData, rctPolygonList, rctChosenPolyName) {

  ## only authorized users can download model outputs

  output$downloadBtn <- renderUI({
    ns <- session$ns

    if (isTRUE(session$userData$userAuthorized())) {
      actionButton("showDownloadOptions", "Download Options")
    } else {
      NULL
    }
  })

  observeEvent(input$downloadBtn, {
    showModal(modalDialog(
      title = "Download options",
      tagList(
        h4(HTML("&nbsp;"), "Download model data (.zip):"),
        h5("Inputs:"), ## TODO: rename this subsection
        checkboxInput("dlPolygon", "Currently selected reporting polygon (.shp)", TRUE),
        checkboxInput("dlCachedRasters", "Cached raster input files (.tif)", TRUE),
        checkboxInput("dlInitCommMap", "Inital communities map (.tif)", TRUE),
        ###
        h5("Outputs:"),
        h6("Large Patches Data for study region"),
        checkboxInput("dlLargePatchesData", "Large Patches Data (.csv)", TRUE),
        checkboxInput("dlLargePatchesHists", "Large patches histograms (.png)", TRUE),

        h6("Leading Vegetation Cover Data for study region"),
        checkboxInput("dlVegData", "Leading Vegetation Cover Data (.csv)", TRUE),
        checkboxInput("dlVegAgeHists", "Leading Vegetaiton Cover histograms (.png)", TRUE),

        h6("Simulation Rasters (cropped to study reagion)"),
        checkboxInput("dlFlammableMaps", "Flammability maps (.grd)", TRUE),
        checkboxInput("dlTimeSinceFireMaps", "Time Since Fire maps (.tif)", TRUE),
        checkboxInput("dlVegTypeMaps", "Vegetation type maps (.grd, .tif)", TRUE),

        h6("Additional R Data Files"),
        checkboxInput("dlSimOutputs", "Simulation data files (.RData, .rds)", FALSE), ## false by default
        ###
        radioButtons("usePrefix", "Prefix all filenames with app version info:",
                     choices = list(yes = TRUE, no = FALSE), selected = "no"), ## false by default
        downloadButton(ns("downloadData"), "Download")
      ),
      footer = paste(appInfo$name, "version", appInfo$version),
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
      fileList <- list(file.path("outputs", "README.Rmd")) ## TODO: create this file to provide info/metadata on the other outputs

      ## TODO: prefix `LandWeb_v1.0.0_` to all file outputs

      ### "inputs"
      if (isTRUE(input$dlPolygon)) {
        polygonFiles <- list.files(file.path("outputs", paste0(subStudyRegionName, "_Proprietary"), "Polygons"),
                                   pattern = rctChosenPolyName(),
                                   recursive = TRUE, full.names = TRUE)
      }

      if (isTRUE(input$dlInitCommMap)) {
        initCommMapFile <- file.path("outputs", paste0(subStudyRegionName, "_Proprietary"), "initialCommunitiesMap.tif")
        append(fileList, initCommMapFile)
      }

      if (isTRUE(input$dlCachedRasters)) {
        cachedRasterFiles <- list.files(file.path("cache", paste0(subStudyRegionName, "_Proprietary"), "rasters"),
                                        recursive = TRUE, full.names = TRUE)
        fileList <- append(fileList, cachedRasterFiles)
      }

      ### Large Patches Data
      if (isTRUE(input$dlLargePatchesData)) {
        largePatchesDataFile <- file.path("outputs", paste0(subStudyRegionName, "_Proprietary"), "largePatches.csv")
        write.csv(rctLargePatchesData(), largePatchesDataFile)
        fileList <- append(fileList, largePatchesDataFile)
      }

      if (isTRUE(input$dlLargePatchesHists)) {
        histFiles1 <- list.files(file.path("outputs", paste0(subStudyRegionName, "_Proprietary"),
                                           gsub(" ", "_", rctChosenPolyName()),
                                           "histograms", "largePatches"),
                                 recursive = TRUE, full.names = TRUE)
        fileList <- append(fileList, histFiles1)
      }

      ### Veg Class Data
      if (isTRUE(input$dlVegData)) {
        vegDataFile <- file.path("outputs", paste0(subStudyRegionName, "_Proprietary"), "vegArea.csv")
        write.csv(rctVegData(), vegDataFile)
        fileList <- append(fileList, vegDataFile)
      }

      if (isTRUE(input$dlVegAgeHists)) {
        histFiles2 <- list.files(file.path("outputs", paste0(subStudyRegionName, "_Proprietary"),
                                           gsub(" ", "_", rctChosenPolyName()),
                                           "histograms", "vegAgeMod"),
                                 recursive = TRUE, full.names = TRUE)
        fileList <- append(fileList, histFiles2)
      }

      ### Simulation rasters
      if (isTRUE(input$dlFlammableMaps)) {
        flammableMapFiles <- list.files(file.path("outputs", paste0(subStudyRegionName, "_Proprietary")),
                                        recursive = TRUE, full.names = TRUE, pattern = "rstFlammable")
        append(fileList, vegTypeMapFIles)
      }

      if (isTRUE(input$dlTimeSinceFireMaps)) {
        tsfMapFiles <- list.files(file.path("outputs", paste0(subStudyRegionName, "_Proprietary")),
                                  recursive = TRUE, full.names = TRUE, pattern = "rstTimeSinceFire")
        append(fileList, vegTypeMapFIles)
      }

      if (isTRUE(input$dlVegTypeMaps)) {
        vegTypeMapFiles <- list.files(file.path("outputs", paste0(subStudyRegionName, "_Proprietary")),
                                      recursive = TRUE, full.names = TRUE, pattern = "vegTypeMap")
        append(fileList, vegTypeMapFIles)
      }

      ### other simulation data files
      if (isTRUE(input$dlSimOutputs)) {
        simOutputFiles <- list.files(file.path("outputs", paste0(subStudyRegionName, "_Proprietary")),
                                     recursive = TRUE, full.names = TRUE, pattern = "[.]RData|[.]rds")
        fileList <- append(fileList, simOutputFiles)
      }

      ### append filename prefix if selected
      if (isTRUE(input$usePrefix)) {
        fileList <- lapply(fileList, function(fname) {
          filePrefix <- paste0(appInfo$name, "_v", appInfo$version, "_")
          file.path(dirname(fname), .prefix(basename(fname), filePrefix)) %>%
            gsub("/\\./", "/", .)
        })
      }

      ## create the zip file containing the selected files
      zip(file, files = unlist(fileList))
    },
    contentType = "application/zip"
  )
}
