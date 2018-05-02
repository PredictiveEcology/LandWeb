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

    if (isTRUE(session$userData$userAuthorized())) {
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
      tmpDir <- file.path(tempdir(), SpaDES.core::rndstr(1, 10, characterFirst = TRUE)) %>%
        reproducible::checkPath(., create = TRUE)
      outputDirs <- c("histograms", "polygons", "rasters")
      lapply(file.path(tmpDir, outputDirs), reproducible::checkPath, create = TRUE)
      on.exit(unlink(tmpDir, recursive = TRUE), add = TRUE)

      fileList <- list()

      currPoly <- rctPolygonList()[[rctChosenPolyName()]][["crsSR"]]

      ### "inputs"
      if (isTRUE(input$dlPolygon)) {
        polygonFile2 <- file.path(tmpDir, "polygons", paste0(rctChosenPolyName(), ".shp"))
        raster::shapefile(currPoly, filename = polygonFile2)
        fileList <- append(fileList, polygonFile2)
      }

      ### Current condition
      if (isTRUE(input$dlCC)) {
        ccFile <- file.path("cache", paste0(subStudyRegionName), "rasters", "CurrentCondition.tif")
        ccFile2 <- file.path(tmpDir, "rasters", basename(ccFile))

        raster::raster(ccFile) %>%
          SpaDES.tools::postProcess(., postProcessedFilename = ccFile2, studyArea = currPoly)
        fileList <- append(fileList, ccFile2)
      }

      ### Large Patches Data
      if (isTRUE(input$dlLargePatchesData)) {
        largePatchesDataFile2 <- file.path(tmpDir, "largePatches.csv")
        write.csv(rctLargePatchesData(), largePatchesDataFile2)
        fileList <- append(fileList, largePatchesDataFile2)
      }

      if (isTRUE(input$dlLargePatchesHists)) {
        histFilesLP <- list.files(
          file.path("outputs", paste0(subStudyRegionName, "_All"),
                    "histograms", gsub(" ", "_", rctChosenPolyName()),
                    "largePatches", patchSize),
          recursive = TRUE, full.names = TRUE
        )
        histFilesLP2 <- file.path(tmpDir, "histograms", "largePatches", patchSize, basename(histFilesLP))
        unique(dirname(histFilesLP2)) %>% reproducible::checkPath(., create = TRUE)
        file.copy(histFilesLP, histFilesLP2)
        fileList <- append(fileList, histFilesLP2)
      }

      ### Veg Class Data
      if (isTRUE(input$dlVegData)) {
        vegDataFile2 <- file.path(tmpDir, "vegArea.csv")
        write.csv(rctVegData(), vegDataFile2)
        fileList <- append(fileList, vegDataFile2)
      }

      if (isTRUE(input$dlVegAgeHists)) {
        histFilesVA <- list.files(
          file.path("outputs", paste0(subStudyRegionName, "_All"),
                    "histograms", gsub(" ", "_", rctChosenPolyName()),
                    "vegAgeMod"),
          recursive = TRUE, full.names = TRUE
        )
        histFilesVA2 <- file.path(tmpDir, "histograms", "vegAgeMod", basename(histFilesVA))
        dir.create(unique(dirname(histFilesVA2)))
        file.copy(histFilesVA, histFilesVA2)
        fileList <- append(fileList, histFilesVA2)
      }

      ### Simulation rasters
      if (isTRUE(input$dlTimeSinceFireMaps)) {
        tsfMapFiles <- list.files(
          file.path("outputs", paste0(subStudyRegionName, "_All")),
          recursive = TRUE, full.names = TRUE, pattern = "rstTimeSinceFire"
        )
        tsfMapFiles2 <- file.path(tmpDir, "rasters",  basename(tsfMapFiles))

        tsfRasterList <- lapply(tsfMapFiles, raster::raster)
        Map(x = tsfRasterList, postProcessedFilename = tsfMapFiles2,
            MoreArgs = list(studyArea = currPoly), SpaDES.tools::postProcess)
        fileList <- append(fileList, tsfMapFiles2)
      }

      if (isTRUE(input$dlVegTypeMaps)) {
        vegTypeMapFiles <- list.files(
          file.path("outputs", paste0(subStudyRegionName, "_All")),
          recursive = TRUE, full.names = TRUE, pattern = "vegTypeMap.+[0-9]\\.tif$"
        )
        vegTypeMapFiles2 <- file.path(tmpDir, "rasters",  basename(vegTypeMapFiles))

        vegRasterList <- lapply(vegTypeMapFiles, raster::raster)
        Map(x = vegRasterList, postProcessedFilename = vegTypeMapFiles2,
            MoreArgs = list(studyArea = currPoly), SpaDES.tools::postProcess)
        fileList <- append(fileList, vegTypeMapFiles2)
      }

      ### other simulation data files
      if (isTRUE(input$dlSimOutputs)) {
        simOutputFiles <- list.files(
          file.path("outputs", paste0(subStudyRegionName, "_All")),
          recursive = TRUE, full.names = TRUE, pattern = "[.]RData|[.]rds"
        )
        simOutputFiles2 <- file.path(tmpDir, basename(simOutputFiles))
        file.copy(simOutputFiles, simOutputFiles2)
        fileList <- append(fileList, simOutputFiles2)
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
      readmeFile2 <- file.path(tmpDir, basename(readmeFile))
      file.copy(readmeFile, readmeFile2)

      ## create the zip file containing the selected files
      cwd <- getwd()
      setwd(tmpDir); on.exit(setwd(cwd), add = TRUE)
      zip(file, files = c(readmeFile2, unlist(fileListRenamed)))
    },
    contentType = "application/zip"
  )
}
