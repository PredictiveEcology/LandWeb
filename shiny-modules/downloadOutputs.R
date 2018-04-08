downloadOutputsUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("downloadBtn"))
}

downloadOutputs <- function(input, output, session, rctLargePatchesData, rctVegData,
                            rctPolygonList, rctChosenPolyName) {

  ## only authorized users can download model outputs

  output$downloadBtn <- renderUI({
    ns <- session$ns

    if (isTRUE(session$userData$userAuthorized())) {
      tagList(
        h4(HTML("&nbsp;"), "Download model outputs (.zip)"),
        downloadButton(ns("downloadData"), "Download")
      )
    } else {
      NULL
    }
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("LandWeb_data-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      write.csv(rctLargePatchesData(), file.path("outputs", paste0(subStudyRegionName, "_Proprietary"), "largePatches.csv"))
      write.csv(rctVegData(), file.path("outputs", paste0(subStudyRegionName, "_Proprietary"), "vegArea.csv"))

      caches2zip <- list.files(file.path("cache", paste0(subStudyRegionName, "_Proprietary"), "rasters"),
                               recursive = TRUE, full.names = TRUE)
      outputs2zip <- list.files(file.path("outputs", paste0(subStudyRegionName, "_Proprietary")),
                                recursive = TRUE, full.names = TRUE)

      ## TODO: add the selected polygon as a shp file for download
      #chosenPoly <- rctPolygonList()[[rctChosenPolyName()]][["crsSR"]][["shpSubStudyRegion"]]
      #polyNameClean <- rctChosenPolyName() %>% gsub(" ", "_", .)
      #shpfile <- rgdal::writeOGR(chosenPoly, file.path(tempdir(), polyNameClean), driver = "ESRI Shapefile")

      allFiles2zip <- c(caches2zip, outputs2zip)#, shpfile)

      zip(file, files = allFiles2zip)
    },
    contentType = "application/zip"
  )
}
