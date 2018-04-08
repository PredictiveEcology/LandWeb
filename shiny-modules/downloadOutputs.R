downloadOutputsUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("downloadBtn"))
}

downloadOutputs <- function(input, output, session) {

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
      caches2zip <- list.files(file.path("cache", paste0(subStudyRegionName, "_Proprietary"), "rasters"),
                               recursive = TRUE, full.names = TRUE)
      outputs2zip <- list.files(file.path("outputs", paste0(subStudyRegionName, "_Proprietary")),
                                recursive = TRUE, full.names = TRUE)

      allFiles2zip <- c(caches2zip, outputs2zip)

      print(allFiles2zip)
      print(file)

      zip(file, files = allFiles2zip)
    },
    contentType = "application/zip"
  )
}
