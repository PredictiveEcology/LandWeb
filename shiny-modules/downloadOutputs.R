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
        downloadButton('downloadData', 'Download')
      )
    } else {
      NULL
    }
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      dir2zip <- list.files(tempdir(), recursive = TRUE)
      zip(tempfile(fileext = ".zip"), files = dir2zip)
    },
    contentType = "application/zip"
  )
}
