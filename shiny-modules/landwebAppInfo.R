landwebAppInfoUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("appInfoUI"))
}

landwebAppInfo <- function(input, output, session, appInfo) {
  output$appInfoUI <- renderUI({
    ns <- session$ns

    tagList(
      fluidRow(
        shinydashboard::box(
          title = "Funding & Partners", status = "success",
          solidHeader = TRUE, collapsible = TRUE, width = 12,
          img(src = "images/fRI_logo.png", align = "left", width = 250)
          ## TODO: add other partner info/logos
        )
      ),
      copyrightAuthorsUI(ns("about-app")) ## already in a fluidRow
    )
  })

  callModule(copyrightAuthors, "about-app",
             appName = paste0(appInfo$name, " v", appInfo$version),
             authorInfo = appInfo$authors,
             copyrightInfo = appInfo$copyright,
             licenseFile = NULL, #"LICENSE",
             status = "success" ## make the boxes have green colour
  )
}
