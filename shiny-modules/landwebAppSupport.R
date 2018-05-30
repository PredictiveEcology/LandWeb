landwebAppSupportUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("appSupportUI"))
}

landwebAppSupport <- function(input, output, session, appInfo) {
  output$appSupportUI <- renderUI({
    ns <- session$ns

    tagList(
      fluidRow(
        shinydashboard::box(
          title = "Frequently Asked Questions", status = "success",
          solidHeader = TRUE, collapsible = TRUE, width = 12,

          h4("1. How do I get a LandWeb.ca account?"),
          p("LandWeb.ca use Google authentication, so you will log in via your Google/Gmail account."),

          h4("2. How do I download model outputs?"),
          p("Downloads are only available to authorized (logged in) users. Please log in using your Google account."),

          h4("3. Can I run the model over my own spatial area instead of using the built-in reporting polygons?"),
          p("Yes. Logged in users can upload their own shapefiles by selecting 'Upload' on the Time Sinnce Fire maps page.")
        ),

        shinydashboard::box(
          title = "Additional Support", status = "success",
          solidHeader = TRUE, collapsible = TRUE, width = 12,

          h4("Model Interpretation"),
          HTML(
            paste0(appInfo$support$model.name, " (",
                   a(appInfo$support$model.email,
                     href = paste0("mailto:", appInfo$support$model.email, "&subject='",
                                   appInfo$name, " Model Support'")), ")")
          ),

          h4("Technical Support"),
          HTML(
            paste0(appInfo$support$tech.name, " (",
                   a(appInfo$support$tech.email,
                     href = paste0("mailto:", appInfo$support$tech.email, "&subject='",
                                   appInfo$name, " App Support'")), ")")
          )
        )
      )
    )
  })
}
