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
          p("LandWeb.ca use Google authentication, so you will log in via your Gmail account.")
        ),

        shinydashboard::box(
          title = "Additional Support", status = "success",
          solidHeader = TRUE, collapsible = TRUE, width = 12,

          p("For additional suuport, please contact: "),
          HTML(
            paste0(appInfo$support$name, " (",
                   a(appInfo$support$email,
                     href = paste0("mailto:", appInfo$support$email, "&subject='",
                                   appInfo$name, " App Support'")), ")")
          )
        )
      )
    )
  })
}
