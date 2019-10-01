rasterAnimationUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    uiOutput(ns("animation")) %>% shinycssloaders::withSpinner(),
    uiOutput(ns("description"))
  )
}

rasterAnimation <- function(input, output, session, appDir, animationsInfo, rctChosenPolyName) {

  filename <- reactive({
    id <- which(animationsInfo[["FMA"]] == rctChosenPolyName())
    f <- if (!is.na(animationsInfo[id, ][["NEWFILE"]])) {
      file.path("rasters", animationsInfo[id, ][["NEWFILE"]])
    } else {
      file.path("rasters", "placeholder.gif")
    }
    message(f) ## TODO: debug only
    f
  })

  output$animation <- renderUI({
    img(src = filename(),
        width = 600,
        height = 600,
        alt = "Simulated fire and vegetation dynamics over 50 years.")
  })

  output$description <- renderUI({
    if (basename(filename()) == "placeholder.gif") {
      p("An animation of the simulation is not avalaible for this management area.")
    } else {
      p("Simulated fire and vegetation dynamics over 50 years.")
    }
  })
}
