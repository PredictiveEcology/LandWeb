inputTablesUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      h3("Currently, these inputs are not changeable by specific regions."),
      p("These species trait values have been customized for LandWeb based on the LANDIS-II defaults."),
      box(
        width = 12,
        solidHeader = TRUE,
        status = "success",
        title = "LandWeb Species Inputs",
        dataTableOutput(ns("speciesInputs"))
      )
    ),
    fluidRow(
      box(
        width = 12,
        solidHeader = TRUE,
        status = "success",
        title = "Geographically varying species inputs. These are means (and SE) across all map regions.",
        dataTableOutput(ns("speciesEcoregionInputs"))
      )
    )
  )
}

inputTables <- function(input, output, session) {
  output$speciesInputs <- renderDataTable({
    #landisInputs ## defined in global.R
    speciesTraits ## defined in global.R
  })

  output$speciesEcoregionInputs <- renderDataTable({
    spEcoReg ## defined in global.R
  })
}
