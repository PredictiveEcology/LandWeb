inputTablesUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      h3("Currently, these inputs are not changeable by specific regions"),
      box(
        width = 10,
        solidHeader = TRUE,
        status = "success",
        title = "Species Inputs",
        dataTableOutput(ns("speciesInputs"))
      )
    ),
    fluidRow(
      box(
        width = 12,
        solidHeader = TRUE,
        status = "success",
        title = "Geographically Varying Species Inputs. These are means (and SE) across all map regions",
        dataTableOutput(ns("speciesEcoregionInputs"))
      )
    )
  )
}

inputTables <- function(input, output, session) {
  output$speciesInputs <- renderDataTable({
    landisInputs
  })

  output$speciesEcoregionInputs <- renderDataTable({
    spEcoReg
  })
}
