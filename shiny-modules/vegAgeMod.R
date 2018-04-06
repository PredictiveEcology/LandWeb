#' Histogram module server function
#'
#' @param datatable         A \code{data.table} object.
#'                          See \code{\link[SpaDES.shiny]{getSubtable}}.
#'
#' @param chosenCategories  ... See \code{\link[SpaDES.shiny]{getSubtable}}.
#'
#' @param chosenValues      ... See \code{\link[SpaDES.shiny]{getSubtable}}.
#'
#' @param nSimTimes         Number of simulation times.
#'
#' @return
#'
#' @author Mateusz Wyszynski
#' @author Alex Chubaty
#' @importFrom assertthat assert_that
#' @importFrom data.table is.data.table
#' @importFrom graphics hist
#' @importFrom purrr map
#' @importFrom shiny callModule reactive
#' @importFrom SpaDES.shiny getSubtable histogram
#' @rdname
vegHistServerFn <- function(datatable, chosenCategories, chosenValues) {
  observeEvent(datatable, label = chosenValues, {
    dt <- if (is.reactive(datatable)) {
      datatable()
    } else {
      datatable
    }
    assertthat::assert_that(
      is.data.table(dt),
      msg = "vegHistServerFn: `datatable` is not a data.table"
    )

    vegDT <- getSubtableMem(dt, chosenCategories, chosenValues)
    propVeg <- vegDT$proportion

    breaksLabels <- (0:11)/10
    breaks <- breaksLabels - 0.05
    barplotBreaks <- breaksLabels + 0.05

    addAxisParams <- list(side = 1, labels = breaksLabels, at = barplotBreaks)

    vegHist <- hist(propVeg, breaks = breaks, plot = FALSE)

    histogramData <- vegHist$counts / sum(vegHist$counts)

    sigdigs <- ceiling(-log10(diff(range(breaks)) / length(breaks) / 10))
    barWidth <- unique(round(diff(vegHist$breaks), digits = sigdigs))

    callModule(histogram, "vegHists", histogramData, addAxisParams,
               width = barWidth,
               xlim = range(breaks), ylim = c(0, 1), xlab = "", ylab = "Proportion in NRV",
               col = "darkgrey", border = "grey", main = "", space = 0)
  })
}

#'
#'
vegAgeModUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(width = 12, h2("NRV of forest, by leading vegetation for each age class in each polygon")),
    column(width = 12, h4("These figures show the NRV of the proportion of forests for each age class,",
                          "in each polygon, that are in each leading vegetation type.",
                          "The proportions are proportions", em("within"), "age class.",
                          "In any given replicate, the numbers below sum to 1.")),
    shinydashboard::box(
      width = 12, solidHeader = TRUE, collapsible = TRUE,
      shinycssloaders::withSpinner(slicerUI(ns("vegSlicer")))
    )
  )
}

#'
#'
vegAgeMod <- function(input, output, session, rctPolygonList, rctChosenPolyName = reactive(NULL),
                      rctLeadingDTlist, rctVtm, ageClasses) {

  rctVegData <- reactive({
    assertthat::assert_that(is.character(rctChosenPolyName()), is.list(rctLeadingDTlist()))

    rctLeadingDTlist()[[rctChosenPolyName()]]
  })

  uiSequence <- reactive({
    assertthat::assert_that(is.list(rctPolygonList()), is.character(rctChosenPolyName()), is.character(rctVtm()))

    polygonIDs <- as.character(seq_along(rctPolygonList()[[rctChosenPolyName()]][["crsSR"]][["shpSubStudyRegion"]]))

    rasVtmTmp <- raster(rctVtm()[1]) # to extract factors
    data.table::data.table(
      category = c("ageClass", "polygonID", "vegCover"),
      uiType = c("tab", "tab", "box"),
      possibleValues = list(ageClasses, polygonIDs, levels(rasVtmTmp)[[1]][, 2])
    )
  })

  callModule(slicer, "vegSlicer", datatable = rctVegData,
             categoryValue = "vegAgeMod",
             uiSequence = uiSequence(),
             serverFunction = vegHistServerFn, ## calls histogram server module
             uiFunction = function(ns) {
               histogramUI(ns("vegHists"), height = 300)
  })
}
