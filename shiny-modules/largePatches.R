#' Histogram module server function
#'
#' @param datatable         A reactive object containing a \code{data.table} object.
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
histServerFn <- function(datatable, chosenCategories, chosenValues, nSimTimes) {
  observeEvent(datatable, {
    dt <- if (is.reactive(datatable)) {
      datatable()
    } else {
      datatable
    }
    assertthat::assert_that(
      is.data.table(dt),
      msg = "largePatches: callModule(slicer): serverFunction: dt is not a data.table"
    )

    subtableWith3DimensionsFixed <- getSubtable(dt, chosenCategories, chosenValues)
    ageClassPolygonSubtable <- getSubtable(dt, head(chosenCategories, 2), head(chosenValues, 2))
    numOfClusters <- ageClassPolygonSubtable[, .N, by = c("vegCover", "rep")]$N
    maxNumClusters <- if (length(numOfClusters) == 0) {
      6
    } else {
      pmax(6, max(numOfClusters) + 1)
    }

    patchesInTimeDistribution <- if (NROW(subtableWith3DimensionsFixed)) {
      numOfPatchesInTime <- subtableWith3DimensionsFixed[, .N, by = "rep"]
      numOfTimesWithPatches <- NROW(numOfPatchesInTime)

      seq(1, nSimTimes) %>%
        map(function(simulationTime) {
          if (simulationTime <= numOfTimesWithPatches) {
            numOfPatchesInTime$N[simulationTime]
          } else {
            0
          }
        })
    } else {
      rep(0, nSimTimes)
    }

    distribution <- as.numeric(patchesInTimeDistribution)

    breaksLabels <- 0:maxNumClusters
    breaks <- breaksLabels - 0.5
    barplotBreaks <- breaksLabels + 0.5

    addAxisParams <- list(side = 1, labels = breaksLabels, at = barplotBreaks)

    actualPlot <- hist(distribution, breaks = breaks, plot = FALSE)

    histogramData <- actualPlot$counts / sum(actualPlot$counts)

    callModule(histogram, "histogram", histogramData, addAxisParams,
               width = rep(1, length(distribution)),
               xlim = range(breaks), xlab = "", ylab = "Proportion in NRV",
               col = "darkgrey", border = "grey", main = "", space = 0)
  })
}

#' Large patches (shiny module)
#'
#' Create summary for large patches function.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#'
#' @return shiny module UI.
#'
#' @author Mateusz Wyszynski
#' @export
#' @importFrom shiny fluidRow NS
#' @importFrom shinydashboard box
#' @rdname largePatches
largePatchesUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    shinydashboard::box(
      width = 12, solidHeader = TRUE, collapsible = TRUE,
      clumpMod2UI(ns("largePatches"))
    ),
    shinydashboard::box(
      width = 12, solidHeader = TRUE, collapsible = TRUE, slicerUI(ns("slicer"))
    )
  )
}

#' @param input    Shiny server input object.
#'
#' @param output   Shiny server output object.
#'
#' @param session  Shiny server session object.
#'
#' @param nSimTimes  How many simulation time stamps there are.
#'
#' @param clumpMod2Args  List containing named arguments passed to \code{clumpMod2}.
#'
#' @return Shiny module server function.
#'
#' @author Mateusz Wyszynski
#' @export
#' @importFrom assertthat assert_that
#' @importFrom data.table data.table is.data.table
#' @importFrom shiny callModule reactive
#' @importFrom SpaDES.shiny histogramUI
#' @rdname largePatches
largePatches <- function(session, input, output, nSimTimes, clumpMod2Args) {
  #patchSize <- callModule(slider, "slider") ## TODO: where is this used? where is the UI component?

  largePatchesData <- reactive({
    clumpMod2Args["id"] <- NULL # remove `id` so it deosn't mess with callModule below

    clumpsReturn <- do.call(callModule, c(list(module = clumpMod2, id = "largePatches"), clumpMod2Args))

    dt_out <- clumpsReturn()$Clumps
    assertthat::assert_that(is.data.table(dt_out))
    dt_out
  })

  uiSequence <- data.table::data.table(category = c("ageClass", "polygonID", "vegCover"),
                                       uiType = c("tab", "tab", "box"))

  callModule(slicer, "slicer", datatable = largePatchesData(),
             categoryValue = "LargePatches", nSimTimes = nSimTimes,
             uiSequence = uiSequence,
             serverFunction = histServerFn, ## calls histogram server module
             uiFunction = function(ns) {
               histogramUI(ns("histogram"), height = 300)
             })
}
