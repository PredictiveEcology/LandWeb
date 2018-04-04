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
vegHistServerFn <- function(datatable, chosenCategories, chosenValues, nSimTimes) {
  observeEvent(datatable, label = chosenValues, {
    dt <- if (is.reactive(datatable)) {
      datatable()
    } else {
      datatable
    }
    assertthat::assert_that(
      is.data.table(dt),
      msg = "histServerFn: `datatable` is not a data.table"
    )

    vegDT <- getSubtable(dt, chosenCategories, chosenValues)
    propVeg <- vegDT$proportion

    breaksLabels <- (0:11)/10
    breaks <- breaksLabels - 0.5
    barplotBreaks <- breaksLabels + 0.5

    addAxisParams <- list(side = 1, labels = breaksLabels, at = barplotBreaks)

    vegHist <- hist(propVeg, breaks = breaks, plot = FALSE)

    histogramData <- vegHist$counts / sum(vegHist$counts)

    callModule(histogram, "histogram", histogramData, addAxisParams,
               width = rep(1, length(distribution)),
               xlim = range(breaks), ylim = c(0, 1), xlab = "", ylab = "Proportion in NRV",
               col = "darkgrey", border = "grey", main = "", space = 0)
  })
}

#
#
vegAgeModUI <- function(id) {
  ns <- NS(id)

  ids <- strsplit(id, split = "_")[[1]]
  ageClassIndex <- as.numeric(ids[1])
  polygonIndex <- as.numeric(ids[2])
  vegTypeIndex <- as.numeric(ids[3])
  tagList(
    box(width = 4, solidHeader = TRUE, collapsible = TRUE,
        title = paste0(vegLeadingTypes[vegTypeIndex]),
        withSpinner(plotOutput(ns("propCoverHists"), height = 300))
    )
  )
}

vegAgeMod <- function(input, output, session, rctPolygonList, rctChosenPolyName = reactive(NULL),
                      rctTsf, rctVtm, cl = NULL, ageClasses, FUN, nPatchesFun, rctPaths) {

  clumpMod2Args <- reactive(label = "clumpMod2Args", {
    ## TODO: add assertions for other args
    assertthat::assert_that(is.list(rctPolygonList()), is.character(rctChosenPolyName()),
                            is.character(rctTsf()), is.character(rctVtm()))

    args <- list(
      tsf = rctTsf(),
      vtm = rctVtm(),
      currentPolygon = rctPolygonList()[[rctChosenPolyName()]][["crsSR"]][["shpSubStudyRegion"]],
      cl = cl,
      ageClasses = ageClasses,
      largePatchesFn = FUN,
      countNumPatches = nPatchesFun,
      paths = rctPaths()
    )
    args <- args[!unlist(lapply(args, is.null))]
    args
  })

  rctVegData <- reactive({
    args <- clumpMod2Args()
    args["id"] <- NULL # remove `id` so it doesn't mess with callModule below

    rctClumps <- do.call(callModule, c(list(module = clumpMod2, id = "clumpMod2"), args))

    return(rctClumps()$ClumpsDT)

    #####
    leadingDT[[rctChosenPolyName()]]
  })

  uiSequence <- reactive({
    polygonIDs <- as.character(seq_along(rctPolygonList()[[rctChosenPolyName()]][["crsSR"]][["shpSubStudyRegion"]]))

    rasVtmTmp <- raster(rctVtm()[1]) # to extract factors
    data.table::data.table(
      category = c("ageClass", "polygonID", "vegCover"),
      uiType = c("tab", "tab", "box"),
      possibleValues = list(ageClasses, polygonIDs, c(levels(rasVtmTmp)[[1]][,2], "All species"))
    )
  })

  callModule(slicer, "slicer", datatable = rctVegDT,
             categoryValue = "vegAgeMod", nSimTimes = length(rctVtm()),
             uiSequence = uiSequence(),
             serverFunction = vegHistServerFn, ## calls histogram server module
             uiFunction = function(ns) {
               histogramUI(ns("histogram"), height = 300)
  })
}
