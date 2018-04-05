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
histServerFn <- function(datatable, chosenCategories, chosenValues, nSimTimes, authStatus) {
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

    subtableWith3DimensionsFixed <- getSubtableMem(dt, chosenCategories, chosenValues)
    ageClassPolygonSubtable <- getSubtableMem(dt, head(chosenCategories, 2), head(chosenValues, 2))

    numOfClusters <- ageClassPolygonSubtable[, .N, by = c("vegCover", "rep")]$N
    maxNumClusters <- if (length(numOfClusters) == 0) {
      6
    } else {
      pmax(6, max(numOfClusters) + 1)
    }

    breaksLabels <- 0:maxNumClusters
    breaks <- breaksLabels - 0.5
    barplotBreaks <- breaksLabels + 0.5

    addAxisParams <- list(side = 1, labels = breaksLabels, at = barplotBreaks)

    subtableWith3DimensionsFixedOnlyCC <- subtableWith3DimensionsFixed[rep == "CurrentCondition"]
    subtableWith3DimensionsFixedNoCC <- subtableWith3DimensionsFixed[rep != "CurrentCondition"]

    out <- .patchesInTimeDistributionFn(subtableWith3DimensionsFixedNoCC, nSimTimes, breaks = breaks)
    outCC <- .patchesInTimeDistributionFn(subtableWith3DimensionsFixedOnlyCC, nSimTimes = 1, breaks = breaks)

    histogramData <- out$actualPlot$counts / sum(out$actualPlot$counts)

    verticalLineAtX <- if (isTRUE(authStatus)) {
      outCC$actualPlot$breaks[c(FALSE, as.logical(outCC$actualPlot$counts))]
    } else {
      NULL
    }

    callModule(histogram, "histogram", histogramData, addAxisParams,
               verticalBar = verticalLineAtX,
               width = rep(1, length(out$distribution)),
               xlim = range(breaks), ylim = c(0, 1), xlab = "", ylab = "Proportion in NRV",
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

  uiOutput(ns("largePatchUI"))
}

#' @param input              Shiny server input object.
#' @param output             Shiny server output object.
#' @param session            Shiny server session object.
#' @param rctPolygonList     A list of polygons for to use while calculating large patches results.
#' @param rctChosenPolyName  Name of the polygon to extract from polygonList.
#' @param nSimTimes          How many simulation time stamps there are.
#' @param rctPaths           A named list of paths containining \code{cachePath},
#'                           \code{modulePath}, \code{inputPath}, \code{outputPath}.
#'
#' @return Shiny module server function.
#'
#' @author Mateusz Wyszynski
#' @author Alex Chubaty
#' @export
#' @importFrom assertthat assert_that
#' @importFrom data.table data.table is.data.table
#' @importFrom shiny callModule reactive
#' @importFrom SpaDES.shiny histogramUI
#' @rdname largePatches
largePatches <- function(input, output, session, rctPolygonList, rctChosenPolyName = reactive(NULL),
                         rctLrgPatches, rctLrgPatchesCC, rctTsf, rctVtm,
                         ageClasses, FUN, nPatchesFun, rctPaths) { # TODO: add docs above

  patchSize <- reactive({
    if (is.null(input$patchSize)) {
      500L
    } else {
      as.integer(input$patchSize)
    }
  })

  output$largePatchUI <- renderUI({
    ns <- session$ns

    fluidRow(
      column(width = 12, h2("NRV of number of 'large' (", strong(as.character(patchSize())),
                            " hectares) patches")),
      column(width = 12, h4("These figures show the NRV of the probability distribution",
                            "of patches that are ", as.character(patchSize()), " hectares ",
                            "or larger, for each given combination of Age Class, ",
                            "Leading Vegetation, and Polygon."),
             h4("To change the patch size that defines these, type a new value below.")),
      shinydashboard::box(
        width = 12, solidHeader = TRUE, collapsible = TRUE,
        numericInput(ns("patchSize"), value = 500L, min = 100L, max = NA_integer_,
                     label = paste0("Type patch size in hectares that defines 'Large', ",
                                    "(numbers below 100 will not work)"))
      ),
      shinydashboard::box(
        width = 12, solidHeader = TRUE, collapsible = TRUE,
        shinycssloaders::withSpinner(slicerUI(ns("largePatchSlicer")))
      )
    )
  })

  # clumpMod2ArgsCC <- reactive(label = "clumpMod2ArgsCC", {
  #   ## TODO: add assertions for other args
  #   assertthat::assert_that(is.list(rctPolygonList()), is.character(rctChosenPolyName()),
  #                           is.character(rctTsf()), is.character(rctVtm()))
  #
  #   args <- list(
  #     tsf = filename(CurrentConditions$CCtsf),
  #     vtm = filename(CurrentConditions$CCvtm),
  #     currentPolygon = rctPolygonList()[[rctChosenPolyName()]][["crsSR"]][["shpSubStudyRegion"]],
  #     cl = cl,
  #     ageClasses = ageClasses,
  #     largePatchesFn = FUN,
  #     countNumPatches = nPatchesFun,
  #     paths = rctPaths()
  #   )
  #   args <- args[!unlist(lapply(args, is.null))]
  #   args
  # })

  # rctLargePatchesData <- reactive({
  #   browser()
  #   args <- clumpMod2Args()
  #   args["id"] <- NULL # remove `id` so it doesn't mess with callModule below
  #
  #   rctClumps <- do.call(callModule, c(list(module = clumpMod2, id = "clumpMod2"), args))
  #
  #   return(rctClumps()$ClumpsDT)
  # })

  # clumpMod2Args <- reactive(label = "clumpMod2Args", {
  #   ## TODO: add assertions for other args
  #   assertthat::assert_that(is.list(rctPolygonList()), is.character(rctChosenPolyName()),
  #                           is.character(rctTsf()), is.character(rctVtm()))
  #
  #   args <- list(
  #     tsf = rctTsf(),
  #     vtm = rctVtm(),
  #     currentPolygon = rctPolygonList()[[rctChosenPolyName()]][["crsSR"]][["shpSubStudyRegion"]],
  #     cl = cl,
  #     ageClasses = ageClasses,
  #     largePatchesFn = FUN,
  #     countNumPatches = nPatchesFun,
  #     paths = rctPaths()
  #   )
  #   args <- args[!unlist(lapply(args, is.null))]
  #   args
  # })

  rctLargePatchesData <- reactive({
    dt <- if (!is.null(rctLrgPatchesCC())) {
      dt <- rbindlist(list(rctLrgPatches()[[rctChosenPolyName()]],
                           rctLrgPatchesCC()[[rctChosenPolyName()]]))
    } else {
      rctLrgPatches()[[rctChosenPolyName()]]
    }

    #currentPolygon = rctPolygonList()[[rctChosenPolyName()]][["crsSR"]][["shpSubStudyRegion"]]

    # args <- clumpMod2ArgsCC()
    # args2 <- clumpMod2Args()
    # args$tsf <- asPath(c(args2$tsf, args$tsf), 2)
    # args$vtm <- asPath(c(args2$vtm, args$vtm), 2)
    # args["id"] <- NULL # remove `id` so it doesn't mess with callModule below
    #
    # rctClumps <- do.call(callModule, c(list(module = clumpMod2, id = "clumpMod2"), args))
    # return(rctClumps()$ClumpsDT)

    dt[sizeInHa > patchSize()]
  })

  uiSequence <- reactive({
    polygonIDs <- as.character(seq_along(rctPolygonList()[[rctChosenPolyName()]][["crsSR"]][["shpSubStudyRegion"]]))

    rasVtmTmp <- raster(rctVtm()[1]) # to extract factors
    data.table::data.table(
      category = c("ageClass", "polygonID", "vegCover"),
      uiType = c("tab", "tab", "box"),
      possibleValues = list(ageClasses, polygonIDs, c(levels(rasVtmTmp)[[1]][, 2], "All species"))
    )
  })

  callModule(slicer, "largePatchSlicer", datatable = rctLargePatchesData,
             categoryValue = "LargePatches",
             uiSequence = uiSequence(),
             serverFunction = histServerFn, ## calls histogram server module
             uiFunction = function(ns) {
               histogramUI(ns("histogram"), height = 300)
             },
             nSimTimes = length(rctTsf()),
             authStatus = session$userData$userAuthorized()
  )
}

.patchesInTimeDistributionFn <- function(dt, nSimTimes, breaks) {
  patchesInTimeDistribution <- if (NROW(dt)) {
    numOfPatchesInTime <- dt[, .N, by = "rep"]
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
  actualPlot <- hist(distribution, breaks = breaks, plot = FALSE)
  return(list(actualPlot = actualPlot, distribution = distribution))
}
