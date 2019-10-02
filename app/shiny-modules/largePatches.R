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
histServerFn <- function(datatable, id, .current, .dtFull, .dtInner, nSimTimes, authStatus,
                         uiSequence, chosenPolyName, patchSize) {
  observeEvent(datatable, label = paste(.current, collapse = "-"), {
    dt <- if (is.reactive(datatable)) {
      datatable()
    } else {
      datatable
    }
    assertthat::assert_that(
      is.data.table(dt),
      msg = "histServerFn2: `datatable` is not a data.table"
    )

    current3Names <- names(.current)[[3]]
    if (NROW(.dtInner) > 0) {
      ## NOTE: now using only one DT with NCC column
      if (isTRUE(authStatus)) {
        outCC <- max(0, dt[["NCC"]])
        verticalLineAtX <- outCC
      } else {
        verticalLineAtX <- NULL
        outCC <- numeric()
      }

      # Sometimes there are missing elements --- this means that zero must be in the range for x axis
      dtInnerEmpty <- data.table(current3 = uiSequence$possibleValues[[3]], N = 0)
      setnames(dtInnerEmpty, old = "current3", new = current3Names)
      nClusters <- dt[["N"]]

      minNumBars <- 6
      maxNumBars <- 30
      rangeNClusters <- range(c(0, outCC, nClusters, minNumBars)) ## TODO: verify
      attemptedNumBars <- max(minNumBars, min(maxNumBars, diff(rangeNClusters)))
      breaksRaw <- seq(rangeNClusters[1], rangeNClusters[2], length.out = attemptedNumBars)
      prettyBreaks <- pretty(breaksRaw, n = attemptedNumBars, min.n = min(attemptedNumBars, minNumBars))
      dataForBreaks <- hist(nClusters, plot = FALSE, breaks = prettyBreaks)
      breaksLabels <- dataForBreaks$breaks
      breaksInterval <- diff(breaksLabels)[1]
      dataForHistogram <- if (length(dt[["N"]]) == 0) {
        # add a bar at zero if there are no patches
        hist(0, plot = FALSE, breaks = prettyBreaks)
      } else {
        hist(dt[["N"]], plot = FALSE, breaks = prettyBreaks)
      }

      histogramData <- dataForHistogram$counts / sum(dataForHistogram$counts)

      histogramData[is.na(histogramData)] <- 0 # NA means that there were no large patches in dt
    } else {
      if (isTRUE(authStatus)) {
        # need a default value for vertical line, in case there are no .dtInner
        verticalLineAtX <- 0
      } else {
        verticalLineAtX <- NULL
      }
      histogramData <- c(1, 0, 0, 0, 0, 0, 0)
      breaksLabels <- 0:6
      breaksInterval <- 1
    }
    breaks <- breaksLabels - breaksInterval / 2
    barplotBreaks <- breaksLabels + breaksInterval / 2
    ticksAt <- barplotBreaks - min(breaksLabels)

    xlim <- range(ticksAt) - breaksInterval / 2 ## c(0, max(dt[["N"]], dt[["NCC"]]))
    addAxisParams <- list(side = 1, labels = breaksLabels, at = barplotBreaks - min(breaksLabels))
    verticalLineAtX <- verticalLineAtX + breaksInterval / 2 # The barplot xaxis is 1/2 a barwidth off

    polyName <- chosenPolyName %>% gsub(" ", "_", .)

    callModule(histogram, id, histogramData, addAxisParams,
               verticalBar = verticalLineAtX, width = breaksInterval, fname = NULL,
               border = "grey",
               col = "darkgrey",
               main = "", #paste(.current, collapse = " "), ## the tabs provide the figure info
               space = 0,
               xlim = xlim, ylim = c(0, 1),
               xlab = paste("Number of patches greater than", patchSize, "ha"),
               ylab = "Proportion in NRV")
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
#' @importFrom magrittr %>%
#' @importFrom shiny fluidRow NS
#' @importFrom shinydashboard box
#' @rdname largePatches
largePatchesUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    htmlOutput(ns("title")),
    htmlOutput(ns("details")),
    shinydashboard::box(
      width = 12, solidHeader = TRUE, collapsible = TRUE,
      selectInput(ns("patchSize"), choices = c(100L, 500L, 1000L, 5000L), selected = 100L,
                  label = "Patch size (ha) that defines a 'large' patch.")
    ),
    shinydashboard::box(
      width = 12, solidHeader = TRUE, collapsible = TRUE,
      slicerUI(ns("largePatchSlicer")) %>% shinycssloaders::withSpinner()
    )
  )
}

#' @param input                 Shiny server input object.
#' @param output                Shiny server output object.
#' @param session               Shiny server session object.
#' @param rctPolygonList        A list of polygons for to use while calculating
#'                              large patches results.
#' @param rctChosenPolyName     Name of the polygon to extract from polygonList.
#' @param rctLrgPatches         \code{reactiveValues} object containing the
#'                              large patches \code{data.table}.
#' @param ageClasses            #TODO: describe
#' @param FUN                   #TODO: describe
#' @param nPatchesFun           #TODO: describe
#'
#' @return Shiny module server function.
#'
#' @author Mateusz Wyszynski
#' @author Alex Chubaty
#' @export
#' @importFrom assertthat assert_that
#' @importFrom data.table data.table is.data.table
#' @importFrom shiny callModule reactive is.reactivevalues
#' @importFrom SpaDES.shiny histogramUI
#' @rdname largePatches
largePatches <- function(input, output, session, rctPolygonList, rctChosenPolyName = reactive(NULL),
                         rctLrgPatches, ageClasses, FUN, nPatchesFun) {

  output$title <- renderUI({
    column(width = 12,
           h2("NRV of number of 'large' (", strong(as.character(input$patchSize)),
              " hectares) patches in ", rctChosenPolyName(), " polygon"))
  })

  output$details <- renderUI({
    column(width = 12,
           h4("These figures show the NRV of the probability distribution",
              "of patches that are ", as.character(input$patchSize), " hectares ",
              "or larger, for each given combination of Age Class, ",
              "Leading Vegetation, and Polygon."),
           h4("Age classes: Young (<40 yrs), Immature (40-80 yrs), Mature (80-120 yrs), Old (>120 yrs)."),
           h4("To change the patch size that defines these, select a new value below."))
  })

  rctLargePatchesDataOrig <- reactive({
    assertthat::assert_that(
      is.character(rctChosenPolyName()),
      is.data.table(rctLrgPatches())
    )

    dt <- rctLrgPatches()

    # WORK AROUND TO PUT THE CORRECT LABELS ON THE POLYGON TABS
    curPoly <- rctPolygonList()[[rctChosenPolyName()]]
    polygonID <- as.character(seq_along(curPoly))
    polygonName <- curPoly[["shinyLabel"]]

    ## TODO: make this work with ANSR and caribou polygons!
    ## these polys are not in the rctPolygonList, so need to get them from `ml` somehow

    set(dt, NULL, "polygonID", polygonID[match(dt[["polygonName"]], polygonName)])

    haveNumericPolyId <- dt[["polygonID"]] %in% polygonID
    dt[["polygonID"]][haveNumericPolyId] <- polygonName[match(dt[["polygonID"]][haveNumericPolyId], polygonID)]

    assertthat::assert_that(is.data.table(dt) | is.null(dt))
    dt[haveNumericPolyId, ] ## TODO: adjust this so things are prefiltered correctly earlier on
  })

  rctLargePatchesData <- reactive({
    rctLargePatchesDataOrig()[size == as.numeric(input$patchSize), ]
  })

  uiSequence <- reactive({
    polygonIDs <- rctPolygonList()[[rctChosenPolyName()]][["shinyLabel"]]
    species <- unique(rctLrgPatches()[["vegCover"]])
    data.table::data.table(
      category = c("polygonID", "ageClass", "vegCover"),
      uiType = c("tab", "tab", "box"),
      possibleValues = list(polygonIDs, ageClasses, species)
    )
  })

  observeEvent({
    rctChosenPolyName()
    input$patchSize
  }, {
    authStatus <- isTRUE(session$userData$userAuthorized())
    callModule(slicer, "largePatchSlicer", datatable = rctLargePatchesData,
               uiSequence = uiSequence(),
               serverFunction = histServerFn, ## calls histogram server module
               uiFunction = function(id) {
                 histogramUI(id, height = 300)
               },
               authStatus = authStatus,
               chosenPolyName = rctChosenPolyName(),
               patchSize = as.character(input$patchSize)
    )
  })

  return(reactive(list(data = rctLargePatchesData(), patchSize = input$patchSize)))
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
