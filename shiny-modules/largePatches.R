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
                         uiSequence, outputPath, chosenPolyName, patchSize, rebuildHistPNGs) {
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
      dtOnlyCC <- dt[rep == "CurrentCondition"]
      dtNoCC <- dt[rep != "CurrentCondition"]

      out <- dtNoCC[, .N, by = c(current3Names, "rep")]$N # not hard coded
      if (isTRUE(authStatus)) {
        outCC <- max(0, dtOnlyCC[, .N, by = c(current3Names, "rep")]$N)
        verticalLineAtX <- outCC
      } else {
        verticalLineAtX <- NULL
        outCC <- numeric()
      }

      # Sometimes there are missing elements --- this means that zero must be in the range for x axis
      dtInnerEmpty <- data.table(current3 = uiSequence$possibleValues[[3]], N = 0)
      setnames(dtInnerEmpty, old = "current3", new = current3Names)
      nClustersDT <- .dtInner[, .N, by = c(current3Names, "rep")]
      nClustersDT <- nClustersDT[dtInnerEmpty, on = c(current3Names), nomatch = NA]
      nClustersDT[is.na(N), N := 0]
      nClusters <- nClustersDT$N

      minNumBars <- 6
      maxNumBars <- 30
      rangeNClusters <- range(c(0, outCC, nClusters, minNumBars)) ## TODO: verify
      attemptedNumBars <- max(minNumBars, min(maxNumBars, diff(rangeNClusters)))
      breaksRaw <- seq(rangeNClusters[1], rangeNClusters[2], length.out = attemptedNumBars)
      prettyBreaks <- pretty(breaksRaw, n = attemptedNumBars, min.n = min(attemptedNumBars, minNumBars))
      dataForBreaks <- hist(nClusters, plot = FALSE, breaks = prettyBreaks)
      breaksLabels <- dataForBreaks$breaks
      breaksInterval <- diff(breaksLabels)[1]
      dataForHistogram <- if (NROW(out) == 0) {
        # add a bar at zero if there are no patches
        hist(0, plot = FALSE, breaks = prettyBreaks)
      } else {
        hist(out, plot = FALSE, breaks = prettyBreaks)
      }

      histogramData <- dataForHistogram$counts / sum(dataForHistogram$counts)

      histogramData[is.na(histogramData)] <- 0 # NA means that there were no large patches in dt
      # dataForHistogramCC <- hist(outCC, plot = FALSE, breaks = prettyBreaks)
      # histogramDataCC <- dataForHistogramCC$counts/sum(dataForHistogramCC$counts)
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
    xlim <- range(ticksAt) - breaksInterval / 2
    addAxisParams <- list(side = 1, labels = breaksLabels, at = barplotBreaks - min(breaksLabels))
    verticalLineAtX <- verticalLineAtX + breaksInterval / 2 # The barplot xaxis is 1/2 a barwidth off

    polyName <- chosenPolyName %>% gsub(" ", "_", .)
    pngDir <- file.path(outputPath, "histograms", polyName, "largePatches", patchSize) %>%
      checkPath(create = TRUE)
    pngFile <- paste0(paste(.current, collapse = "-"), ".png") %>% gsub(" ", "_", .)
    pngPath <- file.path(pngDir, pngFile)
    pngFilePath <- if (isTRUE(rebuildHistPNGs)) {
      if (file.exists(pngPath)) {
        NULL
      } else {
        pngPath
      }
    } else {
      NULL
    }

    callModule(histogram, id, histogramData, addAxisParams,
               verticalBar = verticalLineAtX, width = breaksInterval, fname = pngFilePath,
               border = "grey",
               col = "darkgrey",
               main = paste(.current, collapse = " "),
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
      numericInput(ns("patchSize"), value = 500L, min = 100L, max = NA_integer_,
                   label = paste0("Type patch size in hectares that defines 'Large', ",
                                  "(numbers below 100 will not work)"))
    ),
    shinydashboard::box(
      width = 12, solidHeader = TRUE, collapsible = TRUE,
      shinycssloaders::withSpinner(slicerUI(ns("largePatchSlicer")))
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
#' @param rctLrgPatchesCC       \code{reactiveValues} object containing the
#'                              large patches current condition \code{data.table}.
#' @param rctTsf                    #TODO: describe
#' @param rctVtm                    #TODO: describe
#' @param ageClasses                #TODO: describe
#' @param FUN                       #TODO: describe
#' @param nPatchesFun               #TODO: describe
#' @param outputPath                The path where outputs are stored.
#' @param rebuildHistPNGs           Logical indicating whether PNGs should be rebuilt.
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
                         rctLrgPatches, rctLrgPatchesCC, rctTsf, rctVtm,
                         ageClasses, FUN, nPatchesFun, outputPath, rebuildHistPNGs) {

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
           h4("To change the patch size that defines these, type a new value below."))
  })

  rctLargePatchesDataOrig <- reactive({
    assertthat::assert_that(
      is.character(rctChosenPolyName()),
      is.list(rctLrgPatches()),
      is.list(rctLrgPatchesCC())
      #!is.null(rctLrgPatches()[[rctChosenPolyName()]])
    )

    dt <- if (is.null(rctLrgPatchesCC()[[rctChosenPolyName()]])) {
      ## free
      rctLrgPatches()[[rctChosenPolyName()]]
    } else {
      ## proprietary
      rbindlist(list(rctLrgPatches()[[rctChosenPolyName()]],
                     rctLrgPatchesCC()[[rctChosenPolyName()]]))
    }

    # WORK AROUND TO PUT THE CORRECT LABELS ON THE POLYGON TABS
    curPoly <- rctPolygonList()[[rctChosenPolyName()]][["crsSR"]]
    polygonID <- as.character(seq_along(curPoly))
    polygonName <- curPoly$shinyLabel

    dt$polygonID <- polygonName[match(dt$polygonID, polygonID)]

    haveNumericPolyId <- dt$polygonID %in% polygonID
    dt$polygonID[haveNumericPolyId] <- polygonName[match(dt$polygonID[haveNumericPolyId], polygonID)]

    assertthat::assert_that(is.data.table(dt) || is.null(dt))
    dt
  })

  rctLargePatchesData <- reactive({
    dtFn <- function(rctLargePatchesDataOrig, patchSize) {
      rctLargePatchesDataOrig()[sizeInHa > patchSize]
    }
    # too slow with Cache
    dtFn(rctLargePatchesDataOrig = rctLargePatchesDataOrig, patchSize = input$patchSize)
  })

  uiSequence <- reactive({
    #polygonIDs <- as.character(seq_along(rctPolygonList()[[rctChosenPolyName()]][["crsSR"]]))
    polygonIDs <- rctPolygonList()[[rctChosenPolyName()]][["crsSR"]]$shinyLabel

    rasVtmTmp <- raster(rctVtm()[1]) # to extract factors
    data.table::data.table(
      category = c("polygonID", "ageClass", "vegCover"),
      uiType = c("tab", "tab", "box"),
      possibleValues = list(polygonIDs, ageClasses, c(levels(rasVtmTmp)[[1]][, 2], "All species"))
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
               outputPath = outputPath,
               chosenPolyName = rctChosenPolyName(),
               patchSize = as.character(input$patchSize),
               rebuildHistPNGs = authStatus
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
