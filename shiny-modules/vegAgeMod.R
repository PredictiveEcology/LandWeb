#' Histogram module server function
#'
#' @param datatable         A \code{data.table} object.
#'                          See \code{\link[SpaDES.shiny]{getSubtable}}.
#'
#' @param chosenCategories  ... See \code{\link[SpaDES.shiny]{getSubtable}}.
#'
#' @param chosenValues      ... See \code{\link[SpaDES.shiny]{getSubtable}}.
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
vegHistServerFn <- function(datatable, id, .current, .dtFull, outputPath,
                            chosenPolyName, authStatus, rebuildHistPNGs, ...) {
  observeEvent(datatable, label = paste(.current, collapse = "-"), {
    vegDT <- if (is.reactive(datatable)) {
      datatable()
    } else {
      datatable
    }
    assertthat::assert_that(
      is.data.table(vegDT),
      msg = "vegHistServerFn: `datatable` is not a data.table"
    )

    propVeg <- vegDT$proportion

    breaksLabels <- (0:11) / 10
    breaks <- breaksLabels - 0.05
    barplotBreaks <- breaksLabels + 0.05

    addAxisParams <- list(side = 1, labels = breaksLabels, at = barplotBreaks)

    dtOnlyCC <- vegDT[grepl("CurrentCondition", label)]
    dtNoCC <- vegDT[!grepl("CurrentCondition", label)]

    vegHist <- hist(dtNoCC$proportion, breaks = breaks, plot = FALSE)
    #vegHistCC <- hist(dtOnlyCC$proportion, breaks = breaks, plot = FALSE)

    histogramData <- if (sum(vegHist$counts) == 0) {
      vegHist$counts
    } else {
      vegHist$counts / sum(vegHist$counts)
    }

    sigdigs <- ceiling(-log10(diff(range(breaks)) / length(breaks) / 10))
    barWidth <- unique(round(diff(vegHist$breaks), digits = sigdigs))

    verticalLineAtX <- if (isTRUE(authStatus)) {
      #browser(expr = .current$polygonID != "Taiga Shield")
      verticalLine <- if (length(dtOnlyCC$proportion) == 0) {
        0
      } else  {
        dtOnlyCC$proportion
      }
      verticalLine <- verticalLine + barWidth/2
    } else {
      NULL
    }

    # if (FALSE) {
    #   dtListShort <- split(.dtFull, by = uiSequence$category[-length(uiSequence$category)], flatten = FALSE)
    #
    #   # need to get a single set of breaks for all simultaneously visible histograms
    #   dtInner <- dtListShort[[.current$ageClass]][[.current$polygonID]]
    #
    #   if (NROW(dtInner) > 0) {
    #     dtOnlyCC <- dt[rep == "CurrentCondition"]
    #     dtNoCC <- dt[rep != "CurrentCondition"]
    #
    #     out <- dtNoCC[, .N, by = c("vegCover", "rep")]$N
    #     if (isTRUE(authStatus)) {
    #       outCC <- max(0, dtOnlyCC[, .N, by = c("vegCover", "rep")]$N)
    #       verticalLineAtX <- outCC
    #     } else {
    #       verticalLineAtX <- NULL
    #       outCC <- numeric()
    #     }
    #     nClusters <- dtInner[, .N, by = c("vegCover", "rep")]$N
    #     minNumBars <- 6
    #     maxNumBars <- 30
    #     rangeNClusters <- range(c(outCC, nClusters, minNumBars))
    #     attemptedNumBars <- max(minNumBars, min(maxNumBars, diff(rangeNClusters)))
    #     breaksRaw <- seq(rangeNClusters[1], rangeNClusters[2], length.out = attemptedNumBars)
    #     prettyBreaks <- pretty(breaksRaw, n = attemptedNumBars, min.n = min(attemptedNumBars, minNumBars))
    #     dataForBreaks <- hist(nClusters, plot = FALSE, breaks = prettyBreaks)
    #     breaksLabels <- dataForBreaks$breaks
    #     breaksInterval <- diff(breaksLabels)[1]
    #     dataForHistogram <- hist(out, plot = FALSE, breaks = prettyBreaks)
    #     histogramData <- dataForHistogram$counts/sum(dataForHistogram$counts)
    #
    #     histogramData[is.na(histogramData)] <- 0 # NA means that there were no large patches in dt
    #     # dataForHistogramCC <- hist(outCC, plot = FALSE, breaks = prettyBreaks)
    #     # histogramDataCC <- dataForHistogramCC$counts/sum(dataForHistogramCC$counts)
    #   } else {
    #     if (isTRUE(authStatus)) { # need a default value for vertical line, in case there are no dtInner
    #       verticalLineAtX <- 0
    #     } else {
    #       verticalLineAtX <- NULL
    #     }
    #     histogramData <- c(1,0,0,0,0,0,0)
    #     breaksLabels = 0:6
    #     breaksInterval <- 1
    #   }
    #   breaks <- breaksLabels - breaksInterval/2
    #   barplotBreaks <- breaksLabels + breaksInterval/2
    #   ticksAt <- barplotBreaks - min(breaksLabels)
    #   xlim <- range(ticksAt) - breaksInterval/2
    #   addAxisParams <- list(side = 1, labels = breaksLabels, at = barplotBreaks - min(breaksLabels))
    #   verticalLineAtX <- verticalLineAtX + breaksInterval/2 # THe barplot xaxis is 1/2 a barwidth off
    #
    # }
    polyName <- chosenPolyName %>% gsub(" ", "_", .)
    pngDir <- file.path(outputPath, "histograms", polyName, "vegAgeMod") %>% checkPath(create = TRUE)
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

    callModule(histogram, id, histogramData, addAxisParams, verticalBar = verticalLineAtX,
               width = barWidth, file = pngFilePath,
               xlim = range(breaks), ylim = c(0, 1), xlab = "", ylab = "Proportion in NRV",
               col = "darkgrey", border = "grey", main = "", space = 0)
  })
}

#'
#'
vegAgeModUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    htmlOutput(ns("vegTitle")),
    htmlOutput(ns("vegDetails")),
    shinydashboard::box(
      width = 12, solidHeader = TRUE, collapsible = TRUE,
      shinycssloaders::withSpinner(slicerUI(ns("vegSlicer")))
    )
  )
}

#'
#'
vegAgeMod <- function(input, output, session, rctPolygonList, rctChosenPolyName = reactive({NULL}),
                      rctLeadingDTlist, rctLeadingDTlistCC, rctVtm, ageClasses, outputPath) {

  output$vegTitle <- renderUI({
    column(width = 12,
           h2("NRV of forest, by leading vegetation for each age class in each polygon (",
              rctChosenPolyName(), ")"))
  })

  output$vegDetails <- renderUI({
    column(width = 12,
           h4("These figures show the NRV of the proportion of forests for each age class,",
              "in each polygon, that are in each leading vegetation type.",
              "The proportions are proportions", em("within"), "age class: ",
              "Young (<40 yrs), Immature (40-80 yrs), Mature (80-120 yrs), Old (>120 yrs).",
              "In any given replicate, the numbers below sum to 1."))
  })

  rctVegData <- reactive({
    assertthat::assert_that(is.character(rctChosenPolyName()), is.list(rctLeadingDTlist()))

    dt <- if (is.null(rctLeadingDTlistCC())) {
      ## free
      rctLeadingDTlist()[[rctChosenPolyName()]]
    } else {
      ## proprietary
      rbindlist(list(rctLeadingDTlist()[[rctChosenPolyName()]],
                     rctLeadingDTlistCC()[[rctChosenPolyName()]]))
    }

    dtFn <- function(dt, curPoly) {
      # WORK AROUND TO PUT THE CORRECT LABELS ON THE POLYGON TABS
      #curPoly <- rctPolygonList()[[rctChosenPolyName()]][["crsSR"]]
      polygonID <- as.character(seq_along(curPoly))
      polygonName <- curPoly$shinyLabel

      dt$polygonID <- polygonName[match(dt$polygonID, polygonID)]

      haveNumericPolyId <- dt$polygonID %in% polygonID
      dt$polygonID[haveNumericPolyId] <- polygonName[match(dt$polygonID[haveNumericPolyId], polygonID)]

      assertthat::assert_that(is.data.table(dt) || is.null(dt))
      dt
    }
    dtFn(dt = dt, curPoly = rctPolygonList()[[rctChosenPolyName()]][["crsSR"]])
  })

  uiSequence <- reactive({
    assertthat::assert_that(is.list(rctPolygonList()), is.character(rctChosenPolyName()), is.character(rctVtm()))

    #polygonIDs <- as.character(seq_along(rctPolygonList()[[rctChosenPolyName()]][["crsSR"]]))
    polygonIDs <- rctPolygonList()[[rctChosenPolyName()]][["crsSR"]]$shinyLabel


    rasVtmTmp <- raster(rctVtm()[1]) # to extract factors
    data.table::data.table(
      category = c("polygonID", "vegCover", "ageClass"),
      uiType = c("tab", "tab", "box"),
      possibleValues = list(polygonIDs, levels(rasVtmTmp)[[1]][, 2], ageClasses)
    )
  })

  observeEvent(rctChosenPolyName(), {
    authStatus <- isTRUE(session$userData$userAuthorized())
    callModule(slicer, "vegSlicer", datatable = rctVegData,
               uiSequence = uiSequence(),
               serverFunction = vegHistServerFn, ## calls histogram server module
               uiFunction = function(id) {
                 histogramUI(id, height = 300)
               },
               outputPath = outputPath,
               chosenPolyName = rctChosenPolyName(),
               authStatus = authStatus,
               rebuildHistPNGs = authStatus
    )
  })

  return(rctVegData)
}
