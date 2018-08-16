#' Boxplot module server function
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
#' @author Alex Chubaty
#' @importFrom assertthat assert_that
#' @importFrom data.table is.data.table
#' @importFrom graphics hist
#' @importFrom purrr map
#' @importFrom shiny callModule reactive
#' @importFrom SpaDES.shiny getSubtable histogram
#' @rdname vegBoxplotServerFn
vegBoxplotServerFn <- function(datatable, id, .current, .dtFull, outputPath,
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

    data <- .dtFull[vegCover == .current$vegCover][polygonID == .current$polygonID]
    dataCC <- data[grepl("CurrentCondition", label)]
    data <- data[!grepl("CurrentCondition", label)]

    ids <- match(dataCC$ageClass, ageClasses) %>% unique()
    CCpnts <- dataCC$proportion[ids]
    data$ageClass <- factor(data$ageClass, ageClasses)

    polyName <- chosenPolyName %>% gsub(" ", "_", .)
    pngDir <- file.path(outputPath, "boxplots", polyName, "vegAgeMod") %>% checkPath(create = TRUE)
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

    callModule(boxPlot, id, data, CCpnts, authStatus, fname = pngFilePath,
               col = "limegreen",
               horizontal = TRUE,
               main = paste(.current, collapse = " "),
               xlab = "Proportion of of forest area",
               ylab = "Age class",
               ylim = c(0, 1))
  })
}

#'
vegAgeMod2UI <- function(id) {
  ns <- NS(id)

  fluidRow(
    htmlOutput(ns("vegTitle")),
    htmlOutput(ns("vegDetails")),
    shinydashboard::box(
      width = 12, solidHeader = TRUE, collapsible = TRUE,
      shinycssloaders::withSpinner(slicer2UI(ns("vegSlicer2")))
    )
  )
}

#'
vegAgeMod2 <- function(input, output, session, rctAuthenticationType, rctPolygonList,
                       rctChosenPolyName = reactive({NULL}), leadingDTlist,
                       leadingDTlistCC, rctVtm, ageClasses, outputPath) {

  output$vegTitle <- renderUI({
    column(width = 12,
           h2("NRV of forest, by leading vegetation for each age class in each polygon (",
              rctChosenPolyName(), ")"))
  })

  output$vegDetails <- renderUI({
    column(width = 12,
           h4("These figures show the NRV of the proportion of forests for each age class,",
              "in each polygon, that are in each leading vegetation type."),
           h4("The proportions are proportions", em("within"), "age class: ",
              "Young (<40 yrs), Immature (40-80 yrs), Mature (80-120 yrs), Old (>120 yrs)."),
           h4("In any given replicate, the numbers below sum to 1."))
  })

  rctVegData <- reactive({
    assertthat::assert_that(
      is.character(rctChosenPolyName()),
      is.reactivevalues(leadingDTlist)
      #!is.null(leadingDTlist[[rctChosenPolyName()]])
    )

    dt <- if (rctAuthenticationType() == "Free") {
      ## free
      leadingDTlist[[rctChosenPolyName()]]
    } else if (rctAuthenticationType() == "Proprietary") {
      ## proprietary
      rbindlist(list(leadingDTlist[[rctChosenPolyName()]],
                     leadingDTlistCC[[rctChosenPolyName()]][["Proprietary"]]))
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
    assertthat::assert_that(
      is.list(rctPolygonList()),
      is.character(rctChosenPolyName()),
      is.character(rctVtm())
    )

    #polygonIDs <- as.character(seq_along(rctPolygonList()[[rctChosenPolyName()]][["crsSR"]]))
    polygonIDs <- rctPolygonList()[[rctChosenPolyName()]][["crsSR"]]$shinyLabel

    rasVtmTmp <- raster(rctVtm()[1]) # to extract factors
    data.table::data.table(
      category = c("polygonID", "vegCover"),
      uiType = c("tab", "box"),
      possibleValues = list(polygonIDs, levels(rasVtmTmp)[[1]][, 2])
    )
  })

  observeEvent(rctChosenPolyName(), {
    authStatus <- isTRUE(session$userData$userAuthorized())
    callModule(slicer2, "vegSlicer2", datatable = rctVegData,
               uiSequence = uiSequence(),
               serverFunction = vegBoxplotServerFn,
               uiFunction = function(id) {
                 boxPlotUI(id, height = 500)
               },
               outputPath = outputPath,
               chosenPolyName = rctChosenPolyName(),
               authStatus = authStatus,
               rebuildHistPNGs = authStatus
    )
  })

  return(rctVegData)
}

#===============================================================================

boxPlotUI <- function(id, ...) {
  ns <- NS(id)

  plotOutput(ns("boxplot"), ...)
}

boxPlot <- function(input, output, session, data, CCpnts, authStatus, fname, ...) {
  output$boxplot <- renderPlot({
    if (!is.null(fname)) .doPlotBoxplot(data, CCpnts, authStatus, fname = fname, ...) ## plot once to file
    .doPlotBoxplot(data, CCpnts, authStatus, fname = NULL, ...) ## plot normally to display
  }, height = 400, width = 600)
}

.doPlotBoxplot <- function(data, CCpnts = NULL, authStatus, fname = NULL, ...) {
  if (!is.null(fname)) png(fname, height = 600, width = 800, units = "px")
  boxplot(proportion~as.factor(ageClass), data, ...)

  if (isTRUE(authStatus)) {
    if (length(CCpnts) == 4) {
      points(CCpnts, factor(ageClasses), col = "red", pch = 20, cex = 3)
    } else {
      message(CCpnts)
    }
  }
  if (!is.null(fname)) dev.off()
}
