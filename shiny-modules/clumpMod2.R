#' Clump module
#'
#' @description Shiny module used ...
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#'
#' @return Shiny module UI.
#'
#' @author Damian Rodziewicz
#' @export
#' @importFrom shiny NS tagList numericInput
#' @rdname clumpMod2
clumpMod2UI <- function(id) {
  ns <- NS(id)

  tagList(
    numericInput(ns("patchSize"), value = 500L, min = 100L, max = NA_integer_,
                 label = paste0("Type patch size in hectares that defines 'Large', ",
                                "(numbers below 100 will not work)")
    )
  )
}

#' Clump Module
#'
#' @description Shiny module used to ... # TODO: documentation needed
#'
#' @param input Shiny server input object.
#' @param output Shiny server output object.
#' @param session Shiny server session object.
#' @param tsf ...
#' @param vtm ...
#' @param currentPolygon ...
#' @param cl ...
#' @param ageClasses ...
#' @param largePatchesFn   ...
#' @param countNumPatches  ...
#' @param paths            A named list of paths containining \code{cachePath},
#'                         \code{modulePath}, \code{inputPath}, \code{outputPath}.
#'
#' @return A reactive list containing items \code{Clumps} and \code{patchSize}.
#'
#' @export
#' @importFrom archivist addTagsRepo
#' @importFrom shiny withProgress setProgress
#' @rdname clumpMod2
clumpMod2 <- function(input, output, session, tsf, vtm, currentPolygon, cl,
                      ageClasses, largePatchesFn, countNumPatches, paths) {
  clumps <- reactive({
    patchSize <- as.integer(input$patchSize)

    if (FALSE) {
      message(paste("Running largePatchesFn"))
      shiny::withProgress(message = "Calculation in progress",
                          detail = "...", value = 0, {
                            args <- list(largePatchesFn,
                                         timeSinceFireFiles = tsf,
                                         vegTypeMapFiles = vtm,
                                         cl = if (tryCatch(is(cl, "cluster"),
                                                           error = function(x) FALSE)) cl,
                                         polygonToSummarizeBy = currentPolygon,
                                         ageClasses = ageClasses,
                                         countNumPatches = countNumPatches,
                                         paths = paths,
                                         omitArgs = "cl")
                            args <- args[!unlist(lapply(args, is.null))]
                            lrgPatches <- do.call(Cache, args)
                            assertthat::assert_that(is.data.table(lrgPatches))
                            shiny::setProgress(1)
                          })
      message(paste("  Finished largePatchesFn"))
    }

    return(list(ClumpsDT = lrgPatches[sizeInHa > patchSize], patchSize = patchSize))
  })

  return(clumps)
}
