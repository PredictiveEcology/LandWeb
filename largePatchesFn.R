#' Calculate number of "large" patches in a landscape
#'
#' This function does several things:
#'
#' 1. Takes a time series of rasters, all saved to disk, named vegTypeMapFiles_xxx.tif, identified with outputs(mySimOut[[1]])
#'    This raster shows, for each pixel, which species of tree is the "leading" species, i.e,. most dominant
#' 2. For each age class in the vector ageClass (should be reactive, currently is not), derive a new raster,
#'       where each pixel that is not contained between ageClass[x] and ageClass[x+1] is made NA
#' 3. This new raster will have (currently) 4 values, 1 to 4, labelled: Pine, Deciduous, Spruce, Mixed
#' 4. Again, for each of these rasters, one veg type at a time, set all other veg types to NA
#' 5. Run raster::clump function to determine clump sizes, save as a data.table, 3 columns, polygonID, newRas, sizeInHa
#' 6. Repeat steps for each veg type, and each time stamp... making a list of lists (e.g., outer list = time, inner list = veg type)
#' 7. Collapse this to a data.table with columns polygonID, newRas, sizeInHa, vegCover, rep (meaning time)
#' 8. Collapse this to a data.table with 1 more column, adding ageClass
#' 9. This data.table is then used in shiny app for histograms, currently in menu item "Large Patches". It has
#'       4 dimensions which are placed 1 = menu for ageClass, 2 = tab for polygonID, 3 = histograms for vegCover,
#'       4 = content of histogram is sizeInHa
#' 10. There is a reactive element that lets user choose to "omit" any clump that is "less than XXX hectares", set to 500 at start.
#'
#' @return A matrix with counts of number of large patches
largePatchesFn <- function(timeSinceFireFiles, vegTypeMapFiles, polygonToSummarizeBy, cl,
                           ageCutoffs = ageClassCutOffs, countNumPatches = countNumPatches,
                           ageClasses, paths) {

  #  withProgress(message = 'Calculation in progress',
  #               detail = 'This may take a while...', value = 0, {

  
  withoutPath <- basename(timeSinceFireFiles)
  yearNames <- unlist(lapply(strsplit(withoutPath, split = "_|\\."), # split on _ and . 
                             function(x) x[length(x) -1])) # ... take second last one

  rasWithNAs <- raster(raster(timeSinceFireFiles[1]))
  rasWithNAs[] <- NA

  # identify which polygon each pixel is contained within == data.table with 2 columns, cell and polygonID
  cellIDByPolygon <- Cache(cellNumbersForPolygon, rasWithNAs, polygonToSummarizeBy)
  browser()

  if (missing(cl)) {
    lapplyFn <- "lapply"
  } else {
    lapplyFn <- "parLapplyLB"
    if (Sys.info()[["sysname"]] == "Windows") {

      clusterExport(cl = cl,
                    varlist = list(c(ls(), "countNumPatches")),
                    envir = environment())
      clusterEvalQ(cl = cl, {
        library(raster)
        library(magrittr)
        library(SpaDES.core)
        library(data.table)
      })
    }
  }

  out <- lapply(ageCutoffs, function(ages) {
    y <- match(ages, ageCutoffs)
    if (tryCatch(is(cl, "cluster"), error = function(x) FALSE)) {
      startList <- list(cl = cl)
    } else {
      startList <- list()
    }
    startList <- append(startList, list(y = y))
    out1 <- Cache(do.call, notOlderThan = Sys.time(), lapplyFn, append(startList, list(X = timeSinceFireFiles, function(x, ...) {
                    x <- match(x, timeSinceFireFiles)
                    timeSinceFireFilesRast <- raster(timeSinceFireFiles[x])
                    leadingRast <- raster(vegTypeMapFiles[x])
                    leadingRast[timeSinceFireFilesRast[] < ageCutoffs[y]] <- NA
                    if ((y + 1) < length(ageCutoffs))
                      leadingRast[timeSinceFireFilesRast[] >= ageCutoffs[y + 1]] <- NA

                    clumpedRasts <- lapply(raster::levels(leadingRast)[[1]]$ID, function(ID) {
                      browser()
                      spRas <- leadingRast
                      spRas[spRas != ID] <- NA
                      countNumPatches(spRas, cellIDByPolygon, directions = 8)
                    })
                    names(clumpedRasts) <- raster::levels(leadingRast)[[1]]$Factor
                    clumpedRasts <- append(clumpedRasts,
                                           list("All species" =
                                                  countNumPatches(leadingRast,
                                                                  cellIDByPolygon,
                                                                  directions = 8)
                                           ))
                    clumpedRasts
                  })))
    names(out1) <- yearNames

    # collapse to a single data.table
    outDT <- rbindlist(lapply(seq_along(out1), function(y) {
      a <- rbindlist(lapply(seq_along(out1[[y]]), function(x) out1[[y]][[x]][, vegCover := names(out1[[y]])[x]]))
      a[, rep := names(out1)[y]]
    }))
  })

  out <- setNames(out, ageClasses)
  out <- rbindlist(lapply(seq_along(out), function(z) {
    out[[z]][, ageClass := names(out)[z]]
  }))

  browser()
  if (is.numeric(out[[chosenCategories[[length(chosenCategories)]]]]))
    set(datatable, , chosenCategories[[length(chosenCategories)]], as.character(datatable[[chosenCategories[[length(chosenCategories)]]]]))
  
  
  out[sizeInHa >= 100] # never will need patches smaller than 100 ha
  #setProgress(1)
}
