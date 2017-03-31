#' Core Burn function for Andison Fire Module
#'
#' @description The main function for the Andison Fire Module. See details.
#'
#' @param landscape A RasterLayer. This only provides the extent and resolution for the fire spread algorithm.
#' @param startCells A numeric vector indicating the indices on the \code{landscape} where the fires
#'                   will start with 100\% certainty.
#' @param fireSizes A numeric vector indicating the final size of each of the fires. Must be same length as
#'                  \code{startCells}.
#' @param nActiveCells1 A numeric vector of length 2. These are cutoffs above and below each of which different values
#'                      of \code{spawnNewActive} are used. See details.
#' @param spawnNewActive A numeric vector of length 4. These are the probabilities of creating spreading to 2 neighbours
#'                       instead of the 1 default neighbour, each time step. The 4 values are for 4 different fire
#'                       size conditions. See details.
#' @param sizeCutoffs A numeric vector of length 2. These are 2 size thresholds that affect which
#'                    \code{spawnNewActive} probabilities are used. See details.
#'
#' @details
#' This algorithm is a modified contagious cellular automaton. The algorithm is as follows:
#'
#' @section Algorithm:
#'
#' \subsection{Core}{
#' Each fire starts at a single pixel, \code{startCells} and will spread, i.e,. visit and convert
#' from a 0 to the fire id number. It will iteratively
#' spread until the number of cells visited is equal to \code{floor(fireSizes)}.
#' }
#' \subsection{Adjustments due to current fire size and number of active pixels}{
#'
#' That can vary too, but it gets a bit complicated, so use that for now.
#' Spawning probability was originally set at 13%, but created problems with very large and very small fires, so over time has been adjusted to vary depending on a) number of active “firelets” (NF) and b) fire size (FS):
#'   -         If NF>=10 and <36 and FS <20,000 ha then P = 20%
#'   -         If NF>36 and FS <8,000 ha, P=11%
#'   -         If  NF<36 and FS >20,000 ha, P=26%
#'   -         If NF<10 then P=46%
#' These rule create more heterogeneity in the pattern of burning.
#' }
#'
#' @section Not yet implemented:
#' Chances of fire spread stopping at any given pixel in any given firelet is 23%.
#' The fire can also get stuck when it turns back in on itself and burns islands,
#' but now has no way to grow larger from the perimeter.
#' If the fire has not reached its target size, I create new firelet “spot” fires up to 400 pixels away.
#'
#' @return
#'   A \code{data.table} with 4 columns
burn <- function(landscape, startCells, fireSizes = 5, nActiveCells1 = c(10, 36), spawnNewActive = c(0.46, 0.2, 0.26, 0.11),
                 sizeCutoffs = c(8e3, 2e4), spreadProb = 1) {
  a = spread(landscape, loci = startCells, spreadProb = spreadProb, persistence = 0,
             neighProbs = c(1-spawnNewActive[1], spawnNewActive[1]), iterations = 1,
             mask=NULL, maxSize = fireSizes, directions=8, returnIndices = TRUE,
             id = TRUE, plot.it = FALSE, exactSizes = TRUE);
  while(sum(a$active)>0) {
    b <- a[,list(numActive = sum(active), fireSize = .N),by=id]
    set(b, , "pSpawnNewActive", spawnNewActive[1])
    b[numActive>=nActiveCells1[1] & numActive<nActiveCells1[2] & fireSize < sizeCutoffs[2], pSpawnNewActive:=spawnNewActive[2]]
    b[numActive>nActiveCells1[2] & fireSize < sizeCutoffs[1], pSpawnNewActive:=spawnNewActive[4]]
    b[numActive<nActiveCells1[2] & fireSize > sizeCutoffs[2], pSpawnNewActive:=spawnNewActive[3]]
    set(b, , "pNoNewSpawn", 1-b$pSpawnNewActive)

    # spawnNewActive must be joined sent in here as list...
    b <- b[a]
    a <- spread(landscape, spreadProb = spreadProb, spreadState = a, persistence = 0,
                neighProbs = transpose(as.list(b[active==TRUE,c("pNoNewSpawn", "pSpawnNewActive")])), 
                iterations = 1, quick = TRUE, 
                mask=NULL, maxSize = fireSizes, directions=8, returnIndices = TRUE,
                id = TRUE, plot.it = FALSE, exactSizes = TRUE)
  }
  return(a)
}

burn1 <- function(landscape, startCells, fireSizes = 5, nActiveCells1 = c(10, 36), spawnNewActive = c(0.46, 0.2, 0.26, 0.11),
                 sizeCutoffs = c(8e3, 2e4), spreadProb = 1) {
  a = spreadDT(landscape, start = startCells, spreadProb = spreadProb, #persistence = 0,
             neighProbs = c(1-spawnNewActive[1], spawnNewActive[1]), iterations = 1,
             #mask=NULL, 
             asRaster = FALSE, maxSize = fireSizes, directions=8, #returnIndices = TRUE,
             #id = TRUE, plot.it = FALSE, 
             exactSizes = TRUE);
  whActive <- a$state=="activeSource"
  while(any(whActive)) {
    b <- a[,list(numActive = sum(state=="activeSource"), fireSize = .N),by=initialPixels]
    set(b, , "pSpawnNewActive", spawnNewActive[1])
    b[numActive>=nActiveCells1[1] & numActive<nActiveCells1[2] & fireSize < sizeCutoffs[2], pSpawnNewActive:=spawnNewActive[2]]
    b[numActive>nActiveCells1[2] & fireSize < sizeCutoffs[1], pSpawnNewActive:=spawnNewActive[4]]
    b[numActive<nActiveCells1[2] & fireSize > sizeCutoffs[2], pSpawnNewActive:=spawnNewActive[3]]
    set(b, , "pNoNewSpawn", 1-b$pSpawnNewActive)
    
    # spawnNewActive must be joined sent in here as list...
    b <- b[a, on="initialPixels"]
    a <- spreadDT(landscape, spreadProb = spreadProb, start = a, #persistence = 0,
                neighProbs = transpose(as.list(b[state=="activeSource",c("pNoNewSpawn", "pSpawnNewActive")])), 
                iterations = 1, skipChecks = TRUE, asRaster = FALSE,
                #mask=NULL, 
                #maxSize = fireSizes, 
                directions=8, #returnIndices = TRUE,
                #id = TRUE, plot.it = FALSE, 
                exactSizes = TRUE)
    whActive <- a$state=="activeSource"
    
  }
  return(a)
}
