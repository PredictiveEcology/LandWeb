
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "makeLeafletTiles",
  description = NA, #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9005", makeLeafletTiles = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "makeLeafletTiles.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
  ),
  outputObjects = bind_rows(
    createsOutput("allRasters", "list", "List with all the rasters that had been written to disk, as described in outputs(sim)")
    #createsOutput("objectName", "objectClass", "output object description", ...),
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.makeLeafletTiles = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      # schedule the one event -- making the tiles
      sim <- scheduleEvent(sim, start(sim), "makeLeafletTiles", "makeAllTiles")
    },
    makeAllTiles = {
      makeTiles(sim)
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

makeTiles <- function(sim) {
  outs <- outputs(sim)
  savedObjs <- outs$objectName
  savedObjsUnique <- unique(savedObjs)
  names(savedObjs) <- savedObjs
  names(savedObjsUnique) <- savedObjsUnique
  rastFiles <- outs[grepl("\\.tif|\\.grd", outs$file), "file"]
  
  rasts <- lapply(rastFiles, function(r) {
    raster(r)
  })
  outputPath <- file.path("www", "tiles")
  sim$allRasters <- Cache(lapply, rasts, function(r)
    gdal2Tiles(
      r,
      outputPath,
      zoomRange = 1:11,
      colorTableFile = asPath(colorTableFile),
      rasterForTransparency = asPath(file.path(outputPath(sim), "rstFlammable.grd")),
      cacheRepo = cachePath(sim)
    ))
  return(invisible(sim))
}

