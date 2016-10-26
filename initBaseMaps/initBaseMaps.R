defineModule(sim, list(
  name = "initBaseMaps",
  description = "load, reproject and crop all initial maps and shapefile required to test LandWEB models.",
  keywords = c("testing", "map layer initialisation"),
  authors = c(person(c("Steve", "G"), "Cumming", email="stevec@sbf.ulaval.ca", role=c("aut"))),
  childModules = character(),
  version = numeric_version("1.2.0.9005"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit =  "year", #no relevence. An init module only.
  citation = list(""),
  documentation = list("README.txt", "initBaseMaps.Rmd"),
  reqdPkgs = list("raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur")
  ),
  inputObjects = data.frame(
    objectName = c("LCC05X", "shpStudyRegionX"),
    objectClass = c("RasterLayer","SpatialPolygonsDataFrame"),
    sourceURL = "",
    other = NA_character_,
    stringsAsFactors = FALSE
  ),
  outputObjects = data.frame(
    objectName = c("LCC05", "shpStudyRegion"),
    objectClass = c("RasterLayer","SpatialPolygonsDataFrame"),
    other = NA_character_,
    stringsAsFactors = FALSE
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.initBaseMaps = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)

    # do stuff for this event
    #sim <- initBaseMapCache(sim)
    sim <- sim$initBaseMapsInit(sim)
  } 
  else {
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  }
  return(invisible(sim))
}

### template initialization
initBaseMapsInit <- function(sim) {
 # 
  simProjection <- crs(sim$LCC05X)
  #reproject sim$shpStudyRegion to accord with LCC05
  sim$shpStudyRegion <- SpaDES::cache(cachePath(sim), 
                                      spTransform, 
                                      sim$shpStudyRegionX, CRSobj=simProjection)
  sim$LCC05 <- SpaDES::cache(cachePath(sim),
                             crop,sim$LCC05X,sim$shpStudyRegion)
  rm(LCC05X,envir=envir(sim))
  #sim$LCC05[]<-sim$LCC05[] #this kludge has the effect of forcing hthe raster in memory.
  #crs(sim$LCC05) <- simProjection #somebody once thought that crop does not preserve projections
                                  #so we are blindly propagating this code.  
  tmp<-getColors(sim$LCC05)[[1]]
  sim$shpStudyRegionRas <- SpaDES::cache(cachePath(sim),
                                         rasterize, 
                                         x = sim$shpStudyRegion,
                                         y = sim$LCC05)#,
                                         #field = "LTHRC") # Don't use field
  
  # Instead of mask, just use indexing
  sim$LCC05[is.na(sim$shpStudyRegionRas[])] <- NA
  #sim$LCC05 <- mask(sim$LCC05,sim$shpStudyRegion)
  setColors(sim$LCC05, n = 256) <-  tmp #mask removes colors!
  return(invisible(sim))
}


initBaseMapsCache <- function(sim){
  #
  return(invisible(sim))
}
