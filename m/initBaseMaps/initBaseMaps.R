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
  reqdPkgs = list("raster", "amc"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".useCache", "numeric", TRUE, NA, NA, "Whether the module should be cached for future calls. This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = data.frame(
    objectName = c("LCC05X", "shpStudySubRegion"),
    objectClass = c("RasterLayer","SpatialPolygonsDataFrame"),
    sourceURL = "",
    other = NA_character_,
    stringsAsFactors = FALSE
  ),
  outputObjects = data.frame(
    objectName = c("LCC05", "shpStudyRegion", "rstStudyRegion"),
    objectClass = c("RasterLayer","SpatialPolygonsDataFrame", "RasterLayer"),
    other = NA_character_,
    stringsAsFactors = FALSE
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.initBaseMaps = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
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
  simProjection <- crs(sim$LCC05X)
  #reproject sim$shpStudyRegion to accord with LCC05
  sim$shpStudyRegion <- Cache(
    #sim$shpStudyRegion <- SpaDES::cache(cachePath(sim), 
    spTransform, 
    sim$shpStudySubRegion, CRSobj=simProjection)
  #sim$shpStudyRegion <- spTransform(sim$shpStudySubRegion, CRSobj=simProjection)
  #sim$LCC05 <- crop(sim$LCC05X,sim$shpStudyRegion)
  
  message("fastRasterize for rstStudyRegion")
  sim$rstStudyRegion <- Cache(fastRasterize, 
                              polygon = sim$shpStudyRegion,
                              ras = crop(sim$LCC05X, extent(sim$shpStudyRegion)),
                              field="LTHRC", datatype="INT2U",
                              filename="rstStudyRegion")
  
  cropMask <- function(ras, poly, mask) {
    out <- crop(ras,poly)
    # # Instead of mask, just use indexing
    out[is.na(mask)] <- NA
    out <- writeRaster(out, filename = file.path(tmpDir(), "LCC05_studyArea.tif"), 
                       datatype = "INT1U",
                       overwrite = TRUE)
    out
  }

  sim$LCC05 <- Cache(cropMask, sim$LCC05X, sim$shpStudyRegion, sim$rstStudyRegion)
  
  rm(LCC05X,envir=envir(sim))
  tmp<-getColors(sim$LCC05)[[1]]
  return(invisible(sim))
}


initBaseMapsCache <- function(sim){
  #
  return(invisible(sim))
}

