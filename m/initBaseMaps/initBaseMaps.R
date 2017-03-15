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
  sim$LCC05 <- Cache(crop,sim$LCC05X,sim$shpStudyRegion)
  rm(LCC05X,envir=envir(sim))
  tmp<-getColors(sim$LCC05)[[1]]
  sim$rstStudyRegion <- Cache(cacheRepo=file.path(cachePath(sim), "StudyRegion"),
                              sim$fastRasterize, 
                              polygon = sim$shpStudyRegion,
                              ras = sim$LCC05)#,
  #field = "LTHRC") # Don't use field to keep as factor
  # 
  # # Instead of mask, just use indexing
  sim$LCC05[is.na(sim$rstStudyRegion[])] <- NA
  return(invisible(sim))
}


initBaseMapsCache <- function(sim){
  #
  return(invisible(sim))
}

fastRasterize <- function(polygon, ras, field) {
  nonNACellIDs <- raster::extract(ras, polygon, cellnumbers = TRUE)
  polygonIDs <- seq_along(nonNACellIDs)
  nonNACellIDs <- lapply(polygonIDs, function(x) cbind(nonNACellIDs[[x]], "ID"=x))
  nonNACellIDs <- do.call(rbind,nonNACellIDs)
  singleRas <- raster(ras)
  singleRas[] <- NA
  singleRas[nonNACellIDs[,"cell"]] <- nonNACellIDs[,"ID"]
  if(!missing(field)) {
    if(length(field)==1) {
      singleRas[] <- plyr::mapvalues(singleRas[], from=polygonIDs, to=polygon[[field]])
      numFields <- 1
    } else {
      numFields <- 2
    }
  } else {
    numFields <- 3
  }
  if(numFields==3) {
    field <- names(polygon)
  } 
  levels(singleRas) <- data.frame(ID=polygonIDs, polygon[field])
  singleRas
  
}
