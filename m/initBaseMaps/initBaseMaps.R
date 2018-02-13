defineModule(sim, list(
  name = "initBaseMaps",
  description = "load, reproject and crop all initial maps and shapefile required to test LandWEB models.",
  keywords = c("testing", "map layer initialisation"),
  authors = c(person(c("Steve", "G"), "Cumming", email = "stevec@sbf.ulaval.ca", role = c("aut"))),
  childModules = character(),
  version = numeric_version("1.2.0.9005"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit =  "year", #no relevence. An init module only.
  citation = list(""),
  documentation = list("README.txt", "initBaseMaps.Rmd"),
  reqdPkgs = list("raster", "achubaty/amc@development"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Whether the module should be cached for future calls. This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    expectsInput("LCC2005", "RasterLayer", "Land Cover Classification from 2005, NRCan product"),
    expectsInput("shpStudySubRegion", "SpatialPolygonsDataFrame", "Study Area")
  ),
  outputObjects = bind_rows(
    createsOutput("LCC05", "RasterLayer", "Land Cover Classification from 2005, NRCan product"),
    createsOutput("shpStudyRegion", "SpatialPolygonsDataFrame", "Study Area"),
    createsOutput("rstStudyRegion", "RasterLayer", "Raster version of shpStudyRegion")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.initBaseMaps = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    sim <- Init(sim)
  } else {
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  }
  return(invisible(sim))
}

### template initialization
Init <- function(sim) {
  simProjection <- crs(sim$LCC2005)
  #reproject sim$shpStudyRegion to accord with LCC05
  sim$shpStudyRegion <- Cache(spTransform, sim$shpStudySubRegion, CRSobj = simProjection)

  message("fastRasterize for rstStudyRegion")
  sim$rstStudyRegion <- Cache(fastRasterize, 
  fieldName <- if ("LTHRC" %in% names(sim$shpStudyRegion)) "LTHRC" else names(sim$shpStudyRegion)[1]
                              polygon = sim$shpStudyRegion,
                              ras = crop(sim$LCC2005, extent(sim$shpStudyRegion)),
                              field = fieldName, datatype = "INT2U",
                              filename = "rstStudyRegion")

  cropMask <- function(ras, poly, mask) {
    out <- crop(ras,poly)
    # # Instead of mask, just use indexing
    out[is.na(mask)] <- NA
    out <- writeRaster(out, filename = file.path(tmpDir(), "LCC05_studyArea.tif"),
                       datatype = "INT1U", overwrite = TRUE)
    out
  }

  sim$LCC05 <- Cache(cropMask, ras = sim$LCC2005, poly = sim$shpStudyRegion, mask = sim$rstStudyRegion)

  # rm(LCC05X,envir=envir(sim))
  return(invisible(sim))
}
