
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "fireDataPrep",
  description = "basic data preparation for LCC05 based fire models",
  keywords = c("LCC05"),
  authors = c(person(c("Steve", "G"), "Cumming", email="stevec@sbf.ulaval.ca", role=c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("1.2.0.9005"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireDataPrep.Rmd"),
  reqdPkgs = list("raster","sp"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Whether the module should be cached for future calls. This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = data.frame(
    objectName = c("LCC05", "shpStudyRegion", "rstStudyRegion"),
    objectClass = c("RasterLayer","SpatialPolygonsDataFrame", "RasterLayer"),
    sourceURL = "",
    other = NA_character_,
    stringsAsFactors = FALSE
  ),
  outputObjects = data.frame(
    objectName = c("rstFlammable", "rstBurnProb"),
    objectClass = c("RasterLayer", "RasterLayer"),
    other = NA_character_,
    stringsAsFactors = FALSE
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.fireDataPrep = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)
    # do stuff for this event
    sim <- sim$fireDataPrepInit(sim)
  } else {
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  }
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization

fireDataPrepInit <- function(sim) {
  nonFlammClasses<-c(36,37,38,39)
  oldClass <- 0:39
  newClass <- ifelse(oldClass %in% nonFlammClasses,1,0)   #1 codes for non flammable 
  #see mask argument for SpaDES::spread()
  flammableTable <- cbind(oldClass, newClass)
  #according to Yong, Canada Landcover 2005 is loaded as LCC05 
  sim$rstFlammable <- ratify(reclassify(sim$LCC05, flammableTable,count=TRUE))
  setColors(sim$rstFlammable,n=2) <- colorRampPalette(c("blue", "red"))(2) 
  sim$rstFlammable[is.na(sim$rstStudyRegion[])] <- NA
  # Much faster than call rasterize again
  sim$rstBurnProb <- raster(sim$rstStudyRegion)
  #LTHRC is for some reason the field name of the regional fire cycles according to DA
  sim$rstBurnProb[] <- (1/shpStudyRegion$LTHRC)[sim$rstStudyRegion[]]
  
  
  #doing this here ensures non-flammable cells are accounted for, 
  #no matter when/where rasterBurnProb and rasterFlammable are created.
  sim$rstBurnProb[sim$rstFlammable[] == 1] <- 0 #this could turn some NAs to 0s.
  
  return(invisible(sim))
}


.init = function(sim) {
  # Any code written here will be run during the simInit and subsequently deleted
  # This is useful if there is something required before simulation, such as data downloading, e.g.,
  # downloadData("LCC2005", modulePath(sim))
  # ! ----- EDIT BELOW ----- ! #
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above

