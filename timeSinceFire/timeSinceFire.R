# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "timeSinceFire",
  description = "This tracks time since fire for the LandWEB applications",
  keywords = c("insert key words here"),
  authors = c(person(c("Steve", "G"), "Cumming", email="stevec@sbf.ulaval.ca", role=c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("1.2.0.9005"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  documentation = list("README.txt", "timeSinceFire.Rmd"),
  reqdPkgs = list("raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter("returnInterval", "numeric", 1.0, NA, NA, desc="interval between main events"),
    defineParameter("startTime","numeric", 0, NA, NA, desc="time of first burn event"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur")
  ),
  inputObjects = data.frame(
    objectName = c("LCC05","rstFlammable","shpStudyRegion","burnLoci"),
    objectClass = c("RasterLayer","RasterLayer","SpatialPolygonDataFrame","vector"),
    sourceURL = "",
    other = NA_character_,
    stringsAsFactors = FALSE
  ),
  outputObjects = data.frame(
    objectName = c("rstTimeSinceFire"),
    objectClass = c("RasterLayer"),
    other = NA_character_,
    stringsAsFactors = FALSE
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.timeSinceFire = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)

    # do stuff for this event
    sim <- sim$timeSinceFireInit(sim)

    # schedule future event(s)
    sim <- scheduleEvent(sim, params(sim)$timeSinceFire$.plotInitialTime, "timeSinceFire", "plot")
    sim <- scheduleEvent(sim, params(sim)$timeSinceFire$.saveInitialTime, "timeSinceFire", "save")
    sim <- scheduleEvent(sim, params(sim)$timeSinceFire$startTime, "timeSinceFire", "age")
  } else if (eventType == "age") {
    sim$rasterTimeSinceFire <- sim$rasterTimeSinceFire + params(sim)$fireNull$returnInterval #preserves NAs
    sim$rasterTimeSinceFire[sim$burnLoci] = 0
    #schedule next age event
    sim <- scheduleEvent(sim, time(sim) + params(sim)$timeSinceFire$returnInterval, "timeSinceFire", "age")
  } else if (eventType == "plot") {
    Plot(sim$rstTimeSinceFire)
    # e.g.,
    sim <- scheduleEvent(sim, time(sim) + params(sim)$timeSinceFire$.plotInterval, "timeSinceFire", "plot")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == "save") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "timeSinceFire", "save")

    # ! ----- STOP EDITING ----- ! #
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
timeSinceFireInit <- function(sim) {
    #ideally, we would have loaded an actual initial age map based e.g.
    #on inventory and or the national product that BEACONs uses 
    #(see the BEACONs modules for details and source)
    #sim$rasterTimeSinceFire <- sim$rasterBurnProb * 0 #this conserves NAs
    sim$rstTimeSinceFire <- rasterize(sim$shpStudyRegion,sim$LCC05,field=tsf, mask=TRUE)
    sim$rstTimeSinceFire[] <- sim$rstTimeSinceFire[]   #force into memory
    sim$rstTimeSinceFire[which(sim$rstFlammable[] == 1)] <- NA #non-flammable areas are permanent.
    #assign legend and colours if you are serious
  return(invisible(sim))
}

### template for save events
timeSinceFireSave <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
timeSinceFirePlot <- function(sim) {
  
  return(invisible(sim))
}

