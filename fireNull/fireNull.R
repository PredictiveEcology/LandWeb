
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "fireNull",
  description = "implement a spatially stratified vanWagner fire model",
  keywords = c("fire cycle", "burn probability"),
  authors = c(person(c("Steven", "G"), "Cumming", email="stevec@sbf.ulaval.ca", role=c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("1.1.1.9001"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireNull.Rmd"),
  reqdPkgs = list("raster"),
  parameters = rbind( #should initial times be 0 or 1?
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter("doAgeMapping", "logical", FALSE, FALSE, TRUE, desc="keep track of time since fire?"),
    defineParameter("returnInterval", "numeric", 1.0, NA, NA, desc="interval between main events"),
    defineParameter("startTime","numeric", 0, NA, NA, desc="time of first burn event"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, desc="This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, desc="This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, desc="This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, desc="This describes the simulation time at which the first save event should occur"),
    defineParameter(".statsInitialTime", "numeric", NA, NA, NA, desc="This describes the simulation time at which the first stats event should occur")
  ),
  inputObjects = data.frame(
    #should LCC05, the fire regime shapefile and the study area raster be specified?
    #they should be loaded by the parent, and are needed for init.
    objectName = c("rstFlammable","rstBurnProb"),
    objectClass = c("RasterLayer","RasterLayer"),
    sourceURL = c("",""),
    other = NA_character_,
    stringsAsFactors = FALSE
  ),
  outputObjects = data.frame(
    objectName = c("burnLoci","rstCurrentBurn","rstTimeSinceFire"),
    objectClass = c("vector", "RasterLayer", "RasterLayer"),
    other = NA_character_,
    stringsAsFactors = FALSE
  )
))

doEvent.fireNull = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    # do stuff for this event
    sim <- sim$fireNullInit(sim)
    # schedule future event(s)
    sim <- scheduleEvent(sim, params(sim)$fireNull$startTime, "fireNull", "burn")
    sim <- scheduleEvent(sim, params(sim)$fireNull$.plotInitialTime, "fireNull", "plot")
    sim <- scheduleEvent(sim, params(sim)$fireNull$.saveInitialTime, "fireNull", "save")
    sim <- scheduleEvent(sim, params(sim)$fireNull$.statsInitialTime, "fireNull", "stats")
  } else if (eventType == "plot") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
    if (params(sim)$fireNull$doAgeMapping == TRUE){
      Plot(sim$timeSinceFireMap)
    }
    Plot(sim$rstCurrentBurn)
    # e.g.,
    sim <- scheduleEvent(sim, time(sim) + params(sim)$fireNull$.plotInterval, "fireNull", "plot")
    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == "save") {
    sim <- scheduleEvent(sim, time(sim) + params(sim)$fireNull$.saveInterval, "fireNull", "save")
  } else if (eventType == "burn") {
    sim <- fireNullBurn(sim)
    #do some book-keeping, not part of the actual fire process
    if (params(sim)$fireNull$doAgeMapping == TRUE){
      sim$rasterTimeSinceFire <- sim$rasterTimeSinceFire + params(sim)$fireNull$returnInterval
      sim$rasterTimeSinceFire[sim$burnLoci] = 0 
    }
    #schedule next burn event
    sim <- scheduleEvent(sim, time(sim) + params(sim)$fireNull$returnInterval, "fireNull", "burn")
  } else if (eventType == "stats"){
    sim <- fireNullStatsF(sim)
    sim <- scheduleEvent(sim, time(sim) + params(sim)$fireNull$returnInterval, "fireNull", "stats")
  }
  else {
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
                  "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  }
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
fireNullInit <- function(sim) {
  browser()
  sim$rstCurrentBurn <- sim$rstBurnProb * 0 #this conserves NAs
  setColors(sim$rstCurrentBurn,n=2) <- colorRampPalette(c("grey90", "red"))(2)
 
   if (params(sim)$fireNull$doAgeMapping == TRUE){
    #ideally, we would have loaded an actual initial age map based e.g.
    #on inventory and or the national product that BEACONs uses 
    #(see the BEACONs modules for details and source)
    #sim$rasterTimeSinceFire <- sim$rasterBurnProb * 0 #this conserves NAs
    tsf <- sim$shpStudyRegion$fireReturnInterval
    sim$rstTimeSinceFire <- rasterize(sim$shpStudyRegion,sim$LCC05,field=tsf, mask=TRUE)
    sim$rstTimeSinceFire[which(sim$rstFlammable[] == 1)] <- NA #non-flammable areas are permanent.
    #assign legend and colours if you are serious
  }
  
  #for any stats, we need to caculate how many burnable cells there are
  N<- sum(!is.na(sim$rstBurnProb[]))
  N<- N - length(which(sim$rstBurnProb[] == 1)) # we will "mask" the lakes etc. with 0, not NA
  sim$nBurnableCells <- N
  sim$burnLoci <- vector("numeric")
  ##
  sim$fireNullStats<-list(N=numeric(0),rate=numeric(0))
  
  return(invisible(sim))
}

### template for save events
fireNullSave <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
fireNullPlot <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot("object")
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}


fireNullBurn <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  browser()
  N<-ncell(sim$rstBurnProb)
  sim$rstCurrentBurn<-sim$rstCurrentBurn*0 #zero, but preserve NAs
  sim$burnLoci<-which(runif(N) < sim$rstBurnProb) #this ignores any NAs in the map.
  sim$rstCurrentBurn[sim$burnLoci]<-1 #mark as burned.
  
  return(invisible(sim))
}

fireNullStatsF<-function(sim){
  N<- sim$nBurnableCells
  
  sim$fireNullStats$rate<-c(sim$fireNullStats$rate,length(sim$burnLoci)/N)
  sim$fireNullStats$N<-c(sim$fireNullStats$N,length(sim$burnLoci))
  return(invisible(sim))
}

.init = function(sim) {
  
  #if (!exists("shpStudyRegion",where=envir(sim)) ||
  #    TRUE # test if it is a proper shapefile
  #    ){ 
  #  stop("missing or invalid shapefile: how did you even get here?")
  #}
  
  return(invisible(sim))
}

