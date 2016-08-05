
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
  reqdPkgs = list("raster","sp"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    #defineParameter("pBurn", "numeric", 0.01, 0, 1, desc="the rate of burn"),
    defineParameter("doAgeMapping", "logical", FALSE, TRUE, FALSE, desc="keep track of time since fire?"),
    defineParameter("returnInterval", "numeric", 1.0, NA, NA, desc="interval between main events"),
    defineParameter("startTime","numeric", 0, NA, NA, desc="time of first burn event"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, desc="This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, desc="This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, desc="This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, desc="This describes the simulation time at which the first save event should occur"),
    defineParameter(".statsInitialTime", "numeric", 0, NA, NA, desc="This describes the simulation time at which the first stats event should occur")
  ),
  inputObjects = data.frame(
    objectName = "",
    objectClass = "",
    sourceURL = "",
    other = NA_character_,
    stringsAsFactors = FALSE
  ),
  outputObjects = data.frame(
    objectName = c("timeSinceFireMap","burnLoci","currentBurnMap"),
    objectClass = c("RasterLayer","vector", "RasterLayer"),
    other = NA_character_,
    stringsAsFactors = FALSE
  )
))

## event types
#   - type `init` is required for initialiazation

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
    #Plot(objectFromModule) # uncomment this, replace with object to plot
    # schedule future event(s)
    
    # e.g.,
    sim <- scheduleEvent(sim, params(sim)$fireNull$.plotInitialTime, "fireNull", "plot")
    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == "save") {
  } else if (eventType == "burn") {
    sim <- fireNullBurn(sim)
    #do some book-keeping not part of the actual fire process
    if (params(sim)$fireNull$doAgeMapping == TRUE){
      sim$timeSinceFireMap <- sim$timeSinceFireMap + 1
      sim$timeSinceFireMap[sim$burnLoci] = 0 
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
  
  sim$currentBurnMap <- sim$burnProbMap * 0
  
  if (params(sim)$fireNull$doAgeMapping == TRUE){
    sim$timeSinceFireMap <- sim$burnProbMap * 0
    #assign legend and colours if you are serious
  }
  
  sim$fireNullStats<-list(N=numeric(0),p=numeric(0),rate=numeric(0))
  
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
  #note that this assumes square maps with no holes.
  #we could filyter out the NAs and 0s, 
  #but I bet it is faster to ignore them. 
  
  N<-prod(dim(sim$burnProbMap))
  sim$currentBurnMap<-sim$currentBurnMap*0 #zero, but preserve NAs
  sim$burnLoci<-which(runif(N) < sim$burnProbMap)
  sim$currentBurnMap[sim$burnLoci]<-1
  
  return(invisible(sim))
}

fireNullStatsF<-function(sim){
  N<- sim$nBurnableCells
  
  sim$fireNullStats$rate<-c(sim$fireNullStats$rate,length(sim$burnLoci)/N)
  sim$fireNullStats$p<-c(sim$fireNullStats$p,params(sim)$fireNull$pBurn)
  sim$fireNullStats$N<-c(sim$fireNullStats$N,length(sim$burnLoci))
  return(invisible(sim))
}
.init = function(sim) {
  
  if (!exists(shapeFileFireRegime,where=envir(sim)) ||
      TRUE # test if it is a proper shapefile
      ){ 
    stop("missing or invalid shapefile: how did you even get here?")
  }
  
  pBurn <- 1/sim$shapeFileFireRegime$fireReturnInterval
  sim$burnProbMap <- rasterise(sim$shapeFileFireRegime,sim$studyAreaRaster,field=pBurn, mask=TRUE)
  #now we need to access the flammability map, and mask to 0 any nonflammable cells.
  ##
  #for any stats, we need to caclulate how many burnable cells there are
  N<- sum(!is.na(sim$burnProbMap)) 
  N<- N - which(sim$burnProbMap == 0) # we will "mask" the lakes etc. with 0, not NA
  sim$nBurnableCells <- N
  sim$burnLoci <- vector("numeric")
  #and yes, we could regionalise this.
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above

