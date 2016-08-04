
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "vanWagner",
  description = "insert module description here",
  keywords = c("insert key words here"),
  authors = c(person(c("Steve", "G"), "Cumming", email="stevec@sbf.ulaval.ca", role=c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("1.1.1.9001"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "vanWagner.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter("pBurn", "numeric", 0.01, 0, 1, desc="the rate of burn"),
    defineParameter("returnInterval", "numeric", 1.0, NA, NA, desc="interval between main events"),
    defineParameter("startTime","numeric", 0, NA, NA, desc="time of first burn event"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, desc="This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, desc="This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, desc="This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, desc="This describes the simulation time at which the first save event should occur"),
    defineParameter(".statsInitialTime", "numeric", 0, NA, NA, desc="This describes the simulation time at which the first stats event should occur")
  ),
  inputObjects = data.frame(
    objectName = "ageMap",
    objectClass = "RasterLayer",
    sourceURL = "",
    other = NA_character_,
    stringsAsFactors = FALSE
  ),
  outputObjects = data.frame(
    objectName = c("ageMap","ignitionLoci"),
    objectClass = c("RasterLayer","vector"),
    other = NA_character_,
    stringsAsFactors = FALSE
  )
))

## event types
#   - type `init` is required for initialiazationlob

doEvent.vanWagner = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)
    #browser()
    # do stuff for this event
    sim <- sim$vanWagnerInit(sim)
    
    # schedule future event(s)
    sim <- scheduleEvent(sim, params(sim)$vanWagner$startTime, "vanWagner", "burn")
    sim <- scheduleEvent(sim, params(sim)$vanWagner$.plotInitialTime, "vanWagner", "plot")
    sim <- scheduleEvent(sim, params(sim)$vanWagner$.saveInitialTime, "vanWagner", "save")
    sim <- scheduleEvent(sim, params(sim)$vanWagner$.statsInitialTime, "vanWagner", "stats")
  } else if (eventType == "plot") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
    
    #Plot(objectFromModule) # uncomment this, replace with object to plot
    # schedule future event(s)
    
    # e.g.,
    sim <- scheduleEvent(sim, params(sim)$vanWagner$.plotInitialTime, "vanWagner", "plot")
    
    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == "save") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
    
    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function
    
    # schedule future event(s)
    
    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "vanWagner", "save")
    
    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == "burn") {
    # ! ----- EDIT BELOW ----- ! #
    sim <- vanWagnerBurn(sim)
    sim <- scheduleEvent(sim, time(sim) + params(sim)$vanWagner$returnInterval, "vanWagner", "burn")
    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == "stats"){
    sim <- vanWagnerStatsF(sim)
    sim <- scheduleEvent(sim, time(sim) + params(sim)$vanWagner$returnInterval, "vanWagner", "stats")
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
vanWagnerInit <- function(sim) {
  
  sim$vanWagnerStats<-list(N=numeric(0),p=numeric(0),rate=numeric(0))
  
  return(invisible(sim))
}

### template for save events
vanWagnerSave <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
vanWagnerPlot <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot("object")
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}


vanWagnerBurn <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  #note that this assumes square maps with no holes.
  N<-prod(dim(sim$ageMap))
  sim$ignitionLoci<-which(runif(N) < params(sim)$vanWagner$pBurn)
  sim$ageMap[sim$ignitionLoci]<-0  
  return(invisible(sim))
}

vanWagnerStatsF<-function(sim){
  N<-prod(dim(sim$ageMap))
  sim$vanWagnerStats$rate<-c(sim$vanWagnerStats$rate,length(sim$ignitionLoci)/N)
  sim$vanWagnerStats$p<-c(sim$vanWagnerStats$p,params(sim)$vanWagner$pBurn)
  sim$vanWagnerStats$N<-c(sim$vanWagnerStats$N,length(sim$ignitionLoci))
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

