
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "fireNull",
  description = "implement a spatially stratified vanWagner fire model",
  keywords = c("fire cycle", "burn probability"),
  authors = c(person(c("Steven", "G"), "Cumming", email = "stevec@sbf.ulaval.ca", role = c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("1.1.1.9001"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireNull.Rmd"),
  reqdPkgs = list("raster", "Rcpp"),
  parameters = rbind( #should initial times be 0 or 1?
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter("returnInterval", "numeric", 1.0, NA, NA, desc = "interval between main events"),
    defineParameter("startTime","numeric", 0, NA, NA, desc = "time of first burn event"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, desc = "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, desc = "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, desc = "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, desc = "This describes the simulation time at which the first save event should occur"),
    defineParameter(".statsInitialTime", "numeric", NA, NA, NA, desc = "This describes the simulation time at which the first stats event should occur")
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
    objectName = c("burnLoci","rstCurrentBurn"),
    objectClass = c("vector", "RasterLayer"),
    other = NA_character_,
    stringsAsFactors = FALSE
  )
))

doEvent.fireNull = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    # do stuff for this event
    #browser()
    sim <- sim$fireNullInit(sim)
    # schedule future event(s)
    sim <- scheduleEvent(sim, params(sim)$fireNull$startTime, "fireNull", "burn",
                         eventPriority = 1)
    sim <- scheduleEvent(sim, params(sim)$fireNull$.plotInitialTime, "fireNull", "plot")
    sim <- scheduleEvent(sim, params(sim)$fireNull$.saveInitialTime, "fireNull", "save")
    sim <- scheduleEvent(sim, params(sim)$fireNull$.statsInitialTime, "fireNull", "stats",
                         eventPriority = 2)
  } else if (eventType == "plot") {
    Plot(sim$rstCurrentBurn)
    # e.g.,
    sim <- scheduleEvent(sim, time(sim) + params(sim)$fireNull$.plotInterval, "fireNull", "plot")
    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == "save") {
    sim <- scheduleEvent(sim, time(sim) + params(sim)$fireNull$.saveInterval, "fireNull", "save")
  } else if (eventType == "burn") {
    sim <- fireNullBurn(sim)
    sim <- scheduleEvent(sim, time(sim) + params(sim)$fireNull$returnInterval, "fireNull", "burn",
                         eventPriority = 1)
  } else if (eventType == "stats"){
    #browser()
    sim <- fireNullStatsF(sim)
    sim <- scheduleEvent(sim, time(sim) + params(sim)$fireNull$returnInterval, "fireNull", "stats",
                         eventPriority = 2)
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
  # Use Rcpp sugar runif function which is faster than R runif
  cppFunction("NumericVector runifC(const int N) {
              NumericVector X(N);
              X = runif(N);
              return X;
              }",
              env = envir(sim), cacheDir = cachePath(sim))

  sim$rstCurrentBurn <- raster(sim$rstBurnProb) #the rhs is an input object
  sim$rstCurrentBurn[] <- sim$rstBurnProb[] * 0 #this conserves NAs
  sim$rstZero <- sim$rstCurrentBurn
  #sim$rstCurrentBurn[] <- sim$rstCurrentBurn[]
  setColors(sim$rstCurrentBurn,n=2) <- colorRampPalette(c("grey90", "red"))(2)

  #for any stats, we need to caculate how many burnable cells there are
  N<- sum(sim$rstBurnProb[]>0, na.rm=TRUE) # can do in one step with na.rm = TRUE
  #Lakes etc are coded 0, the crop is NA
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

  sim$rstCurrentBurn<-sim$rstZero #zero, but preserve NAs

  N<-ncell(sim$rstBurnProb)
  ###sim$burnLoci<-runif(N) < sim$rstBurnProb[] #this ignores any NAs in the map.
  sim$burnLoci<-which(sim$runifC(N) < sim$rstBurnProb[]) #this ignores any NAs in the map.
  sim$rstCurrentBurn[sim$burnLoci]<- 1 #currentBurn #mark as burned.

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

