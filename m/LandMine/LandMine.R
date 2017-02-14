
defineModule(sim, list(
  name = "LandMine",
  description = "Rewrite of Andison (1999) LandMine fire model",
  keywords = c("Fire", "Landscape", "Percolation", "Pixel-based"),
  authors = person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
  childModules = character(0),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "LandMine.Rmd"),
  reqdPkgs = list("raster", "RColorBrewer", "data.table", "VGAM", "magrittr", "grDevices"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter("fireTimestep", "numeric", 1, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter("burnInitialTime", "numeric", sim$startPlus1(sim), NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter("flushCachedRandomFRI", "logical", FALSE, NA, NA, "If no Fire Return Interval map is supplied, then a random one will be created and cached. Use this to make a new one."),
    defineParameter(".plotInitialTime", "numeric", sim$startPlus1(sim), NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = data.frame(
    objectName = c("rstStudyRegion", "rstFlammable"),
    objectClass = c("Raster"),
    sourceURL = "",
    other = NA_character_,
    stringsAsFactors = FALSE
  ),
  outputObjects = bind_rows(
    createsOutput("rstCurrentBurn", "RasterLayer", paste(
                  "A raster layer, produced at each timestep, where each",
                  "pixel is either 1 or 0 indicating burned or not burned")
                  ),
    createsOutput("fireTimestep", "numeric", 
      "The number of time units between successive fire events in a fire module"
    ),
    createsOutput("rstFlammableNum", "RasterLayer", paste(
      "A binary, numeric raster indicating NA or 0 for not burnable")
    ),
    createsOutput("numFiresPerYear", "numeric", paste(
      "The average number of fires per year, by fire return interval level on rstCurrentBurn")
    ),
    createsOutput("fireReturnInterval", "RasterLayer", paste(
      "A Raster map showing the fire return interval. THis is created from the rstCurrentBurn")
    )
  )
))


doEvent.LandMine = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)

    # do stuff for this event
    sim <- sim$LandMineInit(sim)
    
    # schedule future event(s)
    sim <- scheduleEvent(sim, P(sim)$burnInitialTime, "LandMine", "LandMineBurn", 2.5)
    sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "LandMine", "plot")
    sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "LandMine", "save")
  } else if (eventType == "plot") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
    sim <- LandMinePlot(sim)
    sim <- scheduleEvent(sim, P(sim)$.plotInterval, "LandMine", "plot")
    
    #Plot(objectFromModule) # uncomment this, replace with object to plot
    # schedule future event(s)
  } else if (eventType == "save") {
  } else if (eventType == "LandMineBurn") {
    sim <- sim$LandMineBurn(sim)
    sim <- scheduleEvent(sim, time(sim) + P(sim)$fireTimestep, "LandMine", "LandMineBurn", 2.5)
  } else {
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  }
  return(invisible(sim))
}


### template initialization
LandMineInit <- function(sim) {
  sim$fireTimestep <- P(sim)$fireTimestep
  
  vals <- factorValues(sim$rstStudyRegion, sim$rstStudyRegion[], att="LTHRC")
  vals <- factor(vals$LTHRC)
  numPixelsPerZone <- tabulate(vals)
  returnInterval <- as.numeric(levels(vals))
  sim$avgFireSize <- rep(100, length(returnInterval))
  numFires <- round(numPixelsPerZone/sim$avgFireSize)
  sim$numFiresPerYear <- numFires/returnInterval
  
  sim$fireReturnInterval <- raster(sim$rstStudyRegion)
  sim$fireReturnInterval[] <- as.numeric(as.character(vals))
  
  sim$rstCurrentBurn <- raster(sim$fireReturnInterval)
  sim$rstFlammableNum <- raster(sim$rstFlammable)
  sim$rstFlammableNum[] <- sim$rstFlammable[]
  sim$rstFlammableNum[is.na(sim$rstFlammableNum)] <- NA
  
  
  
  if(!is.na(P(sim)$.plotInitialTime)) {
    Plot(sim$fireReturnInterval, speedup = 3, new=TRUE)
  }

  return(invisible(sim))
}



### template for plot events
LandMinePlot <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot("object")
  Plot(sim$rstCurrentBurn,new=time(sim)==P(sim)$.plotInitialTime, 
       title = "Cumulative Fire Map",
       cols = c("transparent", adjustcolor("red", alpha.f=0.1)))

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
LandMineBurn <- function(sim) {
  numFires <- rpois(length(sim$numFiresPerYear), lambda=sim$numFiresPerYear)
  
  sim$startCells <- data.table(pixel=1:ncell(sim@.envir$rstStudyRegion),
                                fri=sim@.envir$fireReturnInterval[],key="fri") %>%
                       na.omit() %>%
                       .[,SpaDES:::resample(pixel,numFires[.GRP]),by=fri] %>% 
                       .$V1
  fireSizes <- pmax(1, rtruncpareto(length(sim$startCells), 1, 1e4, 0.4))
  fires <- sim$burn(sim$fireReturnInterval, startCells = sim$startCells, 
                    fireSizes = fireSizes, spreadProb = sim$rstFlammableNum)
  
  #if(any(fires[,.N,by=id]$N < floor(fireSizes))) stop("Fire weren't exact")
  #browser()
  a <- 1
  
  sim$rstCurrentBurn[] <- 0
  sim$rstCurrentBurn[fires$indices] <- 1 # time(sim)+1
  #Plot(hist(fires[,.N,by=id]$N), title = "fire size distribution")
  

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}


.inputObjects = function(sim) {

  # Make random forest cover map
  # emptyRas <- raster(extent(0, 1e3, 0, 1e3), res = 1)
  # 
  # nOT <- if(P(sim)$flushCachedRandomFRI) Sys.time() else NULL
  # sim$fireReturnInterval <- Cache(randomPolygons, emptyRas, numTypes = 4, 
  #                                 notOlderThan = nOT)
  # 
  # vals <- factor(sim$fireReturnInterval[], 
  #                levels = 1:4, 
  #                labels=c(60, 1000, 1050, 1250))
  # sim$fireReturnInterval[] <- as.numeric(as.character(vals))
  # 
  # names(sim$fireReturnInterval) <- "fireReturnInterval"
  return(invisible(sim))
}

startPlus1 <- function(sim) {
  start(sim) + 1
}
