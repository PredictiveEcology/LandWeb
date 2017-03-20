
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
    defineParameter("biggestPossibleFireSizeHa", "numeric", 1e5, 1e4, 1e6, "An upper limit, in hectares, of the truncated Pareto distribution of fire sizes"),
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
  message("1: ", Sys.time())
  sim$fireTimestep <- P(sim)$fireTimestep
  
  numPixelsPerZone <- freq(sim$rstStudyRegion) %>% na.omit(numPixelsPerZone)
  numPixelsPerZone <- numPixelsPerZone[,"count"]
  levs <- levels(sim$rstStudyRegion)[[1]]$LTHRC
  names(numPixelsPerZone) <- as.character(levs)
  
  #vals <- factorValues(sim$rstStudyRegion, sim$rstStudyRegion[], att="LTHRC")
  #vals <- factor(vals$LTHRC)
  message("2: ", Sys.time())
  #numPixelsPerZone <- tabulate(vals)
  #names(numPixelsPerZone) <- levels(vals)
  numHaPerZone <- numPixelsPerZone/(prod(res(sim$rstStudyRegion))/1e4)
  returnInterval <- levs

  message("3: ", Sys.time())
  
  findK_upper <- function(params=c(0.4), upper1 ) {
    fs <- round(rtruncpareto(1e6, 1, upper = upper1, shape=params[1]))
    meanFS <- meanTrucPareto(k = params[1], lower = 1, upper = upper1, alpha = 1)
    diff1 <- abs(quantile(fs, 0.95) - meanFS)
  }
  
  sim$kBest <- Cache(optimize, interval=c(0.05, 0.99), f = findK_upper, 
                     upper1=P(sim)$biggestPossibleFireSizeHa)$minimum
  
  message("4: ", Sys.time())
  
  meanFireSizeHa <- meanTrucPareto(k=sim$kBest, lower=1, 
                                   upper = P(sim)$biggestPossibleFireSizeHa, alpha=1)
  numFiresByZone <- numHaPerZone/meanFireSizeHa
  sim$numFiresPerYear <- numFiresByZone/returnInterval
  #numFires <- sum(!is.na(sim$rstStudyRegion[]))/meanFireSizeHa
  #numFires <- optimize(f=findNumFires, interval=c(5,1e7))$minimum
  
  sim$fireSizesInPixels <- lapply(round(sim$numFiresPerYear), function(x) 
    rtruncpareto(x, lower = 1, upper = P(sim)$biggestPossibleFireSizeHa, 
                 shape = sim$kBest))
  
  message("5: ", Sys.time())
  
  # sim$fireSizesInPixels <- rtruncpareto(numFires, lower = 1, 
  #                                   upper = P(sim)$biggestPossibleFireSizeHa, 
  #                                   shape = sim$kBest)
  # 
  
  #sim$avgFireSize <- rep(100, length(returnInterval))
  #numFires <- round(numPixelsPerZone/sim$avgFireSize)
  #sim$numFiresPerYear <- numFires/returnInterval
  
  sim$fireReturnInterval <- raster(sim$rstStudyRegion)
  sim$fireReturnInterval <- setValues(sim$fireReturnInterval, 
                                      values = levs[sim$rstStudyRegion[]])# as.numeric(as.character(vals))
  
  sim$rstCurrentBurn <- raster(sim$fireReturnInterval)
  sim$rstFlammableNum <- raster(sim$rstFlammable)
  message("6: ", Sys.time())
  
  sim$rstFlammableNum[] <- 1-sim$rstFlammable[]
  sim$rstFlammableNum[is.na(sim$rstFlammableNum)] <- NA
  browser()
  
  if(!is.na(P(sim)$.plotInitialTime)) {
    Plot(sim$fireReturnInterval, title="Fire Return Interval", speedup = 3, new=TRUE)
  }
  
  return(invisible(sim))
}



### template for plot events
LandMinePlot <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot("object")
  if(is.null(sim$rstCurrentBurnCumulative)) {
    sim$rstCurrentBurnCumulative <- raster(sim$rstCurrentBurn)
    sim$rstCurrentBurnCumulative[!is.na(sim$rstCurrentBurn)] <- 0
  }
  sim$rstCurrentBurnCumulative <- sim$rstCurrentBurn +   sim$rstCurrentBurnCumulative
  Plot(sim$rstCurrentBurnCumulative,new=TRUE, 
       title = "Cumulative Fire Map",
       cols = c("pink", "red"), zero.color = "transparent")


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
LandMineBurn <- function(sim) {
  numFiresThisYear <- rpois(length(sim$numFiresPerYear), lambda=sim$numFiresPerYear)
  
  # meanTP <- function(k, lower, upper, alpha) {
  #   k*lower^k*(upper^(1-k) - alpha^(1-k))/((1-k)*(1-(alpha/upper)^k))
  # }
  
  sim$startCells <- data.table(pixel=1:ncell(sim@.envir$rstStudyRegion),
                                fri=sim@.envir$fireReturnInterval[],key="fri") %>%
                       na.omit() %>%
                       .[,SpaDES:::resample(pixel,numFiresThisYear[.GRP]),by=fri] %>% 
                       .$V1
  
  # If fire sizes are in hectares, must adjust based on resolution of maps
  #  NOTE: round causes fires < 0.5 pixels to NOT EXIST ... i.e., 3.25 ha fires are 
  #  "not detectable" if resolution is 6.25 ha
  fireSizesThisYear <- rtruncpareto(length(sim$startCells), lower = 1, 
                                    upper=P(sim)$biggestPossibleFireSizeHa, 
                                    shape=sim$kBest)
  
  fireSizesInPixels <- round(pmax(1, fireSizesThisYear)/
                       (prod(res(sim@.envir$rstFlammableNum))/1e4))
  firesGT0 <- fireSizesInPixels>0
  sim$startCells <- sim$startCells[firesGT0]
  fireSizesInPixels <- fireSizesInPixels[firesGT0]
  
  fires <- sim$burn(sim$fireReturnInterval, startCells = sim$startCells, 
                    fireSizes = fireSizesInPixels, spreadProb = sim$rstFlammableNum,
                    spawnNewActive = c(0.3, 0.2, 0.26, 0.11))
  sim$rstCurrentBurn[] <- 0
  sim$rstCurrentBurn[fires$indices] <- 1 # time(sim)+1
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


meanTrucPareto <- function(k, lower, upper, alpha) {
  k*lower^k*(upper^(1-k) - alpha^(1-k))/((1-k)*(1-(alpha/upper)^k))
}
