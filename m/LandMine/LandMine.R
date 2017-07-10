
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
  reqdPkgs = list("data.table", "grDevices", "magrittr", "raster", "RColorBrewer", "VGAM", "SpaDES.tools"),
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
    objectName = c("rstStudyRegion", "rstFlammable", "rstRateOfSpread"),
    objectClass = c("Raster"),
    sourceURL = "",
    other = c("A raster layer that is a factor raster, with at least 1 column called LTHRC, representing the fire return interval in years",
              "A raster layer, with 0, 1 and NA, where 0 indicates areas that are flammable, 1 not flammable (e.g., lakes) and NA not applicable (e.g., masked)",
              "A raster layer, with numeric rates of spread. This is passed into spreadProbRel in spread2.  NA not applicable (e.g., masked)"),
    stringsAsFactors = FALSE
  ),
  outputObjects = bind_rows(
    createsOutput("rstCurrentBurn", "RasterLayer", paste(
      "A raster layer, produced at each timestep, where each",
      "pixel is either 1 or 0 indicating burned or not burned.")
    ),
    createsOutput("fireTimestep", "numeric", 
                  "The number of time units between successive fire events in a fire module."
    ),
    createsOutput("fireInitialTime", "numeric", 
                  "The initial event time of the burn event. This is simply a reassignment from P(sim)$burnInitialTime."
    ),
    createsOutput("rstFlammableNum", "RasterLayer", paste(
      "A binary, numeric raster indicating NA or 0 for not burnable.")
    ),
    createsOutput("numFiresPerYear", "numeric", paste(
      "The average number of fires per year, by fire return interval level on rstCurrentBurn.")
    ),
    createsOutput("fireReturnInterval", "RasterLayer", paste(
      "A Raster map showing the fire return interval. THis is created from the rstCurrentBurn.")
    ),
    createsOutput("fireReturnIntervalsByPolygonNumeric", "numeric", paste(
      "A vector of the fire return intervals, ordered by the numeric representation of polygon ID")
    ),
    createsOutput("kBest", "numeric", paste(
      "A numeric scalar that is the optimal value of K in the Truncated Pareto distribution (rtruncpareto)")
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

### initialization
LandMineInit <- function(sim) {
  message("1: ", Sys.time())
  sim$fireTimestep <- P(sim)$fireTimestep
  sim$fireInitialTime <- P(sim)$burnInitialTime
  numPixelsPerPolygonNumeric <- Cache(freq, sim$rstStudyRegion) %>% na.omit()
  ordPolygons <- order(numPixelsPerPolygonNumeric[, "value"])
  numPixelsPerPolygonNumeric <- numPixelsPerPolygonNumeric[ordPolygons,]
  sim$fireReturnIntervalsByPolygonNumeric <- numPixelsPerPolygonNumeric[,"value"]
  numPixelsPerPolygonNumeric <- numPixelsPerPolygonNumeric[,"count"]
  #sim$fireReturnIntervalsByPolygonNumeric <- levels(sim$rstStudyRegion)[[1]]$LTHRC[ordPolygons]
  #sim$fireReturnIntervalsByPolygonNumeric <- levels(sim$rstStudyRegion)[[1]]$LTHRC[ordPolygons]
  names(numPixelsPerPolygonNumeric) <- sim$fireReturnIntervalsByPolygonNumeric
  
  message("2: ", Sys.time())
  numHaPerPolygonNumeric <- numPixelsPerPolygonNumeric * (prod(res(sim$rstStudyRegion)) / 1e4)
  returnInterval <- sim$fireReturnIntervalsByPolygonNumeric

  message("3: ", Sys.time())
  
  findK_upper <- function(params=c(0.4), upper1 ) {
    fs <- round(rtruncpareto(1e6, 1, upper = upper1, shape = params[1]))
    meanFS <- meanTrucPareto(k = params[1], lower = 1, upper = upper1, alpha = 1)
    #diff1 <- abs(quantile(fs, 0.95) - meanFS)
    #abs(sum(fs[fs>quantile(fs, 0.95)])/sum(fs) - 0.9) # "90% of area is in 5% of fires" # from Dave rule of thumb
    
    # Eliot Adjustment because each year was too constant -- should create greater variation
    abs(sum(fs[fs > quantile(fs, 0.95)]) / sum(fs) - 0.95) # "95% of area is in 5% of fires" 
  }
  
  sim$kBest <- Cache(optimize, interval = c(0.05, 0.99), f = findK_upper, 
                     upper1 = P(sim)$biggestPossibleFireSizeHa)$minimum
  
  message("4: ", Sys.time())
  
  meanFireSizeHa <- meanTrucPareto(k = sim$kBest, lower = 1, 
                                   upper = P(sim)$biggestPossibleFireSizeHa, alpha = 1)
  numFiresByPolygonNumeric <- numHaPerPolygonNumeric / meanFireSizeHa
  sim$numFiresPerYear <- numFiresByPolygonNumeric / returnInterval
  
  message("5: ", Sys.time())
  
  sim$fireReturnInterval <- sim$rstStudyRegion
  #sim$fireReturnInterval <- setValues(sim$fireReturnInterval, 
  #                                    values = sim$fireReturnIntervalsByPolygonNumeric[sim$rstStudyRegion[]])# as.numeric(as.character(vals))
  sim$fireReturnInterval <- Cache(writeRaster, sim$fireReturnInterval, 
                                        filename = file.path(tempdir(),
                                                             "fireReturnInterval.tif"),
                                        datatype = "INT2U", overwrite = TRUE)
  sim$rstCurrentBurn <- raster(sim$fireReturnInterval)
  sim$rstFlammableNum <- raster(sim$rstFlammable)
  message("6: ", Sys.time())
  
  sim$rstFlammableNum[] <- 1L - as.integer(sim$rstFlammable[])
  sim$rstFlammableNum[is.na(sim$rstFlammableNum)] <- NA

  if (!is.na(P(sim)$.plotInitialTime)) {
    Plot(sim$fireReturnInterval, title = "Fire Return Interval", speedup = 3, new = TRUE)
  }
  
  # rm("rstFlammable", envir = envir(sim)) # don't need this in LandMine ... but it is used in timeSinceFire
  return(invisible(sim))
}

### plot events
LandMinePlot <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot("object")
  if (is.null(sim$rstCurrentBurnCumulative)) {
    sim$rstCurrentBurnCumulative <- raster(sim$rstCurrentBurn)
    sim$rstCurrentBurnCumulative[!is.na(sim$rstCurrentBurn)] <- 0
  }
  sim$rstCurrentBurnCumulative <- sim$rstCurrentBurn +   sim$rstCurrentBurnCumulative
  Plot(sim$rstCurrentBurnCumulative, new = TRUE, 
       title = "Cumulative Fire Map",
       cols = c("pink", "red"), zero.color = "transparent")
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### burn events
LandMineBurn <- function(sim) {
  numFiresThisPeriod <- rpois(length(sim$numFiresPerYear),
                              lambda = sim$numFiresPerYear * P(sim)$fireTimestep)
                                 
  # meanTP <- function(k, lower, upper, alpha) {
  #   k*lower^k*(upper^(1-k) - alpha^(1-k))/((1-k)*(1-(alpha/upper)^k))
  # }
  
  sim$startCells <- data.table(pixel = 1:ncell(sim$rstStudyRegion),
                               polygonNumeric = sim$rstStudyRegion[] * sim$rstFlammableNum[],
                               key = "polygonNumeric") 
  sim$startCells <- sim$startCells[polygonNumeric == 0, polygonNumeric := NA] %>%
    na.omit() %>%
    .[, SpaDES.tools:::resample(pixel, numFiresThisPeriod[.GRP]), by = polygonNumeric] %>% 
    .$V1
  
  # If fire sizes are in hectares, must adjust based on resolution of maps
  #  NOTE: round causes fires < 0.5 pixels to NOT EXIST ... i.e., 3.25 ha fires are 
  #  "not detectable" if resolution is 6.25 ha
  fireSizesThisPeriod <- rtruncpareto(length(sim$startCells), lower = 1,
                                      upper = P(sim)$biggestPossibleFireSizeHa,
                                      shape = sim$kBest)
  
  # Because annual number of fires is 
  # 
  # fireSizesInPixels <- lapply(truncVals + decimalVals, function(x) 
  #   rtruncpareto(x, lower = 1, upper = P(sim)$biggestPossibleFireSizeHa, 
  #                shape = sim$kBest))
  # names(fireSizesInPixels) <- seq_along(sim$fireReturnIntervalsByPolygonNumeric)
  
  
  # fireSizesInPixels <- round(pmax(1, fireSizesThisPeriod)/
  #                     (prod(res(sim$rstFlammableNum))/1e4))
  
  fireSizesInPixels <- fireSizesThisPeriod / (prod(res(sim$rstFlammableNum)) / 1e4)
  ranDraws <- runif(length(fireSizesInPixels))
  truncVals <- trunc(fireSizesInPixels)
  decimalVals <- (unname(fireSizesInPixels - (truncVals))) > ranDraws
  
  fireSizesInPixels <- truncVals + decimalVals
  
  firesGT0 <- fireSizesInPixels > 0
  sim$startCells <- sim$startCells[firesGT0]
  fireSizesInPixels <- fireSizesInPixels[firesGT0]
  
  #Rate of Spread
  mature <- sim$rstTimeSinceFire[]>120
  immature <- (sim$rstTimeSinceFire[]>40) & !mature
  young <- !immature & !mature

  vegTypeMap <- sim$vegTypeMapGenerator(sim$species, sim$cohortData, sim$pixelGroupMap) 
  vegType <- getValues(vegTypeMap)
  vegTypes <- factorValues(vegTypeMap, seq_len(NROW(levels(vegTypeMap)[[1]]))) # [vegType, "Factor"]
  ROS <- rep(NA_integer_, length(vegTypes))
  mixed <- grep(vegTypes$Factor, pattern = "Mix")
  spruce <- grep(vegTypes$Factor, pattern = "Spruce")
  pine <- grep(vegTypes$Factor, pattern = "Pine")
  decid <- grep(vegTypes$Factor, pattern = "Deci")
  softwood <- grep(vegTypes$Factor, pattern = "Soft")
  
  ROS[!mature & vegType == decid] <- 6L
  ROS[mature & vegType == decid] <- 9L
  
  ROS[!mature & vegType == mixed] <- 12L
  ROS[mature & vegType == mixed] <- 17L
  
  ROS[immature & vegType == pine] <- 14L
  ROS[mature & vegType == pine] <- 21L
  ROS[young & vegType == pine] <- 22L
  
  ROS[!mature & vegType == softwood] <- 18L
  ROS[mature & vegType == softwood] <- 27L
  
  ROS[!mature & vegType == spruce] <- 20L
  ROS[mature & vegType == spruce] <- 30L
  
  # Other vegetation that can burn -- e.g., grasslands, lichen, shrub
  ROS[sim$rstFlammableNum[] == 1 & is.na(ROS)] <- 30L
  
  ROSmap <- raster(sim$pixelGroupMap)
  ROSmap[] <- ROS
  
  if (length(sim$startCells) > 0) {
    fires <- sim$burn1(sim$fireReturnInterval, startCells = sim$startCells, 
                       fireSizes = fireSizesInPixels, spreadProbRel = ROSmap,
                       spawnNewActive = c(0.65, 0.6, 0.2, 0.2),
                       #spawnNewActive = c(0.76, 0.45, 1.0, 0.00),
                       spreadProb = 0.77)
    print(attr(fires, "spreadState")$clusterDT[order(maxSize)][(.N - 7):.N])
    print(attr(fires, "spreadState")$clusterDT[, list(numPixelsBurned = sum(size),
                                                      expectedNumBurned = sum(maxSize),
                                                      proportionBurned = sum(size) / sum(maxSize))])
    sim$rstCurrentBurn[] <- 0L
    sim$rstCurrentBurn[fires$pixels] <- 1L#as.numeric(factor(fires$initialPixels))
    #clearPlot();Plot(sim$rstCurrentBurn, new=T, visualSqueeze = 1.25)
    
  }
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
  k * lower^k * (upper^(1 - k) - alpha^(1 - k)) / ((1 - k) * (1 - (alpha/upper)^k))
}

vegTypeMapGenerator <- function(species, cohortdata, pixelGroupMap) {
  species[species == "Pinu_Ban" | species == "Pinu_Con", speciesGroup := "PINU"] 
  species[species == "Betu_Pap" | species == "Popu_Bal" |
            species == "Popu_Tre" | species == "Lari_Lar", speciesGroup := "DECI"] 
  species[species == "Pice_Mar" | species == "Pice_Gla", speciesGroup := "PICE"] 
  #cohortdata <- sim$cohortData
  shortcohortdata <- setkey(cohortdata, speciesCode)[setkey(species[,.(speciesCode, speciesGroup)], 
                                                            speciesCode), nomatch = 0] 
  shortcohortdata[, totalB := sum(B, na.rm = TRUE), by = pixelGroup] 
  shortcohortdata <- shortcohortdata[, .(speciesGroupB = sum(B, na.rm = TRUE),
                                         totalB = mean(totalB, na.rm = TRUE)),
                                     by = c("pixelGroup", "speciesGroup")] 
  shortcohortdata[,speciesPercentage := speciesGroupB/totalB] 
  shortcohortdata[speciesGroup == "PINU" & speciesPercentage > vegLeadingPercent,
                  speciesLeading := 1]# pine leading 
  shortcohortdata[speciesGroup == "DECI" & speciesPercentage > vegLeadingPercent,
                  speciesLeading := 2]# deciduous leading 
  shortcohortdata[speciesGroup == "PICE" & speciesPercentage > vegLeadingPercent,
                  speciesLeading := 3]# spruce leading 
  shortcohortdata[is.na(speciesLeading), speciesLeading := 0] 
  shortcohortdata[,speciesLeading := max(speciesLeading, na.rm = TRUE), by = pixelGroup] 
  shortcohortdata <- unique(shortcohortdata[,.(pixelGroup, speciesLeading)], by = "pixelGroup") 
  shortcohortdata[speciesLeading == 0, speciesLeading := 4] # 4 is mixed forests 
  attritable <- data.table(ID = unique(shortcohortdata$speciesLeading))
  attritable[ID == 1, Factor := "Pine leading"]
  attritable[ID == 2, Factor := "Deciduous leading"]
  attritable[ID == 3, Factor := "Spruce leading"]
  attritable[ID == 4, Factor := "Mixed"]
  #pixelGroupMap <- sim$pixelGroupMap
  vegTypeMap <- rasterizeReduced(shortcohortdata, pixelGroupMap, "speciesLeading") 
  vegTypeMap <- setValues(vegTypeMap, as.integer(getValues(vegTypeMap)))
  levels(vegTypeMap) <- as.data.frame(attritable)
  projection(vegTypeMap) <- projection(pixelGroupMap)
  vegTypeMap
}
