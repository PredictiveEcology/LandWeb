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
  reqdPkgs = list("data.table", "grDevices", "magrittr", "raster", "RColorBrewer", "VGAM",
                  "PredictiveEcology/SpaDES.tools@prepInputs"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter("fireTimestep", "numeric", 1, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter("burnInitialTime", "numeric", start(sim, "year") + 1, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter("biggestPossibleFireSizeHa", "numeric", 1e5, 1e4, 1e6, "An upper limit, in hectares, of the truncated Pareto distribution of fire sizes"),
    defineParameter("flushCachedRandomFRI", "logical", FALSE, NA, NA, "If no Fire Return Interval map is supplied, then a random one will be created and cached. Use this to make a new one."),
    defineParameter("randomDefaultData", "logical", FALSE, NA, NA, "Only used for creating a starting dataset. If TRUE, then it will be randomly generated; FALSE, deterministic and identical each time."),
    defineParameter(".plotInitialTime", "numeric", start(sim, "year") + 1, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant"),
    defineParameter(name = "useParallel", class = "numeric", default = parallel::detectCores(),
                    desc = "Used in burning. Will be passed to data.table::setDTthreads")
  ),
  inputObjects = bind_rows(
    expectsInput("rstFlammable", "Raster", "A raster layer, with 0, 1 and NA, where 0 indicates areas that are flammable, 1 not flammable (e.g., lakes) and NA not applicable (e.g., masked)"),
    expectsInput("rstStudyRegion","Raster", "A raster layer that is a factor raster, with at least 1 column called LTHRC, representing the fire return interval in years"),
    expectsInput("species", "data.table", "Columns: species, speciesCode, Indicating several features about species"),
    expectsInput("cohortData", "data.table", "Columns: B, pixelGroup, speciesCode, Indicating several features about ages and current vegetation of stand"),
    expectsInput("vegLeadingPercent", "numeric", "a proportion, between 0 and 1, that define whether a species is lead for a given pixel", NA),
    expectsInput("rstTimeSinceFire", "Raster", "a time since fire raster layer", NA),
    expectsInput("pixelGroupMap", "RasterLayer", "Pixels with identical values share identical stand features"),
    expectsInput("rstCurrentBurnCumulative", "RasterLayer", "Cumulative number of times a pixel has burned")
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
    createsOutput("kBest", "numeric", "A numeric scalar that is the optimal value of K in the Truncated Pareto distribution (rtruncpareto)"),
    createsOutput("rstCurrentBurnCumulative", "RasterLayer", "Cumulative number of times a pixel has burned")
  )
))

doEvent.LandMine = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)

    # do stuff for this event
    #  ff package, used in SpaDES.tools::spread2, doesn't always set this correctly.
    options(fftempdir = tempdir())
    sim <- EstimateTruncPareto(sim)
    sim <- Init(sim)

    # schedule future event(s)
    sim <- scheduleEvent(sim, P(sim)$burnInitialTime, "LandMine", "Burn", 2.5)
    sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "LandMine", "plot")
    sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "LandMine", "save")
  } else if (eventType == "plot") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
    sim <- plotFn(sim)
    sim <- scheduleEvent(sim, P(sim)$.plotInterval, "LandMine", "plot")

    #Plot(objectFromModule) # uncomment this, replace with object to plot
    # schedule future event(s)
  } else if (eventType == "save") {
  } else if (eventType == "Burn") {
    sim <- Burn(sim)
    sim <- scheduleEvent(sim, time(sim) + P(sim)$fireTimestep, "LandMine", "Burn", 2.5)
  } else {
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  }
  return(invisible(sim))
}

### initialization
EstimateTruncPareto <- function(sim) {
  message("Estimate Truncated Pareto parameters")

  findK_upper <- function(params=c(0.4), upper1 ) {
    fs <- round(rtruncpareto(1e6, 1, upper = upper1, shape = params[1]))
    #meanFS <- meanTruncPareto(k = params[1], lower = 1, upper = upper1, alpha = 1)
    #diff1 <- abs(quantile(fs, 0.95) - meanFS)
    #abs(sum(fs[fs>quantile(fs, 0.95)])/sum(fs) - 0.9) # "90% of area is in 5% of fires" # from Dave rule of thumb

    # Eliot Adjustment because each year was too constant -- should create greater variation
    abs(sum(fs[fs > quantile(fs, 0.95)]) / sum(fs) - 0.95) # "95% of area is in 5% of fires"
  }

  sim$kBest <- Cache(optimize, interval = c(0.05, 0.99), f = findK_upper,
                     upper1 = P(sim)$biggestPossibleFireSizeHa)$minimum
  return(invisible(sim))
}

Init <- function(sim) {
  message("Initializing fire maps")
  sim$fireTimestep <- P(sim)$fireTimestep
  sim$fireInitialTime <- P(sim)$burnInitialTime
  numPixelsPerPolygonNumeric <- Cache(freq, sim$rstStudyRegion) %>% na.omit()
  ordPolygons <- order(numPixelsPerPolygonNumeric[, "value"])
  numPixelsPerPolygonNumeric <- numPixelsPerPolygonNumeric[ordPolygons,,drop = FALSE]
  sim$fireReturnIntervalsByPolygonNumeric <- numPixelsPerPolygonNumeric[,"value"]
  numPixelsPerPolygonNumeric <- numPixelsPerPolygonNumeric[,"count"]
  names(numPixelsPerPolygonNumeric) <- sim$fireReturnIntervalsByPolygonNumeric

  numHaPerPolygonNumeric <- numPixelsPerPolygonNumeric * (prod(res(sim$rstStudyRegion)) / 1e4)
  returnInterval <- sim$fireReturnIntervalsByPolygonNumeric


  message("Determine mean fire size")

  meanFireSizeHa <- meanTruncPareto(k = sim$kBest, lower = 1,
                                   upper = P(sim)$biggestPossibleFireSizeHa,
                                   alpha = 1)
  numFiresByPolygonNumeric <- numHaPerPolygonNumeric / meanFireSizeHa
  sim$numFiresPerYear <- numFiresByPolygonNumeric / returnInterval

  message("Write fire return interval map to disk")

  sim$fireReturnInterval <- raster(sim$rstStudyRegion)
  sim$fireReturnInterval[] <- sim$rstStudyRegion[]
  fireReturnIntFilename <- file.path(tempdir(), "fireReturnInterval.tif")
  fireReturnIntFilename <- file.path(cachePath(sim), "rasters/fireReturnInterval.tif")
  sim$fireReturnInterval <- writeRaster(sim$fireReturnInterval, filename = fireReturnIntFilename,
                                        datatype = "INT2U", overwrite = TRUE)
  sim$rstCurrentBurn <- raster(sim$fireReturnInterval)
  sim$rstCurrentBurn[] <- 0L
  sim$rstFlammableNum <- raster(sim$rstFlammable)
  message("6: ", Sys.time())

  sim$rstFlammableNum[] <- 1L - as.integer(sim$rstFlammable[])
  sim$rstFlammableNum[is.na(sim$rstFlammableNum[])] <- NA

  # rm("rstFlammable", envir = envir(sim)) # don't need this in LandMine ... but it is used in timeSinceFire
  return(invisible(sim))
}

### plot events
plotFn <- function(sim) {
  if (!identical(extent(sim$rstCurrentBurnCumulative), extent(sim$rstCurrentBurn))) {
    sim$rstCurrentBurnCumulative <- raster(sim$rstCurrentBurn)
  }
  if (time(sim) == P(sim)$.plotInitialTime) {
    Plot(sim$fireReturnInterval, title = "Fire Return Interval", speedup = 3, new = TRUE)
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
Burn <- function(sim) {
  # Poisson is too little variation
  # numFiresThisPeriod <- rpois(length(sim$numFiresPerYear),
  #                             lambda = sim$numFiresPerYear * P(sim)$fireTimestep)

  numFiresThisPeriod <- rnbinom(length(sim$numFiresPerYear), 
          mu = sim$numFiresPerYear * P(sim)$fireTimestep, 
          size = 3)
  # meanTP <- function(k, lower, upper, alpha) {
  #   k*lower^k*(upper^(1-k) - alpha^(1-k))/((1-k)*(1-(alpha/upper)^k))
  # }

  thisYrStartCells <- data.table(pixel = 1:ncell(sim$rstStudyRegion),
                               polygonNumeric = sim$rstStudyRegion[] * sim$rstFlammableNum[],
                               key = "polygonNumeric")
  thisYrStartCells <- thisYrStartCells[polygonNumeric == 0, polygonNumeric := NA] %>%
    na.omit() %>%
    .[, SpaDES.tools:::resample(pixel, numFiresThisPeriod[.GRP]), by = polygonNumeric] %>%
    .$V1

  # If fire sizes are in hectares, must adjust based on resolution of maps
  #  NOTE: round causes fires < 0.5 pixels to NOT EXIST ... i.e., 3.25 ha fires are
  #  "not detectable" if resolution is 6.25 ha
  fireSizesThisPeriod <- rtruncpareto(length(thisYrStartCells), lower = 1,
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
  thisYrStartCells <- thisYrStartCells[firesGT0]
  fireSizesInPixels <- fireSizesInPixels[firesGT0]

  #Rate of Spread
  mature <- sim$rstTimeSinceFire[] > 120
  immature <- (sim$rstTimeSinceFire[] > 40) & !mature
  young <- !immature & !mature

  vegTypeMap <- vegTypeMapGenerator(sim$species, sim$cohortData, sim$pixelGroupMap, sim$vegLeadingPercent)
  vegType <- getValues(vegTypeMap)
  vegTypes <- data.frame(raster::levels(vegTypeMap)[[1]][, "Factor", drop = FALSE])
  #vegTypes <- factorValues(vegTypeMap, seq_len(NROW(levels(vegTypeMap)[[1]]))) # [vegType, "Factor"]
  ROS <- rep(NA_integer_, NROW(vegType))
  mixed <- grep(tolower(vegTypes$Factor), pattern = "mix")
  spruce <- grep(tolower(vegTypes$Factor), pattern = "spruce")
  pine <- grep(tolower(vegTypes$Factor), pattern = "pine")
  decid <- grep(tolower(vegTypes$Factor), pattern = "deci")
  softwood <- grep(tolower(vegTypes$Factor), pattern = "soft")

  ROS[!mature & vegType %in% decid] <- 6L
  ROS[mature & vegType %in% decid] <- 9L

  ROS[!mature & vegType %in% mixed] <- 12L
  ROS[mature & vegType %in% mixed] <- 17L

  ROS[immature & vegType %in% pine] <- 14L
  ROS[mature & vegType %in% pine] <- 21L
  ROS[young & vegType %in% pine] <- 22L

  ROS[!mature & vegType %in% softwood] <- 18L
  ROS[mature & vegType %in% softwood] <- 27L

  ROS[!mature & vegType %in% spruce] <- 20L
  ROS[mature & vegType %in% spruce] <- 30L

  # Other vegetation that can burn -- e.g., grasslands, lichen, shrub
  ROS[sim$rstFlammableNum[] == 1 & is.na(ROS)] <- 30L

  ROSmap <- raster(sim$pixelGroupMap)
  ROSmap[] <- ROS

  # From DEoptim fitting - run in the LandMine.Rmd file
  spawnNewActive <- sns <- 10^c(-0.731520, -0.501823, -0.605968, -1.809726)
  spreadProb <- 0.9
  sizeCutoffs <- 10^c(2.202732,  4.696060)
  
  
  if (!all(is.na(thisYrStartCells)) & length(thisYrStartCells) > 0) {
    if (data.table::getDTthreads() < P(sim)$useParallel) 
      data.table::setDTthreads(P(sim)$useParallel)
    fires <- burn1(sim$fireReturnInterval, startCells = thisYrStartCells,
                   fireSizes = fireSizesInPixels, spreadProbRel = ROSmap,
                   #spawnNewActive = c(0.65, 0.6, 0.2, 0.2),
                   sizeCutoffs = sizeCutoffs,
                   spawnNewActive = spawnNewActive,
                   #spawnNewActive = c(0.76, 0.45, 1.0, 0.00),
                   spreadProb = spreadProb)
    fa <- attr(fires, "spreadState")$clusterDT
    print(fa[order(maxSize)][(.N - pmin(7, NROW(fa))):.N])
    print(fa[, list(numPixelsBurned = sum(size),
                    expectedNumBurned = sum(maxSize),
                    proportionBurned = sum(size) / sum(maxSize))])
    sim$rstCurrentBurn[] <- 0L
    sim$rstCurrentBurn[fires$pixels] <- 1L #as.numeric(factor(fires$initialPixels))

  }
  return(invisible(sim))
}

.inputObjects = function(sim) {
  # Make random forest cover map
  nOT <- if (P(sim)$flushCachedRandomFRI) Sys.time() else NULL
  numDefaultPolygons <- 4L
  numDefaultPixelGroups <- 20L
  numDefaultSpeciesCodes <- 2L

  # if(is.null(sim$fireReturnInterval)) {
  #   sim$fireReturnInterval <- Cache(randomPolygons, emptyRas, numTypes = numDefaultPolygons,
  #                                   notOlderThan = nOT)
  #
  #   vals <- factor(sim$fireReturnInterval[],
  #                  levels = 1:numDefaultPolygons,
  #                  labels = c(60, 100, 120, 250))
  #   sim$fireReturnInterval[] <- as.numeric(as.character(vals))
  # }

  if (is.null(sim$rstFlammable)) {
    emptyRas <- raster(extent(0, 2e4, 0, 2e4), res = 250)
    sim$rstFlammable <- raster(emptyRas)
    sim$rstFlammable[] <- 0  # 0 means flammable
  } else {
    emptyRas <- raster(sim$rstFlammable)
  }

  # names(sim$fireReturnInterval) <- "fireReturnInterval"

  if (is.null(sim$rstStudyRegion)) {
    sim$rstStudyRegion <- Cache(randomPolygons, emptyRas,
                                numTypes = numDefaultPolygons, notOlderThan = nOT)

    vals <- factor(sim$rstStudyRegion[],
                   levels = 1:numDefaultPolygons,
                   labels = c(60, 100, 120, 250))
    sim$rstStudyRegion[] <- as.numeric(as.character(vals))
  }

  if (is.null(sim$cohortData)) {
    if (!P(sim)$randomDefaultData) {
      ranNum <- runif(1)
      set.seed(123)
    }
    sampleV <- Vectorize(sample, "size", SIMPLIFY = TRUE)
    repV <- Vectorize(rep.int, c("x","times"))
    numCohortsPerPG <- sample(1:2, replace = TRUE, numDefaultPixelGroups)
    sim$cohortData <- data.table(speciesCode = unlist(sampleV(1:2, numCohortsPerPG)),
                                 B = runif(sum(numCohortsPerPG), 100, 1000),
                                 pixelGroup = unlist(repV(1:numDefaultPixelGroups, times = numCohortsPerPG)))
    if (!P(sim)$randomDefaultData) {
      set.seed(ranNum)
    }

  }

  if (is.null(sim$pixelGroupMap)) {
    sim$pixelGroupMap <- Cache(randomPolygons, emptyRas, numTypes = numDefaultPixelGroups,
                               notOlderThan = nOT)
  }

  if (is.null(sim$rstTimeSinceFire)) {
    sim$rstTimeSinceFire <- raster(sim$pixelGroupMap)
    sim$rstTimeSinceFire[] <- 200
  }

  if (is.null(sim$species)) {
    sim$species <- data.table(species = c("Pinu_sp", "Pice_gla"),
                              speciesCode = 1:numDefaultSpeciesCodes)
  }

  if (is.null(sim$vegLeadingPercent)) {
    sim$vegLeadingPercent <- 0.8
  }

  if (is.null(sim$rstCurrentBurnCumulative)) {
    sim$rstCurrentBurnCumulative <- raster(sim$pixelGroupMap)
    sim$rstCurrentBurnCumulative[sim$rstTimeSinceFire[] == 0] <- 1
  }

  # see https://github.com/PredictiveEcology/SpaDES.tools/issues#17 for discussion about this
  meta <- depends(sim)@dependencies
  mods <- unlist(modules(sim))
  if(all(names(meta) %in% mods)) { # means there is more than just this module in the simList
    # meta <- depends(sim)
    outputs <- lapply(meta, function(x) {x@outputObjects$objectName})
    otherMods <- mods[!(mods %in% currentModule(sim))]
    
    # is it or will it be supplied by another module, if yes, don't load a default here
    if (!("rstCurrentBurnCumulative" %in% unlist(outputs[otherMods]))) { 
      if (is.null(sim$rstCurrentBurnCumulative)) {
        sim$rstCurrentBurnCumulative <- raster(sim$pixelGroupMap)

      }
    }
  }

  return(invisible(sim))
}


meanTruncPareto <- function(k, lower, upper, alpha) {
  k * lower^k * (upper^(1 - k) - alpha^(1 - k)) / ((1 - k) * (1 - (alpha/upper)^k))
}

vegTypeMapGenerator <- function(species, cohortdata, pixelGroupMap, vegLeadingPercent) {
  species[species == "Pinu_ban" | species == "Pinu_con" | species == "Pinu_sp", speciesGroup := "PINU"]
  species[species == "Betu_pap" | species == "Popu_bal" | species == "Popu_tre" |
            species == "Lari_lar", speciesGroup := "DECI"]
  species[species == "Pice_mar" , speciesGroup := "PICE_MAR"]
  species[species == "Pice_gla", speciesGroup := "PICE_GLA"]
  species[species == "Abie_sp" , speciesGroup := "ABIE"]
  #cohortdata <- sim$cohortData
  shortcohortdata <- setkey(cohortdata, speciesCode)[setkey(species[, .(speciesCode, speciesGroup)],
                                                            speciesCode), nomatch = 0]
  shortcohortdata[, totalB := sum(B, na.rm = TRUE), by = pixelGroup]
  shortcohortdata <- shortcohortdata[, .(speciesGroupB = sum(B, na.rm = TRUE),
                                         totalB = mean(totalB, na.rm = TRUE)),
                                     by = c("pixelGroup", "speciesGroup")]
  shortcohortdata[,speciesPercentage := speciesGroupB/totalB]

  speciesLeading <- NULL
  Factor <- NULL
  ID <- NULL
  pixelGroup <- NULL
  speciesPercentage <- NULL
  speciesGroup <- NULL
  speciesCode <- NULL
  totalB <- NULL
  B <- NULL
  speciesGroupB <- NULL

  shortcohortdata[speciesGroup == "PINU" & speciesPercentage > vegLeadingPercent,
                  speciesLeading := 1]# pine leading
  shortcohortdata[speciesGroup == "DECI" & speciesPercentage > vegLeadingPercent,
                  speciesLeading := 2]# deciduous leading
  shortcohortdata[speciesGroup == "PICE_MAR" & speciesPercentage > vegLeadingPercent,
                  speciesLeading := 3]# spruce leading
  shortcohortdata[speciesGroup == "PICE_GLA" & speciesPercentage > vegLeadingPercent,
                  speciesLeading := 4]# spruce leading
  shortcohortdata[is.na(speciesLeading), speciesLeading := 0]
  shortcohortdata[,speciesLeading := max(speciesLeading, na.rm = TRUE), by = pixelGroup]
  shortcohortdata <- unique(shortcohortdata[, .(pixelGroup, speciesLeading)], by = "pixelGroup")
  shortcohortdata[speciesLeading == 0, speciesLeading := 5] # 5 is mixed forests
  attritable <- data.table(ID = sort(unique(shortcohortdata$speciesLeading)))
  attritable[ID == 1, Factor := "Pine leading"]
  attritable[ID == 2, Factor := "Deciduous leading"]
  attritable[ID == 3, Factor := "Black spruce leading"]
  attritable[ID == 4, Factor := "White spruce leading"]
  attritable[ID == 5, Factor := "Mixed"]
  vegTypeMap <- rasterizeReduced(shortcohortdata, pixelGroupMap, "speciesLeading", "pixelGroup")
  vegTypeMap <- setValues(vegTypeMap, as.integer(getValues(vegTypeMap)))
  levels(vegTypeMap) <- as.data.frame(attritable)
  projection(vegTypeMap) <- projection(pixelGroupMap)
  vegTypeMap
}
