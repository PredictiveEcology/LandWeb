
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "LandMine",
  description = "insert module description here",
  keywords = c("insert key words here"),
  authors = person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
  childModules = character(0),
  version = numeric_version("1.3.1.9022"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "LandMine.Rmd"),
  reqdPkgs = list("raster", "RColorBrewer", "data.table"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter("burnInitialTime", "numeric", start(sim), NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter("burnFrequency", "numeric", 1, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = NA, objectClass = NA, desc = NA, sourceURL = NA)
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.LandMine = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)

    # do stuff for this event
    sim <- sim$LandMineInit(sim)
    
    # schedule future event(s)
    sim <- scheduleEvent(sim, P(sim)$burnInitialTime, "LandMine", "LandMineBurn")
    sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "LandMine", "plot")
    sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "LandMine", "save")
  } else if (eventType == "plot") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    #Plot(objectFromModule) # uncomment this, replace with object to plot
    # schedule future event(s)

    # e.g.,
    #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "LandMine", "plot")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == "save") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "LandMine", "save")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == "LandMineBurn") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    sim <- sim$LandMineBurn(sim)
    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "LandMine", "templateEvent")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == "event2") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "LandMine", "templateEvent")

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
LandMineInit <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
LandMineSave <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
LandMinePlot <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot("object")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
LandMineBurn <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  dev();
  seed <- sample(1e6,1)
  #seed <- 692774
  set.seed(seed); print(seed)
  maxSizes <- rexp(length(sim$startCells), rate = 1/500)
  fires <- sim$burn(sim$hab, startCells = sim$startCells, 
                    maxSizes = maxSizes)
  if(any(fires[,.N,by=id]$N < floor(maxSizes))) stop("Fire weren't exact")
  firesRas <- raster(sim$hab)
  firesRas[] <- 0
  firesRas[fires$indices] <- fires$id
  Plot(firesRas,new=TRUE, cols = c("red", "yellow"),
       zero.color = "white")
  Plot(hist(fires[,.N,by=id]$N), title = "fire size distribution")
  

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
LandMineEvent2 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
  sim$event2Test2 <- 777  # for dummy unit test


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects = function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create an named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can use 'sim$.userSuppliedObjNames' in their function below to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call. e.g.,
  # if (!('defaultColor' %in% sim$.userSuppliedObjNames)) {
  #  sim$defaultColor <- 'red'
  # }
  # ! ----- EDIT BELOW ----- ! #
  
  # Make random forest cover map
  emptyRas <- raster(extent(0, 1e3, 0, 1e3), res = 1)
  sim$hab <- randomPolygons(emptyRas, numTypes = 40)
  names(sim$hab) <- "hab"
  mask <- raster(emptyRas)
  mask <- setValues(mask, 0)
  mask[1:5000] <- 1
  numCol <- ncol(emptyRas)
  numCell <- ncell(emptyRas)
  directions <- 8
  
  # Can use transparent as a color
  setColors(sim$hab) <- paste(c("transparent", brewer.pal(8, "Greys")))
  
  # note speedup is equivalent to making pyramids, so, some details are lost
  if (interactive()) {
    clearPlot()
    Plot(sim$hab, speedup = 3)
  }
  
  # initiate 10 fires
  sim$startCells <- as.integer(sample(1:ncell(emptyRas),10))
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above

burn <- function(landscape, startCells, maxSizes = 5, nActiveCells1 = c(10, 36), spawnNewActive = c(0.46, 0.2, 0.26, 0.11),
                 sizeCutoffs = c(8e3, 2e4)) {

  a = spread(landscape, loci = startCells, spreadProb = 1, persistence = 0,
             neighProbs = c(1-spawnNewActive[1], spawnNewActive[1]), iterations = 1,
             mask=NULL, maxSize = maxSizes, directions=8, returnIndices = TRUE,
             id = TRUE, plot.it = FALSE, exactSizes = TRUE);
  while(sum(a$active)>0) {
    b <- a[,list(numActive = sum(active), fireSize = .N),by=id]
    set(b, , "pSpawnNewActive", spawnNewActive[1])
    b[numActive>=nActiveCells1[1] & numActive<nActiveCells1[2] & fireSize < sizeCutoffs[2], pSpawnNewActive:=spawnNewActive[2]]
    b[numActive>nActiveCells1[2] & fireSize < sizeCutoffs[1], pSpawnNewActive:=spawnNewActive[4]]
    b[numActive<nActiveCells1[2] & fireSize > sizeCutoffs[2], pSpawnNewActive:=spawnNewActive[3]]
    set(b, , "pNoNewSpawn", 1-b$pSpawnNewActive)
    
    # spawnNewActive must be joined sent in here as list...
    b <- b[a]
    a <- spread(landscape, spreadProb = 1, spreadState = a, persistence = 0,
           neighProbs = transpose(as.list(b[active==TRUE,c("pNoNewSpawn", "pSpawnNewActive")])), 
           iterations = 1,
           mask=NULL, maxSize = maxSizes, directions=8, returnIndices = TRUE,
           id = TRUE, plot.it = FALSE, exactSizes = TRUE)
  }       
  

  return(a)  
}
