
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "landWebDataPrep",
  description = "insert module description here",
  keywords = c("insert key words here"),
  authors = c(person(c("First", "Middle"), "Last", email="email@example.com", role=c("aut", "cre"))),
  childModules = character(0),
  version = numeric_version("1.3.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "landWebDataPrep.Rmd"),
  reqdPkgs = list("data.table", "raster", "sp"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "calibrate", objectClass = "logical",
                 desc = "determine whether all the growth and mortality in LBMR are detailed recorded,
                 default is FALSE", 
                 sourceURL = NA),
    expectsInput(objectName = "shpStudyRegionX", objectClass = "SpatialPolygonsDataFrame",
                 desc = "this shape file contains two informaton: study area and fire return interval", 
                 sourceURL = NA), # i guess this is study area and fire return interval
    expectsInput(objectName = "biomassMap", objectClass = "RasterLayer", 
                 desc = "total biomass raster layer in study area, default is canada national biomass map", 
                 sourceURL = "http://tree.pfc.forestry.ca/kNN-StructureBiomass.tar"),
    expectsInput(objectName = "LCC2005Orig", objectClass = "RasterLayer", 
                 desc = "2005 land classification map in study area, default is canada national land classification in 2005", 
                 sourceURL = "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip")
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    # input for LBMR
    createsOutput(objectName = "studyArea", objectClass = "SpatialPolygons",
                 desc = "output as input for landWeb_LBMRDataPrep"),
    createsOutput(objectName = "calibrate", objectClass = "logical",
                  desc = "output as input for landWeb_LBMRDataPrep"),
    createsOutput(objectName = "LCC2005", objectClass = "RasterLayer", 
                  desc = "output as input for landWeb_LBMRDataPrep"),
    createsOutput(objectName = "biomassMap", objectClass = "RasterLayer", 
                  desc = "output as input for landWeb_LBMRDataPrep"),
    createsOutput(objectName = "shpStudyRegionX", objectClass = "SpatialPolygonsDataFrame", 
                 desc = "output as input for initBaseMaps"),
    createsOutput(objectName = "LCC05X", objectClass = "RasterLayer", 
                 desc = "output as input for initBaseMaps")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.landWebDataPrep = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)
    
    # do stuff for this event
    sim <- sim$landWebDataPrepInit(sim)
    
    # schedule future event(s)
    sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "landWebDataPrep", "plot")
    sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "landWebDataPrep", "save")
  } else if (eventType == "plot") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
    #Plot(objectFromModule) # uncomment this, replace with object to plot
    # schedule future event(s)
    
    # e.g.,
    #sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "landWebDataPrep", "plot")
    
    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == "save") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
    
    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function
    
    # schedule future event(s)
    
    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "landWebDataPrep", "save")
    
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
landWebDataPrepInit <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  projection(sim$LCC2005Orig) <- projection(sim$biomassMap)
  sim$studyArea <- spTransform(sim$shpStudyRegionX, crs(sim$biomassMap))
  sim$shpStudyRegionX <- sim$studyArea
  sim$LCC05X <- sim$LCC2005Orig
  sim$LCC2005 <- sim$LCC2005Orig
  return(invisible(sim))
}

### template for save events
landWebDataPrepSave <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
landWebDataPrepPlot <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot("object")
  
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
  # if(!('defaultColor' %in% sim$userSuppliedObjNames)) {
  #  defaultColor <- 'red'
  # }
  # ! ----- EDIT BELOW ----- ! #
  dataPath <- file.path(modulePath(sim), "landWebDataPrep", "data")
  fileNames <- c("NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.tif",
                 "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.tif.aux.xml",
                 "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.tif.xml",
                 "LCC2005_V1_4a.tif")
  fileNames <- lapply(fileNames, function(x){file.path(dataPath, x)})
  allFiles <- lapply(fileNames, function(x) {
    file.info(x)[,"size"]}
  )
  names(allFiles) <- unlist(lapply(fileNames, basename))
  needDownload <- digest::digest(allFiles) != "ea72c7607d0ea744b64e182459c940bc"
  if(needDownload){
    checkTable <- data.table(downloadData(module = "landWebDataPrep", path = modulePath(sim)))
    untar(file.path(dataPath, "kNN-StructureBiomass.tar"),
          files = "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.zip",
          exdir = dataPath)
    biomassMaps <- unzip(file.path(dataPath,
                                   "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.zip"),
                         exdir = dataPath)
    file.remove(file.path(dataPath, "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.zip"))
    
    unzip(file.path(dataPath, "LandCoverOfCanada2005_V1_4.zip"),
          exdir = dataPath) 
  } else {
    message("  Download data step skipped for module landWebDataPrep. Local copy exists")
  }
  sim$biomassMap <- raster(file.path(dataPath, "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.tif"))
  sim$LCC2005Orig <- raster(file.path(dataPath, "LCC2005_V1_4a.tif"))
  sim$calibrate <- FALSE
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
