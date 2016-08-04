
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "landWebParent",
  description = "higher level control module for Fire, vegetation and 
  output metrics modules that specifically for LandWeb project",
  keywords = c("insert key words here"),
  authors = c(person(c("Yong"), "Luo", email="yluo1@lakeheadu.ca", role=c("aut", "cre"))),
  childModules = character(), #c("biomassSuccessionLANDIS"),
  version = numeric_version("1.2.0.9004"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", # e.g., "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "landWebParent.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur")
  ),
  inputObjects = data.frame(
    objectName = NA_character_,
    objectClass = NA_character_,
    sourceURL = "",
    other = NA_character_,
    stringsAsFactors = FALSE
  ),
  outputObjects = data.frame(
    objectName = NA_character_,
    objectClass = NA_character_,
    other = NA_character_,
    stringsAsFactors = FALSE
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.landWebParent = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)

    # do stuff for this event
    sim <- sim$landWebParentInit(sim)

    # schedule future event(s)
    sim <- scheduleEvent(sim, p(sim)$.plotInitialTime, "landWebParent", "plot")
    sim <- scheduleEvent(sim, p(sim)$.saveInitialTime, "landWebParent", "save")
  } else if (eventType == "plot") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    #Plot(objectFromModule) # uncomment this, replace with object to plot
    # schedule future event(s)

    # e.g.,
    #sim <- scheduleEvent(sim, p(sim)$.plotInitialTime, "landWebParent", "plot")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == "save") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "landWebParent", "save")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == "event1") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "landWebParent", "templateEvent")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == "event2") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "landWebParent", "templateEvent")

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
landWebParentInit <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  browser()

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
landWebParentSave <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
landWebParentPlot <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot("object")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
landWebParentEvent1 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  sim$event1Test2 <- 999 # for dummy unit test


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
landWebParentEvent2 = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
  sim$event2Test2 <- 777  # for dummy unit test


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.init = function(sim) {
  # study area should be provided by Dr. David Anderson
  # Dr. Steve Cumming will provide a temperary one
  dataPath <- file.path(modulePath(sim), "landWebParent",
                                 "data")
  studyArea <- readRDS(file.path(dataPath, "studyarea.rds"))
  
  # download the ecodistricts shapefile
  download.file(url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip",
               destfile = file.path(dataPath, "ecodistrict_shp.zip"))
  ecoDistrictFiles <- unzip(zipfile = file.path(dataPath, "ecodistrict_shp.zip"), 
                            exdir = dataPath)
  ecoDistrict <- raster::shapefile(ecoDistrictFiles[grep(".shp", ecoDistrictFiles)])

  
  # download the ecoregion shapefile
  download.file(url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip",
                destfile = file.path(dataPath, "ecoregion_shp.zip"))
  ecoRegionFiles <- unzip(file.path(dataPath, "ecoregion_shp.zip"),
                          exdir = dataPath)
  ecoRegion <- raster::shapefile(ecoRegionFiles[grep("shp", ecoRegionFiles)])
  
  # download the ecozone shapefile
  download.file(url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
                destfile = file.path(dataPath, "ecozone_shp.zip"))
  ecoZoneFiles <- unzip(file.path(dataPath, "ecozone_shp.zip"),
                        exdir = dataPath)
  ecoZone <- raster::shapefile(ecoZoneFiles[grep("shp", ecoZoneFiles)])
  dd <- FALSE
  if(dd){
    # download the kNN data
    # 1. biomass map
    download.file(url = "http://tree.pfc.forestry.ca/kNN-StructureBiomass.tar",
                  destfile = file.path(dataPath, "kNN-StructureBiomass.tar"),
                  mode = "wb")
    untar(file.path(dataPath, "kNN-StructureBiomass.tar"),
          files = "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.zip",
          exdir = dataPath)
    biomassMaps <- unzip(file.path(dataPath,
                                   "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.zip"),
                         exdir = file.path(dataPath, "biomassMap"))
    biomassMap <- tools::list_files_with_exts(file.path(dataPath, "biomassMap"), "tif", full.names = TRUE)
    biomassMap <- raster(biomassMap)
    
    # 2. stand age map
    download.file(url = "http://tree.pfc.forestry.ca/kNN-StructureStandVolume.tar",
                  destfile = file.path(dataPath, "kNN-StructureStandVolume.tar"),
                  mode = "wb")
    untar(file.path(dataPath, "kNN-StructureStandVolume.tar"),
          files = "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.zip",
          exdir = dataPath)
    standAgeMap <- unzip(file.path(dataPath,
                                   "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.zip"),
                         exdir = file.path(dataPath, "standAgeMap"))
    standAgeMap <- tools::list_files_with_exts(file.path(dataPath, "standAgeMap"), "tif", full.names = TRUE)
    standAgeMap <- raster(standAgeMap)
    
    # 3. species maps
    download.file(url = "http://tree.pfc.forestry.ca/kNN-Species.tar",
                  destfile = file.path(dataPath, "kNN-Species.tar"),
                  mode = "wb")
    filenames <- untar(file.path(dataPath, "kNN-Species.tar"), list = T)
    # species names are defined by national forest inventory program
    speciesnames <- c("Abie_Bal", "Abie_Las",
                      "Betu_Pap", "Pice_Gla", "Pice_Mar", 
                      "Pinu_Ban", "Pinu_Con", "Pinu_Str",
                      "Popu_Tre", "Pseu_Men")
    speciesfiles <- paste("NFI_MODIS250m_kNN_Species_", speciesnames, "_v0.zip",
                          sep = "")
    untar(file.path(dataPath, "kNN-Species.tar"),
          files = speciesfiles,
          exdir = dataPath)
    specieslayers <- stack()
    for(i in 1:length(speciesfiles)){
      speciesmaps <- unzip(file.path(dataPath, speciesfiles[i]),
                           exdir = file.path(dataPath, speciesnames[i]))
      speciesmap <- tools::list_files_with_exts(file.path(dataPath, speciesnames[i]), "tif", full.names = TRUE)
      speciesmap <- raster(speciesmap)
      specieslayers <- stack(specieslayers, speciesmap)
      names(specieslayers)[i] <- speciesnames[i]
    }
  }
  
  
  
  # Any code written here will be run during the simInit and subsequently deleted
  # This is useful if there is something required before simulation, such as data downloading, e.g.,
  # downloadData("LCC2005", modulePath(sim))
  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
