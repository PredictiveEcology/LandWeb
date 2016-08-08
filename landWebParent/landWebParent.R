
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "landWebParent",
  description = "higher level control module for Fire, vegetation and 
  output metrics modules that specifical for LandWeb project",
  keywords = c("insert key words here"),
  authors = c(person(c("Yong"), "Luo", email="yluo1@lakeheadu.ca", role=c("aut", "cre"))),
  childModules = character(),#c("testmodule"), #c("biomassSuccessionLANDIS"),
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
    objectName = c("ecoDistrict", "ecoRegion", "ecoZone", "biomassMap",
                   "standAgeMap", "speciesMap", "LCC2005"),
    objectClass = NA_character_,
    sourceURL = c("http://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip",
                  "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip",
                  "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
                  "http://tree.pfc.forestry.ca/kNN-StructureBiomass.tar",
                  "http://tree.pfc.forestry.ca/kNN-StructureStandVolume.tar",
                  "http://tree.pfc.forestry.ca/kNN-Species.tar",
                  "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip"),
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
    browser()
    # do stuff for this event
    sim <- sim$landWebParentInit(sim)
    sim <- sim$landWebParentEvent1(sim)
    sim <- scheduleEvent(sim, time(sim), "landWebParent", "event1")
    
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
  
  browser()
  print(time(sim))
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
  dataPath <- file.path(modulePath(sim), "landWebParent", "data")
  checkTable <- data.table(downloadData(module = "landWebParent", path = modulePath(sim)))
  checkContent_passed <- checkTable[result == "OK",]$expectedFile
  # study area should be provided by Dr. David Anderson
  # Dr. Steve Cumming will provide a temperary one
  studyArea <- readRDS(file.path(dataPath, "studyarea.rds"))
  
  if(!all(c("ecodistricts.dbf", "ecodistricts.prj", "ecodistricts.sbn", 
            "ecodistricts.sbx", "ecodistricts.shp", "ecodistricts.shx") %in% checkContent_passed)){
    unzip(zipfile = file.path(dataPath, "ecodistrict_shp.zip"),
          exdir = dataPath)
    filenames <- dir(file.path(dataPath, "Ecodistricts"))
    file.copy(from = file.path(dataPath, "Ecodistricts", filenames),
              to = file.path(dataPath, filenames),
              overwrite = TRUE)
    unlink(file.path(dataPath, "Ecodistricts"), recursive = TRUE) 
    rm(filenames)
  }
  
  ecoDistrict <- raster::shapefile(file.path(dataPath, "ecodistricts.shp"))
  
  
  if(!all(c("ecoregions.dbf", "ecoregions.prj", "ecoregions.sbn", 
            "ecoregions.sbx", "ecoregions.shp", "ecoregions.shx") %in% checkContent_passed)){
    unzip(zipfile = file.path(dataPath, "ecoregion_shp.zip"),
          exdir = dataPath)
    filenames <- dir(file.path(dataPath, "Ecoregions"))
    file.copy(from = file.path(dataPath, "Ecoregions", filenames),
              to = file.path(dataPath, filenames),
              overwrite = TRUE)
    unlink(file.path(dataPath, "Ecoregions"), recursive = TRUE) 
    rm(filenames)
  } 
  ecoRegion <- raster::shapefile(file.path(dataPath, "ecoregions.shp"))
  
  
  
  if(!all(c("ecoregions.dbf", "ecoregions.prj", "ecoregions.sbn", 
            "ecoregions.sbx", "ecoregions.shp", "ecoregions.shx") %in% checkContent_passed)){
    unzip(zipfile = file.path(dataPath, "ecozone_shp.zip"),
          exdir = dataPath)
    filenames <- dir(file.path(dataPath, "Ecozones"))
    file.copy(from = file.path(dataPath, "Ecozones", filenames),
              to = file.path(dataPath, filenames),
              overwrite = TRUE)
    unlink(file.path(dataPath, "Ecozones"), recursive = TRUE) 
  } 
  ecoZone <- raster::shapefile(file.path(dataPath, "ecozones.shp"))
  
  
  if(!all(c("NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.tif",
            "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.tif.aux.xml",
            "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.tif.xml") %in%
          checkContent_passed)){
    untar(file.path(dataPath, "kNN-StructureBiomass.tar"),
          files = "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.zip",
          exdir = dataPath)
    biomassMaps <- unzip(file.path(dataPath,
                                   "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.zip"),
                         exdir = dataPath)
    file.remove(file.path(dataPath, "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.zip"))
  }
  biomassMap <- raster(file.path(dataPath, "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.tif"))
  
  
  # 2. stand age map
  if(!all(c("NFI_MODIS250m_kNN_Structure_Stand_Age_v0.tif",
            "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.tif.aux.xml",
            "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.tif.xml") %in% checkContent_passed)){
    untar(file.path(dataPath, "kNN-StructureStandVolume.tar"),
          files = "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.zip",
          exdir = dataPath)
    unzip(file.path(dataPath,
                    "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.zip"),
          exdir = dataPath)
    file.remove(file.path(dataPath,
                          "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.zip"))
  }
  standAgeMap <- raster(file.path(dataPath, "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.tif"))
  
  # 3. species maps
  specieslayers <- stack()
  speciesnames <- c("Abie_Bal", "Abie_Las",
                    "Betu_Pap", "Pice_Gla", "Pice_Mar", 
                    "Pinu_Ban", "Pinu_Con", "Pinu_Str",
                    "Popu_Tre", "Pseu_Men")
   i <- 1
  for(indispecies in speciesnames){
    if(!all(paste("NFI_MODIS250m_kNN_Species_", indispecies, "_v0.tif",
                  c("",".aux.xml", ".xml"), sep = "") %in% checkContent_passed)){
      untar(file.path(dataPath, "kNN-Species.tar"),
            files = paste("NFI_MODIS250m_kNN_Species_", indispecies, "_v0.zip",
                          sep = ""),
            exdir = dataPath)
      unzip(file.path(dataPath, paste("NFI_MODIS250m_kNN_Species_", indispecies, "_v0.zip",
                                      sep = "")),
            exdir = dataPath)
      file.remove(file.path(dataPath, paste("NFI_MODIS250m_kNN_Species_", indispecies, "_v0.zip",
                                            sep = ""))) 
      
    }
    speciesmap <- raster(file.path(dataPath, paste("NFI_MODIS250m_kNN_Species_", indispecies,
                                                   "_v0.tif", sep = "")))
    specieslayers <- stack(specieslayers, speciesmap)
    names(specieslayers)[i] <- indispecies
    i <- i+1
  }
  
  if(!("LCC2005_V1_4a.tif" %in% checkContent_passed)){
    unzip(file.path(dataPath, "LandCoverOfCanada2005_V1_4.zip"),
          exdir = dataPath)
  }
    LCC05 <- raster::raster(file.path(dataPath, "LCC2005_V1_4a.tif"))
  
  browser()
   source('~/GitHub/landwebNRV/landwebNRV/R/initialCommunityMapProducer_kNN.R')
   initialCommFiles <- initialCommunityMapProducer_kNN(speciesLayers = specieslayers, 
                                                       speciesPresence = 50,
                                                       studyArea = studyArea)
   ecoregionstatus <- data.table(active = "yes",
                                 ecoregion = 1:1031)
   source('~/GitHub/landwebNRV/landwebNRV/R/ecoregionMapProducer.R')
   ecoregionFiles <- ecoregionMapProducer(studyAreaRaster = initialCommFiles$initialCommunityMap,
                                          ecoregionMapFull = ecoDistrict,
                                          ecoregionName = "ECODISTRIC",
                                          ecoregionActiveStatus = ecoregionstatus,
                                          studyArea = studyArea)
   
   
  
   activeStatusTable <- data.table(active = c(rep("yes", 15), rep("no", 25)),
                                   mapcode = 1:40)  # this is based on description
   source('~/GitHub/landwebNRV/landwebNRV/R/nonactiveEcoFromRaster.R')
   simulationMaps <- nonactiveEcoFromRaster(nonactiveRaster = LCC05,
                                            activeStatus = activeStatusTable,
                                            ecoregionMap = ecoregionFiles$ecoregionMap,
                                            ecoregion = ecoregionFiles$ecoregion,
                                            initialCommunityMap = initialCommFiles$initialCommunityMap,
                                            initialCommunity = initialCommFiles$initialCommunity)
   source('~/GitHub/landwebNRV/landwebNRV/R/biomassAttributes_kNN.R')
   speciesEcoregionTable <- biomassAttributes_kNN(speciesLayers = specieslayers,
                                                  biomassLayer = biomassMap,
                                                  SALayer = standAgeMap,
                                                  ecoregionMap = simulationMaps$ecoregionMap)
   browser()
   source('~/GitHub/landwebNRV/landwebNRV/R/speciesRelativeAbundance_kNN.R')
   speciesSEP <- speciesRelativeAbundance_kNN(ecoregionMap = simulationMaps$ecoregionMap,
                                              speciesLayers = specieslayers)
  # 
  # 
   septable <- speciesSEP$speciesAbundanceTable
  # sepmaps <- speciesSEP$speciesAbundanceMaps
   names(septable) <- c("ecoregion", "species", "SEP")
   septable[, SEP:=round(SEP, 2)]
  # 
  # 
   speciesEcoregionTable <- left_join(speciesEcoregionTable, septable, by = c("ecoregion", "species")) %>%
     data.table
  # 
   speciesEcoregionTable[SEP==0, ':='(maxBiomass = 0, maxANPP = 0)]
  # 
   NON_NAdata <- speciesEcoregionTable[!is.na(maxBiomass),]
  # 
   NAdata <- speciesEcoregionTable[is.na(maxBiomass),]
  # 
  # # replace NA values with ecoregion  value
   source('~/GitHub/landwebNRV/landwebNRV/R/biomassAttributes_kNN_biggerEcoAddition.R')
  # 
   dd <- biomassAttributes_kNN_biggerEcoAddition(speciesLayers = specieslayers,
                                                 biomassLayer = biomassMap,
                                                 SALayer = standAgeMap,
                                                 ecoregionMap = simulationMaps$ecoregionMap,
                                                 biggerEcoMap = ecoRegion,
                                                 NAData = NAdata)
  # 
  # names(NON_NAdata)
  # biggerEcoMap<- shapefile("M:/data/ecoFramework/Ecoregions/ecoregions.shp")
  # NON_NAdata <- rbind(NON_NAdata, dd$addData[!is.na(maxBiomass), .(ecoregion, species, maxBiomass, maxANPP, SEP)])
  # 
  # NAdata <- dd$addData[is.na(maxBiomass),.(ecoregion, species, maxBiomass, maxANPP, SEP)]
  # 
  # biggerEcoMap1<- shapefile("M:/data/ecoFramework/Ecozones/ecozones.shp")
  # 
  # names(biggerEcoMap1@data)[grep("ECOZONE",names(biggerEcoMap1@data))] <- "ECOREGION"
  # source('~/GitHub/landwebNRV/landwebNRV/R/biomassAttributes_kNN_biggerEcoAddition.R')
  # ecozoneValues <- biomassAttributes_kNN_biggerEcoAddition(speciesLayers = specieslayersfull,
  #                                                          biomassLayer = biomassmap,
  #                                                          SALayer = samap,
  #                                                          ecoregionMap = simulationMaps$ecoregionMap,
  #                                                          biggerEcoMap = biggerEcoMap1,
  #                                                          NAData = NAdata)
  # 
  # 
  # 
  # 
  
  
  
  # Any code written here will be run during the simInit and subsequently deleted
  # This is useful if there is something required before simulation, such as data downloading, e.g.,
  # downloadData("LCC2005", modulePath(sim))
  # ! ----- EDIT BELOW ----- ! #
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above



