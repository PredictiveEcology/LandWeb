
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "landWebDataPrep",
  description = "prepare the data for simulation, including 1. ecoregion map, 2. initial community map, 3. species table",
  keywords = c("insert key words here"),
  authors = c(person(c("First", "Middle"), "Last", email="email@example.com", role=c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("1.2.0.9006"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_, # e.g., "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "landWebDataPrep.Rmd"),
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
                   "standAgeMap", "speciesMap", "LCC2005", "speciesTable"),
    objectClass = NA_character_,
    sourceURL = c("http://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip",
                  "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip",
                  "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
                  "http://tree.pfc.forestry.ca/kNN-StructureBiomass.tar",
                  "http://tree.pfc.forestry.ca/kNN-StructureStandVolume.tar",
                  "http://tree.pfc.forestry.ca/kNN-Species.tar",
                  "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip",
                  "https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master/speciesTraits.csv"),
    other = NA_character_,
    stringsAsFactors = FALSE
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "initialCommunities", objectClass = "data.table",
                 desc = "initial community table", 
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/initial-communities.txt"),
    createsOutput(objectName = "species", objectClass = "data.table", 
                 desc = "a table that has species traits such as longevity...", 
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/species.txt"),
    createsOutput(objectName = "ecoregionMap", objectClass = "RasterLayer", 
                 desc = "ecoregion map that has mapcodes match ecoregion table and speciesEcoregion table",
                 sourceURL = "https://github.com/LANDIS-II-Foundation/Extensions-Succession/raw/master/biomass-succession-archive/trunk/tests/v6.0-2.0/ecoregions.gis"),
    createsOutput(objectName = "initialCommunitiesMap", objectClass = "RasterLayer", 
                 desc = "initial community map that has mapcodes match initial community table", 
                 sourceURL = "https://github.com/LANDIS-II-Foundation/Extensions-Succession/raw/master/biomass-succession-archive/trunk/tests/v6.0-2.0/initial-communities.gis"),
    createsOutput(objectName = "ecoregion", objectClass = "data.table", 
                 desc = "ecoregion look up table", 
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/ecoregions.txt"),
    createsOutput(objectName = "speciesEcoregion", objectClass = "data.table", 
                 desc = "define the maxANPP, maxB and SEP change with both ecoregion and simulation time", 
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession-dynamic-inputs_test.txt"),
    createsOutput(objectName = "minRelativeB", objectClass = "data.frame", 
                 desc = "define the cut points to classify stand shadeness", 
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession_test.txt"),
    createsOutput(objectName = "sufficientLight", objectClass = "data.frame", 
                 desc = "define how the species with different shade tolerance respond to stand shadeness",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession_test.txt"),
    createsOutput(objectName = "spinupMortalityfraction", objectClass = "numeric", 
                 desc = "define the mortality loss fraction in spin up-stage simulation, default is 0.001", 
                 sourceURL = "NA"),
    createsOutput(objectName = "successionTimestep", objectClass = "numeric", 
                 desc = "define the simulation time step, default is 10 years", sourceURL = "NA"),
    createsOutput(objectName = "cellSize", objectClass = "numeric", 
                 desc = "define the cell size", sourceURL = "NA"),
    createsOutput(objectName = "seedingAlgorithm", objectClass = "character", 
                 desc = "choose which seeding algorithm will be used among noDispersal, universalDispersal,
                 and wardDispersal, default is wardDispersal", sourceURL = "NA"),
    createsOutput(objectName = "useCache", objectClass = "logic", 
                 desc = "define which the caching for spinup simulation should be used, default is TRUE",
                 sourceURL = "NA"),
    createsOutput(objectName = "calibrate", objectClass = "logic", 
                 desc = "should the detailed simulation information be outputed in 
                 spinupOutput and simulationTreeOutput, default is FALSE", 
                 sourceURL = "NA")
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
    sim <- scheduleEvent(sim, p(sim)$.plotInitialTime, "landWebDataPrep", "plot")
    sim <- scheduleEvent(sim, p(sim)$.saveInitialTime, "landWebDataPrep", "save")
  } else if (eventType == "plot") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    #Plot(objectFromModule) # uncomment this, replace with object to plot
    # schedule future event(s)

    # e.g.,
    #sim <- scheduleEvent(sim, p(sim)$.plotInitialTime, "landWebDataPrep", "plot")

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
  } else if (eventType == "event1") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "landWebDataPrep", "templateEvent")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == "event2") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "landWebDataPrep", "templateEvent")

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

  # ! ----- STOP EDITING ----- ! #

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

### template for your event1
landWebDataPrepEvent1 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  sim$event1Test2 <- 999 # for dummy unit test


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
landWebDataPrepEvent2 = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
  sim$event2Test2 <- 777  # for dummy unit test


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects = function(sim) {
  dataPath <- file.path(modulePath(sim), "landWebDataPrep", "data")
  checkTable <- data.table(downloadData(module = "landWebDataPrep", path = modulePath(sim)))
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
  speciesnames <- c("Abie_Las",
                    "Pice_Gla", "Pice_Mar", 
                    "Pinu_Ban", "Pinu_Con", 
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
  speciesEcoregionTable[, species:=as.character(species)]
  septable[,species:=as.character(species)]
  speciesEcoregionTable <- left_join(speciesEcoregionTable, septable, by = c("ecoregion", "species")) %>%
    data.table
  # 
  speciesEcoregionTable[SEP==0, ':='(maxBiomass = 0, maxANPP = 0)]
  # 
  NON_NAdata <- speciesEcoregionTable[!is.na(maxBiomass),]
  # 
  NAdata <- speciesEcoregionTable[is.na(maxBiomass),]
  if(nrow(NAdata) > 1){
    # # replace NA values with ecoregion  value
    source('~/GitHub/landwebNRV/landwebNRV/R/biomassAttributes_kNN_biggerEcoAddition.R')
    # 
    biomassFrombiggerMap <- biomassAttributes_kNN_biggerEcoAddition(speciesLayers = specieslayers,
                                                                    biomassLayer = biomassMap,
                                                                    SALayer = standAgeMap,
                                                                    ecoregionMap = simulationMaps$ecoregionMap,
                                                                    biggerEcoMap = ecoRegion,
                                                                    NAData = NAdata)
    NON_NAdata <- rbind(NON_NAdata, biomassFrombiggerMap$addData[!is.na(maxBiomass), .(ecoregion, species, maxBiomass, maxANPP, SEP)])
    NAdata <- biomassFrombiggerMap$addData[is.na(maxBiomass),.(ecoregion, species, maxBiomass, maxANPP, SEP)]
  }
  if(nrow(NAdata) > 1){
    names(ecoZone@data)[grep("ECOZONE",names(ecoZone@data))] <- "ECOREGION"
    biomassFrombiggerMap <- biomassAttributes_kNN_biggerEcoAddition(speciesLayers = specieslayers,
                                                                    biomassLayer = biomassMap,
                                                                    SALayer = standAgeMap,
                                                                    ecoregionMap = simulationMaps$ecoregionMap,
                                                                    biggerEcoMap = ecoZone,
                                                                    NAData = NAdata)
    NON_NAdata <- rbind(NON_NAdata, biomassFrombiggerMap$addData[!is.na(maxBiomass), .(ecoregion, species, maxBiomass, maxANPP, SEP)])
    NAdata <- biomassFrombiggerMap$addData[is.na(maxBiomass),.(ecoregion, species, maxBiomass, maxANPP, SEP)]
  }
  NAdata[,':='(maxBiomass=0, maxANPP=0, SEP=0)]
  sim$speciesEcoregion <- rbind(NON_NAdata,NAdata)
  sim$ecoregion <- simulationMaps$ecoregion
  sim$ecoregionMap <- simulationMaps$ecoregionMap
  sim$initialCommunities <- simulationMaps$initialCommunity
  sim$initialCommunitiesMap <- simulationMaps$initialCommunityMap
  saveRDS(simulationMaps$ecoregionMap,
          file.path(modulePath(sim), "landWebDataPrep", "outputs", "ecoregionMap.rds"))
  saveRDS(simulationMaps$initialCommunityMap,
          file.path(modulePath(sim), "landWebDataPrep", "outputs", "initialCommunitiesMap.rds"))
  write.csv(simulationMaps$ecoregion,
            file.path(modulePath(sim), "landWebDataPrep", "outputs", "ecoregion.csv"),
            row.names = FALSE)
  write.csv(simulationMaps$initialCommunity,
            file.path(modulePath(sim), "landWebDataPrep", "outputs", "initialCommunities.csv"),
            row.names = FALSE)
  write.csv(sim$speciesEcoregion,
            file.path(modulePath(sim), "landWebDataPrep", "outputs", "speciesEcoregion.csv"),
            row.names = FALSE)

  # species traits inputs
  speciesTable <- read.csv(file.path(dataPath, "speciesTraits.csv"), header = TRUE,
                      stringsAsFactors = FALSE) %>%
    data.table
  names(speciesTable) <- c("species", "Area", "longevity", "sexualmature", "shadetolerance", "firetolerance", 
                           "seeddistance_eff", "seeddistance_max", "resproutprob", "resproutage_min", 
                           "resproutage_max", "postfireregen", "leaflongevity", "wooddecayrate", 
                           "mortalityshape", "growthcurve", "leafLignin", "hardsoft")
  speciesTable[,':='(Area = NULL, hardsoft = NULL)]
  set(speciesTable, ,c("species1", "species2"), NA)
  speciesTable[,':='(species1=NA, species2=NA)]
  speciesTable[, ':='(species1 = as.character(substring(species, 1, 4)),
                      species2 = as.character(substring(species, 6, nchar(species))))]
  
  
  speciesTable[,':='(species = paste(as.character(substring(species1, 1, 1)), 
                     tolower(as.character(substring(species1, 2, nchar(species1)))),
                     "_",
                     as.character(substring(species2, 1, 1)), 
                     tolower(as.character(substring(species2, 2, nchar(species2)))),
                     sep = ""))]
  speciesTable[species == "Pinu_Con.lat", species:="Pinu_Con"]
  
  
  speciesTable <- speciesTable[species %in% speciesnames,][, ':='(species1 = NULL, species2 = NULL)]
  sim$speciesTable <- unique(speciesTable, by = "species")
  write.csv(sim$speciesTable,
            file.path(modulePath(sim), "landWebDataPrep", "outputs", "speciesTable.csv"),
            row.names = FALSE)
  # save the tables and maps
  # split initial community map and ecoregion map 5*5 with 100 pixels buffer (25000m buffer)
  # and save them for the simulation
  names(simulationMaps$ecoregionMap) <- "ecoregionMap"
  splitRaster(simulationMaps$ecoregionMap, nx = 5, ny = 5, buffer = 100,
              path = file.path(modulePath(sim), "landWebDataPrep", "outputs"))
  names(simulationMaps$initialCommunityMap) <- "initialCommunitiesMap"
  splitRaster(simulationMaps$initialCommunityMap, nx = 5, ny = 5, buffer = 100,
              path = file.path(modulePath(sim), "landWebDataPrep", "outputs"))
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
