
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
    expectsInput(objectName = "ecoDistrict", objectClass = "SpatialPolygonsDataFrame",
                 desc = "ecodistricts in study area, default is canada national ecodistricts", 
                 sourceURL = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip"),
    expectsInput(objectName = "ecoRegion", objectClass = "SpatialPolygonsDataFrame", 
                 desc = "ecoregions in study area, default is canada national ecoregions", 
                 sourceURL = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip"),
    expectsInput(objectName = "ecoZone", objectClass = "SpatialPolygonsDataFrame", 
                 desc = "ecozones in study area, default is canada national ecozones", 
                 sourceURL = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip"),
    expectsInput(objectName = "biomassMap", objectClass = "RasterLayer", 
                 desc = "total biomass raster layer in study area, default is canada national biomass map", 
                 sourceURL = "http://tree.pfc.forestry.ca/kNN-StructureBiomass.tar"),
    expectsInput(objectName = "standAgeMap", objectClass = "RasterLayer", 
                 desc = "stand age map in study area, default is canada national stand age map", 
                 sourceURL = "http://tree.pfc.forestry.ca/kNN-StructureStandVolume.tar"),
    expectsInput(objectName = "speciesMap", objectClass = "RasterStack", 
                 desc = "biomass percentage raster layers by species in canada species map", 
                 sourceURL = "http://tree.pfc.forestry.ca/kNN-Species.tar"),
    expectsInput(objectName = "LCC2005", objectClass = "RasterLayer", 
                 desc = "2005 land classification map in study area, default is canada national land classification in 2005", 
                 sourceURL = "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip"),
    expectsInput(objectName = "speciesTable", objectClass = "data.table", 
                 desc = "species attributes table, default is from Dominic and Yan's project", 
                 sourceURL = "https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master/speciesTraits.csv"),
    expectsInput(objectName = "studyArea", objectClass = "SpatialPolygons", 
                 desc = "study area specific for landweb project, should be provided by Dave", 
                 sourceURL = "NA"),
    expectsInput(objectName = "fireReturnInterval", objectClass = "SpatialPolygonsDataFrame", 
                 desc = "the fire return interval map, will be used by FireNull module", 
                 sourceURL = "NA")
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    # input for LBMR
    createsOutput(objectName = "initialCommunities", objectClass = "data.table",
                 desc = "initial community table"),
    createsOutput(objectName = "species", objectClass = "data.table", 
                 desc = "a table that has species traits such as longevity..."),
    createsOutput(objectName = "ecoregionMap", objectClass = "RasterLayer", 
                 desc = "ecoregion map that has mapcodes match ecoregion table and speciesEcoregion table"),
    createsOutput(objectName = "initialCommunitiesMap", objectClass = "RasterLayer", 
                 desc = "initial community map that has mapcodes match initial community table"),
    createsOutput(objectName = "ecoregion", objectClass = "data.table", 
                 desc = "ecoregion look up table"),
    createsOutput(objectName = "speciesEcoregion", objectClass = "data.table", 
                 desc = "define the maxANPP, maxB and SEP change with both ecoregion and simulation time"),
    createsOutput(objectName = "minRelativeB", objectClass = "data.frame", 
                 desc = "define the cut points to classify stand shadeness"),
    createsOutput(objectName = "sufficientLight", objectClass = "data.frame", 
                 desc = "define how the species with different shade tolerance respond to stand shadeness"),
    createsOutput(objectName = "spinupMortalityfraction", objectClass = "numeric", 
                 desc = "define the mortality loss fraction in spin up-stage simulation, default is 0.001"),
    createsOutput(objectName = "successionTimestep", objectClass = "numeric", 
                 desc = "define the simulation time step, default is 10 years"),
    createsOutput(objectName = "cellSize", objectClass = "numeric", 
                 desc = "define the cell size"),
    createsOutput(objectName = "seedingAlgorithm", objectClass = "character", 
                 desc = "choose which seeding algorithm will be used among noDispersal, universalDispersal,
                 and wardDispersal, default is wardDispersal"),
    createsOutput(objectName = "useCache", objectClass = "logic", 
                 desc = "define which the caching for spinup simulation should be used, default is TRUE"),
    createsOutput(objectName = "calibrate", objectClass = "logic", 
                 desc = "should the detailed simulation information be outputed in 
                 spinupOutput and simulationTreeOutput, default is FALSE"),
    # for FireNULL module
    createsOutput(objectName = "LCC05", objectClass = "RasterLayer", 
                  desc = "LCC05 map"),
    createsOutput(objectName = "shpStudyRegion", objectClass = "SpatialPolygonsDataFrame", 
                  desc = "")
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
  # standardize all the spatial objects into one projection system
  # take crs of species layers as destiny 
  # check whether biomass map , stand age map and species layers have same crs and res
  # if not must be reproject
  specieslayers_crs <- as.character(crs(sim$specieslayers))
  specieslayers_xres <- xres(sim$specieslayers)
  specieslayers_yres <- yres(sim$specieslayers)
  if(as.character(crs(sim$biomassMap)) != specieslayers_crs |
     xres(sim$biomassMap) != specieslayers_xres |
     yres(sim$biomassMap) != specieslayers_yres){
   sim$biomassMap <- projectRaster(sim$biomassMap, crs = crs(sim$specieslayers),
                                     res = c(specieslayers_xres, specieslayers_yres)) 
  }
  if(as.character(crs(sim$standAgeMap)) != specieslayers_crs |
     xres(sim$standAgeMap) != specieslayers_xres |
     yres(sim$standAgeMap) != specieslayers_yres){
    sim$standAgeMap <- projectRaster(sim$standAgeMap, crs = crs(sim$specieslayers),
                                      res = c(specieslayers_xres, specieslayers_yres)) 
  }
  # if(as.character(crs(sim$LCC05)) != specieslayers_crs |
  #    xres(sim$LCC05) != specieslayers_xres |
  #    yres(sim$LCC05) != specieslayers_yres){
  #   sim$LCC05 <- projectRaster(sim$LCC05, crs = crs(sim$specieslayers),
  #                                     res = c(specieslayers_xres, specieslayers_yres)) 
  # }
  
  # for the spatial polygons
  sim$studyArea <- spTransform(sim$studyArea, crs(sim$specieslayers))
  sim$ecoDistrict <- spTransform(sim$ecoDistrict, crs(sim$specieslayers))
  sim$ecoRegion <- spTransform(sim$ecoRegion, crs(sim$specieslayers))
  sim$ecoZone <- spTransform(sim$ecoZone, crs(sim$specieslayers))
  
  # for the raster layers
  
  
  
  browser()
  sim <- cachedFunctions(sim)
  
  initialCommFiles <- sim$initialCommunityProducerCached(speciesLayers = sim$specieslayers, 
                                                         speciesPresence = 50,
                                                         studyArea = sim$studyArea)
  browser()
  ecoregionstatus <- data.table(active = "yes",
                                ecoregion = 1:1031)
  ecoregionFiles <- sim$ecoregionProducerCached(studyAreaRaster = initialCommFiles$initialCommunityMap,
                                                ecoregionMapFull = ecoDistrict,
                                                ecoregionName = "ECODISTRIC",
                                                ecoregionActiveStatus = ecoregionstatus,
                                                studyArea = studyArea)
  browser()
  
  
  activeStatusTable <- data.table(active = c(rep("yes", 15), rep("no", 25)),
                                  mapcode = 1:40)  # this is based on description
  simulationMaps <- sim$nonActiveEcoregionProducerCached(nonactiveRaster = LCC05,
                                           activeStatus = activeStatusTable,
                                           ecoregionMap = ecoregionFiles$ecoregionMap,
                                           ecoregion = ecoregionFiles$ecoregion,
                                           initialCommunityMap = initialCommFiles$initialCommunityMap,
                                           initialCommunity = initialCommFiles$initialCommunity)
  browser()
  
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
  # ! ----- STOP EDITING ----- ! #
  
  return(invisible(sim))
}

cachedFunctions <- function(sim) {
  # for slow functions, add cached versions. Then use sim$xxx() throughout module instead of xxx()
  if(sim$useCache) {
    # Step 1 - create a location for the cached data
    sim$cachedFuncdLoc <- file.path(paths(sim)$cachePath,
                                                       "cachedFunctions")
    # Step 1a - check whether that location already exists
    if(!dir.exists(sim$cachedFuncdLoc) ){
      # Step 1b - if not, create it
      archivist::createEmptyRepo(sim$cachedFuncdLoc)
    }
    # Step 2 - create a version of every function that is slow that includes the caching implicitly
    sim$initialCommunityProducerCached <- function(...) {
      archivist::cache(cacheRepo = sim$cachedFuncdLoc,
                       FUN = initialCommunityProducer, ...)
    }
    sim$ecoregionProducerCached <- function(...) {
      archivist::cache(cacheRepo = sim$cachedFuncdLoc,
                       FUN = ecoregionProducer, ...)
    }
    sim$nonActiveEcoregionProducerCached <- function(...) {
      archivist::cache(cacheRepo = sim$cachedFuncdLoc,
                       FUN = nonActiveEcoregionProducer, ...)
    }
  } else {
    # Step 3 - create a non-caching version in case caching is not desired
    #  sim$spinUp <- sim$spinUpRaw
    sim$initialCommunityProducerCached <- initialCommunityProducer
    sim$ecoregionProducerCached <- ecoregionProducer
    sim$nonActiveEcoregionProducerCached <- nonActiveEcoregionProducer
  }
  return(invisible(sim))
}
initialCommunityProducer <- function(speciesLayers, speciesPresence, studyArea) {
  specieslayerInStudyArea <- crop(speciesLayers,
                                  studyArea)
  specieslayerInStudyArea <- suppressWarnings(mask(specieslayerInStudyArea,
                                                   studyArea))
  speciesNames <- names(specieslayerInStudyArea)[which(maxValue(specieslayerInStudyArea)>=speciesPresence)]
  specieslayerBySpecies <- subset(specieslayerInStudyArea, speciesNames[1])
  specieslayerBySpecies[Which(is.na(specieslayerBySpecies) & specieslayerBySpecies<=5,
                              cells = TRUE)] <- 0 # 5% or less presence removed
  speciesComMap <- as.logical(specieslayerBySpecies)
  rm(specieslayerBySpecies)
  k <- 1
  for(species in speciesNames[2:length(speciesNames)]){
    specieslayerBySpecies <- subset(specieslayerInStudyArea, species)
    specieslayerBySpecies[Which(is.na(specieslayerBySpecies) & specieslayerBySpecies <=5,
                                cells = TRUE)] <- 0
    speciesMap <- as.logical(specieslayerBySpecies)
    speciesComMap <- speciesMap*(10^k)+speciesComMap
    k <- k+1
    rm(specieslayerBySpecies, speciesMap)
  }
  # set the non-forested area as NA
  speciesComMap[Which(speciesComMap==0, cells = TRUE, na.rm = FALSE)] <- NA
  initialCommunities <- data.table(mapcode=sort(unique(getValues(speciesComMap))))
  initialCommunities[,mapCodeStr:=as.character(mapcode)]
  initialCommunities[, NofStr:=nchar(mapCodeStr)]
  for(i in 1:(length(speciesNames)-1)){
    initialCommunities[NofStr==i, mapCodeFull:=paste(paste(rep("0",(length(speciesNames)-i)),
                                                           collapse = ""),
                                                     mapCodeStr,
                                                     sep = "")]
  }
  initialCommunities[NofStr==length(speciesNames), mapCodeFull:=mapCodeStr]
  output <- data.table(mapcode = numeric(), speciesPresence = character(),
                       species = character())
  for(i in 1:nrow(initialCommunities)){
    outputAdd <- data.table(mapcode = initialCommunities$mapcode[i],
                            speciesPresence = substring(initialCommunities$mapCodeFull[i],
                                                        seq(1, length(speciesNames), 1),
                                                        seq(1, length(speciesNames), 1)),
                            species = speciesNames[length(speciesNames):1])
    
    output <- rbind(output, outputAdd)
  }
  initialCommunities <- output[speciesPresence!="0",]
  initialCommunities[,newMapCode:=as.numeric(as.factor(mapcode))]
  mapcodeconnection <- unique(initialCommunities[,.(mapcode, newMapCode)], by = "mapcode")
  indexTable <- data.table(pixelIndex=1:ncell(speciesComMap),
                           mapcode=getValues(speciesComMap))
  indexTable <- indexTable[!is.na(mapcode),]
  indexTable <- setkey(indexTable, mapcode)[setkey(mapcodeconnection, mapcode),
                                            nomatch = 0]
  speciesComMap[indexTable$pixelIndex] <- indexTable$newMapCode
  initialCommunities[, ':='(mapcode = newMapCode, newMapCode = NULL, speciesPresence = NULL)]
  return(list(initialCommunityMap = speciesComMap,
              initialCommunity = initialCommunities))
}
ecoregionProducer <- function(studyAreaRaster,
                                 ecoregionMapFull,
                                 ecoregionName,
                                 ecoregionActiveStatus,
                                 studyArea) {
  # change the coordinate reference for all spatialpolygons
  ecoregionMapFull <- spTransform(ecoregionMapFull, crs(studyAreaRaster))
  studyArea <- spTransform(studyArea, crs(studyAreaRaster))
  ecoregionMapInStudy <- raster::intersect(ecoregionMapFull, studyArea)
  ecoregions <- ecoregionMapInStudy@data[,ecoregionName]
  ecoregionTable <- data.table(mapcode = numeric(),
                               ecoregion = character())
  mapcode <- 1
  for(ecoregion in ecoregions){
    singleecoMapPoly <- ecoregionMapInStudy[ecoregionMapInStudy@data[,ecoregionName]==ecoregion,]
    studyAreaRaster <- setValues(studyAreaRaster, mapcode)
    singleecoMapRaster <- crop(studyAreaRaster, singleecoMapPoly)
    singleecoMapRaster <- suppressWarnings(mask(singleecoMapRaster, singleecoMapPoly))
    if(length(unique(getValues(singleecoMapRaster)))==1){
      if(is.na(unique(getValues(singleecoMapRaster)))){
        ecoregionTable <- rbind(ecoregionTable, 
                                data.table(mapcode = NA,
                                           ecoregion = ecoregion)) 
      } else {
        ecoregionTable <- rbind(ecoregionTable, 
                                data.table(mapcode = mapcode,
                                           ecoregion = ecoregion))
      }
    } else {
      ecoregionTable <- rbind(ecoregionTable, 
                              data.table(mapcode = mapcode,
                                         ecoregion = ecoregion))
    }
    
    if(mapcode == 1){
      ecoregionMap <- singleecoMapRaster
    } else {
      ecoregionMap <- merge(ecoregionMap, singleecoMapRaster)
    }
    mapcode <- mapcode + 1
  }
  ecoregionActiveStatus[, ecoregion:=as.character(ecoregion)]
  ecoregionTable <- ecoregionTable[!is.na(mapcode),]
  ecoregionTable <- dplyr::left_join(ecoregionTable,
                                     ecoregionActiveStatus,
                                     by = "ecoregion") %>%
    data.table
  ecoregionTable[is.na(active), active:="no"]
  ecoregionTable <- ecoregionTable[,.(active, mapcode, ecoregion)]
  return(list(ecoregionMap = ecoregionMap,
              ecoregion = ecoregionTable))
}
nonActiveEcoregionProducer <- function(nonactiveRaster,
                      activeStatus,
                      ecoregionMap,
                      ecoregion,
                      initialCommunityMap,
                      initialCommunity) {
  browser()
  projection(nonactiveRaster) <- projection(ecoregionMap)
  nonactiveRasterSmall <- crop(nonactiveRaster, ecoregionMap)
  nonecomapcode <- activeStatus[active=="no",]$mapcode
  nonactiveRasterSmall[Which(nonactiveRasterSmall %in% nonecomapcode, cells = TRUE)] <- NA
  initialCommunityMap[Which(is.na(nonactiveRasterSmall), cells = TRUE)] <- NA
  ecoregionMap[Which(is.na(nonactiveRasterSmall), cells = TRUE)] <- NA
  initialCommunity <- initialCommunity[mapcode %in% sort(unique(getValues(initialCommunityMap))),]
  ecoregion <- ecoregion[mapcode %in% sort(unique(getValues(ecoregionMap))),]
  return(list(ecoregionMap = ecoregionMap,
              ecoregion = ecoregion,
              initialCommunityMap = initialCommunityMap,
              initialCommunity = initialCommunity))
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
landWebDataPrepEvent2 <- function(sim) {
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
  # if(!('defaultColor' %in% sim$userSuppliedObjNames)) {
  #  defaultColor <- 'red'
  # }
  # ! ----- EDIT BELOW ----- ! #
  dataPath <- file.path(modulePath(sim), "landWebDataPrep", "data")
  # checkTable <- data.table(downloadData(module = "landWebDataPrep", path = modulePath(sim)))
  checkTable <- data.table(checksums("landWebDataPrep", path = modulePath(sim)))
  checkContent_passed <- checkTable[result == "OK",]$expectedFile
  # study area should be provided by Dr. David Anderson
  # Dr. Steve Cumming will provide a temperary one
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
  sim$ecoDistrict <- raster::shapefile(file.path(dataPath, "ecodistricts.shp"))
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
  sim$ecoRegion <- raster::shapefile(file.path(dataPath, "ecoregions.shp"))
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
  sim$ecoZone <- raster::shapefile(file.path(dataPath, "ecozones.shp"))
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
  sim$biomassMap <- raster(file.path(dataPath, "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.tif"))
  
  
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
  sim$standAgeMap <- raster(file.path(dataPath, "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.tif"))
  
  # 3. species maps
  sim$specieslayers <- stack()
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
    sim$specieslayers <- stack(sim$specieslayers, speciesmap)
    names(sim$specieslayers)[i] <- indispecies
    i <- i+1
  }
  if(!("LCC2005_V1_4a.tif" %in% checkContent_passed)){
    unzip(file.path(dataPath, "LandCoverOfCanada2005_V1_4.zip"),
          exdir = dataPath)
  }
  sim$LCC05 <- raster(file.path(dataPath, "LCC2005_V1_4a.tif"))
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
