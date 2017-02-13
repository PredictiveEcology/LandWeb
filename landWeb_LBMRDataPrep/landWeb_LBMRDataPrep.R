
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "landWeb_LBMRDataPrep",
  description = "this is data preparation module for running LBMR module in landWeb project",
  keywords = c("LandWeb", "LBMR"),
  authors = c(person(c("Yong"), "Luo", email="yong.luo@canada.ca", role=c("aut", "cre"))),
  childModules = character(0),
  version = numeric_version("1.3.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "landWeb_LBMRDataPrep.Rmd"),
  reqdPkgs = list("data.table", "raster", "dplyr"),
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
                 sourceURL = NA)
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
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
                  desc = "define which the caching for spinup simulation should be used, default is TRUE")
    )
))

## event types
#   - type `init` is required for initialiazation

doEvent.landWeb_LBMRDataPrep = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)
    
    # do stuff for this event
    sim <- sim$landWeb_LBMRDataPrepInit(sim)
    
    # schedule future event(s)
    sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "landWeb_LBMRDataPrep", "plot")
    sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "landWeb_LBMRDataPrep", "save")
  } else if (eventType == "save") {
    sim <- sim$landWeb_LBMRDataPrepSave(sim)
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
landWeb_LBMRDataPrepInit <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  sim$studyArea <- spTransform(sim$studyArea, crs(sim$specieslayers))
  sim$ecoDistrict <- spTransform(sim$ecoDistrict, crs(sim$specieslayers))
  sim$ecoRegion <- spTransform(sim$ecoRegion, crs(sim$specieslayers))
  sim$ecoZone <- spTransform(sim$ecoZone, crs(sim$specieslayers))
  
  sim <- cachedFunctions(sim)
  
  initialCommFiles <- sim$initialCommunityProducerCached(speciesLayers = sim$specieslayers, 
                                                         speciesPresence = 50,
                                                         studyArea = sim$studyArea)
  ecoregionstatus <- data.table(active = "yes",
                                ecoregion = 1:1031)
  ecoregionFiles <- sim$ecoregionProducerCached(studyAreaRaster = initialCommFiles$initialCommunityMap,
                                                ecoregionMapFull = sim$ecoDistrict,
                                                ecoregionName = "ECODISTRIC",
                                                ecoregionActiveStatus = ecoregionstatus,
                                                studyArea = sim$studyArea)
  
  
  activeStatusTable <- data.table(active = c(rep("yes", 15), rep("no", 25)),
                                  mapcode = 1:40)  # this is based on description
  simulationMaps <- sim$nonActiveEcoregionProducerCached(nonactiveRaster = sim$LCC2005,
                                                         activeStatus = activeStatusTable,
                                                         ecoregionMap = ecoregionFiles$ecoregionMap,
                                                         ecoregion = ecoregionFiles$ecoregion,
                                                         initialCommunityMap = initialCommFiles$initialCommunityMap,
                                                         initialCommunity = initialCommFiles$initialCommunity)
  speciesEcoregionTable <- sim$obtainMaxBandANPPCached(speciesLayers = sim$specieslayers,
                                                       biomassLayer = sim$biomassMap,
                                                       SALayer = sim$standAgeMap,
                                                       ecoregionMap = simulationMaps$ecoregionMap)
  
  septable <- sim$obtainSEPCached(ecoregionMap = simulationMaps$ecoregionMap,
                                  speciesLayers = sim$specieslayers)
  names(septable) <- c("ecoregion", "species", "SEP")
  septable[, SEP:=round(SEP, 2)]
  # 
  # 
  speciesEcoregionTable[, species:=as.character(species)]
  septable[,species:=as.character(species)]
  speciesEcoregionTable <- left_join(speciesEcoregionTable, septable, by = c("ecoregion", "species")) %>%
    data.table
  speciesEcoregionTable[SEP==0, ':='(maxBiomass = 0, maxANPP = 0)]
  NON_NAdata <- speciesEcoregionTable[!is.na(maxBiomass),]
  NAdata <- speciesEcoregionTable[is.na(maxBiomass),]
  if(nrow(NAdata) > 1){
    # # replace NA values with ecoregion  value
    biomassFrombiggerMap <- sim$obtainMaxBandANPPFormBiggerEcoAreaCached(speciesLayers = sim$specieslayers,
                                                                         biomassLayer = sim$biomassMap,
                                                                         SALayer = sim$standAgeMap,
                                                                         ecoregionMap = simulationMaps$ecoregionMap,
                                                                         biggerEcoArea = sim$ecoRegion,
                                                                         biggerEcoAreaSource = "ecoRegion",
                                                                         NAData = NAdata)
    NON_NAdata <- rbind(NON_NAdata, biomassFrombiggerMap$addData[!is.na(maxBiomass), .(ecoregion, species, maxBiomass, maxANPP, SEP)])
    NAdata <- biomassFrombiggerMap$addData[is.na(maxBiomass),.(ecoregion, species, maxBiomass, maxANPP, SEP)]
  }
  if(nrow(NAdata) > 1){
    biomassFrombiggerMap <- sim$obtainMaxBandANPPFormBiggerEcoAreaCached(speciesLayers = sim$specieslayers,
                                                                         biomassLayer = sim$biomassMap,
                                                                         SALayer = sim$standAgeMap,
                                                                         ecoregionMap = simulationMaps$ecoregionMap,
                                                                         biggerEcoArea = sim$ecoZone,
                                                                         biggerEcoAreaSource = "ecoZone",
                                                                         NAData = NAdata)
    NON_NAdata <- rbind(NON_NAdata, biomassFrombiggerMap$addData[!is.na(maxBiomass), .(ecoregion, species, maxBiomass, maxANPP, SEP)])
    NAdata <- biomassFrombiggerMap$addData[is.na(maxBiomass),.(ecoregion, species, maxBiomass, maxANPP, SEP)]
  }
  NAdata[,':='(maxBiomass=0, maxANPP=0, SEP=0)]
  speciesEcoregion <- rbind(NON_NAdata,NAdata)
  setnames(speciesEcoregion, "ecoregion", "mapcode")
  speciesEcoregion <- setkey(speciesEcoregion,
                             mapcode)[setkey(simulationMaps$ecoregion, mapcode),
                                      nomatch = 0][,.(year = 0, ecoregion, species,
                                                      maxB = maxBiomass,
                                                      maxANPP, establishprob = SEP)]
  sim$speciesEcoregion <- speciesEcoregion
  sim$ecoregion <- simulationMaps$ecoregion
  sim$ecoregionMap <- simulationMaps$ecoregionMap
  sim$initialCommunitiesMap <- simulationMaps$initialCommunityMap
  
  # species traits inputs
  names(speciesTable) <- c("species", "Area", "longevity", "sexualmature", "shadetolerance", "firetolerance", 
                           "seeddistance_eff", "seeddistance_max", "resproutprob", "resproutage_min", 
                           "resproutage_max", "postfireregen", "leaflongevity", "wooddecayrate", 
                           "mortalityshape", "growthcurve", "leafLignin", "hardsoft")
  speciesTable[,':='(Area = NULL, hardsoft = NULL)]
  speciesTable$species1 <- as.character(substring(speciesTable$species, 1, 4))
  speciesTable$species2 <- as.character(substring(speciesTable$species, 6, nchar(speciesTable$species)))
  speciesTable[,':='(species = paste(as.character(substring(species1, 1, 1)), 
                                     tolower(as.character(substring(species1, 2, nchar(species1)))),
                                     "_",
                                     as.character(substring(species2, 1, 1)), 
                                     tolower(as.character(substring(species2, 2, nchar(species2)))),
                                     sep = ""))]
  speciesTable[species == "Pinu_Con.lat", species:="Pinu_Con"]
  
  
  speciesTable <- speciesTable[species %in% names(sim$specieslayers),][
    , ':='(species1 = NULL, species2 = NULL)] %>%
    unique(., by = "species")
  initialCommunities <- simulationMaps$initialCommunity[,.(mapcode, description = NA,
                                                           species)]
  set(initialCommunities, , paste("age", 1:15, sep = ""), NA)
  initialCommunities <- data.frame(initialCommunities)
  
  fn <- function(initialCommunities, speciesTable) {
    for(i in 1:nrow(initialCommunities)){
      agelength <- sample(1:15, 1)
      ages <- sort(sample(1:speciesTable[species == initialCommunities$species[i],]$longevity,
                          agelength))
      initialCommunities[i, 4:(agelength+3)] <- ages
    }
    data.table::data.table(initialCommunities)
  }

  sim$initialCommunities <- Cache(fn, initialCommunities, speciesTable)
  
  sim$species <- speciesTable
  sim$minRelativeB <- data.frame(ecoregion = sim$ecoregion[active == "yes",]$ecoregion, 
                                 X1 = 0.2, X2 = 0.4, X3 = 0.5, 
                                 X4 = 0.7, X5 = 0.9)
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

landWeb_LBMRDataPrepSave = function(sim) {
  saveFiles(sim)
  return(invisible(sim))
}


cachedFunctions <- function(sim) {
  # for slow functions, add cached versions. Then use sim$xxx() throughout module instead of xxx()
  if(sim$useCache) {
    sim$initialCommunityProducerCached <- function(...) {
      SpaDES::Cache(FUN = initialCommunityProducer, ...)
    }
    sim$ecoregionProducerCached <- function(...) {
      SpaDES::Cache(FUN = ecoregionProducer, ...)
    }
    sim$nonActiveEcoregionProducerCached <- function(...) {
      SpaDES::Cache(FUN = nonActiveEcoregionProducer, ...)
    }
    sim$obtainMaxBandANPPCached <- function(...){
      SpaDES::Cache(FUN = obtainMaxBandANPP, ...)
    }
    sim$obtainSEPCached <- function(...){
      SpaDES::Cache(FUN = obtainSEP, ...)
    }
    sim$obtainMaxBandANPPFormBiggerEcoAreaCached <- function(...){
      SpaDES::Cache(FUN = obtainMaxBandANPPFormBiggerEcoArea, ...)
    }
    sim$projectRasterCached <- function(...){
      SpaDES::Cache(FUN = raster::projectRaster, ...)
    }
  } else {
    # Step 3 - create a non-caching version in case caching is not desired
    #  sim$spinUp <- sim$spinUpRaw
    sim$initialCommunityProducerCached <- initialCommunityProducer
    sim$ecoregionProducerCached <- ecoregionProducer
    sim$nonActiveEcoregionProducerCached <- nonActiveEcoregionProducer
    sim$obtainMaxBandANPPCached <- obtainMaxBandANPP
    sim$obtainSEPCached <- obtainSEP
    sim$obtainMaxBandANPPFormBiggerEcoAreaCached <- obtainMaxBandANPPFormBiggerEcoArea
    sim$projectRasterCached <- raster::projectRaster
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

obtainMaxBandANPP <- function(speciesLayers,
                              biomassLayer,
                              SALayer,
                              ecoregionMap) {
  speciesinstudyarea <- crop(speciesLayers, ecoregionMap)
  speciesinstudyarea <- suppressWarnings(mask(speciesinstudyarea, ecoregionMap))
  biomassinstudyarea <- crop(biomassLayer, ecoregionMap)
  biomassinstudyarea <- suppressWarnings(mask(biomassinstudyarea, ecoregionMap))
  speciesTable <- data.table(biomass=getValues(biomassinstudyarea))
  SAinstudyarea <- crop(SALayer, ecoregionMap)
  SAinstudyarea <- suppressWarnings(mask(SAinstudyarea, ecoregionMap))
  
  speciesTable[,':='(SA = getValues(SAinstudyarea), ecoregion = getValues(ecoregionMap))]
  outputPartial <- data.table(ecoregion = numeric(), species = character(),
                              maxBiomass = numeric(), maxANPP = numeric())
  speciess <- names(speciesLayers)
  for(species in speciess){
    indispeciesraster <- subset(speciesinstudyarea, species)
    speciesTable[, percentage:=getValues(indispeciesraster)]
    speciesTable_narmed <- speciesTable[!is.na(ecoregion),]
    speciesTable_narmed[, speciesBiomass:=biomass*percentage*0.01]
    speciesTable_narmed <- speciesTable_narmed[percentage>=50,]
    speciesTable_narmed[,species:=species]
    speciesTable_narmed <- speciesTable_narmed[,.(maxBiomass = 100*quantile(speciesBiomass, 0.8, na.rm = TRUE)),
                                               by = c("ecoregion", "species")]
    speciesTable_narmed[, maxANPP:=maxBiomass/30]
    outputPartial <- rbind(outputPartial, speciesTable_narmed)
  }
  output <- data.table(expand.grid(ecoregion = as.numeric(unique(getValues(ecoregionMap))),
                                   species = speciess))[!is.na(ecoregion),][,species:=as.character(species)]
  output <- dplyr::left_join(output, outputPartial, by = c("ecoregion", "species")) %>%
    data.table
  return(speciesBiomass = output)
}

obtainSEP <- function(speciesLayers,
                      ecoregionMap) {
  speciesLayerEcoregion <- crop(speciesLayers, ecoregionMap)
  speciesLayerEcoregion <- suppressWarnings(mask(speciesLayerEcoregion, ecoregionMap))
  speciesTableEcoregion <- cbind(data.table(ecoregion = getValues(ecoregionMap)), 
                                 data.table(getValues(speciesLayerEcoregion)))
  speciesTableEcoregion <- speciesTableEcoregion[complete.cases(speciesTableEcoregion)]
  speciesTableEcoregion <- melt.data.table(speciesTableEcoregion,
                                           measure.vars = names(speciesTableEcoregion)[-1],
                                           variable.name = "species") 
  speciesTableEcoregion[value < 10, presence := 0]
  speciesTableEcoregion[value >= 10, presence := 1]
  speciesTableEcoregion <- speciesTableEcoregion[,.(relativeAbundance = sum(presence)/length(value)),
                                                 by = c("ecoregion", "species")]
  speciesLevels <- unique(speciesTableEcoregion$species)
  abundanceMapStack <- stack()
  names(ecoregionMap) <- "ecoregion"
  for(i in 1:length(speciesLevels)){
    speciesTableEcoregionBySpecies <- speciesTableEcoregion[species == speciesLevels[i],][
      , species:=NULL]
    abundanceMap <- rasterizeReduced(speciesTableEcoregionBySpecies, ecoregionMap, "relativeAbundance")
    names(abundanceMap) <- as.character(speciesLevels[i])
    abundanceMapStack <- stack(abundanceMapStack, abundanceMap)
  }
  return(speciesAbundanceTable = speciesTableEcoregion)
}

obtainMaxBandANPPFormBiggerEcoArea = function(speciesLayers,
                                              biomassLayer,
                                              SALayer,
                                              ecoregionMap,
                                              biggerEcoArea,
                                              biggerEcoAreaSource,
                                              NAData) {
  subEcoregion <- ecoregionMap
  subEcoregion[!(getValues(subEcoregion) %in% unique(NAData$ecoregion))] <- NA
  subbiggerEcoMap <- raster::crop(biggerEcoArea, subEcoregion)
  if(biggerEcoAreaSource == "ecoRegion"){
    subbiggerEcoLevel <- unique(subbiggerEcoMap@data$ECOREGION)
    subbigEcoMap <- biggerEcoArea[biggerEcoArea@data$ECOREGION %in% subbiggerEcoLevel,]
  } else if (biggerEcoAreaSource == "ecoZone"){
    subbiggerEcoLevel <- unique(subbiggerEcoMap@data$ECOZONE)
    subbigEcoMap <- biggerEcoArea[biggerEcoArea@data$ECOZONE %in% subbiggerEcoLevel,]
  }
  subbiggerEcoMap_Raster <- crop(biomassLayer, subbigEcoMap)
  subbiggerEcoMap_Raster <- setValues(subbiggerEcoMap_Raster, NA)
  for(indiEcoregion in subbiggerEcoLevel){
    if(biggerEcoAreaSource == "ecoRegion"){
      indiSubBiggerEcoMap <- subbigEcoMap[subbigEcoMap@data$ECOREGION == indiEcoregion,]
    } else if (biggerEcoAreaSource == "ecoZone"){
      indiSubBiggerEcoMap <- subbigEcoMap[subbigEcoMap@data$ECOZONE == indiEcoregion,]
    }
    indiEcoMapRaster <- setValues(subbiggerEcoMap_Raster, indiEcoregion) 
    indiEcoMapRaster <- crop(indiEcoMapRaster, indiSubBiggerEcoMap)
    indiEcoMapRaster <- suppressWarnings(mask(indiEcoMapRaster, indiSubBiggerEcoMap))
    if(indiEcoregion == subbiggerEcoLevel[1]){
      biggerEcoMapRaster <- indiEcoMapRaster
    } else {
      biggerEcoMapRaster <- merge(biggerEcoMapRaster, indiEcoMapRaster)
    }
  }
  biggerEcoMapRaster_ST <- crop(biggerEcoMapRaster, subEcoregion)
  biggerEcoMapRaster_ST <- suppressWarnings(mask(biggerEcoMapRaster_ST, subEcoregion))
  ecodistrictEcoregionTable <- data.table(ecoregion = getValues(subEcoregion),
                                          biggerEcoregion = getValues(biggerEcoMapRaster_ST))[!is.na(ecoregion),]
  #check whether one district has more than one ecoregion, which is not correct
  ecodistrictEcoregionTable[,totLength:=length(biggerEcoregion), by = ecoregion]
  ecodistrictEcoregionTable[,ecoLength:=length(totLength), by = c("biggerEcoregion", "ecoregion")]
  ecodistrictEcoregionTable[, percentage:=ecoLength/totLength]
  ecodistrictEcoregionTable[, maxPercent:=max(percentage), by = ecoregion]
  ecodistrictEcoregionTable <- ecodistrictEcoregionTable[percentage == maxPercent, .(biggerEcoregion, ecoregion)] %>%
    unique(., by = c("biggerEcoregion", "ecoregion"))
  ecoregionBiomass <- obtainMaxBandANPP(speciesLayers = speciesLayers,
                                        biomassLayer = biomassLayer,
                                        SALayer = SALayer,
                                        ecoregionMap = biggerEcoMapRaster)
  setnames(ecoregionBiomass, "ecoregion", "biggerEcoregion")
  NAData <- setkey(NAData, ecoregion)[setkey(ecodistrictEcoregionTable, ecoregion), nomatch = 0]
  NAData[,species:=as.character(species)]
  NAData <- dplyr::left_join(NAData[,.(biggerEcoregion, ecoregion, species, SEP)], ecoregionBiomass,
                             by = c("biggerEcoregion", "species")) %>%
    data.table
  return(list(addData = NAData, biggerEcoMapRaster = biggerEcoMapRaster))
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
  dataPath <- file.path(modulePath(sim), "landWeb_LBMRDataPrep", "data")
  fileNames <- c("ecodistricts.dbf", "ecodistricts.prj", "ecodistricts.sbn", "ecodistricts.sbx", 
                 "ecodistricts.shp", "ecodistricts.shx", "ecoregions.dbf", "ecoregions.prj", 
                 "ecoregions.sbn", "ecoregions.sbx", "ecoregions.shp", "ecoregions.shx", "ecozones.dbf", 
                 "ecozones.prj", "ecozones.sbn", "ecozones.sbx", "ecozones.shp", "ecozones.shx", 
                 "LCC2005_V1_4a.tif", "NFI_MODIS250m_kNN_Species_Abie_Bal_v0.tif", 
                 "NFI_MODIS250m_kNN_Species_Abie_Bal_v0.tif.aux.xml", "NFI_MODIS250m_kNN_Species_Abie_Bal_v0.tif.xml", 
                 "NFI_MODIS250m_kNN_Species_Abie_Las_v0.tif", "NFI_MODIS250m_kNN_Species_Abie_Las_v0.tif.aux.xml", 
                 "NFI_MODIS250m_kNN_Species_Abie_Las_v0.tif.xml", "NFI_MODIS250m_kNN_Species_Betu_Pap_v0.tif", 
                 "NFI_MODIS250m_kNN_Species_Betu_Pap_v0.tif.aux.xml", "NFI_MODIS250m_kNN_Species_Betu_Pap_v0.tif.xml", 
                 "NFI_MODIS250m_kNN_Species_Pice_Gla_v0.tif", "NFI_MODIS250m_kNN_Species_Pice_Gla_v0.tif.aux.xml", 
                 "NFI_MODIS250m_kNN_Species_Pice_Gla_v0.tif.xml", "NFI_MODIS250m_kNN_Species_Pice_Mar_v0.tif", 
                 "NFI_MODIS250m_kNN_Species_Pice_Mar_v0.tif.aux.xml", "NFI_MODIS250m_kNN_Species_Pice_Mar_v0.tif.xml", 
                 "NFI_MODIS250m_kNN_Species_Pinu_Ban_v0.tif", "NFI_MODIS250m_kNN_Species_Pinu_Ban_v0.tif.aux.xml", 
                 "NFI_MODIS250m_kNN_Species_Pinu_Ban_v0.tif.xml", "NFI_MODIS250m_kNN_Species_Pinu_Con_v0.tif", 
                 "NFI_MODIS250m_kNN_Species_Pinu_Con_v0.tif.aux.xml", "NFI_MODIS250m_kNN_Species_Pinu_Con_v0.tif.xml", 
                 "NFI_MODIS250m_kNN_Species_Pinu_Str_v0.tif", "NFI_MODIS250m_kNN_Species_Pinu_Str_v0.tif.aux.xml", 
                 "NFI_MODIS250m_kNN_Species_Pinu_Str_v0.tif.xml", "NFI_MODIS250m_kNN_Species_Popu_Tre_v0.tif", 
                 "NFI_MODIS250m_kNN_Species_Popu_Tre_v0.tif.aux.xml", "NFI_MODIS250m_kNN_Species_Popu_Tre_v0.tif.xml", 
                 "NFI_MODIS250m_kNN_Species_Pseu_Men_v0.tif", "NFI_MODIS250m_kNN_Species_Pseu_Men_v0.tif.aux.xml", 
                 "NFI_MODIS250m_kNN_Species_Pseu_Men_v0.tif.xml", "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.tif", 
                 "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.tif.aux.xml", 
                 "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.tif.xml", 
                 "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.tif", "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.tif.aux.xml", 
                 "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.tif.xml", 
                 "speciesTraits.csv")
  fileNames <- lapply(fileNames, function(x){file.path(dataPath, x)})
  allFiles <- lapply(fileNames, function(x) {
    file.info(x)[,"size"]}
    )
  names(allFiles) <- unlist(lapply(fileNames, basename))
  needDownload <- all(digest::digest(allFiles) != c("9a99479fea036a03f188f71cbabca49e",
                                                "05a98a7eab2fcd0ebef7cc21fbfdf75b"))
  if(needDownload) {
    checkTable <- data.table(downloadData(module = "landWeb_LBMRDataPrep", 
                                          path = modulePath(sim)))
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
  } else {
    message("  Download data step skipped for module landWeb_LBMRDataPrep. Local copy exists")
  }
  sim$ecoDistrict <- Cache(raster::shapefile, file.path(dataPath, "ecodistricts.shp"))
  sim$ecoRegion <- Cache(raster::shapefile, file.path(dataPath, "ecoregions.shp"))
  sim$ecoZone <- Cache(raster::shapefile, file.path(dataPath, "ecozones.shp"))
  sim$biomassMap <- raster(file.path(dataPath, "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.tif"))
  sim$standAgeMap <- raster(file.path(dataPath, "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.tif"))
  
  
  # 3. species maps
  sim$specieslayers <- stack()
  speciesnames <- c("Abie_Las",
                    "Pice_Gla", "Pice_Mar", 
                    "Pinu_Ban", "Pinu_Con", 
                    "Popu_Tre", "Pseu_Men")
  i <- 1
  for(indispecies in speciesnames){
    if(needDownload) {
      
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
    }
    speciesmap <- raster(file.path(dataPath, paste("NFI_MODIS250m_kNN_Species_", indispecies,
                                                   "_v0.tif", sep = "")))
    sim$specieslayers <- stack(sim$specieslayers, speciesmap)
    names(sim$specieslayers)[i] <- indispecies
    i <- i+1
  }
  
  if(needDownload) {
    
    if(!("LCC2005_V1_4a.tif" %in% checkContent_passed)){
      unzip(file.path(dataPath, "LandCoverOfCanada2005_V1_4.zip"),
            exdir = dataPath)
    }
  }
  sim$LCC2005 <- raster(file.path(dataPath, "LCC2005_V1_4a.tif"))
  # projection(sim$LCC2005) <- projection(sim$specieslayers)
  sim$speciesTable <- read.csv(file.path(dataPath, "speciesTraits.csv"), header = TRUE,
                               stringsAsFactors = FALSE) %>%
    data.table
  
  
  sim$sufficientLight <- data.frame(speciesshadetolerance = 1:5,
                                    X0 = 1, X1 = c(0.5, rep(1, 4)), X2 = c(0, 0.5, rep(1, 3)),
                                    X3 = c(rep(0, 2), 0.5, rep(1, 2)), 
                                    X4 = c(rep(0, 3), 0.5, 1), X5 = c(rep(0, 4), 1))
  
  sim$seedingAlgorithm <- "wardDispersal"
  sim$spinupMortalityfraction <- 0.002
  sim$cellSize <- 250
  sim$successionTimestep <- 10
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
