
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "Boreal_LBMRDataPrep",
  description = "A data preparation module for running the LBMR module in the LandWeb project",
  keywords = c("LandWeb", "LBMR"),
  authors = c(person(c("Yong", "Luo"), email="yong.luo@canada.ca", role=c("aut", "cre")),
              person(c("Eliot", "J", "B"), "McIntire", email="eliot.mcintire@canada.ca", role=c("aut"))),
  childModules = character(0),
  version = numeric_version("1.3.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "Boreal_LBMRDataPrep.Rmd"),
  reqdPkgs = list("data.table", "raster", "rgeos", "dplyr", "amc", "gdalUtils"),
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
    expectsInput(objectName = "shpStudyRegionFull", objectClass = "SpatialPolygonsDataFrame",
                 desc = "this shape file contains two informaton: Full study areawith fire return interval attribute",
                 sourceURL = ""), # i guess this is study area and fire return interval
    expectsInput(objectName = "rstStudyRegion", objectClass = "RasterLayer",
                 desc = "this raster contains two pieces of informaton: Full study area with fire return interval attribute",
                 sourceURL = ""), # i guess this is study area and fire return interval
    expectsInput(objectName = "studyArea", objectClass = "SpatialPolygons",
                 desc = "study area",
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

doEvent.Boreal_LBMRDataPrep = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)
    
    # do stuff for this event
    sim <- estimateParameters(sim)
    
    # schedule future event(s)
    sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "Boreal_LBMRDataPrep", "plot")
    sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "Boreal_LBMRDataPrep", "save")
  } else if (eventType == "save") {
    sim <- Save(sim)
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
estimateParameters <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  
  sim$studyArea <- spTransform(sim$studyArea, crs(sim$specieslayers))
  sim$ecoDistrict <- spTransform(sim$ecoDistrict, crs(sim$specieslayers))
  sim$ecoRegion <- spTransform(sim$ecoRegion, crs(sim$specieslayers))
  sim$ecoZone <- spTransform(sim$ecoZone, crs(sim$specieslayers))
  
  message("1: ", Sys.time())
  rstStudyRegionBinary <- raster(sim$rstStudyRegion)
  rstStudyRegionBinary[] <- NA
  rstStudyRegionBinary[!is.na(sim$rstStudyRegion[])] <- 1
  
  message("2: ", Sys.time())
  initialCommFiles <- Cache(initialCommunityProducer, speciesLayers = sim$specieslayers,
                            speciesPresence = 50, 
                            studyArea = sim$studyArea,
                            rstStudyArea = rstStudyRegionBinary, 
                            digestPathContent = .quickCheck,
                            userTags = "stable")
  ecoregionstatus <- data.table(active = "yes",
                                ecoregion = 1:1031)
  message("ecoregionProducer: ", Sys.time())
  #ecoregionFiles <- sim$ecoregionProducerCached(studyAreaRaster = initialCommFiles$initialCommunityMap,
  ecoregionFiles <- Cache(ecoregionProducer, studyAreaRaster = initialCommFiles$initialCommunityMap,
                          ecoregionMapFull = sim$ecoDistrict,
                          ecoregionName = "ECODISTRIC",
                          ecoregionActiveStatus = ecoregionstatus,
                          studyArea = sim$studyArea,
                          rstStudyArea = rstStudyRegionBinary,
                          maskFn = fastMask,
                          digestPathContent = .quickCheck,
                          userTags = "stable")
  
  message("3: ", Sys.time())
  # LCC05 -- land covers 1 to 15 are forested with tree dominated... 34 and 35 are recent burns
  activeStatusTable <- data.table(active = c(rep("yes", 15), rep("no", 25)),
                                  mapcode = 1:40)[mapcode %in% c(34, 35), active:="yes"]  # this is based on description in LCC05 
  #simulationMaps <- sim$nonActiveEcoregionProducerCached(nonactiveRaster = sim$LCC2005,
  simulationMaps <- Cache(nonActiveEcoregionProducer, nonactiveRaster = sim$LCC2005,
                          activeStatus = activeStatusTable,
                          ecoregionMap = ecoregionFiles$ecoregionMap,
                          ecoregion = ecoregionFiles$ecoregion,
                          initialCommunityMap = initialCommFiles$initialCommunityMap,
                          initialCommunity = initialCommFiles$initialCommunity,
                          digestPathContent = .quickCheck,
                          userTags = "stable")
  message("4: ", Sys.time())
  #speciesEcoregionTable <- sim$obtainMaxBandANPPCached(speciesLayers = sim$specieslayers,
  speciesEcoregionTable <- Cache(obtainMaxBandANPP, speciesLayers = sim$specieslayers,
                                 biomassLayer = sim$biomassMap,
                                 SALayer = sim$standAgeMap,
                                 ecoregionMap = simulationMaps$ecoregionMap,
                                 digestPathContent = .quickCheck,
                                 userTags = "stable")
  
  message("5: ", Sys.time())
  #septable <- sim$obtainSEPCached(ecoregionMap = simulationMaps$ecoregionMap,
  septable <- Cache(obtainSEP, ecoregionMap = simulationMaps$ecoregionMap,
                    speciesLayers = sim$specieslayers,
                    digestPathContent = .quickCheck,
                    userTags = "stable")
  names(septable) <- c("ecoregion", "species", "SEP")
  septable[, SEP:=round(SEP, 2)]
  #
  #
  message("6: ", Sys.time())
  speciesEcoregionTable[, species:=as.character(species)]
  septable[,species:=as.character(species)]
  speciesEcoregionTable <- left_join(speciesEcoregionTable, septable, by = c("ecoregion", "species")) %>%
    data.table
  speciesEcoregionTable[SEP==0, ':='(maxBiomass = 0, maxANPP = 0)]
  NON_NAdata <- speciesEcoregionTable[!is.na(maxBiomass),]
  NAdata <- speciesEcoregionTable[is.na(maxBiomass),]
  
  if(nrow(NAdata) > 1){
    # # replace NA values with ecoregion  value
    #biomassFrombiggerMap <- sim$obtainMaxBandANPPFormBiggerEcoAreaCached(speciesLayers = sim$specieslayers,
    
    
    message("  6a obtainMaxBandANPPFormBiggerEcoArea: ", Sys.time())
    biomassFrombiggerMap <- Cache(obtainMaxBandANPPFormBiggerEcoArea, 
                                  speciesLayers = sim$specieslayers,
                                  biomassLayer = sim$biomassMap,
                                  SALayer = sim$standAgeMap,
                                  ecoregionMap = simulationMaps$ecoregionMap,
                                  biggerEcoArea = sim$ecoRegion,
                                  biggerEcoAreaSource = "ecoRegion",
                                  NAData = NAdata,
                                  digestPathContent = .quickCheck,
                                  maskFn = fastMask,
                                  userTags = "stable")
    message("  6b obtainMaxBandANPPFormBiggerEcoArea: ", Sys.time())
    NON_NAdata <- rbind(NON_NAdata, biomassFrombiggerMap$addData[!is.na(maxBiomass), .(ecoregion, species, maxBiomass, maxANPP, SEP)])
    NAdata <- biomassFrombiggerMap$addData[is.na(maxBiomass),.(ecoregion, species, maxBiomass, maxANPP, SEP)]
  }
  message("7: ", Sys.time())
  if(nrow(NAdata) > 1){
    #biomassFrombiggerMap <- sim$obtainMaxBandANPPFormBiggerEcoAreaCached(speciesLayers = sim$specieslayers,
    message("  7a obtainMaxBandANPPFormBiggerEcoArea if NAdata exist: ", Sys.time())
    biomassFrombiggerMap <- Cache(obtainMaxBandANPPFormBiggerEcoArea, 
                                  speciesLayers = sim$specieslayers, biomassLayer = sim$biomassMap,
                                  SALayer = sim$standAgeMap, ecoregionMap = simulationMaps$ecoregionMap,
                                  biggerEcoArea = sim$ecoZone, biggerEcoAreaSource = "ecoZone",
                                  NAData = NAdata, maskFn = fastMask,
                                  digestPathContent = .quickCheck,
                                  userTags = "stable")
    message("  7b obtainMaxBandANPPFormBiggerEcoArea if NAdata exist: ", Sys.time())
    NON_NAdata <- rbind(NON_NAdata, biomassFrombiggerMap$addData[!is.na(maxBiomass), .(ecoregion, species, maxBiomass, maxANPP, SEP)])
    NAdata <- biomassFrombiggerMap$addData[is.na(maxBiomass),.(ecoregion, species, maxBiomass, maxANPP, SEP)]
  }
  message("8: ", Sys.time())
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
  
  sim$initialCommunitiesMap <- Cache(createInitCommMap, simulationMaps$initialCommunityMap,
                                     as.integer(simulationMaps$initialCommunityMap[]),
                                     file.path(outputPath(sim), "initialCommunitiesMap.tif"),
                                     digestPathContent = .quickCheck,
                                     userTags = "stable")
  
  message("9: ", Sys.time())
  
  # species traits inputs
  speciesTable <- sim$speciesTable
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
  
  newNames <- toSentenceCase(speciesTable$species)
  
  speciesTable$species <- newNames
  
  speciesTable[species %in% c("Abie_las", "Abie_bal"), species:="Abie_sp"]
  speciesTable[species %in% c("Pinu_ban", "Pinu_con", "Pinu_con.con"), species:="Pinu_sp"]
  
  message("10: ", Sys.time())
  
  # Take the smallest values of every column, within species, because it is northern boreal forest
  speciesTable <- speciesTable[species %in% names(sim$specieslayers),][
    , ':='(species1 = NULL, species2 = NULL)] %>%
    .[,lapply(.SD, function(x) if(is.numeric(x)) min(x, na.rm=TRUE) else x[1]), by = "species"]
  
  initialCommunities <- simulationMaps$initialCommunity[,.(mapcode, description = NA,
                                                           species)]
  set(initialCommunities, , paste("age", 1:15, sep = ""), NA)
  initialCommunities <- data.frame(initialCommunities)
  message("11: ", Sys.time())
  
  fn <- function(initialCommunities, speciesTable) {
    for(i in 1:nrow(initialCommunities)){
      agelength <- sample(1:15, 1)
      ages <- sort(sample(1:speciesTable[species == initialCommunities$species[i],]$longevity,
                          agelength))
      initialCommunities[i, 4:(agelength+3)] <- ages
    }
    data.table::data.table(initialCommunities)
  }
  message("12: ", Sys.time())
  
  sim$initialCommunities <- Cache(fn, initialCommunities, speciesTable,
                                  userTags = "stable")
  
  sim$species <- speciesTable
  sim$minRelativeB <- data.frame(ecoregion = sim$ecoregion[active == "yes",]$ecoregion,
                                 X1 = 0.2, X2 = 0.4, X3 = 0.5,
                                 X4 = 0.7, X5 = 0.9)
  message("Done Boreal_LBMRDataPrep: ", Sys.time())
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

Save = function(sim) {
  saveFiles(sim)
  return(invisible(sim))
}



initialCommunityProducer <- function(speciesLayers, speciesPresence, studyArea, rstStudyArea) {
  specieslayerInStudyArea <- crop(speciesLayers,
                                  studyArea)
  #if(isTRUE(tryCatch(getCluster(), error=function(x) TRUE, silent=TRUE))) beginCluster()
  specieslayerInStudyArea <- specieslayerInStudyArea*rstStudyArea
  names(specieslayerInStudyArea) <- names(speciesLayers)
  
  #specieslayerInStudyArea <- specieslayerInStudyArea*(!is.na(rstStudyArea))
  # specieslayerInStudyArea <- suppressWarnings(fastMask(specieslayerInStudyArea,
  #                                                  studyArea))
  speciesNames <- names(specieslayerInStudyArea)[which(maxValue(specieslayerInStudyArea)>=speciesPresence)]
  specieslayerBySpecies <- subset(specieslayerInStudyArea, speciesNames[1])
  specieslayerBySpecies[which(is.na(specieslayerBySpecies[]) & specieslayerBySpecies[]<=5)] <- 0
  # specieslayerBySpecies[Which(is.na(specieslayerBySpecies) & specieslayerBySpecies<=5,
  #                             cells = TRUE)] <- 0 # 5% or less presence removed
  speciesComMap <- as.logical(specieslayerBySpecies)
  rm(specieslayerBySpecies)
  k <- 1
  for(species in speciesNames[2:length(speciesNames)]){
    specieslayerBySpecies <- subset(specieslayerInStudyArea, species)
    specieslayerBySpecies[which(is.na(specieslayerBySpecies[]) & specieslayerBySpecies[]<=5)] <- 0
    # specieslayerBySpecies[Which(is.na(specieslayerBySpecies) & specieslayerBySpecies <=5,
    #                             cells = TRUE)] <- 0
    speciesMap <- as.logical(specieslayerBySpecies)
    speciesComMap <- speciesMap*(10^k)+speciesComMap
    k <- k+1
    rm(specieslayerBySpecies, speciesMap)
  }
  # set the non-forested area as NA
  #speciesComMap1 <- speciesComMap
  #speciesComMap[Which(speciesComMap==0, cells = TRUE, na.rm = FALSE)] <- NA
  speciesComMap[which(speciesComMap[]==0)] <- NA
  
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
                              studyArea,
                              rstStudyArea, maskFn) {
  # change the coordinate reference for all spatialpolygons
  message("ecoregionProducer 1: ", Sys.time())
  ecoregionMapInStudy <- raster::intersect(ecoregionMapFull, aggregate(studyArea))
  # ecoregions <- ecoregionMapInStudy@data[,ecoregionName]
  # ecoregionTable <- data.table(mapcode = numeric(),
  #                              ecoregion = character())
  # mapcode <- 1
  # for(ecoregion in unique(ecoregions)){
  # #  for(ecoregion in ecoregions){
  #     singleecoMapPoly <- ecoregionMapInStudy[ecoregionMapInStudy@data[,ecoregionName]==ecoregion,]
  #   studyAreaRaster <- setValues(studyAreaRaster, mapcode)
  #   singleecoMapRaster <- crop(studyAreaRaster, singleecoMapPoly)
  #   singleecoMapRaster <- suppressWarnings(maskFn(singleecoMapRaster, singleecoMapPoly))
  #   if(length(unique(getValues(singleecoMapRaster)))==1){
  #     if(is.na(unique(getValues(singleecoMapRaster)))){
  #       ecoregionTable <- rbind(ecoregionTable,
  #                               data.table(mapcode = NA,
  #                                          ecoregion = ecoregion))
  #     } else {
  #       ecoregionTable <- rbind(ecoregionTable,
  #                               data.table(mapcode = mapcode,
  #                                          ecoregion = ecoregion))
  #     }
  #   } else {
  #     ecoregionTable <- rbind(ecoregionTable,
  #                             data.table(mapcode = mapcode,
  #                                        ecoregion = ecoregion))
  #   }
  #
  #   if(mapcode == 1){
  #     ecoregionMap <- singleecoMapRaster
  #   } else {
  #     ecoregionMap <- merge(ecoregionMap, singleecoMapRaster)
  #   }
  #   mapcode <- mapcode + 1
  # }
  
  # Alternative
  message("ecoregionProducer fastRasterize: ", Sys.time())
  ecoregionMap <- fastRasterize(ecoregionMapInStudy, studyAreaRaster, field = "ECODISTRIC")
  
  #ecoregionMap1 <- rasterize(ecoregionMapInStudy, studyAreaRaster, field = "ECODISTRIC")
  ecoregionFactorValues <- unique(ecoregionMap[])
  
  ecoregionTable <- data.table(mapcode = seq_along(ecoregionFactorValues[!is.na(ecoregionFactorValues)]),
                               ecoregion = as.numeric(ecoregionFactorValues[!is.na(ecoregionFactorValues)]))
  message("ecoregionProducer mapvalues: ", Sys.time())
  ecoregionMap[] <- plyr::mapvalues(ecoregionMap[], from = ecoregionTable$ecoregion, to = ecoregionTable$mapcode)
  ecoregionActiveStatus[, ecoregion:=as.character(ecoregion)]
  ecoregionTable <- ecoregionTable[!is.na(mapcode),][, ecoregion := as.character(ecoregion)]
  message("ecoregionProducer dplyr_leftjoin: ", Sys.time())
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
  whNANonActiveRasterSmall <- which(nonactiveRasterSmall[] %in% nonecomapcode)
  initialCommunityMap[whNANonActiveRasterSmall] <- NA
  ecoregionMap[whNANonActiveRasterSmall] <- NA
  
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
  speciesinstudyarea <- crop(speciesLayers, ecoregionMap) # slowest 
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
                                              NAData,
                                              maskFn) {
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
  
  
  # don't need to Cache because it whole function is cached
  biggerEcoMapRaster <- fastRasterize(polygon = subbigEcoMap, ras = subbiggerEcoMap_Raster,
                                      field=toupper(biggerEcoAreaSource))#, filename="biggerEcoMapRaster")
  
  # biggerEcoMapRaster <- Cache(fastRasterizeFn, polygon = subbigEcoMap, ras = subbiggerEcoMap_Raster,
  #                             field=toupper(biggerEcoAreaSource))
  
  
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
  # don't need to Cache because whole outer function is cached
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
  dataPath <- file.path(modulePath(sim), "Boreal_LBMRDataPrep", "data")
  
  # 1. test if all input objects are already present (e.g., from inputs, objects or another module)
  a <- depends(sim)
  whThisMod <- which(unlist(lapply(a@dependencies, function(x) x@name))=="Boreal_LBMRDataPrep")
  objNames <- a@dependencies[[whThisMod]]@inputObjects$objectName
  objExists <- !unlist(lapply(objNames, 
         function(x) is.null(sim[[x]])))
  names(objExists) <- objNames
  needDownloads <- !all(objExists) | 
    !all(objNames %in% sim$.userSuppliedObjNames)
  
  # LCC2005 may be loaded by other modules
  lcc2005Filename <- file.path(dataPath, "LCC2005_V1_4a.tif")
  #if(!is.null(sim$LCC2005)) lcc2005Filename <- filename(sim$LCC2005)
  
  if(needDownloads) {
    dd <- downloadData(module = "Boreal_LBMRDataPrep",
                       path = modulePath(sim), quickCheck = .quickCheck)
    checkTable <- data.table(dd)
    checkContent_passed <- checkTable[result == "OK",]$expectedFile
    # study area should be provided by Dr. David Anderson
    # Dr. Steve Cumming will provide a temperary one
    if(is.null(sim$ecoDistrict)) {
      if(length(grep("ecodistricts\\..{3}", checkContent_passed))!=4) {
        unzip(zipfile = file.path(dataPath, "ecodistrict_shp.zip"),
              exdir = dataPath)
        filenames <- dir(file.path(dataPath, "Ecodistricts"))
        file.copy(from = file.path(dataPath, "Ecodistricts", filenames),
                  to = file.path(dataPath, filenames),
                  overwrite = TRUE)
        unlink(file.path(dataPath, "Ecodistricts"), recursive = TRUE)
        rm(filenames)
      }
      ecodistrictFilename <-   file.path(dataPath, "ecodistricts.shp")
      sim$ecoDistrict <- Cache(raster::shapefile, asPath(ecodistrictFilename), 
                               digestPathContent = .quickCheck,
                               userTags = "stable")
    }
    
    
    if(is.null(sim$ecoRegion)) {
      if(length(grep("ecoregions\\..{3}", checkContent_passed))!=4) {
        unzip(zipfile = file.path(dataPath, "ecoregion_shp.zip"),
              exdir = dataPath)
        filenames <- dir(file.path(dataPath, "Ecoregions"))
        file.copy(from = file.path(dataPath, "Ecoregions", filenames),
                  to = file.path(dataPath, filenames),
                  overwrite = TRUE)
        unlink(file.path(dataPath, "Ecoregions"), recursive = TRUE)
        rm(filenames)
      }
      sim$ecoregionFilename <-   file.path(dataPath, "ecoregions.shp")
      sim$ecoRegion <- Cache(raster::shapefile, asPath(ecoregionFilename),
                             digestPathContent = .quickCheck,
                             userTags = "stable")
    }
    
    if(is.null(sim$ecoZone)) {
      if(length(grep("ecozones\\..{3}", checkContent_passed))!=4) {
        unzip(zipfile = file.path(dataPath, "ecozone_shp.zip"),
              exdir = dataPath)
        filenames <- dir(file.path(dataPath, "Ecozones"))
        file.copy(from = file.path(dataPath, "Ecozones", filenames),
                  to = file.path(dataPath, filenames),
                  overwrite = TRUE)
        unlink(file.path(dataPath, "Ecozones"), recursive = TRUE)
      }
      ecozoneFilename <-   file.path(dataPath, "ecozones.shp")
      sim$ecoZone <- Cache(raster::shapefile, asPath(ecozoneFilename),
                           digestPathContent = .quickCheck,
                           userTags = "stable")
    }
    
    biomassMapFilename <- file.path(dataPath, "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.tif")
    if(is.null(sim$biomassMap)) {
      if(length(grep("Structure_Biomass.*tif", checkContent_passed))!=3){
        message("  Unzipping Biomass")
        untar(file.path(dataPath, "kNN-StructureBiomass.tar"),
              files = "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.zip",
              exdir = dataPath, tar = "internal")
        biomassMaps <- unzip(file.path(dataPath,
                                       "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.zip"),
                             exdir = dataPath)
      }
      sim$biomassMap <- raster(biomassMapFilename)
    }
    
    # 2. stand age map
    standAgeMapFilename <- file.path(dataPath, "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.tif")
    if(is.null(sim$standAgeMap)) {
      if(!(basename(standAgeMapFilename) %in% checkContent_passed)) {
        message("  Unzipping Stand Age")
        Cache(untar, file.path(dataPath, "kNN-StructureStandVolume.tar"),
              files = "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.zip",
              exdir = dataPath, tar = "internal",
              sideEffect = dataPath, quick = .quickCheck)
        Cache(unzip, file.path(dataPath, "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.zip"),
              exdir = dataPath, overwrite = TRUE,
              sideEffect = dataPath, quick = .quickCheck)
      }
      sim$standAgeMap <- raster(standAgeMapFilename)
    }

    # LCC2005
    if(is.null(sim$LCC2005)) {
      if(length(grep("LCC2005.*tif", checkContent_passed))!=1){
        unzip(file.path(dataPath, "LandCoverOfCanada2005_V1_4.zip"), exdir = dataPath)
      }
      sim$LCC2005 <- raster(lcc2005Filename)
    }
    if(is.null(sim$specieslayers)) {
      speciesnames <- c("Abie_Las", "Pice_Gla", "Pice_Mar",
                             "Pinu_sp", "Popu_Tre")
      speciesnamesRaw <- c("Abie_Las", "Pice_Gla", "Pice_Mar",
                           "Pinu_Ban", "Pinu_Con", "Popu_Tre")
      dataPathFiles <- dir(dataPath, full.names = TRUE)
      needSpecies <- unlist(lapply(speciesnames, function(n) grep(paste0(n,".*tif$"), checkContent_passed)))
      
      if(length(needSpecies) != length(speciesnames)) { # need to untar/unzip
        tifs <- paste0("NFI_MODIS250m_kNN_Species_", speciesnamesRaw, "_v0.tif")
        zips <- paste0("NFI_MODIS250m_kNN_Species_", speciesnamesRaw, "_v0.zip")
        tifs1 <- zips[!(tifs %in% basename(dataPathFiles))]
        
        if(length(tifs1)) {
          zips1 <- zips[!(zips %in% basename(dataPathFiles))]
          if(length(zips1)) {
            untar(file.path(dataPath, "kNN-Species.tar"),
                  files = zips,
                  exdir = dataPath, tar = "internal")
          }
          if(length(tifs1)) {
              unzipped <- lapply(zips, function(t) {
                unzip(file.path(dataPath, t), exdir = dataPath, overwrite = TRUE)
                message("  ", t, " unzipped")
              })
          }
        }
          
        dataPathFiles <- dir(dataPath, full.names = TRUE)
        speciesFiles <- unlist(lapply(speciesnamesRaw, function(n) grep(paste0(n,".*tif$"), dataPathFiles, value = TRUE)))
      } else {
        dataPathFiles <- dir(dataPath, full.names = TRUE)
        speciesFiles <- unlist(lapply(speciesnames, function(n) grep(paste0(n,".*tif$"), dataPathFiles, value = TRUE)))
      }
    }
    sim$specieslayers <- stack(lapply(speciesFiles, raster))
    if(length(names(sim$specieslayers))==length(speciesnames)) {
      names(sim$specieslayers) <- speciesnames  
    } else {
      names(sim$specieslayers) <- speciesnamesRaw
    }
    
  }
 
  # 3. species maps
  ## load Paul Pickell et al. and CASFRI
  #dataPath <- file.path(modulePath(sim), "Boreal_LBMRDataPrep", "data")
  if(all(c("SPP_1990_FILLED_100m_NAD83_LCC_BYTE_VEG.dat", "Landweb_CASFRI_GIDs.tif",
           "Landweb_CASFRI_GIDs_attributes3.csv", "Landweb_CASFRI_GIDs_README.txt") 
         %in% dir(dataPath) )) {
    message("  Loading CASFRI and Pickell et al layers")
    stackOut <- Cache(loadPaulAndCASFRI, paths = lapply(paths(sim), basename), 
                      PaulRawFileName = asPath(
                        file.path(dataPath, "SPP_1990_FILLED_100m_NAD83_LCC_BYTE_VEG.dat")),
                      existingSpeciesLayers = sim$specieslayers,
                      CASFRITifFile = asPath(
                        file.path(dataPath, "Landweb_CASFRI_GIDs.tif")),
                      CASFRIattrFile = asPath(
                        file.path(dataPath, "Landweb_CASFRI_GIDs_attributes3.csv")),
                      CASFRIheaderFile = asPath(
                        file.path(dataPath,"Landweb_CASFRI_GIDs_README.txt")),
                      digestPathContent = .quickCheck#, debugCache = "quick"
    )
  } else {
    message("Using only 'Open source data sets'")
  }
  
  if(!identical(crs(sim$biomassMap), crs(sim$shpStudyRegionFull)))
    sim$shpStudyRegionFull <- spTransform(sim$shpStudyRegionFull, crs(sim$biomassMap)) #faster without Cache
  if(!identical(crs(sim$biomassMap), crs(sim$ecoDistrict)))
    sim$ecoDistrict <- Cache(spTransform, sim$ecoDistrict, crs(sim$biomassMap)) #faster with Cache
  
  needShrinking <- !all(checkTable$result == "OK")
  if(needShrinking) {
    if(!is.null(sim$shpStudyRegionFull)) {
      message("  Shrinking number and size of files that were just downloaded")
      
      sim$ecoDistrict <- spTransform(sim$ecoDistrict, crs(sim$biomassMap))
      sim$ecoRegion <- spTransform(sim$ecoRegion, crs(sim$biomassMap))
      sim$ecoZone <- spTransform(sim$ecoZone, crs(sim$biomassMap))
      
      message("  Crop files to shpSTudyRegionFull")
      sim$ecoDistrict <- crop(sim$ecoDistrict, sim$shpStudyRegionFull)
      sim$ecoRegion <- crop(sim$ecoRegion, sim$shpStudyRegionFull)
      sim$ecoZone <- crop(sim$ecoZone, sim$shpStudyRegionFull)
      
      message("  Resave them")
      shapefile(sim$ecoDistrict, ecodistrictFilename, overwrite = TRUE)
      shapefile(sim$ecoRegion, ecoregionFilename, overwrite = TRUE)
      shapefile(sim$ecoZone, ecozoneFilename, overwrite = TRUE)
      
      # rasters
      # LCC2005
      message("  Crop LCC2005")
      if(!grepl("Boreal_LBMRDataPrep", dirname(filename(sim$LCC2005)))) {
        sim$LCC2005 <- writeRaster(sim$LCC2005, overwrite=TRUE, format = "GTiff", datatype = "INT2U",
                                      filename = file.path(dirname(lcc2005Filename), paste0("Small",basename(lcc2005Filename))))
      }
      sim$LCC2005 <- crop(sim$LCC2005, sim$shpStudyRegionFull,
                          filename = file.path(dirname(lcc2005Filename), paste0("Small", basename(lcc2005Filename))),
                          overwrite=TRUE)
      
      # Biomass
      if(!grepl("Boreal_LBMRDataPrep", dirname(filename(sim$biomassMap)))) {
        sim$biomassMap <- writeRaster(sim$biomassMap, overwrite=TRUE, format = "GTiff", datatype = "INT2U",
                                      filename = file.path(dirname(biomassMapFilename), paste0("Small",basename(biomassMapFilename))))
      }
      sim$biomassMap <- crop(sim$biomassMap, sim$shpStudyRegionFull)
      
      # Stand Age
      if(!grepl("Boreal_LBMRDataPrep", dirname(filename(sim$standAgeMap)))) {
        sim$standAgeMap <- writeRaster(sim$standAgeMap, overwrite=TRUE, format = "GTiff", datatype = "INT2U",
                                      filename = file.path(dirname(standAgeMapFilename), paste0("Small",basename(standAgeMapFilename))))
      }
      sim$standAgeMap <- crop(sim$standAgeMap, sim$shpStudyRegionFull,
                              overwrite=TRUE, format = "GTiff", datatype = "INT2U",
                              filename = file.path(dirname(standAgeMapFilename),
                                                   paste0("Small",basename(standAgeMapFilename))))
      
      # Species
      message("  Crop species layers to shpStudyRegionFull")
      sim$specieslayers <- cropSpeciesLayers(asPath(speciesFiles), sim)
      
      # Assign correct names
      spNames <- unique(c(speciesnames, speciesnamesRaw))
      ll <- lapply(tolower(spNames), function(n) {
        grep(pattern = n, tolower(names(sim$specieslayers)))})
      names(sim$specieslayers)[unlist(ll)] <- spNames[sapply(ll, function(x) length(x)>0)]
        
      
    } # end crop to shpStudyRegion
    
    speciesLayersLocal <- sim$specieslayers
    message("  Merging Pines (lodgepole and jack) into one species")
    if(all(speciesnamesRaw %in% names(speciesLayersLocal))) {
      sim$specieslayers <- Cache(sumRastersBySpecies, #notOlderThan = Sys.time(),
                                 speciesLayersLocal, c("Pinu_Ban", "Pinu_Con"),
                                 filenameToSave = asPath(file.path(dirname(filename(sim$specieslayers[["Popu_Tre"]])),
                                                                   "KNNPinu_sp.tif")), 
                                 cachePath = cachePath(sim), # for sumRastersBySpecies arg
                                 cacheRepo = cachePath(sim), # for Cache arg
                                 userTags = "stable")
    }
    names(sim$specieslayers)[grep("Pinu", names(sim$specieslayers))] <- "Pinu_sp"
    
    if(!isTRUE(all(dd$result=="OK"))) { # might be NA, which returns NA for the == "OK"
      a <- checksums("Boreal_LBMRDataPrep", modulePath(sim), write = TRUE)
      a <- a[grep(a$file, pattern = ".tar|.zip", invert = TRUE),]
      write.table(a, file = file.path(dataPath, "CHECKSUMS.txt"), row.names = FALSE)
      message("*** Created a new CHECKSUMS.txt file with downloaded objects for ","Boreal_LBMRDataPrep","***")
    }
  }
  
  names(sim$specieslayers)[grep("Abie", names(sim$specieslayers))] <- "Abie_sp"
  names(sim$specieslayers) <- toSentenceCase(names(sim$specieslayers))
  
  sim$speciesTable <- read.csv(file.path(dataPath, "speciesTraits.csv"), header = TRUE,
                               stringsAsFactors = FALSE) %>%
    data.table()
  
  
  sim$sufficientLight <- data.frame(speciesshadetolerance = 1:5,
                                    X0 = 1, X1 = c(0.5, rep(1, 4)), X2 = c(0, 0.5, rep(1, 3)),
                                    X3 = c(rep(0, 2), 0.5, rep(1, 2)),
                                    X4 = c(rep(0, 3), 0.5, 1), X5 = c(rep(0, 4), 1))
  
  sim$seedingAlgorithm <- "wardDispersal"
  sim$spinupMortalityfraction <- 0.002
  sim$cellSize <- 250
  sim$successionTimestep <- 10
  
  if(is.null(sim$studyArea)) {
    sim$studyArea <- sim$shpStudyRegionFull
  }
  if(is.null(sim$rstStudyRegion)) {
    needRstSR <- TRUE
  } else {
    if(!identical(extent(sim$rstStudyRegion), extent(biomassMap)))
      needRstSR <- TRUE
  }
  if(needRstSR) {
    message("  Rasterizing the shpStudyRegionFull polygon map")
    sim$rstStudyRegion <- Cache(fastRasterize, shpStudyRegionFull, biomassMap, 
                                field = "LTHRC", digestPathContent = .quickCheck)
  }
  
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above


createInitCommMap <- function(initCommMap, values, filename) {
  map <- setValues(initCommMap, values = values)
  writeRaster( map, overwrite = TRUE, filename = filename, datatype="INT2U")
}

sumRastersBySpecies <- function(specieslayers, layersToSum, 
                                filenameToSave, cachePath) {
  Pinu_sp <- calc(specieslayers[[layersToSum]], sum)
  Pinu_sp <- writeRaster(Pinu_sp, 
                   filename = filenameToSave, 
                   datatype="INT2U", overwrite=TRUE)
  # Need to have names be in sentence case
  newNames <- toSentenceCase(layerNames(specieslayers))
  names(specieslayers) <- newNames
  
  specieslayers <- dropLayer(specieslayers, grep(pattern = "Pinu", names(specieslayers)))
  specieslayers <- addLayer(specieslayers, Pinu_sp)
  specieslayers
}

cropSpeciesLayers <- function(speciesFilenames, sim) {

  specieslayersLocal <- lapply(speciesFilenames, function(x) {
    filenameNoExt <- strsplit(basename(x), "\\.")[[1]][1]
    a <- raster(x) %>%
      crop(sim$shpStudyRegionFull,
           overwrite=TRUE, format = "GTiff", datatype = "INT1U",
           filename = file.path(dirname(x), paste0("Small",basename(x))))
    file.rename(filename(a), x)
    a@file@name <- x
  }) %>% stack()
  specieslayersLocal
}

digestNeededFiles <- function(dataPath, fileNames1) {
  filesList <- dir(dataPath, full.names = TRUE)
  filesList2 <- filesList[basename(filesList) %in% fileNames1]
  allFiles2 <- filesList2 %>% lapply(., function(x) digest::digest(file=x, length = 6e6, algo = "xxhash64"))
  names(allFiles2) <- basename(filesList2)
  allFiles2
}

toSentenceCase <- function(strings) {
  newNames <- tolower(strings)
  substr(newNames,1,1) <- toupper(substr(newNames,1,1))
  newNames
  
}
