# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "Boreal_LBMRDataPrep",
  description = "A data preparation module for running the LBMR module in the LandWeb project",
  keywords = c("LandWeb", "LBMR"),
  authors = c(person(c("Yong", "Luo"), email = "yong.luo@canada.ca", role = c("aut", "cre")),
              person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut"))),
  childModules = character(0),
  version = numeric_version("1.3.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "Boreal_LBMRDataPrep.Rmd"),
  reqdPkgs = list("data.table", "dplyr", "gdalUtils", "raster", "rgeos", "ecohealthalliance/fasterize"),
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
                 desc = "ecodistricts in study area, default is Canada national ecodistricts",
                 sourceURL = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip"),
    expectsInput(objectName = "ecoRegion", objectClass = "SpatialPolygonsDataFrame",
                 desc = "ecoregions in study area, default is Canada national ecoregions",
                 sourceURL = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip"),
    expectsInput(objectName = "ecoZone", objectClass = "SpatialPolygonsDataFrame",
                 desc = "ecozones in study area, default is Canada national ecozones",
                 sourceURL = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip"),
    expectsInput(objectName = "biomassMap", objectClass = "RasterLayer",
                 desc = "total biomass raster layer in study area, default is Canada national biomass map",
                 sourceURL = "http://tree.pfc.forestry.ca/kNN-StructureBiomass.tar"),
    expectsInput(objectName = "standAgeMap", objectClass = "RasterLayer",
                 desc = "stand age map in study area, default is Canada national stand age map",
                 sourceURL = "http://tree.pfc.forestry.ca/kNN-StructureStandVolume.tar"),
    expectsInput(objectName = "specieslayers", objectClass = "RasterStack",
                 desc = "biomass percentage raster layers by species in Canada species map",
                 sourceURL = "http://tree.pfc.forestry.ca/kNN-Species.tar"),
    expectsInput(objectName = "LCC2005", objectClass = "RasterLayer",
                 desc = "2005 land classification map in study area, default is Canada national land classification in 2005",
                 sourceURL = "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip"),
    expectsInput(objectName = "speciesTable", objectClass = "data.table",
                 desc = "species attributes table, default is from Dominic and Yan's project",
                 sourceURL = "https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master/speciesTraits.csv"),
    expectsInput(objectName = "shpStudyRegionFull", objectClass = "SpatialPolygonsDataFrame",
                 desc = "this shape file contains two informaton: Full study area with fire return interval attribute",
                 sourceURL = ""), # i guess this is study area and fire return interval
    expectsInput(objectName = "shpStudySubRegion", objectClass = "SpatialPolygonsDataFrame",
                 desc = "this shape file contains two informaton: Sub study area with fire return interval attribute",
                 sourceURL = ""), # i guess this is study area and fire return interval
    expectsInput(objectName = "rstStudyRegion", objectClass = "RasterLayer",
                 desc = "this raster contains two pieces of informaton: Full study area with fire return interval attribute",
                 sourceURL = ""), # i guess this is study area and fire return interval
    expectsInput("sufficientLight", "data.frame",
                 desc = "define how the species with different shade tolerance respond to stand shadeness"),
    expectsInput("seedingAlgorithm", "character",
                 desc = "choose which seeding algorithm will be used among noDispersal, universalDispersal,
                 and wardDispersal, default is wardDispersal"),
    expectsInput("successionTimestep", "numeric",
                 desc = "define the simulation time step, default is 10 years"),
    # expectsInput(objectName = "cellSize", objectClass = "numeric",
    #              desc = "define the cell size"),
    # expectsInput(objectName = "spinupMortalityfraction", objectClass = "numeric",
    #               desc = "define the mortality loss fraction in spin up-stage simulation, default is 0.001"),
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
    createsOutput(objectName = "useCache", objectClass = "logic",
                  desc = "define which the caching for spinup simulation should be used, default is TRUE"),
    createsOutput("studyArea", "", ""),
    createsOutput("ecoDistrict", "", ""),
    createsOutput("ecoRegion", "", ""),
    createsOutput("ecoZone", "", "")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.Boreal_LBMRDataPrep = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
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
  initialCommFiles <- Cache(initialCommunityProducer,
                            speciesLayers = sim$specieslayers,
                            speciesPresence = 50,
                            studyArea = sim$studyArea,
                            rstStudyArea = rstStudyRegionBinary,
                            userTags = "stable")
  ecoregionstatus <- data.table(active = "yes",
                                ecoregion = 1:1031)

  .gc()

  message("ecoregionProducer: ", Sys.time())
  ecoregionFiles <- Cache(ecoregionProducer,
                          studyAreaRaster = initialCommFiles$initialCommunityMap,
                          ecoregionMapFull = sim$ecoDistrict,
                          ecoregionName = "ECODISTRIC",
                          ecoregionActiveStatus = ecoregionstatus,
                          studyArea = sim$studyArea,
                          rstStudyArea = rstStudyRegionBinary,
                          maskFn = fastMask,
                          userTags = "stable")

  .gc()

  message("3: ", Sys.time())
  # LCC05 -- land covers 1 to 15 are forested with tree dominated... 34 and 35 are recent burns
  activeStatusTable <- data.table(active = c(rep("yes", 15), rep("no", 25)),
                                  mapcode = 1:40)[mapcode %in% c(34, 35), active := "yes"]  # this is based on description in LCC05
  #simulationMaps <- sim$nonActiveEcoregionProducerCached(nonactiveRaster = sim$LCC2005,
  simulationMaps <- Cache(nonActiveEcoregionProducer, nonactiveRaster = sim$LCC2005,
                          activeStatus = activeStatusTable,
                          ecoregionMap = ecoregionFiles$ecoregionMap,
                          ecoregion = ecoregionFiles$ecoregion,
                          initialCommunityMap = initialCommFiles$initialCommunityMap,
                          initialCommunity = initialCommFiles$initialCommunity,
                          userTags = "stable")

  .gc()

  message("4: ", Sys.time())
  #speciesEcoregionTable <- sim$obtainMaxBandANPPCached(speciesLayers = sim$specieslayers,
  speciesEcoregionTable <- Cache(obtainMaxBandANPP, speciesLayers = sim$specieslayers,
                                 biomassLayer = sim$biomassMap,
                                 SALayer = sim$standAgeMap,
                                 ecoregionMap = simulationMaps$ecoregionMap,
                                 userTags = "stable")

  .gc()

  message("5: ", Sys.time())
  #septable <- sim$obtainSEPCached(ecoregionMap = simulationMaps$ecoregionMap,
  septable <- Cache(obtainSEP, ecoregionMap = simulationMaps$ecoregionMap,
                    speciesLayers = sim$specieslayers,
                    userTags = "stable")
  names(septable) <- c("ecoregion", "species", "SEP")
  septable[, SEP := round(SEP, 2)]

  .gc()

  message("6: ", Sys.time())
  speciesEcoregionTable[, species := as.character(species)]
  septable[, species := as.character(species)]
  speciesEcoregionTable <- left_join(speciesEcoregionTable, septable, by = c("ecoregion", "species")) %>%
    data.table
  speciesEcoregionTable[SEP == 0, ':='(maxBiomass = 0, maxANPP = 0)]
  NON_NAdata <- speciesEcoregionTable[!is.na(maxBiomass),]
  NAdata <- speciesEcoregionTable[is.na(maxBiomass),]

  if (nrow(NAdata) > 1) {
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
                                  maskFn = fastMask,
                                  userTags = "stable")
    message("  6b obtainMaxBandANPPFormBiggerEcoArea: ", Sys.time())
    NON_NAdata <- rbind(NON_NAdata, biomassFrombiggerMap$addData[!is.na(maxBiomass), .(ecoregion, species, maxBiomass, maxANPP, SEP)])
    NAdata <- biomassFrombiggerMap$addData[is.na(maxBiomass),.(ecoregion, species, maxBiomass, maxANPP, SEP)]
  }

  .gc()

  message("7: ", Sys.time())
  if (nrow(NAdata) > 1) {
    #biomassFrombiggerMap <- sim$obtainMaxBandANPPFormBiggerEcoAreaCached(speciesLayers = sim$specieslayers,
    message("  7a obtainMaxBandANPPFormBiggerEcoArea if NAdata exist: ", Sys.time())
    biomassFrombiggerMap <- Cache(obtainMaxBandANPPFormBiggerEcoArea,
                                  speciesLayers = sim$specieslayers, biomassLayer = sim$biomassMap,
                                  SALayer = sim$standAgeMap, ecoregionMap = simulationMaps$ecoregionMap,
                                  biggerEcoArea = sim$ecoZone, biggerEcoAreaSource = "ecoZone",
                                  NAData = NAdata, maskFn = fastMask,
                                  userTags = "stable")
    message("  7b obtainMaxBandANPPFormBiggerEcoArea if NAdata exist: ", Sys.time())
    NON_NAdata <- rbind(NON_NAdata, biomassFrombiggerMap$addData[!is.na(maxBiomass), .(ecoregion, species, maxBiomass, maxANPP, SEP)])
    NAdata <- biomassFrombiggerMap$addData[is.na(maxBiomass),.(ecoregion, species, maxBiomass, maxANPP, SEP)]
  }

  .gc()

  message("8: ", Sys.time())
  NAdata[, ':='(maxBiomass = 0, maxANPP = 0, SEP = 0)]
  speciesEcoregion <- rbind(NON_NAdata,NAdata)
  setnames(speciesEcoregion, "ecoregion", "mapcode")
  speciesEcoregion <- setkey(speciesEcoregion,
                             mapcode)[setkey(simulationMaps$ecoregion, mapcode),
                                      nomatch = 0][, .(year = 0, ecoregion, species,
                                                       maxB = maxBiomass,
                                                       maxANPP, establishprob = SEP)]
  sim$speciesEcoregion <- speciesEcoregion
  sim$ecoregion <- simulationMaps$ecoregion
  sim$ecoregionMap <- simulationMaps$ecoregionMap

  sim$initialCommunitiesMap <- Cache(createInitCommMap, simulationMaps$initialCommunityMap,
                                     as.integer(simulationMaps$initialCommunityMap[]),
                                     file.path(outputPath(sim), "initialCommunitiesMap.tif"),
                                     userTags = "stable")

  .gc()

  message("9: ", Sys.time())

  # species traits inputs
  speciesTable <- sim$speciesTable
  names(speciesTable) <- c("species", "Area", "longevity", "sexualmature", "shadetolerance", "firetolerance",
                           "seeddistance_eff", "seeddistance_max", "resproutprob", "resproutage_min",
                           "resproutage_max", "postfireregen", "leaflongevity", "wooddecayrate",
                           "mortalityshape", "growthcurve", "leafLignin", "hardsoft")
  speciesTable[, ':='(Area = NULL, hardsoft = NULL)]
  speciesTable$species1 <- as.character(substring(speciesTable$species, 1, 4))
  speciesTable$species2 <- as.character(substring(speciesTable$species, 6, nchar(speciesTable$species)))
  speciesTable[, ':='(species = paste(as.character(substring(species1, 1, 1)),
                                      tolower(as.character(substring(species1, 2, nchar(species1)))),
                                      "_", as.character(substring(species2, 1, 1)),
                                      tolower(as.character(substring(species2, 2, nchar(species2)))),
                                      sep = ""))]
  speciesTable[species == "Pinu_Con.lat", species := "Pinu_Con"]

  newNames <- toSentenceCase(speciesTable$species)

  speciesTable$species <- newNames

  speciesTable[species %in% c("Abie_las", "Abie_bal"), species := "Abie_sp"]
  speciesTable[species %in% c("Pinu_ban", "Pinu_con", "Pinu_con.con"), species := "Pinu_sp"]

  message("10: ", Sys.time())

  # Take the smallest values of every column, within species, because it is northern boreal forest
  speciesTable <- speciesTable[species %in% names(sim$specieslayers), ][
    , ':='(species1 = NULL, species2 = NULL)] %>%
    .[, lapply(.SD, function(x) if (is.numeric(x)) min(x, na.rm = TRUE) else x[1]), by = "species"]

  initialCommunities <- simulationMaps$initialCommunity[, .(mapcode, description = NA, species)]
  set(initialCommunities, , paste("age", 1:15, sep = ""), NA)
  initialCommunities <- data.frame(initialCommunities)
  message("11: ", Sys.time())

  initialCommunitiesFn <- function(initialCommunities, speciesTable) {
    for (i in 1:nrow(initialCommunities)) {
      agelength <- sample(1:15, 1)
      ages <- sort(sample(1:speciesTable[species == initialCommunities$species[i],]$longevity,
                          agelength))
      initialCommunities[i, 4:(agelength + 3)] <- ages
    }
    data.table::data.table(initialCommunities)
  }
  message("12: ", Sys.time())

  sim$initialCommunities <- Cache(initialCommunitiesFn, initialCommunities, speciesTable,
                                  userTags = "stable")

  # assign("species", speciesTable, envir = .GlobalEnv)

  sim$species <- speciesTable
  sim$minRelativeB <- data.frame(ecoregion = sim$ecoregion[active == "yes",]$ecoregion,
                                 X1 = 0.2, X2 = 0.4, X3 = 0.5,
                                 X4 = 0.7, X5 = 0.9)
  message("Done Boreal_LBMRDataPrep: ", Sys.time())

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

Save <- function(sim) {
  sim <- saveFiles(sim)
  return(invisible(sim))
}

.gc <- function() for (i in 1:10) gc() ## free memory if possible

## see other helper functions in R/ subdirectory

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
  dPath <- dataPath(sim)

  # 1. test if all input objects are already present (e.g., from inputs, objects or another module)
  a <- depends(sim)
  whThisMod <- which(unlist(lapply(a@dependencies, function(x) x@name)) == "Boreal_LBMRDataPrep")
  objNames <- a@dependencies[[whThisMod]]@inputObjects$objectName
  objExists <- !unlist(lapply(objNames,
                              function(x) is.null(sim[[x]])))
  names(objExists) <- objNames

  # Filenames
  ecoregionFilename <-   file.path(dPath, "ecoregions.shp")
  ecodistrictFilename <- file.path(dPath, "ecodistricts.shp")
  ecozoneFilename <-   file.path(dPath, "ecozones.shp")
  biomassMapFilename <- file.path(dPath, "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.tif")
  lcc2005Filename <- file.path(dPath, "LCC2005_V1_4a.tif")
  standAgeMapFilename <- file.path(dPath, "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.tif")

  # Also extract
  fexts <- c("dbf", "prj", "sbn", "sbx", "shx")
  ecoregionAE <- basename(paste0(tools::file_path_sans_ext(ecoregionFilename), ".", fexts))
  ecodistrictAE <- basename(paste0(tools::file_path_sans_ext(ecodistrictFilename), ".", fexts))
  ecozoneAE <- basename(paste0(tools::file_path_sans_ext(ecozoneFilename), ".", fexts))

  # This is from sim$biomassMap
  crsUsed <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

  if (!identical(crsUsed, crs(sim$shpStudyRegionFull)))
    sim$shpStudyRegionFull <- spTransform(sim$shpStudyRegionFull, crsUsed) #faster without Cache

  if (!identical(crsUsed, crs(sim$shpStudySubRegion)))
    sim$shpStudySubRegion <- spTransform(sim$shpStudySubRegion, crsUsed) #faster without Cache

  cacheTags = c(currentModule(sim), "function:.inputObjects", "function:spades")
  
  if (!suppliedElsewhere("biomassMap", sim)) {
    sim$biomassMap <- Cache(prepInputs,
                            targetFile = biomassMapFilename,
                            archive = asPath(c("kNN-StructureBiomass.tar",
                                               "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.zip")),
                            destinationPath = asPath(dPath),
                            studyArea = sim$shpStudySubRegion,
                            rasterInterpMethod = "bilinear",
                            rasterDatatype = "INT2U",
                            writeCropped = TRUE,
                            cacheTags = c("stable", currentModule(sim)))#,
    #dataset = "EOSD2000")

    # sim$biomassMap <- Cache(prepareIt,
    #                         tarfileName = "kNN-StructureBiomass.tar",
    #                         untarfileNames = asPath("NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.zip"),
    #                         spatialObjectFilename = biomassMapFilename,
    #                         dataPath = dPath, #rasterToMatch = sim$standAgeMap,
    #                         studyArea = sim$shpStudySubRegion,
    #                         userTags = cacheTags,
    #                         modulePath = modulePath(sim))
  }

  # LCC2005
  if (!suppliedElsewhere("LCC2005", sim)) {
    sim$LCC2005 <- Cache(prepInputs,
                         targetFile = lcc2005Filename,
                         archive = asPath("LandCoverOfCanada2005_V1_4.zip"),
                         destinationPath = asPath(dPath),
                         studyArea = sim$shpStudySubRegion,
                         rasterToMatch = sim$biomassMap,
                         rasterInterpMethod = "bilinear",
                         rasterDatatype = "INT2U",
                         writeCropped = TRUE,
                         cacheTags = currentModule(sim))

    projection(sim$LCC2005) <- projection(sim$biomassMap)
    # sim$LCC2005 <- Cache(prepareIt,
    #                      zipfileName = asPath("LandCoverOfCanada2005_V1_4.zip"),
    #                      spatialObjectFilename = lcc2005Filename,
    #                      dataPath = dPath, rasterToMatch = sim$biomassMap,
    #                      studyArea = sim$shpStudySubRegion,
    #                      userTags = cacheTags,
    #                      modulePath = modulePath(sim))
  }
  
  if (!suppliedElsewhere("ecoDistrict", sim)) {
    sim$ecoDistrict <- Cache(prepInputs,
                             targetFile = asPath(ecodistrictFilename),
                             archive = asPath("ecodistrict_shp.zip"),
                             alsoExtract = ecodistrictAE,
                             destinationPath = asPath(dPath),
                             studyArea = sim$shpStudyRegionFull,
                             pkg = "raster",
                             fun = "shapefile",
                             writeCropped = TRUE,
                             cacheTags = cacheTags,
                             userTags = cacheTags)
    # sim$ecoDistrict <- Cache(prepareIt,
    #                       zipfileName = "ecodistrict_shp.zip",
    #                       zipExtractFolder = "Ecodistricts",
    #                       spatialObjectFilename = asPath(ecodistrictFilename),
    #                       dataPath = dPath, #rasterToMatch = sim$biomassMap,
    #                       userTags = cacheTags,
    #                       studyArea = sim$shpStudyRegionFull,
    #                       modulePath = modulePath(sim))
  }
  
  if (!suppliedElsewhere("ecoRegion", sim)) {
    # sim$ecoRegion <- Cache(prepareIt,
    #                        zipfileName = asPath("ecoregion_shp.zip"),
    #                        zipExtractFolder = "Ecoregions",
    #                        spatialObjectFilename = ecoregionFilename,
    #                        dataPath = dPath, #rasterToMatch = sim$biomassMap,
    #                        studyArea = sim$shpStudyRegionFull,
    #                        userTags = cacheTags,
    #                        modulePath = modulePath(sim))
    sim$ecoRegion <- Cache(prepInputs,
                           targetFile = asPath(ecoregionFilename),
                           archive = asPath("ecoregion_shp.zip"),
                           alsoExtract = ecoregionAE,
                           destinationPath = asPath(dPath),
                           studyArea = sim$shpStudyRegionFull,
                           pkg = "raster",
                           fun = "shapefile",
                           writeCropped = TRUE,
                           cacheTags = cacheTags,
                           userTags = cacheTags)
  }
  
  if (!suppliedElsewhere("ecoZone", sim)) {
    sim$ecoZone <- Cache(prepInputs, #notOlderThan = Sys.time(),
                         targetFile = asPath(ecozoneFilename),
                         archive = asPath("ecozone_shp.zip"),
                         alsoExtract = ecozoneAE,
                         destinationPath = asPath(dPath),
                         studyArea = sim$shpStudyRegionFull,
                         pkg = "raster",
                         fun = "shapefile",
                         writeCropped = TRUE,
                         cacheTags = cacheTags,
                         userTags = cacheTags)
  }

  # stand age map
  if (!suppliedElsewhere("standAgeMap", sim)) {
    sim$standAgeMap <- Cache(prepInputs, #notOlderThan = Sys.time(),
                             targetFile = standAgeMapFilename,
                             archive = asPath(c("kNN-StructureStandVolume.tar",
                                                "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.zip")),
                             destinationPath= asPath(dPath),
                             fun = "raster",
                             pkg = "raster",
                             studyArea = sim$shpStudyRegionFull,
                             rasterToMatch = sim$biomassMap,
                             rasterInterpMethod = "bilinear",
                             rasterDatatype = "INT2U",
                             writeCropped = TRUE,
                             cacheTags = c("stable", currentModule(sim)))
  }
  
  if (!suppliedElsewhere("specieslayers", sim)) {
    sim$specieslayers <- Cache(loadAllSpeciesLayers, dPath, sim$biomassMap,
                               sim$shpStudyRegionFull, moduleName = currentModule(sim),
                               cacheTags = cacheTags)#, # This is for the internal caching
                               #userTags = cacheTags)
  }



  # 3. species maps
  # ## load Paul Pickell et al. and CASFRI
  # if (!exists("sessionCacheFile")) { 
  #   sessionCacheFile <<- tempfile() 
  # } 
  # .cacheVal <<- if (grepl("VIC-A", Sys.info()["nodename"])) sessionCacheFile else FALSE
  # googledrive::drive_auth(use_oob = TRUE, verbose = TRUE, cache = .cacheVal)
  # file_url <- "https://drive.google.com/file/d/1sJoZajgHtsrOTNOE3LL8MtnTASzY0mo7/view?usp=sharing"
  # aaa <- testthat::capture_error(googledrive::drive_download(googledrive::as_id(file_url), path = tempfile(),
  #                             overwrite = TRUE, verbose = TRUE))
  # browser()
  # if (is.null(aaa)) { # means got the file
  #   message("  Loading CASFRI and Pickell et al. layers")
  #   extraExtents <- raster(file.path(dataPath(sim), "SPP_1990_100m_NAD83_LCC_BYTE_VEG_NO_TIES_FILLED_FINAL.dat"))
  #   Paul <- Cache(prepInputs,
  #         targetFile = asPath("SPP_1990_100m_NAD83_LCC_BYTE_VEG_NO_TIES_FILLED_FINAL.dat"),
  #         archive = asPath("SPP_1990_100m_NAD83_LCC_BYTE_VEG_NO_TIES_FILLED_FINAL.zip"),
  #         alsoExtract = asPath("SPP_1990_100m_NAD83_LCC_BYTE_VEG_NO_TIES_FILLED_FINAL.hdr"), 
  #         destinationPath= asPath(dPath),
  #         fun = "raster",
  #         pkg = "raster",
  #         studyArea = sim$shpStudySubRegion,
  #         rasterToMatch = sim$biomassMap,
  #         rasterInterpMethod = "bilinear",
  #         rasterDatatype = "INT2U",
  #         writeCropped = TRUE,
  #         cacheTags = c("stable", currentModule(sim)))#, notOlderThan = Sys.time())
  #   
  #   CASFRITifFile = asPath(file.path(dPath, "Landweb_CASFRI_GIDs.tif"))
  #   CASFRIattrFile = asPath(file.path(dPath, "Landweb_CASFRI_GIDs_attributes3.csv"))
  #   CASFRIheaderFile = asPath(file.path(dPath,"Landweb_CASFRI_GIDs_README.txt"))
  #   CASFRIRas <- Cache(prepInputs,
  #                 targetFile = asPath("Landweb_CASFRI_GIDs.tif"),
  #                 archive = asPath("CASFRI for Landweb.zip"),
  #                 alsoExtract = c(CASFRITifFile, CASFRIattrFile, CASFRIheaderFile),
  #                 destinationPath= asPath(dPath),
  #                 fun = "raster",
  #                 pkg = "raster",
  #                 studyArea = sim$shpStudySubRegion,
  #                 rasterToMatch = sim$biomassMap,
  #                 rasterInterpMethod = "bilinear",
  #                 rasterDatatype = "INT2U",
  #                 writeCropped = TRUE,
  #                 cacheTags = c("stable", currentModule(sim)))
  #   
  #   message("Load CASFRI data and headers, and convert to long format, and define species groups")
  #   loadedCASFRI <- Cache(loadCASFRI, CASFRIRas, CASFRIattrFile, CASFRIheaderFile, 
  #                         debugCache = "complete", userTags = "BigDataTable")
  #   
  #   message("Make stack of species layers from Paul's layer")
  #   uniqueKeepSp <- unique(loadedCASFRI$keepSpecies$spGroup)
  #   # "Abie_sp"  "Betu_pap" "Lari_lar" "Pice_gla" "Pice_mar" "Pinu_sp" "Popu_tre"
  #   PaulSpStack <- Cache(makePaulStack, paths = lapply(paths(sim), basename), 
  #                        PaulRaster = Paul, uniqueKeepSp)
  #   crs(PaulSpStack) <- crs(sim$biomassMap) # bug in writeRaster
  #   
  #   message('Make stack from CASFRI data and headers')
  #   CASFRISpStack <- Cache(CASFRItoSpRasts, CASFRIRas, loadedCASFRI)
  #   
  #   message("Overlay Paul and CASFRI stacks")
  #   outStack <- Cache(overlayStacks, CASFRISpStack, PaulSpStack, 
  #                     outputFilenameSuffix = "CASFRI_PAUL")#, notOlderThan = Sys.time())
  #   crs(outStack) <- crs(sim$biomassMap) # bug in writeRaster
  #   
  #   message("Overlay Paul_CASFRI with open data set stacks")
  #   sim$specieslayers2 <- Cache(overlayStacks, outStack, sim$specieslayers, 
  #                      outputFilenameSuffix = "CASFRI_PAUL_KNN")
  #   crs(sim$specieslayers2) <- crs(sim$biomassMap)
  #   sim$specieslayers <- sim$specieslayers2
  #   rm(specieslayers2, envir = envir(sim))
  # } else {
  #   message("Using only 'Open source data sets'")
  # }

  sim$speciesTable <- prepInputs("speciesTraits.csv", destinationPath = dPath,
                                 fun = "read.csv", pkg = "utils")
  sim$speciesTable <- read.csv(file.path(dPath, "speciesTraits.csv"), header = TRUE,
                               stringsAsFactors = FALSE) %>%
    data.table()

  sim$sufficientLight <- data.frame(speciesshadetolerance = 1:5,
                                    X0 = 1,
                                    X1 = c(0.5, rep(1, 4)),
                                    X2 = c(0, 0.5, rep(1, 3)),
                                    X3 = c(rep(0, 2), 0.5, rep(1, 2)),
                                    X4 = c(rep(0, 3), 0.5, 1), X5 = c(rep(0, 4), 1))

  if (!suppliedElsewhere("seedingAlgorithm", sim))
    sim$seedingAlgorithm <- "wardDispersal"
  if (!suppliedElsewhere("successionTimestep", sim))
    sim$successionTimestep <- 10

  if (!suppliedElsewhere(sim$studyArea)) {
    sim$studyArea <- sim$shpStudyRegionFull
  }
  if (!suppliedElsewhere(sim$rstStudyRegion)) {
    needRstSR <- TRUE
  } else {
    if (!identical(extent(sim$rstStudyRegion), extent(biomassMap))) {
      needRstSR <- TRUE
    } else {
      needRstSR <- FALSE
    }
  }
  if (needRstSR) {
    message("  Rasterizing the shpStudyRegionFull polygon map")

    fieldName <- if (is.null(nrow(studyArea))) {
      NULL
    } else if ("LTHRC" %in% names(sim$shpStudyRegionFull)) {
      "LTHRC"
    } else {
      names(sim$shpStudyRegionFull)[1]
    }

    fasterizeFromSp <- function(sp, raster, fieldName) {
      fasterize::fasterize(sf::st_as_sf(sp), raster, field = fieldName)
    }
    sim$rstStudyRegion <- crop(fasterizeFromSp(sim$shpStudyRegionFull, sim$biomassMap, fieldName),
                               sim$shpStudyRegionFull)
  }

  return(invisible(sim))
}
