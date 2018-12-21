defineModule(sim, list(
  name = "LandWeb_preamble",
  description = "define FMA-specific study areas etc. for LandWeb",
  keywords = c("LandWeb"),
  authors = c(
    person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
    person(c("Alex", "M."), "Chubaty", email = "achubaty@friresearch.ca", role = c("aut"))
  ),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.3.9006", LandWeb_preamble = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "LandWeb_preamble.Rmd"),
  reqdPkgs = list("achubaty/amc", "fasterize", "magrittr", "maptools",
                  "PredictiveEcology/LandR@development",
                  "PredictiveEcology/map@development",
                  "PredictiveEcology/pemisc@development",
                  "raster", "RColorBrewer", "reproducible", "rgeos",
                  "sf", "sp", "SpaDES.tools"),
  parameters = rbind(
    defineParameter("minFRI", "numeric", 40, 0, 200, "The value of fire return interval below which, pixels will be changed to NA, i.e., ignored"),
    defineParameter("runName", "character", NA, NA, NA, "A description for run; this will form the basis of cache path and output path"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    ## TODO: uses CC and fire return interval maps from URL in init
    expectsInput("canProvs", "SpatialPolygonsDataFrame", "Canadian provincial boundaries shapefile", NA)
  ),
  outputObjects = bind_rows(
    createsOutput("CC TSF", "RasterLayer", desc = NA), ## TODO: need descriptions for all outputs
    createsOutput("fireReturnInterval", "RasterLayer", desc = NA),
    createsOutput("LandTypeCC", "RasterLayer", desc = NA),
    createsOutput("LCC2005", "RasterLayer", desc = NA),
    createsOutput("nonTreePixels", "integer", desc = NA),
    createsOutput("rasterToMatch", "RasterLayer", desc = NA),
    createsOutput("rasterToMatchReporting", "RasterLayer", desc = NA),
    createsOutput("rstFlammable", "RasterLayer", desc = NA),
    createsOutput("studyArea", "SpatialPolygonsDataFrame", desc = NA),
    createsOutput("studyAreaLarge", "SpatialPolygonsDataFrame", desc = NA),
    createsOutput("studyAreaReporting", "SpatialPolygonsDataFrame", desc = NA)
  )
))

doEvent.LandWeb_preamble = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

Init <- function(sim) {
  targetCRS <- CRS(paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                         "+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))

  ## LandWeb study area -- LTHFC (aka "fire return interval") map
  ml <- mapAdd(layerName = "LandWeb Study Area",
               targetCRS = targetCRS, overwrite = TRUE,
               url = "https://drive.google.com/open?id=1JptU0R7qsHOEAEkxybx5MGg650KC98c6", ## landweb_ltfc_v6.shp
               columnNameForLabels = "NSN", isStudyArea = TRUE, filename2 = NULL)

  ## Updated FMA boundaries
  ml <- mapAdd(map = ml, layerName = "FMA Boundaries Updated",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/file/d/1nTFOcrdMf1hIsxd_yNCSTr8RrYNHHwuc/view?usp=sharing",
               columnNameForLabels = "Name", isStudyArea = FALSE, filename2 = NULL)

  ## Alberta Natural Subregions (ANSRs)
  ml <- mapAdd(map = ml, layerName = "Alberta Natural Subregions",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/file/d/1mCEynahKnFkStJUJC8ho5ndRD41olz9F/view?usp=sharing",
               columnNameForLabels = "Name", isStudyArea = FALSE, filename2 = NULL)

  ## Boreal Caribou Ranges
  ml <- mapAdd(map = ml, layerName = "Boreal Caribou Ranges",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/file/d/1PYLou8J1wcrme7Z2tx1wtA4GvaWnU1Jy/view?usp=sharing",
               columnNameForLabels = "Name", isStudyArea = FALSE, filename2 = NULL)

  ## Provincial Boundaries
  ml <- mapAdd(sim$canProvs, map = ml, layerName = "Provincial Boundaries",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               columnNameForLabels = "Name", isStudyArea = FALSE, filename2 = NULL)

  ################################################################################
  ## COMPANY-SPECIFIC STUDY AREAS
  dataDir <- file.path("inputs", "FMA_Boundaries")

  if (grepl("ANC", P(sim)$runName)) {
    ml <- fmaANC(ml, P(sim)$runName, dataDir, sim$canProvs)
  } else if (grepl("DMI", P(sim)$runName)) {
    ml <- fmaDMI(ml, P(sim)$runName, dataDir, sim$canProvs)
  } else if (grepl("LP", P(sim)$runName)) {
    ml <- fmaLP(ml, P(sim)$runName, dataDir, sim$canProvs)
  } else if (grepl("tolko", P(sim)$runName)) {
    ml <- fmaTolko(ml, P(sim)$runName, dataDir, sim$canProvs)
  } else {
    # Make a random small study area
    seed <- 863
    ranSeed <- .Random.seed
    set.seed(seed)
    sp2 <- Cache(SpaDES.tools::randomPolygon, ml[[studyAreaName]], 4e5) # was 4e5
    ml <- mapAdd(obj = sp2, map = ml, filename2 = FALSE,
                 #targetCRS = targetCRS,
                 layerName = "Small Study Area",
                 columnNameForLabels = "Name", isStudyArea = TRUE,
                 filename1 = NULL, poly = TRUE,
                 analysisGroupReportingPolygon = "Small Study Area"
    )
    # re-add the LandWeb polygon, but this time crop it to the Small Study Area
    ml <- mapAdd(layerName = "Small Study Area", map = ml,
                 #studyArea = studyArea(ml),
                 overwrite = TRUE, useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Small Study Area",
                 url = "https://drive.google.com/open?id=1JptU0R7qsHOEAEkxybx5MGg650KC98c6",
                 columnNameForLabels = "NSN", isStudyArea = TRUE, filename2 = NULL
    )
  }

  ##########################################################
  # LCC2005
  ##########################################################
  LCC2005 <- prepInputsLCC(studyArea = studyArea(ml), destinationPath = Paths$inputPath)
  ml <- mapAdd(LCC2005, layerName = "LCC2005", map = ml, filename2 = NULL, leaflet = FALSE,
               isRasterToMatch = TRUE, method = "ngb")

  ##########################################################
  # Current Conditions
  ##########################################################
  ccURL <- "https://drive.google.com/file/d/1JnKeXrw0U9LmrZpixCDooIm62qiv4_G1/view?usp=sharing"
  LandTypeFileCC <- file.path(Paths$inputPath, "LandType1.tif")
  sim$LandTypeCC <- Cache(prepInputs, LandTypeFileCC, studyArea = studyArea(ml),
                          url = ccURL, method = "ngb",
                          rasterToMatch = rasterToMatch(ml), filename2 = NULL)


  ################
  # Non Tree pixels
  ##################
  # Setting NA values
  # 3 is shrub, wetland, grassland -- no veg dynamics happen -- will burn in fire modules
  # 4 is water, rock, ice
  # 5 is no Data ... this is currently cropland -- will be treated as grassland for fires
  treeClassesCC <- c(0, 1, 2)
  nontreeClassesCC <- c(3, 4)
  treePixelsTF <- sim$LandTypeCC[] %in% treeClassesCC
  #nonTreePixels <- sim$LandTypeCC[] %in% nontreeClassesCC

  treeClassesLCC <- c(1:15, 34:35)
  treePixelsLCCTF <- ml$LCC2005[] %in% treeClassesLCC

  LandTypeCCNA <- is.na(sim$LandTypeCC[])
  noDataPixels <- LandTypeCCNA | sim$LandTypeCC[] == 5
  noDataPixelsLCC <- is.na(ml$LCC2005[]) | ml$LCC2005[] == 0

  treePixels <- which(treePixelsTF)
  treePixelsLCCTF[!noDataPixels] <- NA
  treePixelsLCC <- which(treePixelsLCCTF)

  treePixelsCombined <- unique(c(treePixels, treePixelsLCC))
  nonTreePixels <- seq(ncell(ml$LCC2005))
  nonTreePixels <- nonTreePixels[!nonTreePixels %in% treePixelsCombined]

  sim$nonTreePixels <- nonTreePixels

  # Update rasterToMatch layer with all trees
  ml[[ml@metadata[ml@metadata$rasterToMatch == 1, ]$layerName]][sim$nonTreePixels] <- NA

  fname_age <- "Age1.tif"
  TSFLayerName <- "CC TSF"
  ml <- mapAdd(map = ml, url = ccURL, layerName = TSFLayerName, CC = TRUE,
               tsf = file.path(Paths$inputPath, fname_age), analysisGroup1 = "CC",
               targetFile = fname_age, filename2 = NULL,
               useCache = TRUE, isRasterToMatch = FALSE,
               alsoExtract = "similar", leaflet = FALSE)


  ########################################################################
  # Age from KNN
  ########################################################################

  standAgeMapFilename <- "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.tif"
  standAgeMap <- Cache(prepInputs, #notOlderThan = Sys.time(),
                       targetFile = standAgeMapFilename,
                       archive = asPath(c("kNN-StructureStandVolume.tar",
                                          "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.zip")),
                       destinationPath = Paths$inputPath,
                       url = "http://tree.pfc.forestry.ca/kNN-StructureStandVolume.tar",
                       fun = "raster::raster",
                       studyArea = studyArea(ml),
                       rasterToMatch = rasterToMatch(ml),
                       maskWithRTM = TRUE,
                       method = "bilinear",
                       datatype = "INT2U",
                       filename2 = NULL, overwrite = TRUE,
                       userTags = c("stable", currentModule(sim)))
  ml[[TSFLayerName]][noDataPixels] <- standAgeMap[noDataPixels]
  ml[[TSFLayerName]][sim$nonTreePixels] <- NA

  ##########################################################
  # Clean up the study area
  ##########################################################
  studyArea(ml) <- polygonClean(studyArea(ml), type = P(sim)$runName, minFRI = P(sim)$minFRI)

  ##########################################################
  # Flammability and Fire Return Interval maps
  ##########################################################

  ## flammability map shouldn't be masked (no gaps!);
  #    NAs outside the buffered study & snow/rock/ice area
  #    the only values we want NA
  #    use the LCC flammability map to fill in NA / nodata values

  # No data class is 5 -- these will be filled in by LCC2005 layer
  # NA_ids <- which(is.na(sim$LandTypeCC[]) | sim$LandTypeCC[] == 5)
  # Only class 4 is considered non-flammable
  rstFlammableCC <- defineFlammable(sim$LandTypeCC, nonFlammClasses = 4,
                                    mask = NULL, filename2 = NULL)
  rstFlammableCC <- deratify(rstFlammableCC, complete = TRUE)

  #LandTypeFileLCC <- file.path(Paths$inputPath, "LCC2005_V1_4a.tif")
  # Only classes 36, 37, 38, 39 is considered non-flammable
  rstFlammableLCC <- defineFlammable(LCC2005, nonFlammClasses = 36:39, mask = NULL, filename2 = NULL)
  rstFlammableLCC <- deratify(rstFlammableLCC, complete = TRUE)

  #rstFlammableLCC <- Cache(prepInputs, LandTypeFileLCC, studyArea = studyArea(ml),
  #                         url = ccURL, method = "ngb",
  #                         rasterToMatch = rasterToMatch(ml), filename2 = NULL) %>%
  #  defineFlammable(., nonFlammClasses = 36:39, mask = NULL, filename2 = NULL)

  sim$rstFlammable <- rstFlammableCC
  sim$rstFlammable[LandTypeCCNA] <- rstFlammableLCC[LandTypeCCNA]

  ## fireReturnInterval needs to be masked by rstFlammable
  rstFireReturnInterval <- fasterize::fasterize(sf::st_as_sf(studyArea(ml)),
                                                raster = rasterToMatch(ml),
                                                field = "fireReturnInterval")
  #rtm <- rasterToMatch(studyArea(ml), rasterToMatch = rasterToMatch(ml))
  #rstFireReturnInterval <- Cache(postProcess, rtm, maskvalue = 0L, filename2 = NULL)
  ml <- mapAdd(rstFireReturnInterval, layerName = "fireReturnInterval", filename2 = NULL,
               map = ml, leaflet = FALSE, maskWithRTM = FALSE)

  #fireReturnInterval <- factorValues2(ml$fireReturnInterval,
  #                                    ml$fireReturnInterval[],
  #                                    att = "fireReturnInterval")

  if (grepl("doubleFRI", P(sim)$runName))
    ml$fireReturnInterval <- 2 * ml$fireReturnInterval

  #ml$fireReturnInterval <- raster(ml$fireReturnInterval) # blank out values for new, non-factor version
  #ml$fireReturnInterval[] <- fireReturnInterval
  # ml@metadata[layerName == "LCC2005", rasterToMatch := NA]

  sim$studyArea <- studyArea(ml, 3)
  sim$studyAreaLarge <- studyArea(ml, 1)
  sim$studyAreaReporting <- studyArea(ml, 2)
  sim$rasterToMatch <- rasterToMatch(ml)
  #sim$rasterToMatch[sim$nonTreePixels] <- NA

  sim$fireReturnInterval <- ml$fireReturnInterval # no NAing here because this needs only

  sim$LCC2005 <- ml$LCC2005
  #sim$LCC2005[sim$nonTreePixels] <- NA
  #LCC2005 <- sim$LCC2005
  #LCC2005[sim$nonTreePixels] <- NA

  sim[[TSFLayerName]] <- ml[[TSFLayerName]]

  sim$rasterToMatchReporting <- postProcess(rasterToMatch(ml),
                                            studyArea = studyArea(ml, 2),
                                            filename2 = NULL) # this is the small one

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere("canProvs", sim))
    sim$canProvs <- getData("GADM", country = "CAN", level = 1, path = dPath)

  return(sim)
}
