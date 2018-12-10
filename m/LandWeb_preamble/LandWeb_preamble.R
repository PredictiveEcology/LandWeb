defineModule(sim, list(
  name = "LandWeb_preamble",
  description = "define FMA-specific study areas etc. for LandWeb",
  keywords = c("LandWeb"),
  authors = c(
    person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
    person("Alex M", "Chubaty", email = "alex.chubaty@gmail.com", role = c("aut"))
  ),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.3.9006", LandWeb_preamble = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "LandWeb_preamble.Rmd"),
  reqdPkgs = list("PredictiveEcology/map@development",
                  "PredictiveEcology/pemisc@development",
                  "raster", "SpaDES.tools"),
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
  ),
  outputObjects = bind_rows(
    createsOutput("CC TSF", "RasterLayer", desc = NA),
    createsOutput("fireReturnInterval", "RasterLayer", desc = NA),
    createsOutput("LandTypeCC", "RasterLayer", desc = NA),
    createsOutput("LCC2005", "RasterLayer", desc = NA),
    createsOutput("nonVegPixels", "integer", desc = NA),
    createsOutput("rasterToMatch", "RasterLayer", desc = NA),
    createsOutput("rstFlammable", "RasterLayer", desc = NA),
    createsOutput("studyArea", "SpatialPolygonsDataFram", desc = NA),
    createsOutput("studyAreaLarge", "SpatialPolygonsDataFram", desc = NA)
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

  studyAreaName <- "LandWeb Study Area"
  ml <- mapAdd(layerName = studyAreaName,
               targetCRS = targetCRS, overwrite = TRUE,
               url = "https://drive.google.com/open?id=1JptU0R7qsHOEAEkxybx5MGg650KC98c6", # This is landweb_ltfc_v6.shp
               columnNameForLabels = "NSN", isStudyArea = TRUE, filename2 = NULL
  )

  if (grepl("testing", P(sim)$runName)) {
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
    # create rasterToMatch from LCC layer
  }

  ################################################################################
  ## COMPANY-SPECIFIC STUDY AREAS

  dataDir <- file.path("inputs", "FMA_Boundaries")
  dataDirDMI <- file.path(dataDir, "DMI")
  dataDirLP <- file.path(dataDir, "LP")
  dataDirTolko <- file.path(dataDir, "Tolko")

  ### ADMINISTRATIVE POLYGONS
  if (grepl("tolko_AB_N", P(sim)$runName)) {
    studyAreaName <- "Tolko AB North SR"

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    tolko_ab_n_sr <- shapefile(file.path(dataDirTolko, "Tolko_AB_N_SR.shp"))
    ml <- mapAdd(tolko_ab_n_sr, ml, isStudyArea = TRUE, layerName = studyAreaName,
                 useSAcrs = TRUE, poly = TRUE,
                 columnNameForLabels = "NSN", filename2 = NULL)

    ## reportingPolygons
    tolko_ab_n <- shapefile(file.path(dataDirTolko, "Tolko_AB_N.shp"))
    tolko_ab_n.ansr <- shapefile(file.path(dataDirTolko, "Tolko_AB_N_ANSR.shp"))
    tolko_ab_n.caribou <- shapefile(file.path(dataDirTolko, "Tolko_AB_N_caribou.shp"))

    ml <- mapAdd(tolko_ab_n, ml, layerName = "Tolko AB North", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Tolko AB North",
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(tolko_ab_n.ansr, ml, layerName = "Tolko AB North ANSR", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Tolko AB North ANSR",
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(tolko_ab_n.caribou, ml, layerName = "Tolko AB North Caribou", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Tolko AB North Caribou",
                 columnNameForLabels = "Name", filename2 = NULL)
  } else if (grepl("tolko_AB_S", P(sim)$runName)) {
    studyAreaName <- "Tolko AB South SR"

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    tolko_ab_s_sr <- shapefile(file.path(dataDirTolko, "Tolko_AB_S_SR.shp"))
    ml <- mapAdd(tolko_ab_s_sr, ml, isStudyArea = TRUE, layerName = studyAreaName,
                 useSAcrs = TRUE, poly = TRUE,
                 columnNameForLabels = "NSN", filename2 = NULL)

    ## reportingPolygons
    tolko_ab_s <- shapefile(file.path(dataDirTolko, "Tolko_AB_S.shp"))
    tolko_ab_s.ansr <- shapefile(file.path(dataDirTolko, "Tolko_AB_S_ANSR.shp"))
    tolko_ab_s.caribou <- shapefile(file.path(dataDirTolko, "Tolko_AB_S_caribou.shp"))

    ml <- mapAdd(tolko_ab_s, ml, layerName = "Tolko AB South", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Tolko AB South",
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(tolko_ab_s.ansr, ml, layerName = "Tolko AB South ANSR", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Tolko AB South ANSR",
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(tolko_ab_s.caribou, ml, layerName = "Tolko AB South Caribou", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Tolko AB South Caribou",
                 columnNameForLabels = "Name", filename2 = NULL)
  } else if (grepl("tolko_SK", P(sim)$runName)) {
    studyAreaName <- "Tolko SK SR"
    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    tolko_sk_sr <- shapefile(file.path(dataDirTolko, "Tolko_SK_SR.shp"))
    ml <- mapAdd(tolko_sk_sr, ml, isStudyArea = TRUE, layerName = studyAreaName,
                 useSAcrs = TRUE, poly = TRUE,
                 columnNameForLabels = "NSN", filename2 = NULL)

    ## reportingPolygons
    tolko_sk <- shapefile(file.path(dataDirTolko, "Tolko_SK.shp"))
    tolko_sk.caribou <- shapefile(file.path(dataDirTolko, "Tolko_SK_caribou.shp"))

    ml <- mapAdd(tolko_sk, ml, layerName = "Tolko SK", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Tolko SK",
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(tolko_sk.caribou, ml, layerName = "Tolko SK Caribou", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Tolko SK Caribou",
                 columnNameForLabels = "Name", filename2 = NULL)
  } else if (grepl("LP_MB", P(sim)$runName)) {
    studyAreaName <- "LP MB SR"
    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    lp_mb_sr <- shapefile(file.path(dataDirLP, "LP_MB_SR.shp"))
    ml <- mapAdd(lp_mb_sr, ml, isStudyArea = TRUE, layerName = studyAreaName,
                 useSAcrs = TRUE, poly = TRUE,
                 columnNameForLabels = "NSN", filename2 = NULL)

    ## reportingPolygons
    lp_mb <- shapefile(file.path(dataDirLP, "LP_MB.shp"))
    lp_mb.caribou <- shapefile(file.path(dataDirLP, "LP_MB_caribou.shp"))

    ml <- mapAdd(lp_mb, ml, layerName = "LP MB", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "LP MB",
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(lp_mb.caribou, ml, layerName = "LP MB Caribou", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "LP MB Caribou",
                 columnNameForLabels = "Name", filename2 = NULL)
  }

  ##########################################################
  # Current Conditions (Age Map)
  ##########################################################
  ccURL <- "https://drive.google.com/file/d/1JnKeXrw0U9LmrZpixCDooIm62qiv4_G1/view?usp=sharing"

  fname_age <- "Age1.tif"
  ml <- mapAdd(map = ml, url = ccURL, layerName = "CC TSF", CC = TRUE,
               tsf = file.path(Paths$inputPath, fname_age), analysisGroup1 = "CC",
               targetFile = fname_age, filename2 = NULL,
               useCache = TRUE, isRasterToMatch = TRUE,
               alsoExtract = "similar", leaflet = FALSE)

  ##########################################################
  # LCC2005
  ##########################################################
  LCC2005 <- pemisc::prepInputsLCC(studyArea = studyArea(ml), destinationPath = Paths$inputPath)
  ml <- mapAdd(LCC2005, layerName = "LCC2005", map = ml, filename2 = NULL, leaflet = FALSE,
               isRasterToMatch = FALSE, method = "ngb")

  ##########################################################
  # Clean up the study area
  ##########################################################
  studyArea(ml) <- pemisc::polygonClean(studyArea(ml), type = P(sim)$runName, minFRI = P(sim)$minFRI)

  ##########################################################
  # Flammability and Fire Return Interval maps
  ##########################################################

  ## flammability map shouldn't be masked (no gaps!); NAs outside the buffered study area allowed.
  ## use the LCC flammability map to fill in NA / nodata values
  LandTypeFileCC <- file.path(Paths$inputPath, "LandType1.tif")
  sim$LandTypeCC <- Cache(prepInputs, LandTypeFileCC, studyArea = studyArea(ml),
                      url = ccURL, method = "ngb",
                      rasterToMatch = rasterToMatch(ml), filename2 = NULL)
  NA_ids <- which(is.na(sim$LandTypeCC[]) | sim$LandTypeCC[] == 5)
  rstFlammableCC <- defineFlammable(sim$LandTypeCC, nonFlammClasses = c(1, 2, 4),
                                    mask = NULL, filename2 = NULL)

  LandTypeFileLCC <- file.path(Paths$inputPath, "LCC2005_V1_4a.tif")
  rstFlammableLCC <- Cache(prepInputs, LandTypeFileLCC, studyArea = studyArea(ml),
                           url = ccURL, method = "ngb",
                           rasterToMatch = rasterToMatch(ml), filename2 = NULL) %>%
    defineFlammable(., nonFlammClasses = c(36, 37, 38, 39), mask = NULL, filename2 = NULL)

  sim$rstFlammable <- rstFlammableCC
  sim$rstFlammable[NA_ids] <- rstFlammableLCC[NA_ids]

  ## fireReturnInterval needs to be masked by rstFlammable
  rtm <- rasterToMatch(studyArea(ml), rasterToMatch = rasterToMatch(ml))
  rstFireReturnInterval <- Cache(postProcess, rtm, maskvalue = 0L, filename2 = NULL)
  ml <- mapAdd(rstFireReturnInterval, layerName = "fireReturnInterval", filename2 = NULL,
               map = ml, leaflet = FALSE, maskWithRTM = TRUE)

  fireReturnInterval <- pemisc::factorValues2(ml$fireReturnInterval,
                                              ml$fireReturnInterval[],
                                              att = "fireReturnInterval")

  if (grepl("doubleFRI", P(sim)$runName))
    fireReturnInterval <- 2 * fireReturnInterval

  ml$fireReturnInterval <- raster(ml$fireReturnInterval) # blank out values for new, non-factor version
  ml$fireReturnInterval[] <- fireReturnInterval
  ml@metadata[layerName == "LCC2005", rasterToMatch := NA]

  sim$studyArea <- studyArea(ml, 2)
  sim$studyAreaLarge <- studyArea(ml, 1)
  sim$rasterToMatch <- rasterToMatch(ml)
  sim$fireReturnInterval <- ml$fireReturnInterval
  sim$LCC2005 <- ml$LCC2005
  sim$`CC TSF` <- ml$`CC TSF`
  # list2env(mget(ls(ml), envir = ml@.xData), envir = envir(sim))
  sim$nonVegPixels <- which(sim$LandTypeCC[] == 4)

  return(invisible(sim))
}

