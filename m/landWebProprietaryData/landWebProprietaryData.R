# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "landWebProprietaryData",
  description = NA, #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9006", landWebProprietaryData = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "landWebProprietaryData.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "logical", TRUE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant"),
    defineParameter(name = "useParallel", class = "numeric", default = parallel::detectCores(),
                    desc = "Used in reading csv file with fread. Will be passed to data.table::setDTthreads")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "biomassMap", objectClass = "RasterLayer",
                 desc = "total biomass raster layer in study area, default is Canada national biomass map",
                 sourceURL = "http://tree.pfc.forestry.ca/kNN-StructureBiomass.tar"),
    expectsInput(objectName = "specieslayers", objectClass = "RasterStack",
                 desc = "biomass percentage raster layers by species in Canada species map",
                 sourceURL = "http://tree.pfc.forestry.ca/kNN-Species.tar"),
    expectsInput(objectName = "shpStudySubRegion", objectClass = "SpatialPolygonsDataFrame",
                 desc = "this shape file contains two informaton: Sub study area with fire return interval attribute",
                 sourceURL = ""),
    expectsInput(objectName = "shpStudyRegionFull", objectClass = "SpatialPolygonsDataFrame",
                 desc = "this shape file contains two informaton: Full study area with fire return interval attribute",
                 sourceURL = ""), # i guess this is study area and fire return interval
    expectsInput(objectName = "SPP_1990_100m_NAD83_LCC_BYTE_VEG_NO_TIES_FILLED_FINAL.zip", objectClass = "RasterStack",
                 desc = "biomass percentage raster layers by species in Canada species map, created by Pickell et al, UBC, resolution 100m x 100m from LandSat and KNN based on CASFRI",
                 sourceURL = "https://drive.google.com/open?id=1M_L-7ovDpJLyY8dDOxG3xQTyzPx2HSg4"),
    expectsInput(objectName = "CASFRI for Landweb.zip", objectClass = "RasterStack",
                 desc = "biomass percentage raster layers by species in Canada species map, created by Pickell et al, UBC, resolution 100m x 100m from LandSat and KNN based on CASFRI",
                 sourceURL = "https://drive.google.com/file/d/1y0ofr2H0c_IEMIpx19xf3_VTBheY0C9h/view?usp=sharing")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "specieslayers", objectClass = "RasterStack",
                 desc = "biomass percentage raster layers by species in Canada species map")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.landWebProprietaryData = function(sim, eventTime, eventType) {
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

### template initialization
Init <- function(sim) {
  ## load Paul Pickell et al. and CASFRI
  if (!exists("sessionCacheFile")) {
    sessionCacheFile <<- tempfile()
  }
  isKnownUser <- (grepl("emcintir", Sys.info()["user"]))
  .cacheVal <<- if (grepl("VIC-A", Sys.info()["nodename"])) {
    sessionCacheFile 
  } else if (isKnownUser) {
    oauthFilePath <- file.path(modulePath(sim), "..", ".httr-oauth")
    options(httr_oauth_cache = oauthFilePath)
    oauthFilePath
  } else {
    FALSE
  }

  aaa <- testthat::capture_error({
    googledrive::drive_auth(use_oob = TRUE, verbose = TRUE, cache = .cacheVal)
    file_url <- "https://drive.google.com/file/d/1sJoZajgHtsrOTNOE3LL8MtnTASzY0mo7/view?usp=sharing"
    googledrive::drive_download(googledrive::as_id(file_url), path = tempfile(),
                                overwrite = TRUE, verbose = TRUE)
  })
  if (is.null(aaa)) { # means got the file
    dPath <- dataPath(sim)
    message("  Loading CASFRI and Pickell et al. layers")
    Paul <- Cache(prepInputs,
                  targetFile = asPath("SPP_1990_100m_NAD83_LCC_BYTE_VEG_NO_TIES_FILLED_FINAL.dat"),
                  archive = asPath("SPP_1990_100m_NAD83_LCC_BYTE_VEG_NO_TIES_FILLED_FINAL.zip"),
                  alsoExtract = asPath("SPP_1990_100m_NAD83_LCC_BYTE_VEG_NO_TIES_FILLED_FINAL.hdr"),
                  destinationPath = asPath(dPath),
                  fun = "raster",
                  pkg = "raster",
                  studyArea = sim$shpStudySubRegion,
                  rasterToMatch = sim$biomassMap,
                  rasterInterpMethod = "bilinear",
                  rasterDatatype = "INT2U",
                  writeCropped = TRUE,
                  cacheTags = c("stable", currentModule(sim)))#, notOlderThan = Sys.time())

    CASFRITifFile <- asPath(file.path(dPath, "Landweb_CASFRI_GIDs.tif"))
    CASFRIattrFile <- asPath(file.path(dPath, "Landweb_CASFRI_GIDs_attributes3.csv"))
    CASFRIheaderFile <- asPath(file.path(dPath,"Landweb_CASFRI_GIDs_README.txt"))
    CASFRIRas <- Cache(prepInputs,
                       targetFile = asPath("Landweb_CASFRI_GIDs.tif"),
                       archive = asPath("CASFRI for Landweb.zip"),
                       alsoExtract = c(CASFRITifFile, CASFRIattrFile, CASFRIheaderFile),
                       destinationPath = asPath(dPath),
                       fun = "raster",
                       pkg = "raster",
                       studyArea = sim$shpStudySubRegion,
                       rasterToMatch = sim$biomassMap,
                       rasterInterpMethod = "bilinear",
                       rasterDatatype = "INT2U",
                       writeCropped = TRUE,
                       cacheTags = c("stable", currentModule(sim)))

    message("Load CASFRI data and headers, and convert to long format, and define species groups")
    if (P(sim)$useParallel > 1) data.table::setDTthreads(P(sim)$useParallel)
    loadedCASFRI <- Cache(loadCASFRI, CASFRIRas, CASFRIattrFile, CASFRIheaderFile,
                          #debugCache = "complete", 
                          userTags = "BigDataTable")

    message("Make stack of species layers from Paul's layer")
    uniqueKeepSp <- unique(loadedCASFRI$keepSpecies$spGroup)
    # "Abie_sp"  "Betu_pap" "Lari_lar" "Pice_gla" "Pice_mar" "Pinu_sp" "Popu_tre"
    PaulSpStack <- Cache(makePaulStack, paths = lapply(paths(sim), basename),
                         PaulRaster = Paul, uniqueKeepSp)
    crs(PaulSpStack) <- crs(sim$biomassMap) # bug in writeRaster

    message('Make stack from CASFRI data and headers')
    CASFRISpStack <- Cache(CASFRItoSpRasts, CASFRIRas, loadedCASFRI)

    message("Overlay Paul and CASFRI stacks")
    outStack <- Cache(overlayStacks, CASFRISpStack, PaulSpStack,
                      outputFilenameSuffix = "CASFRI_PAUL")#, notOlderThan = Sys.time())
    crs(outStack) <- crs(sim$biomassMap) # bug in writeRaster

    message("Overlay Paul_CASFRI with open data set stacks")
    specieslayers2 <- Cache(overlayStacks, outStack, sim$specieslayers,
                            outputFilenameSuffix = "CASFRI_PAUL_KNN")
    crs(specieslayers2) <- crs(sim$biomassMap)
    sim$specieslayers <- specieslayers2
    message("Using LandWeb datasets from Paul Pickell and CASFRI")
  }
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  dPath <- dataPath(sim)
  if (!suppliedElsewhere("biomassMap", sim)) {
    biomassMapFilename <- file.path(dPath, "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.tif")
    sim$biomassMap <- Cache(prepInputs,
                            targetFile = biomassMapFilename,
                            archive = asPath(c("kNN-StructureBiomass.tar",
                                               "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.zip")),
                            destinationPath = asPath(dPath),
                            studyArea = sim$shpStudySubRegion,
                            rasterInterpMethod = "bilinear",
                            rasterDatatype = "INT2U",
                            writeCropped = TRUE,
                            cacheTags = c("stable", currentModule(sim)))
  }
  
  if (!suppliedElsewhere("shpStudySubRegion")) {
    stop("shpStudySubRegion is required. Please supply a polygon of the study area")
  }

  return(invisible(sim))
}
