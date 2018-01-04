
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "landWebDataPrep",
  description = "A module that prepares data for LandWeb NRV project",
  keywords = c("Data Preparation"),
  authors = c(person(c("Yong", "Luo"), email = "yong.luo@canada.ca", role = c("aut", "cre")),
              person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut"))),
  childModules = character(0),
  version = numeric_version("1.3.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "landWebDataPrep.Rmd"),
  reqdPkgs = list("data.table", "raster", "sp", "magrittr"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "any", c(".inputObjects", "init"), NA, NA, "This describes the simulation time interval between save events")    
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "shpStudyRegionFull", objectClass = "SpatialPolygonsDataFrame",
                 desc = "this shape file contains two informaton: Full study areawith fire return interval attribute", 
                 #sourceURL = "https://ln.sync.com/dl/4c0ccab80#txwgbma8-d7ta56ar-4pf8rp5d-icpis7ga"), # i guess this is study area and fire return interval
                 sourceURL = ""), # i guess this is study area and fire return interval
    expectsInput(objectName = "shpStudySubRegion", objectClass = "SpatialPolygonsDataFrame",
                 desc = "this shape file contains two informaton: Subset of the study area, with fire return interval attribute", 
                 sourceURL = ""), # i guess this is study area and fire return interval
    expectsInput(objectName = "biomassMap", objectClass = "RasterLayer", 
                 desc = "total biomass raster layer in study area, default is canada national biomass map", 
                 sourceURL = "http://tree.pfc.forestry.ca/kNN-StructureBiomass.tar"),
    expectsInput(objectName = "LCC2005", objectClass = "RasterLayer", 
                 desc = "2005 land classification map in study area, default is canada national land classification in 2005", 
                 sourceURL = "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip"),
    expectsInput(objectName = "prepareIt", objectClass = "Function",
                  desc = "Function that is an omnibus for loading objects from internet, cropping etc."),
    expectsInput(objectName = "smallNamify", objectClass = "Function",
                  desc = "Function prepends 'Small' to object names")
    
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    # input for LBMR
    createsOutput(objectName = "studyArea", objectClass = "SpatialPolygons",
                 desc = "output as input for Boreal_LBMRDataPrep"),
    createsOutput(objectName = "calibrate", objectClass = "logical",
                  desc = "output as input for Boreal_LBMRDataPrep"),
    createsOutput(objectName = "LCC2005", objectClass = "RasterLayer", 
                  desc = "output as input for Boreal_LBMRDataPrep"),
    createsOutput(objectName = "shpStudySubRegion", objectClass = "SpatialPolygonsDataFrame", 
                 desc = "output as input for initBaseMaps")#,
    # createsOutput(objectName = "LCC05X", objectClass = "RasterLayer", 
    #              desc = "output as input for initBaseMaps")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.landWebDataPrep = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)
    
    # do stuff for this event
    sim <- Init(sim)
    
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
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  projection(sim$LCC2005) <- projection(sim$biomassMap)
  sim$studyArea <- spTransform(sim$shpStudySubRegion, crs(sim$biomassMap))
  sim$shpStudySubRegion <- sim$studyArea
  # sim$LCC05X <- sim$LCC2005
  sim$calibrate <- FALSE
  
  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
Plot <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot("object")
  
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
  
  dataPath <- file.path(modulePath(sim), "landWebDataPrep", "data")
  biomassMapFilename <- file.path(dataPath, "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.tif")
  lcc2005Filename <- file.path(dataPath, "LCC2005_V1_4a.tif")
  
  #if(!identical(crsUsed, crs(sim$shpStudyRegionFull)))
  #  sim$shpStudyRegionFull <- spTransform(sim$shpStudyRegionFull, crsUsed) #faster without Cache
  
  if(is.null(sim$biomassMap)) {
    sim$biomassMap <- Cache(prepareIt, 
                            tarfileName = "kNN-StructureBiomass.tar",
                            untarfileNames = asPath("NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.zip"),
                            spatialObjectFilename = biomassMapFilename,
                            dataPath = dataPath, #rasterToMatch = sim$standAgeMap,
                            studyArea = sim$shpStudySubRegion,
                            userTags = "stable",
                            modulePath = modulePath(sim),
                            moduleName = "landWebDataPrep")
  }
  
  if(is.null(sim$LCC2005)) {
    sim$LCC2005 <- Cache(prepareIt, 
                         zipfileName = asPath("LandCoverOfCanada2005_V1_4.zip"),
                         spatialObjectFilename = lcc2005Filename,
                         dataPath = dataPath, rasterToMatch = sim$biomassMap,
                         studyArea = sim$shpStudySubRegion,
                         userTags = "stable",
                         modulePath = modulePath(sim),
                         moduleName = "landWebDataPrep")
    projection(sim$LCC2005) <- projection(sim$biomassMap)
  }
  
  
  
  # If there is no study Area, use a random one
  if(is.null(sim$shpStudyRegionFull)) {
    sim$shpStudyRegionFull <- SpaDES.tools::randomPolygon(cbind(-110, 59), 1e5) # somewhere in N-Central Canada
  }
  if(is.null(sim$shpStudySubRegion)) {
    sim$shpStudySubRegion <- sim$shpStudyRegionFull
  }
  
  sim$prepareIt <- prepareIt
  sim$smallNamify <- smallNamify
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above

prepareIt <- function(tarfileName = NULL, untarfileNames = NULL,  
                                                     zipfileName = untarfileNames, 
                                                     zipExtractFolder = NULL, spatialObjectFilename, 
                                                     dataPath, rasterToMatch = NULL,
                                                     studyArea, rasterDatatype = "INT2U", 
                                                     modulePath, moduleName = "Boreal_LBMRDataPrep",
                      notOlderThan = NULL) {

  message("Preparing: ", basename(spatialObjectFilename))
  
  if(!isAbsolutePath(spatialObjectFilename)) spatialObjectFilename <- file.path(dataPath, spatialObjectFilename)
  
  # Test whether dataPath has been updated with file deletions or additions or whatever, 
  #  If it has changed, then Cache of checksums will be rerun
  bb <- capture.output(aa <- Cache(file.info, asPath(dir(dataPath, full.names = TRUE))), type = "message")
  if (length(bb) == 0){#isTRUE(!grepl(bb, pattern = "loading cached"))) {
    notOlderThan <- Sys.time()
  } else {
    notOlderThan <- NULL
  }
  checksum <- data.table(Cache(checksums, module = moduleName, path = modulePath, write = FALSE,
                               checksumFile = asPath(file.path(modulePath, moduleName, "data", "CHECKSUMS.txt")), 
                               digestPathContent = TRUE, 
                               quickCheck = .quickCheck, notOlderThan = notOlderThan))
  # Work outwards from final step, penultimate step, 3rd from last step etc.
  #  In prinicple, the steps are 
  #  1. Download
  #  2. Unpack (tar or zip or both)
  #  3. Load object into R
  #  4. Perform geographic steps -- crop, reproject, mask
  needRawObject <- !isTRUE(compareNA(checksum[grepl(expectedFile, pattern = paste0("^",basename(spatialObjectFilename),"$")), result], "OK"))
  if(needRawObject) {
    # Try untar
    untarIt(tarfileName, checksum, moduleName, modulePath, untarfileNames, 
                         zipfileName, dataPath, spatialObjectFilename, .quickCheck)
    # Try unzip
    unzipIt(zipfileName, checksum, moduleName, modulePath, spatialObjectFilename, 
                         zipExtractFolder, dataPath, .quickCheck)
  }
  objs <- cropReprojectIt(spatialObjectFilename, rasterToMatch, studyArea)
  obj <- maskIt(objs$obj, spatialObjectFilename, objs$studyArea)
  obj <- writeIt(obj, spatialObjectFilename, smallNamify(spatialObjectFilename), rasterDatatype)  
  return(obj)
}

smallNamify <- function(name) {
  file.path(dirname(name), paste0("Small", basename(name)))
}


untarIt <- function(tarfileName = NULL, checksum = NULL, moduleName = NULL, modulePath = NULL,
                                 untarfileNames = NULL, zipfileName = NULL, 
                                 dataPath = NULL, spatialObjectFilename = NULL, .quickCheck = FALSE) {
  if(!is.null(tarfileName)) {
    needTar <- !all(compareNA(checksum[grepl(expectedFile, pattern = tarfileName), result], "OK"))
    if(needTar) {
      ee <- data.table(Cache(downloadData, module = moduleName, path = modulePath, quickCheck = .quickCheck))
      if(!all(compareNA(ee[grepl(expectedFile, pattern = tarfileName), result], "OK"))) {
        warning("The version downloaded of ", tarfileName, " does not match the checksums")
      }
    }
    if(is.null(untarfileNames)) untarfileNames <- zipfileName
    if(all((!file.exists(file.path(dataPath, basename(untarfileNames)))))) {
      message("  Untarring ", tarfileName)
      untar(file.path(dataPath, tarfileName),
            files = untarfileNames,
            exdir = dataPath, tar = "internal")
    }
  } 
  
}

unzipIt <- function(zipfileName = NULL, checksum = NULL, moduleName = NULL, modulePath = NULL,
                                 spatialObjectFilename = NULL, zipExtractFolder = NULL,
                                 dataPath = NULL, .quickCheck = FALSE) 
  {
  
    if(!is.null(zipfileName)) {
      if(!isTRUE(compareNA(checksum[grepl(expectedFile, pattern = zipfileName), result], "OK")))  {
        ee <- data.table(Cache(downloadData, module = moduleName, path = modulePath, quickCheck = .quickCheck))
        if(!isTRUE(compareNA(ee[grepl(expectedFile, pattern = zipfileName), result], "OK"))) {
          warning("The version downloaded of ", zipfileName, " does not match the checksums")
        }
      }
      if (!isTRUE(compareNA(checksum[grepl(expectedFile, pattern = basename(spatialObjectFilename)), result], "OK"))) {
        message("  Unzipping ", zipfileName)
        unzip(file.path(dataPath, zipfileName), exdir = dataPath)
        if(!is.null(zipExtractFolder)) {
          filenames <- dir(file.path(dataPath, zipExtractFolder))
          file.copy(from = file.path(dataPath, zipExtractFolder, filenames),
                    to = file.path(dataPath, filenames),
                    overwrite = TRUE)
          unlink(file.path(dataPath, zipExtractFolder), recursive = TRUE)
          rm(filenames)
        }
      }
    }
  }

cropReprojectIt <- function(spatialObjectFilename, rasterToMatch, studyArea) {
  if(grepl(".shp", spatialObjectFilename)) {
    message("  Cropping, reprojecting")
    a <- Cache(raster::shapefile, asPath(spatialObjectFilename), digestPathContent = TRUE)
    crsUsed <- if(is.null(studyArea)) crs(a) else crs(studyArea)
    if(!suppressWarnings(rgeos::gIsValid(a))) b1 <- Cache(buffer, a, dissolve = FALSE, width = 0) else b1 <- a
    b2 <- SpatialPolygonsDataFrame(b1, data = as.data.frame(a))
    b3 <- Cache(spTransform, b2, crsUsed)
    b <- Cache(raster::crop, b3, studyArea)
    if (!is.null(rasterToMatch)) {
      if(!identical(crs(b3), crs(rasterToMatch))) {
        b3 <- Cache(spTransform, b3, crs(rasterToMatch))
      }
      b <- Cache(raster::crop, b3, rasterToMatch)
    }
  } else if (grepl(".tif", spatialObjectFilename)) {
    message("  Cropping, reprojecting")
    b <- raster::raster(spatialObjectFilename)
    crsUsed <- if(is.null(rasterToMatch)) as.character(crs(b)) else as.character(crs(rasterToMatch))
    if(!identical(crs(b), crs(studyArea))) studyArea <- spTransform(studyArea, crs(b))
    if(!identical(crs(b), CRS(crsUsed))) {
      #studyAreaExtentCRSUsed <- projectExtent(raster(extent(studyArea), crs = crs(studyArea)), crs = CRS(crsUsed))
      b <- Cache(crop, b, studyArea)
      b <- Cache(projectRaster, from = b, to = rasterToMatch, method = "bilinear")
    } else {
      b <- Cache(crop, b, studyArea)
    }
  }
  return(list(obj = b, studyArea = studyArea))
}

maskIt <- function(obj, spatialObjectFilename, studyArea) {
  message("  Masking")
  if(grepl(".shp", spatialObjectFilename)) {
    
  } else if (grepl(".tif", spatialObjectFilename)) {
    obj <- fastMask(obj, studyArea)
  }
  obj
}

writeIt <- function(obj, spatialObjectFilename = NULL, smallSOF = NULL, rasterDatatype = NULL) {
  message("  writing to disk")
  if(grepl(".shp", spatialObjectFilename)) {
    # write small, cropped object to disk
    shapefile(obj, smallSOF, overwrite = TRUE)
  } else if (grepl(".tif", spatialObjectFilename)) {
    obj <- writeRaster(obj, overwrite=TRUE, format = "GTiff", datatype = rasterDatatype,
                     filename = smallSOF)
  }
  obj
  
}
