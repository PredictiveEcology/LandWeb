loadPaulAndCASFRI <- function(paths, PaulRawFileName, existingSpeciesLayers,
                              CASFRITifFile, CASFRIattrFile, CASFRIheaderFile, 
                              .quickChecking = NULL) {
  message("  Rescanning for gdal")
  if (!("Windows" %in% Sys.info()["sysname"])) gdal_setInstallation(rescan = TRUE)
  message("  Finished rescanning for gdal")
  # Step 1 -- Load LandWeb study area shapefile
  origDir <- getwd(); on.exit(setwd(origDir), add = TRUE)
  oldfilename <- "shpLandWEB.shp"

  # make absolute paths
  paths <- lapply(paths, function(x) file.path(origDir, x))

  cpath <- asPath(paths$cachePath)
  dpath <- asPath(file.path(paths$modulePath, "Boreal_LBMRDataPrep", "data"))
  setwd(dpath)

  ## Paul's data
  message("Load Paul's study area layer from original source")
  PaulRawFileName <- basename(PaulRawFileName)
  zipDownload <- file.path(dpath, "SPP_1990_FILLED_100m_NAD83_LCC_BYTE_VEG.zip")

  ## TODO: what if they aren't downloaded ?? need to error-proof this function!!!
  if (!file.exists(PaulRawFileName))  {
    if (!file.exists(zipDownload)) {
      googledrive::drive_auth(cache = FALSE, use_oob = TRUE, verbose = TRUE)
      file_url <- "https://drive.google.com/file/d/0BxZrk9psrK4nTFVMY295WFFXZk0/view?usp=sharing"
      googledrive::drive_download(googledrive::as_id(file_url), path = zipDownload,
                                  overwrite = TRUE, verbose = TRUE)
    }
    unzip(zipDownload, exdir = dpath, overwrite = TRUE)
  }

  ## TODO: what if they aren't downloaded ?? need to error-proof this function!!!
  if (file.exists(PaulRawFileName)) {
    PaulOnGoogleDrive1 <- raster(PaulRawFileName)
    LandWebStudyAreaRawPoly <- file.path(paths$inputPath, oldfilename)

    PaulOnGoogleDrive <- Cache(writeRaster, PaulOnGoogleDrive1,
                               filename = asPath(file.path(dpath, "PaulSppFilled.tif")),
                               datatype = "INT2U",
                               overwrite = TRUE,
                               quick = .quickChecking,
                               cacheRepo = cpath)#, debugCache="quick")
    shpStudyRegionFull <- Cache(loadShpAndMakeValid, file = asPath(LandWebStudyAreaRawPoly),
                                cacheRepo = cpath)

    message("Make Study Area Mask polygon")
    #browser()
    studyAreaMask <- Cache(makeStudyAreaMask, PaulOnGoogleDrive,
                           maskFilename = asPath(file.path(dpath, "StudyAreaMask.tif")),
                           quick = .quickChecking,
                           cache_path = cpath, # cache path arg passed to makeStudyAreaMask
                           cacheRepo = cpath)  # cache path arg for Cache

    message("Make Study Area Mask polygon into 250m res")
    studyAreaMask250filename <- file.path(dpath, "StudyAreaMask250.tif")
    Cache(gdalwarp, overwrite = TRUE,
          dstalpha = TRUE,
          s_srs = as.character(crs(studyAreaMask)),
          t_srs = as.character(crs(studyAreaMask)),
          multi = TRUE, of = "GTiff",
          tr = c(250, 250),
          filename(studyAreaMask), ot = "Byte",
          basename(studyAreaMask250filename),
          cacheRepo = cpath)

    shapeFile <- file.path(dpath, "shpLandWeb5.shp")

    # TO get this to work
    #  https://stackoverflow.com/questions/5599872/python-windows-importerror-no-module-named-site
    message("Make Study Area Mask polygon shapefile")
    Cache(system, #notOlderThan = Sys.time(),
          paste("python", file.path(getOption("gdalUtils_gdalPath")[[1]]$path, "gdal_polygonize.py"),
                basename(studyAreaMask250filename), basename(shapeFile), "-f \"ESRI Shapefile\""),
          cacheRepo = cpath)

    message("Make smaller study area polygon shapefile, sized to Paul's layer")
    studyArea <- Cache(loadStudyArea, asPath(shapeFile), shpStudyRegionFull,
                       crsPaul = crs(PaulOnGoogleDrive),
                       cacheRepo = cpath)
    #write file
    Cache(shapefile, studyArea, filename = "studyArea.shp", overwrite = TRUE, cacheRepo = cpath)

    message("Convert Paul's 100m layer to 250m and mask to new study area polygon")
    Paul250MaskedFilename <- "PaulTrimmed.tif"
    Cache(gdalwarp, overwrite = TRUE, cutline = "studyArea.shp",
          dstalpha = TRUE,
          s_srs = as.character(crs(PaulOnGoogleDrive)),
          t_srs = as.character(crs(PaulOnGoogleDrive)),
          multi = TRUE, of = "GTiff",
          crop_to_cutline = TRUE, tr = c(250, 250),
          filename(PaulOnGoogleDrive), ot = "Byte",
          Paul250MaskedFilename,
          cacheRepo = cpath)

    if (!startsWith(Sys.info()["nodename"], prefix = "W-VIC-A105")) .gc()
  }

  ## CASFRI data
  message("Starting CASFRI stuff")
  CASFRITifFile <- file.path(dpath, "Landweb_CASFRI_GIDs.tif")

  PaulRawFileName <- basename(PaulRawFileName)
  zipDownload <- file.path(dpath, "CASFRI for Landweb.zip")

  ## TODO: what if they aren't downloaded ?? need to error-proof this function!!!
  if (!file.exists(CASFRITifFile)) {
    if (!file.exists(zipDownload)) {
      googledrive::drive_auth(cache = FALSE, use_oob = TRUE, verbose = TRUE)
      file_url <- "https://drive.google.com/file/d/1nhZwBKR1dJASFtZinO28yt0RVaCI-NOz/view?usp=sharing"
      googledrive::drive_download(googledrive::as_id(file_url), path = zipDownload,
                                  overwrite = TRUE, verbose = TRUE)
    }
    unzip(zipDownload, exdir = dpath, overwrite = TRUE)
  }

  ## TODO: what if they aren't downloaded ?? need to error-proof this function!!!
  if (file.exists(CASFRITifFile)) {
    CASFRIRas <- raster(CASFRITifFile)

    CASFRIStudyAreaFilename <- gsub(filename(CASFRIRas), pattern = "\\.tif",
                                    replacement = "StudyArea.tif")

    message("Convert CASFRI 100m layer to 250m and mask to new study area polygon")
    Cache(gdalwarp, overwrite = TRUE, cutline = "studyArea.shp",
          s_srs = as.character(crs(CASFRIRas)),
          t_srs = as.character(crs(PaulOnGoogleDrive)),
          dstalpha = TRUE,tr = c(250, 250),
          multi = TRUE, of = "GTiff",
          crop_to_cutline = TRUE,
          basename(filename(CASFRIRas)), #ot = "Byte",
          basename(CASFRIStudyAreaFilename),
          cacheRepo = cpath)
    CASFRIRas <- raster(CASFRIStudyAreaFilename)

    message("Load CASFRI data and headers, and convert to long format, and define species groups")
    loadedCASFRI <- Cache(loadCASFRI, CASFRIRas, asPath(basename(CASFRIattrFile)),
                          asPath(basename(CASFRIheaderFile)),
                          cacheRepo = cpath, digestPathContent = TRUE)

    message("Make stack of species layers from Paul's layer")
    uniqueKeepSp <- unique(loadedCASFRI$keepSpecies$spGroup)
    # "Abie_sp"  "Betu_pap" "Lari_lar" "Pice_gla" "Pice_mar" "Pinu_sp" "Popu_tre"
    PaulSpStack <- Cache(makePaulStack, paths = paths, asPath(Paul250MaskedFilename),
                         uniqueKeepSp, cacheRepo = cpath)
    message('Make stack from CASFRI data and headers')
    #spRas <- raster(CASFRIRas) %>% setValues(., NA_integer_)
    #dd <- CASFRItoSpRasts(spRas, loadedCASFRI)

    #CASFRISpStack <- CASFRItoSpRasts(paths = paths, spRas, loadedCASFRI)
    browser()
    CASFRISpStack <- Cache(CASFRItoSpRasts, cachePath = cpath,
                           CASFRIRas, loadedCASFRI, cacheRepo = cpath)

    message("Overlay Paul and CASFRI stacks")
    outStack <- Cache(overlayStacks, CASFRISpStack, PaulSpStack, cachePath = cpath,
                      outputFilenameSuffix = "CASFRI_PAUL",
                      cacheRepo = cpath)#, notOlderThan = Sys.time())

    message("Overlay Paul and CASFRI stack with low quality open source stack")
    outStack2 <- Cache(overlayStacks, outStack, existingSpeciesLayers, 
                       cachePath = cpath,
                       outputFilenameSuffix = "CASFRI_PAUL_KNN",
                       cacheRepo = cpath)#, notOlderThan = Sys.time())

    if (!startsWith(Sys.info()["nodename"], prefix = "W-VIC-A105")) .gc()
  }

  return(outStack2)
}
