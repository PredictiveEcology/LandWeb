loadPaulAndCASFRI <- function(paths, PaulRawFileName, existingSpeciesLayers,
                              CASFRITifFile, CASFRIattrFile, CASFRIheaderFile) {
  #gdal_setInstallation(ignore.full_scan = TRUE, verbose = TRUE)
  # Step 1 -- Load LandWeb study area shapefile
  origDir <- getwd()
  oldfilename <- "shpLandWEB.shp"
  # makek absolute paths
  paths <- lapply(paths, function(x) file.path(origDir,x))
  dataFolder <- file.path(paths$modulePath, "Boreal_LBMRDataPrep", "data")
  setwd(dataFolder)

  message("Load Paul's study area layer from original source")
  PaulRawFileName <- basename(PaulRawFileName)
  zipDownload <- file.path(dataFolder, "SPP_1990_FILLED_100m_NAD83_LCC_BYTE_VEG.zip")
  if (!file.exists(PaulRawFileName))  {
    if (!file.exists(zipDownload)) {
      googledrive::drive_auth(cache = FALSE, verbose = TRUE)
      file_url <- "https://drive.google.com/file/d/0BxZrk9psrK4nTFVMY295WFFXZk0/view?usp=sharing"
      googledrive::drive_download(googledrive::as_id(file_url), path = zipDownload,
                                  overwrite = TRUE, verbose = TRUE)
    }
    unzip(zipDownload, exdir = dataFolder, overwrite = TRUE)
  }
  PaulOnGoogleDrive1 <- raster(PaulRawFileName)
  LandWebStudyAreaRawPoly <- file.path(paths$inputPath, oldfilename)

  PaulOnGoogleDrive <- Cache(writeRaster, PaulOnGoogleDrive1, cacheRepo = paths$cachePath,
                             filename = asPath(file.path(dataFolder, "PaulSppFilled.tif")),
                             datatype = "INT2U",
                             overwrite = TRUE)#, debugCache="quick")
  shpStudyRegionFull <- Cache(loadShpAndMakeValid, file = asPath(LandWebStudyAreaRawPoly),
                              cacheRepo = paths$cachePath)

  message("Make Study Area Mask polygon")
  studyAreaMask <- Cache(makeStudyAreaMask, PaulOnGoogleDrive,
                         maskFilename = asPath(file.path(dataFolder,"StudyAreaMask.tif")),
                         cacheRepo=paths$cachePath)

  message("Make Study Area Mask polygon into 250m res")
  studyAreaMask250filename <- file.path(dataFolder,"StudyAreaMask250.tif")
  Cache(gdalwarp, overwrite=TRUE,
        dstalpha = TRUE,
        s_srs= as.character(crs(studyAreaMask)),
        t_srs= as.character(crs(studyAreaMask)),
        multi=TRUE, of="GTiff",
        tr=c(250, 250),
        filename(studyAreaMask), ot = "Byte",
        basename(studyAreaMask250filename),
        cacheRepo=paths$cachePath)

  shapeFile <- file.path(dataFolder,"shpLandWeb5.shp")
  # TO get this to work
  #  https://stackoverflow.com/questions/5599872/python-windows-importerror-no-module-named-site
  message("Make Study Area Mask polygon shapefile")
  Cache(system, #notOlderThan = Sys.time(),
        paste("python", file.path(getOption("gdalUtils_gdalPath")[[1]]$path,"gdal_polygonize.py"),
              basename(studyAreaMask250filename), basename(shapeFile), "-f \"ESRI Shapefile\""),
        cacheRepo=paths$cachePath)

  message("Make smaller study area polygon shapefile, sized to Paul's layer")
  studyArea <- Cache(loadStudyArea, asPath(shapeFile), shpStudyRegionFull,
                     crsPaul=crs(PaulOnGoogleDrive),
                     cacheRepo=paths$cachePath)
  #write file
  Cache(shapefile, studyArea, filename = "studyArea.shp", overwrite = TRUE,
        cacheRepo=paths$cachePath)

  message("Convert Paul's 100m layer to 250m and mask to new study area polygon")
  Paul250MaskedFilename <- "PaulTrimmed.tif"
  Cache(gdalwarp, overwrite=TRUE, cutline = "studyArea.shp",
        dstalpha = TRUE,
        s_srs = as.character(crs(PaulOnGoogleDrive)),
        t_srs = as.character(crs(PaulOnGoogleDrive)),
        multi = TRUE, of = "GTiff",
        crop_to_cutline = TRUE, tr = c(250, 250),
        filename(PaulOnGoogleDrive), ot = "Byte",
        Paul250MaskedFilename,
        cacheRepo=paths$cachePath)

  message("Starting CASFRI stuff")
  CASFRITifFile <- file.path(paths$modulePath, "Boreal_LBMRDataPrep", "data", "Landweb_CASFRI_GIDs.tif")

  PaulRawFileName <- basename(PaulRawFileName)
  zipDownload <- file.path(dataFolder, "CASFRI for Landweb.zip")
  if (!file.exists(CASFRITifFile)) {
    if (!file.exists(zipDownload)) {
      googledrive::drive_auth(cache = FALSE, verbose = TRUE)
      file_url <- "https://drive.google.com/file/d/1nhZwBKR1dJASFtZinO28yt0RVaCI-NOz/view?usp=sharing"
      googledrive::drive_download(googledrive::as_id(file_url), path = zipDownload,
                                  overwrite = TRUE, verbose = TRUE)
    }
    unzip(zipDownload, exdir = dataFolder, overwrite = TRUE)
  }

  if (file.exists(CASFRITifFile)) {
    CASFRIRas <- raster(CASFRITifFile)
  } else {
    stop("You need the CASFRI tif, supplied by Melina Houle and Pierre Racine")
  }
  CASFRIStudyAreaFilename <- gsub(filename(CASFRIRas), pattern = "\\.tif", replacement = "StudyArea.tif")

  message("Convert CASFRI 100m layer to 250m and mask to new study area polygon")
  Cache(gdalwarp, overwrite=TRUE, cutline = "studyArea.shp",
        s_srs= as.character(crs(CASFRIRas)),
        t_srs= as.character(crs(PaulOnGoogleDrive)),
        dstalpha = TRUE,tr=c(250, 250),
        multi=TRUE, of="GTiff",
        crop_to_cutline = TRUE,
        basename(filename(CASFRIRas)), #ot = "Byte",
        basename(CASFRIStudyAreaFilename),
        cacheRepo=paths$cachePath)
  CASFRIRas <- raster(CASFRIStudyAreaFilename)

  message("Load CASFRI data and headers, and convert to long format, and define species groups")
  loadedCASFRI <- Cache(loadCASFRI, CASFRIRas, asPath(basename(CASFRIattrFile)),
                        asPath(basename(CASFRIheaderFile)),
                        cacheRepo=paths$cachePath, digestPathContent = TRUE)

  message("Make stack of species layers from Paul's layer")
  uniqueKeepSp <- unique(loadedCASFRI$keepSpecies$spGroup)
  # "Abie_sp"  "Betu_pap" "Lari_lar" "Pice_gla" "Pice_mar" "Pinu_sp" "Popu_tre"
  PaulSpStack <- Cache(makePaulStack, paths = paths, asPath(Paul250MaskedFilename), uniqueKeepSp,
                       cacheRepo=paths$cachePath)
  message('Make stack from CASFRI data and headers')
  #spRas <- raster(CASFRIRas) %>% setValues(.,NA_integer_)
  #dd <- CASFRItoSpRasts(spRas, loadedCASFRI)

  #CASFRISpStack <- CASFRItoSpRasts(paths = paths, spRas, loadedCASFRI)
  CASFRISpStack <- Cache(CASFRItoSpRasts, cachePath=asPath(paths$cachePath),
                         CASFRIRas, loadedCASFRI,
                         cacheRepo=paths$cachePath)

  message("Overlay Paul and CASFRI stacks")
  outStack <- Cache(overlayStacks, CASFRISpStack, PaulSpStack, cachePath = paths$cachePath,
                    outputFilenameSuffix = "CASFRI_PAUL",
                    cacheRepo=paths$cachePath)#, notOlderThan = Sys.time())

  outStack2 <- Cache(overlayStacks, outStack, specieslayers, cachePath = paths$cachePath,
                     outputFilenameSuffix = "CASFRI_PAUL_KNN",
                     cacheRepo=paths$cachePath)#, notOlderThan = Sys.time())
  setwd(origDir)
  return(outStack2)
}
