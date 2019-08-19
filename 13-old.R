  ##########################################################
  # Reporting Polygons
  ##########################################################
  ml2 <- mapAdd(map = ml, layerName = "AB Natural Sub Regions",
                url = "https://drive.google.com/file/d/1mCEynahKnFkStJUJC8ho5ndRD41olz9F/view?usp=sharing",
                columnNameForLabels = "Name")

  ##########################################################
  # Load other maps
  ##########################################################

  ml <- mapAdd(map = ml,
               destinationPath = "~/GitHub/LandWeb/inputs/FMA_Boundaries/DMI/",
               targetCRS = targetCRS,
               targetFile = "DMI_Full.shp", #studyArea = studyArea(ml, 1),
               layerName = "DMI Full", overwrite = TRUE, isStudyArea = TRUE,
               columnNameForLabels = "Name", administrative = TRUE)

  ml <- mapAdd(map = ml, layerName = "AB Natural Sub Regions", overwrite = TRUE,
               url = "https://drive.google.com/file/d/1mCEynahKnFkStJUJC8ho5ndRD41olz9F/view?usp=sharing",
               columnNameForLabels = "Name", filename2 = NULL)

  ml <- mapAdd(url = "https://drive.google.com/open?id=1JnKeXrw0U9LmrZpixCDooIm62qiv4_G1",
               map = ml, leaflet = TRUE, #studyArea = studyArea(ml, 2),
               #targetFile = "age1.tif", overwrite = TRUE,
               filename2 = NULL,
               layerName = "Age") # dots include things like method = "ngb" for projectRaster

  ################################
  # set some options
  #################################
  source("appInfo.R")

  # Options
  originalOpts <- options("spades.moduleCodeChecks" = FALSE, "reproducible.quick" = FALSE,
                          reproducible.verbose = FALSE, reproducible.useMemoise = TRUE,
                          spades.browserOnError = FALSE)

  # Google Authentication setup
  options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/userinfo.email",
                                          "https://www.googleapis.com/auth/userinfo.profile"))

  if (Sys.info()["nodename"] == "landweb.ca") {
    ## LandWeb.ca (live version)
    options(googleAuthR.webapp.client_id = "680957910261-kmlslu6vu0fo9129oj1fckksapg94gja.apps.googleusercontent.com")
    options(googleAuthR.webapp.client_secret = "Qe0TE327wRf9DYM-BEhDxe4a")
  } else {
    ## LandWeb.org (Alex's development version)
    options(googleAuthR.webapp.client_id = "869088473060-a7o2bc7oit2vn11gj3ieh128eh8orb04.apps.googleusercontent.com")
    options(googleAuthR.webapp.client_secret = "FR-4jL12j_ynAtsl-1Yk_cEL")
  }
  options(httr_oob_default = TRUE)

  appURL <- "http://landweb.ca"

  ##########################################################
  # Set paths
  ##########################################################
  paths <- list(
    cachePath = "cache",
    modulePath = "m", # short name because shinyapps.io can't handle longer than 100 characters
    inputPath = "inputs",
    outputPath = "outputs"
  )
  do.call(setPaths, paths) # Set them here so that we don't have to specify at each call to Cache

  ##########################################################
  # source auxiliary functions
  ##########################################################
  source("R/functions.R")
  ## source additional shiny modules
  vapply(list.files("shiny-modules", "[.]R", full.names = TRUE), source, vector("list", 2))

  # This needs simInit call to be run already
  # a few map details for shiny app
  message("Preparing polygon maps for reporting histograms")
  source(file.path("R", "colorPaletteForShiny.R"))
  labelColumn <- "shinyLabel"




  # leaflet parameters
  leafletZoomInit <- 5

  # Some shinycssloaders options
  options("spinner.type" = 5)

  # This will search for gdal utilities. If it finds nothing, and you are on Windows,
  #   you should install the GDAL that comes with QGIS -- use OSGeo4W Network Installer 64 bit
  #   may be still here: http://www.qgis.org/en/site/forusers/download.html
  options(gdalUtils_gdalPath = Cache(gdalSet, cacheRepo = paths$cachePath))

  ########################################
  # simInit
  ########################################
  # Time steps
  fireTimestep <- 1
  successionTimestep <- 10

  ## spades module variables -- creates
  # eventCaching, maxAge, vegLeadingProportion
  # ageClasses, ageClassCutOffs, ageClas0s0Zones
  source("R/LandWeb user parameters.R")
  landisInputs <- readRDS(file.path(paths$inputPath, "landisInputs.rds"))
  spEcoReg <- readRDS(file.path(paths$inputPath, "SpEcoReg.rds"))

  # The CRS for the Study -- spTransform converts this first one to the second one, they are identical geographically
  # crsStudyArea <- CRS(paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0",
  #                         "+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  crsStudyArea <- CRS(paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0",
                              "+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))


  ######################################################
  ml <- mapAdd(map = ml,
               url = "https://drive.google.com/file/d/1Oz2vSor3oIKf2uGv3KRtLoLRWEfX5Mas/view?usp=sharing",
               layerName = "Mountain Northern Caribou Ranges",
               columnNameForLabels = "Name")

  ml <- mapAdd(map = ml, layerName = "Provincial Parks",
               url = "https://drive.google.com/file/d/1GHgTI4JY-YhAXvWkgV20vugbvLNqEEGH/view?usp=sharing",
               columnNameForLabels = "Name")
  ml <- mapAdd(map = ml, layerName = "NWT Ecoregions",
               url = "https://drive.google.com/file/d/1iRAQfARkmS6-XVHFnTkB-iltzMNPAczC/view?usp=sharing",
               columnNameForLabels = "Name")
  ml <- mapAdd(map = ml, layerName = "National Parks",
               url = "https://drive.google.com/file/d/1B3VUU8PDn4NPveAyF76OBPY0vZkxScEt/view?usp=sharing",
               columnNameForLabels = "Name")
  ml <- mapAdd(map = ml, layerName = "AB Natural Sub Regions",
               url = "https://drive.google.com/file/d/1mCEynahKnFkStJUJC8ho5ndRD41olz9F/view?usp=sharing",
               columnNameForLabels = "Name")
  # "LP MASTERFILE June62012",
  #   url = "https://drive.google.com/file/d/1J38DKQQavjBV9F3z2gGzHNuNE0s2rmhh/view?usp=sharing",
  #   columnNameForLabels = "Name"),
  ml <- mapAdd(map = ml, layerName = "BC Bio Geoclimatic Zones",
               url = "https://drive.google.com/file/d/1VAwsax63l2akOM2j_O4Je9p0ZiYg8Hl-/view?usp=sharing",
               columnNameForLabels = "ZONE_NAME")
  ml <- mapAdd(map = ml, layerName = "FMU Alberta 2015-11",
               url = "https://drive.google.com/file/d/1JiCLcHh5fsBAy8yAx8NgtK7fxaZ4Tetl/view?usp=sharing",
               columnNameForLabels = "FMU_NAME")
  ml <- mapAdd(map = ml, layerName = "FMA Boundary Updated",
               url = "https://drive.google.com/file/d/1nTFOcrdMf1hIsxd_yNCSTr8RrYNHHwuc/view?usp=sharing",
               columnNameForLabels = "Name")
  ml <- mapAdd(map = ml, layerName = "Boreal Caribou Ranges",
               url = "https://drive.google.com/file/d/1PYLou8J1wcrme7Z2tx1wtA4GvaWnU1Jy/view?usp=sharing",
               columnNameForLabels = "Name")


  ### RASTERS
  # STOPPED HERE
  # Current Condition
  preProcess(url = "https://drive.google.com/file/d/1JnKeXrw0U9LmrZpixCDooIm62qiv4_G1/view?usp=sharing")

  ml <- mapAdd(map = ml, url = "https://drive.google.com/file/d/1Oz2vSor3oIKf2uGv3KRtLoLRWEfX5Mas/view?usp=sharing",
               layerName = "Mountain Northern Caribou Ranges",
               columnNameForLabels = "Name")
