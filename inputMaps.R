# Thsee are used in server.R for filling the tables of parameters
loadLandisParams <- function(path, envir) {
  assign("landisInputs", readRDS(file.path(path, "landisInputs.rds")),  envir = envir)
  assign("spEcoReg", readRDS(file.path(path, "SpEcoReg.rds")), envir = envir)
  return(invisible(NULL))
}

#seralStageData <- readRDS(file.path(paths$inputPath, "seralStageData.rds"))
#vegTypeData <- readRDS(file.path(paths$inputPath, "vegTypeData.rds"))
#availableRegions <- unique(vegTypeData$ecoregion)

# Study area original shapefile
loadShpAndMakeValid <- function(file) {
  shapefile(file) %>% gBuffer(byid = TRUE, width = 0)
}

useEcozoneMask <- function(studyArea, ecozoneFilename){
  A <- loadShpAndMakeValid(ecozoneFilename)
  #A <- shapefile(ecozoneFilename)
  B <- A[grep("Cordillera", A$ZONE_NAME, invert = TRUE), ] %>%
    .[grep("Prairie", B$ZONE_NAME, invert = TRUE), ]
  C <- raster::intersect(shpStudyRegionFull, B)
}

crsKNNMaps <- CRS(paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0",
                        "+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))

loadStudyRegion <- function(shpPath, studyArea, crsKNNMaps) {
  shpStudyRegionFull <- loadShpAndMakeValid(file = shpPath)
  shpStudyRegionFull$fireReturnInterval <- shpStudyRegionFull$LTHRC
  shpStudyRegionFull@data <- shpStudyRegionFull@data[, !(names(shpStudyRegionFull) %in% "ECODISTRIC")]
  shpStudyRegionFull <- spTransform(shpStudyRegionFull, crsKNNMaps)

  shpStudyRegion <- shpStudyRegionCreate(shpStudyRegionFull, studyArea = studyArea, targetCRS = crsKNNMaps)
  list(shpStudyRegion = shpStudyRegion, shpStudyRegionFull = shpStudyRegionFull)
}

shpStudyRegionCreate <- function(shpStudyRegionFull, studyArea, targetCRS) {
  set.seed(853839)#set.seed(5567913)
  if (studyArea != "FULL") {
    if (studyArea == "NWT") {
      shpStudyRegionFullLL <- spTransform(shpStudyRegionFull, CRS("+proj=longlat +datum=WGS84"))
      ext <- extent(shpStudyRegionFullLL)
      ext@ymin <- 60
      shpStudyRegionFullNWT <- crop(shpStudyRegionFullLL, ext)
      shpStudyRegion <- spTransform(shpStudyRegionFullNWT, crs(shpStudyRegionFull))
      shpStudyRegion <- rgeos::gBuffer(shpStudyRegion, width = 0, byid = TRUE)
    } else {
      if (studyArea == "SMALL") {
        areaKm2 <- 10000#700000#2000#600000#too big for laptop
      } else if (studyArea == "VERYSMALL") {
        areaKm2 <- 2000 #700000#2000#600000#too big for laptop
      } else if (studyArea == "MEDIUM") {
        areaKm2 <- 40000 #700000#2000#600000#too big for laptop
      } else if (studyArea == "LARGE") {
        areaKm2 <- 80000 #700000#2000#600000#too big for laptop
      } else if (studyArea == "EXTRALARGE") {
        areaKm2 <- 180000 #700000#2000#600000#too big for laptop
      }

      minY <- 7778877 - 1.6e5
      minX <- -1202250.2
      maxX <- minX + sqrt(areaKm2 * 1e6)
      maxY <- minY + sqrt(areaKm2 * 1e6)
      meanY <- mean(c(minY, maxY))

      # Add random noise to polygon
      xAdd <- -3e5#round(runif(1,-5e5, 1.5e6))
      yAdd <- 5e5#round(runif(1, 1e5, 5e5)) - xAdd / 2
      nPoints <- 20
      betaPar <- 0.6
      X <- c(jitter(sort(rbeta(nPoints, betaPar, betaPar) * (maxX - minX) + minX)),
             jitter(sort(rbeta(nPoints, betaPar, betaPar) * (maxX - minX) + minX, decreasing = TRUE)))
      Y <- c(jitter(sort(rbeta(nPoints / 2, betaPar, betaPar) * (maxY - meanY) + meanY)),
             jitter(sort(rbeta(nPoints, betaPar, betaPar) * (maxY - minY) + minY, decreasing = TRUE)),
             jitter(sort(rbeta(nPoints / 2, betaPar, betaPar) * (meanY - minY) + minY)))

      Sr1 <- Polygon(cbind(X + xAdd, Y + yAdd))
      Srs1 <- Polygons(list(Sr1), "s1")
      inputMapPolygon <- SpatialPolygons(list(Srs1), 1L)
      crs(inputMapPolygon) <- targetCRS
      shpStudyRegion <- raster::intersect(shpStudyRegionFull, inputMapPolygon)
    }
  } else {
    shpStudyRegion <- shpStudyRegionFull
  }
  return(shpStudyRegion)
}

#ggStudyRegion <- ggvisFireReturnInterval(shpStudyRegion, shpStudyRegionFull)

set.seed(Sys.time())
