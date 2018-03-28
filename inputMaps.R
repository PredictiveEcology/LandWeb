# Thsee are used in server.R for filling the tables of parameters
loadLandisParams <- function(path) {
  landisInputs <- readRDS(file.path(path, "landisInputs.rds"))
  spEcoReg <- readRDS(file.path(path, "SpEcoReg.rds"))
  return(list(landisInputs = landisInputs, spEcoReg = spEcoReg))
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


loadStudyRegion <- function(shpPath, fireReturnIntervalMap, studyArea, crsStudyArea) {
  if ("RIA" %in% studyArea) {
    shpStudyRegion <- Cache(shapefile, userTags = "stable",
                            file.path(paths$inputPath, "RIA_SE_ResourceDistricts_Clip.shp"))
    loadAndBuffer <- function(shapefile) {
      a <- shapefile(shapefile)
      b <- buffer(a, 0, dissolve = FALSE)
      SpatialPolygonsDataFrame(b, data = as.data.frame(a))
    }
    fireReturnIntervalTemp <- 400
    shpStudyRegion[["LTHRC"]] <- fireReturnIntervalTemp # Fire return interval
    shpStudyRegion[["fireReturnInterval"]] <- shpStudyRegion$LTHRC # Fire return interval

    shpStudyRegionFull <- Cache(loadAndBuffer, file.path(paths$inputPath, "RIA_StudyArea.shp"),
                                cacheRepo = paths$cachePath, userTags = "stable")
    shpStudyRegionFull[["LTHRC"]] <- fireReturnIntervalTemp # Fire return interval
    shpStudyRegionFull$fireReturnInterval <- shpStudyRegionFull$LTHRC
    shpStudyRegionFull <- shpStudyRegion

  } else {
    # Dave Andison doesn't have .prj files -- this line will create one with NAD83 UTM11N downloading from spatialreference.org
    createPrjFile(shpPath)
    shpStudyRegionFull <- loadShpAndMakeValid(file = shpPath)

    if (is.null(shpStudyRegionFull$fireReturnInterval)) {
      # Dave Andison doesn't have .prj files -- this line will create one with NAD83 UTM11N downloading from spatialreference.org
      createPrjFile(fireReturnIntervalMap)
      fireReturnInterval <- loadShpAndMakeValid(file = fireReturnIntervalMap)
    }
    if (!identical(extent(shpStudyRegionFull), extent(fireReturnInterval))) {
      shpStudyRegionFull <- raster::intersect(shpStudyRegionFull, fireReturnInterval)
    }
    if (!isTRUE("LTHRC" %in% names(shpStudyRegionFull))) {
      shpStudyRegionFull$LTHRC <- shpStudyRegionFull$LTHFC # Apparently, sometimes it is LTHFC, sometimes LTHRC # Get rid of LTHFC
      shpStudyRegionFull$LTHFC <- NULL
      # The fires of Fire Return Interval 30 years are not correctly simulated by LandMine, so they are removed.
      shpStudyRegionFull$LTHRC[shpStudyRegionFull$LTHRC <= 30] <- NA
    }
    shpStudyRegionFull$fireReturnInterval <- shpStudyRegionFull$LTHRC
    shpStudyRegionFull@data <- shpStudyRegionFull@data[, !(names(shpStudyRegionFull) %in% "ECODISTRIC")]
    shpStudyRegionFull <- spTransform(shpStudyRegionFull, crsStudyArea)
    shpStudyRegionFull <- rgeos::gBuffer(shpStudyRegionFull, byid = TRUE, width = 0)
    shpStudyRegion <- shpStudyRegionCreate(shpStudyRegionFull, studyArea = studyArea,
                                           crsStudyArea = crsStudyArea)
  }
  list(shpStudyRegion = shpStudyRegion, shpStudyRegionFull = shpStudyRegionFull)
}

shpStudyRegionCreate <- function(shpStudyRegionFull, studyArea, crsStudyArea) {
  canadaAdminNames <- c(BC = "British Columbia",
                        AB = "Alberta",
                        SK = "Saskatchewan",
                        MB = "Manitoba")
  canadaAdminNamesAll <- c(names(canadaAdminNames), canadaAdminNames)

  if (!("FULL" %in% studyArea)) {
    if ("NWT" %in% studyArea) {
      shpStudyRegionFullLL <- spTransform(shpStudyRegionFull, CRS("+proj=longlat +datum=WGS84"))
      ext <- extent(shpStudyRegionFullLL)
      ext@ymin <- 60
      shpStudyRegionFullNWT <- crop(shpStudyRegionFullLL, ext)
      shpStudyRegion <- spTransform(shpStudyRegionFullNWT, crs(shpStudyRegionFull))
      shpStudyRegion <- rgeos::gBuffer(shpStudyRegion, width = 0, byid = TRUE)
    } else if (any(studyArea %in% canadaAdminNamesAll)) {
      canadaMap <- Cache(getData, 'GADM', country = 'CAN', level = 1,
                         cacheRepo = paths$cachePath, userTags = "stable")
      studyArea <- canadaAdminNames[canadaAdminNames %in% studyArea |
                                      names(canadaAdminNames) %in% studyArea]
      inputMapPolygon <- spTransform(canadaMap[canadaMap$NAME_1 %in% studyArea,], crsStudyArea)
      aa <- sf::st_intersection(sf::st_as_sf(shpStudyRegionFull), sf::st_as_sf(inputMapPolygon))
      shpStudyRegion <- as(aa, "Spatial")

    } else {
      set.seed(853839)#set.seed(5567913)
      if ("SMALL" %in% studyArea) {
        areaKm2 <- 10000#700000#2000#600000#too big for laptop
      } else if ("VERYSMALL" %in% studyArea) {
        areaKm2 <- 3000 #700000#2000#600000#too big for laptop
      } else if ("MEDIUM" %in% studyArea) {
        areaKm2 <- 40000 #700000#2000#600000#too big for laptop
      } else if ("LARGE" %in% studyArea) {
        areaKm2 <- 80000 #700000#2000#600000#too big for laptop
      } else if ("VERYLARGE" %in% studyArea) {
        areaKm2 <- 180000 #700000#2000#600000#too big for laptop
      }

      minY <- 7678877 - 1.6e5
      minX <- -1002250.2
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
      crs(inputMapPolygon) <- crsStudyArea
      shpStudyRegion <- raster::intersect(shpStudyRegionFull, inputMapPolygon)
      options("digits.secs" = 7)
      on.exit(options("digits.secs" = NULL))
      set.seed(as.numeric(format(Sys.time(), format = "%OS")))

    }
  } else {
    shpStudyRegion <- shpStudyRegionFull
  }

  return(shpStudyRegion)
}

#ggStudyRegion <- ggvisFireReturnInterval(shpStudyRegion, shpStudyRegionFull)

createPrjFile <- function(shp) {
  basenameWithoutExt <- strsplit(shp, "\\.")[[1]]
  basenameWithoutExt <- basenameWithoutExt[-length(basenameWithoutExt)]
  prjFile <- paste0(basenameWithoutExt, ".prj")
  if (!file.exists(prjFile)) {
    download.file("http://spatialreference.org/ref/epsg/nad83-utm-zone-11n/prj/",
                  destfile = prjFile)
  }
}
