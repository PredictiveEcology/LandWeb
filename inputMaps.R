# Thsee are used in server.R for filling the tables of parameters
loadLandisParams <- function(path) {
  landisInputs <- readRDS(file.path(path, "landisInputs.rds"))
  spEcoReg <- readRDS(file.path(path, "SpEcoReg.rds"))
  return(list(landisInputs = landisInputs, spEcoReg = spEcoReg))
}

# Study area original shapefile
loadShpAndMakeValid <- function(file) {
  shapefile(file) %>% gBuffer(byid = TRUE, width = 0)
}

loadStudyRegions <- function(shpPath, fireReturnIntervalMap, subSRNameXYXY, crsSRXYXY) {
  if ("RIA" %in% subSRNameXYXY) {
    sSubSRXYXY <- Cache(shapefile, userTags = "stable",
                            file.path(paths$inputPath, "RIA_SE_ResourceDistricts_Clip.shp"))
    loadAndBuffer <- function(shapefile) {
      a <- shapefile(shapefile)
      b <- buffer(a, 0, dissolve = FALSE)
      SpatialPolygonsDataFrame(b, data = as.data.frame(a))
    }
    fireReturnIntervalTemp <- 400
    sSubSRXYXY[["LTHRC"]] <- fireReturnIntervalTemp # Fire return interval
    sSubSRXYXY[["fireReturnInterval"]] <- sSubSRXYXY$LTHRC # Fire return interval

    sSRXYXY <- Cache(loadAndBuffer, file.path(paths$inputPath, "RIA_StudyArea.shp"),
                                cacheRepo = paths$cachePath, userTags = "stable")
    sSRXYXY[["LTHRC"]] <- fireReturnIntervalTemp # Fire return interval
    sSRXYXY$fireReturnInterval <- sSRXYXY$LTHRC
    sSRXYXY <- sSubSRXYXY

  } else {
    # Dave Andison doesn't have .prj files -- this line will create one with NAD83 UTM11N downloading from spatialreference.org
    createPrjFile(shpPath)
    sSRXYXY <- loadShpAndMakeValid(file = shpPath)

    if (is.null(sSRXYXY$fireReturnInterval)) {
      # Dave Andison doesn't have .prj files -- this line will create one with NAD83 UTM11N downloading from spatialreference.org
      createPrjFile(fireReturnIntervalMap)
      fireReturnInterval <- loadShpAndMakeValid(file = fireReturnIntervalMap)
    }
    if (!identical(extent(sSRXYXY), extent(fireReturnInterval))) {
      sSRXYXY <- raster::intersect(sSRXYXY, fireReturnInterval)
    }
    if (!isTRUE("LTHRC" %in% names(sSRXYXY))) {
      sSRXYXY$LTHRC <- sSRXYXY$LTHFC # Apparently, sometimes it is LTHFC, sometimes LTHRC # Get rid of LTHFC
      sSRXYXY$LTHFC <- NULL
      # The fires of Fire Return Interval 30 years are not correctly simulated by LandMine, so they are removed.
      sSRXYXY$LTHRC[sSRXYXY$LTHRC <= 30] <- NA
    }
    sSRXYXY$fireReturnInterval <- sSRXYXY$LTHRC
    sSRXYXY@data <- sSRXYXY@data[, !(names(sSRXYXY) %in% "ECODISTRIC")]
    sSRXYXY <- spTransform(sSRXYXY, crsSRXYXY)
    sSRXYXY <- rgeos::gBuffer(sSRXYXY, byid = TRUE, width = 0)
    sSubSRXYXY <- shpStudyRegionCreate(sSRXYXY, subSRNameXYXY = subSRNameXYXY,
                                           crsSRXYXY = crsSRXYXY)
  }
  list(sSubSRXYXY = sSubSRXYXY, sSRXYXY = sSRXYXY)
}

shpStudyRegionCreate <- function(sSRXYXY, subSRNameXYXY, crsSRXYXY) {
  canadaAdminNames <- c(BC = "British Columbia",
                        AB = "Alberta",
                        SK = "Saskatchewan",
                        MB = "Manitoba")
  canadaAdminNamesAll <- c(names(canadaAdminNames), canadaAdminNames)

  if (!("FULL" %in% subSRNameXYXY)) {
    if ("NWT" %in% subSRNameXYXY) {
      sSRLFLTXYXY <- spTransform(sSRXYXY, SpaDES.shiny::proj4stringLFLT)
      ext <- extent(sSRLFLTXYXY)
      ext@ymin <- 60
      sSRNWTXYXY <- crop(sSRLFLTXYXY, ext)
      sSubSRXYXY <- spTransform(sSRNWTXYXY, crs(sSRXYXY))
      sSubSRXYXY <- rgeos::gBuffer(sSubSRXYXY, width = 0, byid = TRUE)
    } else if (any(subSRNameXYXY %in% canadaAdminNamesAll)) {
      canadaMap <- Cache(getData, 'GADM', country = 'CAN', level = 1,
                         cacheRepo = paths$cachePath, userTags = "stable")
      subSRNameXYXY <- canadaAdminNames[canadaAdminNames %in% subSRNameXYXY |
                                      names(canadaAdminNames) %in% subSRNameXYXY]
      inputMapPolygon <- spTransform(canadaMap[canadaMap$NAME_1 %in% subSRNameXYXY,], crsSRXYXY)
      aa <- sf::st_intersection(sf::st_as_sf(sSRXYXY), sf::st_as_sf(inputMapPolygon))
      sSubSRXYXY <- as(aa, "Spatial")

    } else {
      set.seed(853839)#set.seed(5567913)
      if ("SMALL" %in% subSRNameXYXY) {
        areaKm2 <- 10000#700000#2000#600000#too big for laptop
      } else if ("VERYSMALL" %in% subSRNameXYXY) {
        areaKm2 <- 3000 #700000#2000#600000#too big for laptop
      } else if ("MEDIUM" %in% subSRNameXYXY) {
        areaKm2 <- 40000 #700000#2000#600000#too big for laptop
      } else if ("LARGE" %in% subSRNameXYXY) {
        areaKm2 <- 80000 #700000#2000#600000#too big for laptop
      } else if ("VERYLARGE" %in% subSRNameXYXY) {
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
      crs(inputMapPolygon) <- crsSRXYXY
      sSubSRXYXY <- raster::intersect(sSRXYXY, inputMapPolygon)
      options("digits.secs" = 7)
      on.exit(options("digits.secs" = NULL))
      set.seed(as.numeric(format(Sys.time(), format = "%OS")))

    }
  } else {
    sSubSRXYXY <- sSRXYXY
  }

  return(sSubSRXYXY)
}

createPrjFile <- function(shp) {
  basenameWithoutExt <- strsplit(shp, "\\.")[[1]]
  basenameWithoutExt <- basenameWithoutExt[-length(basenameWithoutExt)]
  prjFile <- paste0(basenameWithoutExt, ".prj")
  if (!file.exists(prjFile)) {
    download.file("http://spatialreference.org/ref/epsg/nad83-utm-zone-11n/prj/",
                  destfile = prjFile)
  }
}
