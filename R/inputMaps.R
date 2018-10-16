# Study area original shapefile
loadShpAndMakeValid <- function(file) {
  shapefile(file) %>% gBuffer(byid = TRUE, width = 0)
}

loadStudyRegions <- function(shpPath, shpStudyRegionCreateFn,
                             fireReturnIntervalMap, subStudyRegionName, crsStudyRegion) {
  if ("RIA" %in% subStudyRegionName) {
    shpfile <- file.path(paths$inputPath, "RIA_SE_ResourceDistricts_Clip.shp")
    shpSubStudyRegion <- Cache(shapefile, userTags = "stable", shpfile)
    loadAndBuffer <- function(shapefile) {
      a <- shapefile(shapefile)
      b <- buffer(a, 0, dissolve = FALSE)
      SpatialPolygonsDataFrame(b, data = as.data.frame(a))
    }
    fireReturnIntervalTemp <- 400
    shpSubStudyRegion[["LTHRC"]] <- fireReturnIntervalTemp # Fire return interval
    shpSubStudyRegion[["fireReturnInterval"]] <- shpSubStudyRegion$LTHRC # Fire return interval

    shpStudyRegion <- Cache(loadAndBuffer, file.path(paths$inputPath, "RIA_StudyArea.shp"),
                            cacheRepo = paths$cachePath, userTags = "stable")
    shpStudyRegion[["LTHRC"]] <- fireReturnIntervalTemp # Fire return interval
    shpStudyRegion$fireReturnInterval <- shpStudyRegion$LTHRC
    shpStudyRegion <- shpSubStudyRegion

  } else {
    ## Dave Andison doesn't have .prj files
    ## this will create one with NAD83 UTM11N downloading from spatialreference.org
    createPrjFile(shpPath)
    shpStudyRegion <- loadShpAndMakeValid(file = shpPath)

    if (is.null(shpStudyRegion$fireReturnInterval)) {
      ## Dave Andison doesn't have .prj files
      ## this will create one with NAD83 UTM11N downloading from spatialreference.org
      createPrjFile(fireReturnIntervalMap)
      fireReturnInterval <- loadShpAndMakeValid(file = fireReturnIntervalMap)
    }
    if (!identical(extent(shpStudyRegion), extent(fireReturnInterval))) {
      shpStudyRegion <- raster::intersect(shpStudyRegion, fireReturnInterval)
    }
    if (!isTRUE("LTHRC" %in% names(shpStudyRegion))) {
      # Apparently, sometimes it is LTHFC, sometimes LTHRC; get rid of LTHFC
      shpStudyRegion$LTHRC <- shpStudyRegion$LTHFC
      shpStudyRegion$LTHFC <- NULL

      #shpStudyRegion$LTHRC <- 2*shpStudyRegion$LTHRC ## TODO: remove this

      # The fires of Fire Return Interval 30 years are not correctly simulated by LandMine, so they are removed.
      shpStudyRegion$LTHRC[shpStudyRegion$LTHRC <= 30] <- NA
    }
    shpStudyRegion$fireReturnInterval <- shpStudyRegion$LTHRC
    shpStudyRegion@data <- shpStudyRegion@data[, !(names(shpStudyRegion) %in% "ECODISTRIC")]
    shpStudyRegion <- spTransform(shpStudyRegion, crsStudyRegion)
    shpStudyRegion <- rgeos::gBuffer(shpStudyRegion, byid = TRUE, width = 0)
    shpSubStudyRegion <- shpStudyRegionCreateFn(shpStudyRegion,
                                                subStudyRegionName = subStudyRegionName,
                                                crsStudyRegion = crsStudyRegion)
  }
  list(shpSubStudyRegion = shpSubStudyRegion, shpStudyRegion = shpStudyRegion)
}

shpStudyRegionCreate <- function(shpStudyRegion, subStudyRegionName, crsStudyRegion) {
  canadaAdminNames <- c(BC = "British Columbia",
                        AB = "Alberta",
                        SK = "Saskatchewan",
                        MB = "Manitoba")
  canadaAdminNamesAll <- c(names(canadaAdminNames), canadaAdminNames)

  if (!("FULL" %in% subStudyRegionName)) {
    if ("NWT" %in% subStudyRegionName) {
      shpStudyRegionLFLT <- spTransform(shpStudyRegion, SpaDES.shiny::proj4stringLFLT)
      ext <- extent(shpStudyRegionLFLT)
      ext@ymin <- 60
      shpStudyRegionNWT <- crop(shpStudyRegionLFLT, ext)
      shpSubStudyRegion <- spTransform(shpStudyRegionNWT, crs(shpStudyRegion))
      shpSubStudyRegion <- rgeos::gBuffer(shpSubStudyRegion, width = 0, byid = TRUE)
    } else if (any(subStudyRegionName %in% canadaAdminNamesAll)) {
      canadaMap <- Cache(getData, "GADM", country = "CAN", level = 1, path = "inputs",
                         cacheRepo = paths$cachePath, userTags = "stable")
      subStudyRegionName <- canadaAdminNames[canadaAdminNames %in% subStudyRegionName |
                                               names(canadaAdminNames) %in% subStudyRegionName]
      inputMapPolygon <- spTransform(canadaMap[canadaMap$NAME_1 %in% subStudyRegionName, ], crsStudyRegion)
      aa <- sf::st_intersection(sf::st_as_sf(shpStudyRegion), sf::st_as_sf(inputMapPolygon))
      shpSubStudyRegion <- as(aa, "Spatial")
    } else {
      #set.seed(5567913)
      set.seed(853839)
      if ("VERYSMALL" %in% subStudyRegionName) {
        areaKm2 <- 3000
      } else if ("SMALL" %in% subStudyRegionName) {
        areaKm2 <- 10000
      } else if ("MEDIUM" %in% subStudyRegionName) {
        areaKm2 <- 40000
      } else if ("LARGE" %in% subStudyRegionName) {
        areaKm2 <- 80000
      } else if ("VERYLARGE" %in% subStudyRegionName) {
        areaKm2 <- 180000
      }

      minY <- 7678877 - 3.6e5
      minX <- -1002250.2
      maxX <- minX + sqrt(areaKm2 * 1e6)
      maxY <- minY + sqrt(areaKm2 * 1e6)
      meanY <- mean(c(minY, maxY))

      # Add random noise to polygon
      xAdd <- -3e5 #round(runif(1,-5e5, 1.5e6))
      yAdd <- 5e5  #round(runif(1, 1e5, 5e5)) - xAdd / 2
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
      crs(inputMapPolygon) <- crsStudyRegion
      shpSubStudyRegion <- raster::intersect(shpStudyRegion, inputMapPolygon)
      options("digits.secs" = 7)
      on.exit(options("digits.secs" = NULL))
      set.seed(as.numeric(format(Sys.time(), format = "%OS")))
    }
  } else {
    shpSubStudyRegion <- shpStudyRegion
  }

  return(shpSubStudyRegion)
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
