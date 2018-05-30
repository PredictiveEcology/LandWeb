largePatchesCalc <- function(tsfFile, vtmFile, byPoly, polyName,
                             ageClasses, ageClassCutOffs,
                             labelColumn, id = NULL, useParallelCluster = NULL) {
  if (is.list(byPoly)) {
    moreArgsList <- list(tsfFile = tsfFile,
                         vtmFile = vtmFile,
                         ageClasses = ageClasses,
                         ageClassCutOffs = ageClassCutOffs,
                         labelColumn = labelColumn,
                         id = id,
                         useParallelCluster = useParallelCluster)
    out <- Map(byPoly = byPoly,
               polyName = names(byPoly),
               largePatchesCalc,
               MoreArgs = moreArgsList)
  } else {
    if (is.null(id)) {
      id <- gsub(basename(tsfFile), pattern = "\\.tif", replacement = "")
      id <- strsplit(id, split = "_")
      id <- unlist(lapply(id, function(x) {
        x[length(x)]
      }))
    }
    if (length(tsfFile) > 1) {
      message("" , polyName, ": Calculating patch sizes")
      if (useParallelCluster) {
        library(parallel)
        message("  Starting a cluster with ", numClus," threads")
        cl <- parallel::makeForkCluster(numClus)
        clusterSetRNGStream(cl, sample(1e7,1))

        out <- clusterMap(cl = cl, tsfFile = tsfFile, vtmFile = vtmFile, id = id,
                          Cache,
                          MoreArgs = list(largePatchesCalc, byPoly = byPoly,
                                          polyName = polyName,
                                          ageClasses = ageClasses,
                                          ageClassCutOffs = ageClassCutOffs,
                                          labelColumn = labelColumn),
                          .scheduling = "dynamic")
      } else {
        out <- Map(tsfFile = tsfFile, vtmFile = vtmFile, id = id,
                   Cache,
                   MoreArgs = list(largePatchesCalc, byPoly = byPoly,
                                   polyName = polyName,
                                   ageClasses = ageClasses,
                                   ageClassCutOffs = ageClassCutOffs,
                                   labelColumn = labelColumn))

      }
      out1 <- rbindlist(out)
    } else {
      # once for each raster combo
      startTime <- Sys.time()
      message(" ", polyName,": ", basename(tsfFile), " -- calculating patch sizes")

      timeSinceFireFilesRast <- raster(tsfFile)
      timeSinceFireFilesRast[] <- timeSinceFireFilesRast[] # 10 seconds
      tsf <- reclassify(timeSinceFireFilesRast,
                        cbind(from = ageClassCutOffs-0.1, to = c(ageClassCutOffs[-1], Inf), seq_along(ageClasses)))
      levels(tsf) <- data.frame(ID = seq_along(ageClasses), Factor = ageClasses)

      byPoly$tmp <- factor(byPoly[[labelColumn]])
      rasRepPoly <- fasterize::fasterize(sf::st_as_sf(byPoly),
                                         raster = timeSinceFireFilesRast, field = "tmp")
      levels(rasRepPoly) <- data.frame(ID = seq_along(byPoly), Factor = byPoly[[labelColumn]])

      # 3rd raster
      rasVeg <- raster(vtmFile)
      rasVeg[] <- rasVeg[] # 3 seconds

      splitVal <- paste0("_", 75757575, "_") # unlikely to occur for any other reason

      # Individual species
      nas3 <- is.na(rasRepPoly[])
      nas2 <- is.na(rasVeg[])
      nas1 <- is.na(tsf[])
      nas <- nas3 | nas2 | nas1
      name1 <- as.character(raster::levels(tsf)[[1]]$Factor)[tsf[][!nas]]
      name2 <- as.character(raster::levels(rasVeg)[[1]]$Factor)[rasVeg[][!nas]]
      name3 <- as.character(raster::levels(rasRepPoly)[[1]]$Factor)[rasRepPoly[][!nas]]
      ff <- paste(name1, name2, name3, sep = splitVal) # 4 seconds
      #ff[nas] <- NA # faster
      #ff[grepl("NA", ff)] <- NA
      ras <- raster(rasVeg)
      ffFactor <- factor(ff)
      ras[!nas] <- ffFactor # 2 seconds

      #ras <- raster::trim(ras) # 18 seconds many polygons will be smaller than full area -- this takes of NAs
      areaAndPolyOut <- Cache(areaAndPolyValue, ras, length = Inf) # maybe lots of NAs on edge
      #polyIndivSpecies <- Cache(gdal_polygonizeR, ras) # 99 seconds with full ras
      #pArea <- as.numeric(sf::st_area(polyIndivSpecies)/1e4)
      #pArea <- rgeos::gArea(polyIndivSpecies, byid = TRUE)/1e4
      eTable <- data.frame(ID = seq_along(levels(ffFactor)), VALUE = levels(ffFactor))
      #eTable <- raster::levels(ras)[[1]]
      types <- strsplit(as.character(eTable$VALUE), split = splitVal)
      types <- do.call(rbind, types)

      facPolygonID <- factor(types[areaAndPolyOut$polyID,3])
      outBySpecies <- data.table(polygonID = as.numeric(facPolygonID),
                                 sizeInHa = areaAndPolyOut$sizeInHa,
                                 vegCover = types[areaAndPolyOut$polyID,2],
                                 rep = id,
                                 ageClass = types[areaAndPolyOut$polyID,1],
                                 polygonName = as.character(facPolygonID))

      # All species combined # remove name2
      ff <- paste(name1, name3, sep = splitVal)
      ff[grepl("NA", ff)] <- NA
      ras <- raster(rasVeg)
      ffFactor <- factor(ff)
      ras[!nas] <- ffFactor

      rm(areaAndPolyOut)
      #ras <- raster::trim(ras) # many polygons will be smaller than full area -- this takes of NAs
      areaAndPolyOut2 <- Cache(areaAndPolyValue, ras, length = Inf) # maybe lots of NAs on edge

      #polyAllSpecies <- gdal_polygonizeR(ras)
      #areaAndPolyOut2$sizeInHa <- as.numeric(sf::st_area(polyAllSpecies)/1e4 )
      eTable <- data.frame(ID = seq_along(levels(ffFactor)), VALUE = levels(ffFactor))
      #eTable <- raster::levels(ras)[[1]]
      types <- strsplit(as.character(eTable$VALUE), split = splitVal)
      types <- do.call(rbind, types)
      facPolygonID <- factor(types[areaAndPolyOut2$polyID,2])
      outAllSpecies <- data.table(polygonID = as.numeric(facPolygonID),
                                  sizeInHa = areaAndPolyOut2$sizeInHa,
                                  vegCover = "All species",
                                  rep = id,
                                  ageClass = types[areaAndPolyOut2$polyID,1],
                                  polygonName = as.character(facPolygonID))

      out <- rbindlist(list(outBySpecies, outAllSpecies))
      bb <- out[sizeInHa >= 100] # never will need patches smaller than 100 ha
      endTime <- Sys.time()
      message("    Patch size calculation took ", format(endTime - startTime, digits = 2))
    }
  }
}

#' Polygonize with gdal
#'
#' Copied from https://johnbaumgartner.wordpress.com/2012/07/26/getting-rasters-into-shape-from-r/
#'
gdal_polygonizeR <- function(x, outshape = NULL, gdalformat = 'ESRI Shapefile',
                             pypath = NULL, readpoly = TRUE, quiet = TRUE) {
  if (isTRUE(readpoly)) require(rgdal)
  if (is.null(pypath)) {
    pypath <- Sys.which('gdal_polygonize.py')
  }
  if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.")
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dirname(pypath))
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if (any(f.exists))
      stop(sprintf('File already exists: %s',
                   toString(paste(outshape, c('shp', 'shx', 'dbf'),
                                  sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    writeRaster(x, {f <- tempfile(fileext='.tif')})
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  system2('python', args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
                                  pypath, rastpath, gdalformat, outshape)))
  if (isTRUE(readpoly)) {
    #shp <- as(sf::read_sf(dsn = dirname(outshape), layer = basename(outshape)), "Spatial")
    shp <- sf::read_sf(dsn = dirname(outshape), layer = basename(outshape))
    #shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quiet)
    return(shp)
  }
  return(NULL)
}

areaAndPolyValue <- function(ras) {
  polyIndivSpecies <- gdal_polygonizeR(ras) # 99 seconds with full ras
  pArea <- as.numeric(sf::st_area(polyIndivSpecies)/1e4)
  list(sizeInHa = pArea, polyID = polyIndivSpecies$DN)
}
