#' A recursive function for calculating patch size, given tsf and vtm files
largePatchesCalc <- function(tsfFile, vtmFile, byPoly, polyName,
                             ageClasses, ageClassCutOffs,
                             labelColumn, id = NULL, useParallelCluster = NULL,
                             .largePatchesCalc) {
  if (is.list(byPoly)) {
    moreArgsList <- list(tsfFile = tsfFile,
                         vtmFile = vtmFile,
                         ageClasses = ageClasses,
                         ageClassCutOffs = ageClassCutOffs,
                         labelColumn = labelColumn,
                         id = id,
                         useParallelCluster = useParallelCluster,
                         .largePatchesCalc = .largePatchesCalc)
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
      rasTmp <- raster(unlist(tsfFile)[[1]])
      if (missing(polyName)) polyName <- "Unnamed Polygon"
      message("" , polyName, ": Calculating patch sizes")
      tries <- 0
      while(tries >= 0) {
        cl <- makeOptimalCluster(useParallel = useParallelCluster,
                                 MBper = ncell(rasTmp) / 4000,
                                 maxNumClusters = length(tsfFile))
        on.exit(try(parallel::stopCluster(cl), silent = TRUE))
        out <- try(Cache(Map2, cl = cl,
                   tsfFile = tsfFile, vtmFile = vtmFile,
                   id = id,
                   fun = largePatchesCalc,
                   MoreArgs = list(
                     byPoly = byPoly,
                     polyName = polyName,
                     ageClasses = ageClasses,
                     ageClassCutOffs = ageClassCutOffs,
                     labelColumn = labelColumn,
                     .largePatchesCalc = .largePatchesCalc
                   ),
                   .scheduling = "dynamic",
                   omitArgs = "cl")
        )
        try(parallel::stopCluster(cl), silent = TRUE)
        if (tries > 2) stop("Needs manual intervention, Map2 of largePatchesCalc keeps failing")
        if (is(out, "try-error")) {
          tries <- tries + 1
        } else {
          tries <- -1
        }
      }
      out <- rbindlist(out)
      message("Running gc for list of tsfs")
      .gc()
      message("  End running gc for list of tsfs")
    } else {
      # The single tsf/vtm; once for each raster combo
      startTime <- Sys.time()
      message(" ", polyName,": ", basename(tsfFile), " -- calculating patch sizes")
      out <- Cache(.largePatchesCalc,
                   tsfFile = tsfFile, vtmFile = vtmFile,
                   byPoly = byPoly, labelColumn = labelColumn,
                   id = id, ageClassCutOffs = ageClassCutOffs,
                   ageClasses = ageClasses)
      endTime <- Sys.time()
      message("Running gc")
      .gc()
      message("  End running gc")
      message(" ", polyName,": ", basename(tsfFile), " -- patch size calculation took ",
              format(endTime - startTime, digits = 2))
    }
  }
  return(out)
}

.largePatchesCalc <- function(tsfFile, vtmFile, byPoly, labelColumn,
                              id, ageClassCutOffs, ageClasses) {
  timeSinceFireFilesRast <- Cache(rasterToMemory, tsfFile)

  tsf <- reclassify(timeSinceFireFilesRast,
                    cbind(from = ageClassCutOffs - 0.1,
                          to = c(ageClassCutOffs[-1], Inf),
                          seq_along(ageClasses)))
  levels(tsf) <- data.frame(ID = seq_along(ageClasses), Factor = ageClasses)

  byPoly$tmp <- factor(byPoly[[labelColumn]])
  rasRepPoly <- Cache(
    fasterize2,
    byPoly,
    emptyRaster = raster(timeSinceFireFilesRast), # doesn't need to the data -- makes Caching more effective
    field = "tmp"
  )
  # rasRepPoly2 <- fasterize::fasterize(sf::st_as_sf(byPoly),
  #                                    raster = timeSinceFireFilesRast, field = "tmp")
  # levels(rasRepPoly2) <-
  #   data.frame(ID = seq_len(nlevels(byPoly$tmp)),
  #              Factor = levels(byPoly$tmp))

  # 3rd raster
  rasVeg <- Cache(rasterToMemory, vtmFile)

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
  ras <- raster(rasVeg)
  ffFactor <- factor(ff)
  ras[!nas] <- ffFactor # 2 seconds

  areaAndPolyOut <- Cache(areaAndPolyValue, ras, length = Inf) # maybe lots of NAs on edge
  eTable <- data.frame(ID = seq_along(levels(ffFactor)), VALUE = levels(ffFactor))
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
  areaAndPolyOut2 <- Cache(areaAndPolyValue, ras, length = Inf) # maybe lots of NAs on edge

  eTable <- data.frame(ID = seq_along(levels(ffFactor)), VALUE = levels(ffFactor))
  types <- strsplit(as.character(eTable$VALUE), split = splitVal)
  types <- do.call(rbind, types)
  facPolygonID <- factor(types[areaAndPolyOut2$polyID, 2])
  outAllSpecies <- data.table(polygonID = as.numeric(facPolygonID),
                              sizeInHa = areaAndPolyOut2$sizeInHa,
                              vegCover = "All species",
                              rep = id,
                              ageClass = types[areaAndPolyOut2$polyID, 1],
                              polygonName = as.character(facPolygonID))

  out <- rbindlist(list(outBySpecies, outAllSpecies))
  out <- out[sizeInHa >= 100] # never will need patches smaller than 100 ha
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
  system2('python', args = (sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
                                    pypath, rastpath, gdalformat, outshape)))
  if (isTRUE(readpoly)) {
    shp <- sf::read_sf(dsn = dirname(outshape), layer = basename(outshape))
    return(shp)
  }
  return(NULL)
}

areaAndPolyValue <- function(ras) {
  polyIndivSpecies <- gdal_polygonizeR(ras) # 99 seconds with full ras
  pArea <- as.numeric(sf::st_area(polyIndivSpecies) / 1e4)
  list(sizeInHa = pArea, polyID = polyIndivSpecies$DN)
}
