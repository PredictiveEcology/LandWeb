makeTiles <- function(sim) {
  outs <- outputs(sim)
  savedObjs <- outs$objectName
  savedObjsUnique <- unique(savedObjs)
  names(savedObjs) <- savedObjs
  names(savedObjsUnique) <- savedObjsUnique
  rastFiles <- outs[grepl("\\.tif|\\.grd", outs$file), "file"]

  rasts <- lapply(rastFiles, function(r) {
    raster(r)
  })
  outputPath <- file.path("www", "tiles")
  sim$allRasters <- Cache(lapply, rasts, function(r)
    gdal2Tiles(
      r,
      outputPath,
      zoomRange = 1:11,
      colorTableFile = asPath(colorTableFile),
      rasterForTransparency = asPath(file.path(outputPath(sim), "rstFlammable.grd")),
      cacheRepo = cachePath(sim)
    ))
  return(invisible(sim))
}
