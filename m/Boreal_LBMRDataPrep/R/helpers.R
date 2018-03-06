createInitCommMap <- function(initCommMap, values, filename) {
  map <- raster::setValues(initCommMap, values = values)
  raster::writeRaster(map, overwrite = TRUE, filename = filename, datatype = "INT2U")
}

sumRastersBySpecies <- function(speciesLayers, layersToSum,
                                filenameToSave, newLayerName, cachePath) {
  ras_out <- raster::calc(raster::stack(speciesLayers[layersToSum]), sum)
  names(ras_out) <- newLayerName
  writeRaster(ras_out, filename = filenameToSave, datatype = "INT2U", overwrite = TRUE)
  ras_out # Work around for Cache
}

toSentenceCase <- function(x) {
  newNames <- tolower(x)
  substr(newNames, 1, 1) <- toupper(substr(newNames, 1, 1))
  newNames
}
