createInitCommMap <- function(initCommMap, values, filename) {
  map <- raster::setValues(initCommMap, values = values)
  raster::writeRaster(map, overwrite = TRUE, filename = filename, datatype = "INT2U")
}

sumRastersBySpecies <- function(speciesLayers, layersToSum,
                                filenameToSave, newLayerName, cachePath) {
  Pinu_sp <- calc(stack(specieslayers[layersToSum]), sum)
  names(Pinu_sp) <- newLayerName
  Pinu_sp <- writeRaster(Pinu_sp,
              filename = filenameToSave,
              datatype = "INT2U", overwrite = TRUE)
  Pinu_sp # Work around for Cache
}

toSentenceCase <- function(x) {
  newNames <- tolower(x)
  substr(newNames, 1, 1) <- toupper(substr(newNames, 1, 1))
  newNames
}
