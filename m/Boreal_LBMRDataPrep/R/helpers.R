createInitCommMap <- function(initCommMap, values, filename) {
  map <- setValues(initCommMap, values = values)
  writeRaster(map, overwrite = TRUE, filename = filename, datatype = "INT2U")
}

sumRastersBySpecies <- function(specieslayers, layersToSum,
                                filenameToSave, newLayerName, cachePath) {
  Pinu_sp <- calc(stack(specieslayers[layersToSum]), sum)
  names(Pinu_sp) <- newLayerName
  writeRaster(Pinu_sp,
              filename = filenameToSave,
              datatype = "INT2U", overwrite = TRUE)
  Pinu_sp # Work around for Cache
}

toSentenceCase <- function(strings) {
  newNames <- tolower(strings)
  substr(newNames, 1, 1) <- toupper(substr(newNames, 1, 1))
  newNames
}
