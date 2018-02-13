obtainMaxBandANPPFormBiggerEcoArea <- function(speciesLayers,
                                               biomassLayer,
                                               SALayer,
                                               ecoregionMap,
                                               biggerEcoArea,
                                               biggerEcoAreaSource,
                                               NAData,
                                               maskFn) {
  subEcoregion <- ecoregionMap
  subEcoregion[!(getValues(subEcoregion) %in% unique(NAData$ecoregion))] <- NA
  subbiggerEcoMap <- raster::crop(biggerEcoArea, subEcoregion)
  if (biggerEcoAreaSource == "ecoRegion") {
    subbiggerEcoLevel <- unique(subbiggerEcoMap@data$ECOREGION)
    subbigEcoMap <- biggerEcoArea[biggerEcoArea@data$ECOREGION %in% subbiggerEcoLevel, ]
  } else if (biggerEcoAreaSource == "ecoZone") {
    subbiggerEcoLevel <- unique(subbiggerEcoMap@data$ECOZONE)
    subbigEcoMap <- biggerEcoArea[biggerEcoArea@data$ECOZONE %in% subbiggerEcoLevel, ]
  }
  subbiggerEcoMap_Raster <- crop(biomassLayer, subbigEcoMap)
  subbiggerEcoMap_Raster <- setValues(subbiggerEcoMap_Raster, NA)


  biggerEcoMapRaster <- fasterize::fasterize(sf::st_as_sf(subbigEcoMap),
                                             raster = subbiggerEcoMap_Raster, field = toupper(biggerEcoAreaSource))

  biggerEcoMapRaster_ST <- crop(biggerEcoMapRaster, subEcoregion)
  biggerEcoMapRaster_ST <- suppressWarnings(mask(biggerEcoMapRaster_ST, subEcoregion))


  ecodistrictEcoregionTable <- data.table(ecoregion = getValues(subEcoregion),
                                          biggerEcoregion = getValues(biggerEcoMapRaster_ST))[!is.na(ecoregion),]
  #check whether one district has more than one ecoregion, which is not correct
  ecodistrictEcoregionTable[,totLength := length(biggerEcoregion), by = ecoregion]
  ecodistrictEcoregionTable[,ecoLength := length(totLength), by = c("biggerEcoregion", "ecoregion")]
  ecodistrictEcoregionTable[, percentage := ecoLength/totLength]
  ecodistrictEcoregionTable[, maxPercent := max(percentage), by = ecoregion]
  ecodistrictEcoregionTable <- ecodistrictEcoregionTable[percentage == maxPercent, .(biggerEcoregion, ecoregion)] %>%
    unique(., by = c("biggerEcoregion", "ecoregion"))
  # don't need to Cache because whole outer function is cached
  ecoregionBiomass <- obtainMaxBandANPP(speciesLayers = speciesLayers,
                                        biomassLayer = biomassLayer,
                                        SALayer = SALayer,
                                        ecoregionMap = biggerEcoMapRaster)
  setnames(ecoregionBiomass, "ecoregion", "biggerEcoregion")
  NAData <- setkey(NAData, ecoregion)[setkey(ecodistrictEcoregionTable, ecoregion), nomatch = 0]
  NAData[,species := as.character(species)]
  NAData <- dplyr::left_join(NAData[,.(biggerEcoregion, ecoregion, species, SEP)], ecoregionBiomass,
                             by = c("biggerEcoregion", "species")) %>%
    data.table
  return(list(addData = NAData, biggerEcoMapRaster = biggerEcoMapRaster))
}
