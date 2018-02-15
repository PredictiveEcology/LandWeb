obtainSEP <- function(speciesLayers, ecoregionMap) {
  speciesLayerEcoregion <- crop(speciesLayers, ecoregionMap)
  speciesLayerEcoregion <- suppressWarnings(mask(speciesLayerEcoregion, ecoregionMap))
  speciesTableEcoregion <- cbind(data.table(ecoregion = getValues(ecoregionMap)),
                                 data.table(getValues(speciesLayerEcoregion)))
  speciesTableEcoregion <- speciesTableEcoregion[complete.cases(speciesTableEcoregion)]
  speciesTableEcoregion <- melt.data.table(speciesTableEcoregion,
                                           measure.vars = names(speciesTableEcoregion)[-1],
                                           variable.name = "species")
  speciesTableEcoregion[value < 10, presence := 0]
  speciesTableEcoregion[value >= 10, presence := 1]
  speciesTableEcoregion <- speciesTableEcoregion[,.(relativeAbundance = sum(presence)/length(value)),
                                                 by = c("ecoregion", "species")]
  speciesLevels <- unique(speciesTableEcoregion$species)
  abundanceMapStack <- stack()
  names(ecoregionMap) <- "ecoregion"
  for (i in 1:length(speciesLevels)) {
    speciesTableEcoregionBySpecies <- speciesTableEcoregion[species == speciesLevels[i], ][
      , species := NULL]
    abundanceMap <- rasterizeReduced(speciesTableEcoregionBySpecies, ecoregionMap, "relativeAbundance")
    names(abundanceMap) <- as.character(speciesLevels[i])
    abundanceMapStack <- stack(abundanceMapStack, abundanceMap)
  }
  return(speciesAbundanceTable = speciesTableEcoregion)
}
