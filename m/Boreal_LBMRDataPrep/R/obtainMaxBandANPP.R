obtainMaxBandANPP <- function(speciesLayers, biomassLayer, SALayer, ecoregionMap) {
  speciesinstudyarea <- crop(speciesLayers, ecoregionMap) # slowest
  speciesinstudyarea <- suppressWarnings(mask(speciesinstudyarea, ecoregionMap))
  biomassinstudyarea <- crop(biomassLayer, ecoregionMap)
  biomassinstudyarea <- suppressWarnings(mask(biomassinstudyarea, ecoregionMap))
  speciesTable <- data.table(biomass = getValues(biomassinstudyarea))
  SAinstudyarea <- crop(SALayer, ecoregionMap)
  SAinstudyarea <- suppressWarnings(mask(SAinstudyarea, ecoregionMap))

  speciesTable[, ':='(SA = getValues(SAinstudyarea), ecoregion = getValues(ecoregionMap))]
  outputPartial <- data.table(ecoregion = numeric(), species = character(),
                              maxBiomass = numeric(), maxANPP = numeric())
  speciess <- names(speciesLayers)
  for (species in speciess) {
    indispeciesraster <- raster::subset(speciesinstudyarea, species)
    speciesTable[, percentage := getValues(indispeciesraster)]
    speciesTable_narmed <- speciesTable[!is.na(ecoregion), ]
    speciesTable_narmed[, speciesBiomass := biomass*percentage*0.01]
    speciesTable_narmed <- speciesTable_narmed[percentage >= 50, ]
    speciesTable_narmed[,species := species]
    speciesTable_narmed <- speciesTable_narmed[,.(maxBiomass = 100*quantile(speciesBiomass, 0.8, na.rm = TRUE)),
                                               by = c("ecoregion", "species")]
    speciesTable_narmed[, maxANPP := maxBiomass/30]
    outputPartial <- rbind(outputPartial, speciesTable_narmed)
  }
  output <- data.table(expand.grid(ecoregion = as.numeric(unique(getValues(ecoregionMap))),
                                   species = speciess))[!is.na(ecoregion),][,species := as.character(species)]
  output <- dplyr::left_join(output, outputPartial, by = c("ecoregion", "species")) %>%
    data.table()
  return(speciesBiomass = output)
}
