loadAllSpeciesLayers <- function(dataPath, biomassMap, shpStudyRegionFull, moduleName,
                                 cachePath, cacheTags, quickCheck = FALSE) {
  speciesNamesEnd <- c("Abie_sp", "Pice_Gla", "Pice_Mar", "Pinu_sp", "Popu_Tre")
  speciesnamesRaw <- c("Abie_Las", "Pice_Gla", "Pice_Mar", "Pinu_Ban", "Pinu_Con", "Popu_Tre")
  species1 <- list()
  a11 <- 1
  for (sp in speciesnamesRaw) {
    species1[[sp]] <- prepInputs(
      targetFile = paste0("NFI_MODIS250m_kNN_Species_", sp, "_v0.tif"),
      archive = asPath(c("kNN-Species.tar", paste0("NFI_MODIS250m_kNN_Species_", sp, "_v0.zip"))),
      #alsoExtract = if (sp == speciesnamesRaw[1]) paste0("NFI_MODIS250m_kNN_Species_", speciesnamesRaw[-1], "_v0.tif"),
      destinationPath = asPath(dataPath),
      fun = "raster",
      pkg = "raster",
      studyArea = shpStudyRegionFull,
      rasterToMatch = biomassMap,
      rasterInterpMethod = "bilinear",
      rasterDatatype = "INT2U",
      writeCropped = TRUE,
      cacheRepo = cachePath#,
      #cacheTags = c("stable", moduleName),
      #dataset = "EOSD2000",
      #quickCheck =  quickCheck
    )
  }

  sumSpecies <- c("Pinu_Ban", "Pinu_Con")
  newLayerName <- grep("Pinu", speciesNamesEnd, value = TRUE)
  fname <- .prefix(file.path(dataPath, "KNNPinu_sp.tif"), "Small")
  a <- Cache(sumRastersBySpecies,
             species1[sumSpecies], newLayerName = newLayerName,
             filenameToSave = fname,
             cacheRepo = cachePath, userTags = "stable")
  a <- raster(fname) ## ensure a gets a filename
  species1[sumSpecies] <- NULL
  species1[[newLayerName]] <- a
  names(species1)[grep("Abie", names(species1))] <- grep("Abie", speciesNamesEnd, value = TRUE)
  names(species1) <- toSentenceCase(names(species1))

  stack(species1)
}
