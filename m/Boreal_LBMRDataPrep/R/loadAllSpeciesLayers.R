loadAllSpeciesLayers <- function(dataPath, biomassMap, shpStudyRegionFull, moduleName,
                                 cachePath, ...) {
  speciesNamesEnd <- c("Abie_sp", "Pice_Gla", "Pice_Mar", "Pinu_sp", "Popu_Tre")
  speciesnamesRaw <- c("Abie_Las", "Pice_Gla", "Pice_Mar", "Pinu_Ban", "Pinu_Con", "Popu_Tre")
  species1 <- list()
  a11 <- 1
  suffix <- if (basename(cachePath) == "cache") paste0(as.character(ncell(biomassMap)),"px") else
    basename(cachePath)
  suffix <- paste0("_", suffix)
  for (sp in speciesnamesRaw) {
    targetFile <- paste0("NFI_MODIS250m_kNN_Species_", sp, "_v0.tif")
    postProcessedFilename <- .suffix(targetFile, suffix = suffix)
    species1[[sp]] <- prepInputs(
      targetFile = targetFile,
      archive = asPath(c("kNN-Species.tar", paste0("NFI_MODIS250m_kNN_Species_", sp, "_v0.zip"))),
      #alsoExtract = if (sp == speciesnamesRaw[1]) paste0("NFI_MODIS250m_kNN_Species_", speciesnamesRaw[-1], "_v0.tif"),
      destinationPath = asPath(dataPath),
      fun = "raster::raster",
      studyArea = shpStudyRegionFull,
      rasterToMatch = biomassMap,
      method = "bilinear",
      datatype = "INT2U",
      postProcessedFilename = postProcessedFilename
    )
  }

  sumSpecies <- c("Pinu_Ban", "Pinu_Con")
  newLayerName <- grep("Pinu", speciesNamesEnd, value = TRUE)
  fname <- .suffix(file.path(dataPath, "KNNPinu_sp.tif"), suffix)
  a <- Cache(sumRastersBySpecies,
             species1[sumSpecies], newLayerName = newLayerName,
             filenameToSave = asPath(fname),
             ...)
  a <- raster(fname) ## ensure a gets a filename
  species1[sumSpecies] <- NULL
  species1[[newLayerName]] <- a
  names(species1)[grep("Abie", names(species1))] <- grep("Abie", speciesNamesEnd, value = TRUE)
  names(species1) <- toSentenceCase(names(species1))

  stack(species1)
}
