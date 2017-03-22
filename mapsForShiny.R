if (FALSE) {
  readSpTransform <- function(shapefilePath, crs, cacheRepo){
    AlbertaFMUFull <- shapefile(shapefilePath)
    AlbertaFMUFull <- spTransform(AlbertaFMUFull, crs)
  }
  AlbertaFMUFull <- Cache(cacheRepo = paths$cachePath,
                          readSpTransform, 
                          shapefilePath=file.path(paths$inputPath, "FMU_Alberta_2015-11", "FMU_Alberta_2015-11"),
                          crs = crs(shpStudyRegion))
  AlbertaFMU <- Cache(crop, AlbertaFMUFull, shpStudyRegion, cacheRepo = paths$cachePath)
  
}

ecodistricts <- Cache(shapefile, file.path(paths$modulePath,"LW_LBMRDataPrep", "data", "ecodistricts"),
                      cacheRepo = paths$cachePath)
ecodistrictsFull <- Cache(shapefile, file.path(paths$modulePath,"LW_LBMRDataPrep", "data", "ecodistricts"),
                          cacheRepo = paths$cachePath)
shpStudyRegionEco <- spTransform(shpStudyRegion, crs(ecodistricts))
ecodistrictsStudyRegion <- Cache(crop, ecodistricts, shpStudyRegionEco, cacheRepo = paths$cachePath)
#ecodistrictsCan <- spTransform(ecodistrictsStudyRegion, crs(CanadaMap))
ecodistricts <- spTransform(ecodistrictsStudyRegion, crs(shpStudyRegion))

lflt <- "+init=epsg:4326"

# Available polygons
ecodistrictsDemoLFLT <- spTransform(ecodistricts, sp::CRS(lflt))
ecodistrictsFullLFLT <- spTransform(ecodistrictsFull, sp::CRS(lflt))
#AlbertaFMUDemoLFLT <- spTransform(AlbertaFMU, sp::CRS(lflt))
#AlbertaFMUFullLFLT <- spTransform(AlbertaFMUFull, sp::CRS(lflt))
ecodistrictsDemo <- ecodistricts
#AlbertaFMUDemo <- AlbertaFMU
#AlbertaFMUFull <- AlbertaFMUFull

availablePolygons <- c("ecodistricts")#, "AlbertaFMU")
availablePolygonAdjective <- c("Ecodistrict")#, "AlbertaFMU")
availableProjections <- c("", "LFLT")
availableScales <- c("Full", "Demo")
available <- data.frame(stringsAsFactors = FALSE,
                        expand.grid(stringsAsFactors = FALSE,
                                    polygons = availablePolygons,
                                    scales = availableScales,
                                    projections = availableProjections),
                        names = rep(c("Ecodistricts Full", #"Alberta FMUs Full", 
                                      "Ecodistricts Demo"#, "Alberta FMUs Demo"
                        ), 2))
polygons <- lapply(seq_len(NROW(available)), function(ii) {
  get(paste0(available$polygons[ii], available$scales[ii], available$projections[ii]))}) %>%
  setNames(available$names)

rm(ecodistrictsFull)
rm(ecodistrictsFullLFLT)

polygonColours <- c(rep(c("red", "blue"), 2))
polygonIndivIdsColum <- list("ECODISTRIC", "FMU_NAME") %>% setNames(names(polygons[1:(length(polygons)/4)+(length(polygons)/4)*3]))

timeSinceFirePalette <- leaflet::colorNumeric(
  c(rep("red", 10), paste0(colorRampPalette(c("light green", "dark green"))(100),"FF")),
  domain = NULL)
attr(timeSinceFirePalette, "colorArgs")$na.color <- "#00000000"
