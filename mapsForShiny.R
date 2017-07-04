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
#lflt <- "+init=epsg:4326"
lflt <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

getEcoMaps <- function(ecoDistrictPath, cacheRepo, lfltEPSG) {
  ecodistricts <- shapefile(ecoDistrictPath)
  ecodistrictsFull <- shapefile(ecoDistrictPath)
  shpStudyRegionEco <- spTransform(shpStudyRegion, crs(ecodistricts))
  ecodistrictsStudyRegion <- crop(ecodistricts, shpStudyRegionEco)
  #ecodistrictsCan <- spTransform(ecodistrictsStudyRegion, crs(CanadaMap))
  ecodistricts <- spTransform(ecodistrictsStudyRegion, crs(shpStudyRegion))
  
  # Available polygons
  ecodistrictsDemoLFLT <- spTransform(ecodistricts, sp::CRS(lfltEPSG))
  ecodistrictsFullLFLT <- spTransform(ecodistrictsFull, sp::CRS(lfltEPSG))
  #AlbertaFMUDemoLFLT <- spTransform(AlbertaFMU, sp::CRS(lfltEPSG))
  #AlbertaFMUFullLFLT <- spTransform(AlbertaFMUFull, sp::CRS(lfltEPSG))
  ecodistrictsDemo <- ecodistricts
  #AlbertaFMUDemo <- AlbertaFMU
  #AlbertaFMUFull <- AlbertaFMUFull
  list(ecodistricts=ecodistricts,
       ecodistrictsDemo=ecodistrictsDemo,
       ecodistrictsFull=ecodistrictsFull,
       ecodistrictsDemoLFLT=ecodistrictsDemoLFLT,
       ecodistrictsFullLFLT=ecodistrictsFullLFLT
       )
  
}

out <- Cache(getEcoMaps, ecoDistrictPath=asPath(file.path(paths$modulePath,"LW_LBMRDataPrep", "data", "ecodistricts")), 
             lfltEPSG=lflt, cacheRepo=paths$cachePath, digestPathContent = TRUE)
list2env(out, envir=.GlobalEnv)

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

timeSinceFirePalette <- leaflet::colorNumeric(na.color = "transparent",
  c(rep("red", 2), rep("orange", 2), rep("yellow", 2), paste0(colorRampPalette(c("light green", "dark green"))(30),"FF")),
  domain = NULL)
