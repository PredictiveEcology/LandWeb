
#### 
# This will load in several maps that are used 

####

CanadaMap <- readRDS(file.path(paths$inputPath, "CanadaMap.rds"))
landisInputs <- readRDS(file.path(paths$inputPath, "landisInputs.rds"))
spEcoReg <- readRDS(file.path(paths$inputPath, "SpEcoReg.rds"))

seralStageData <- readRDS(file.path(paths$inputPath, "seralStageData.rds"))
vegTypeData <- readRDS(file.path(paths$inputPath, "vegTypeData.rds"))
availableRegions <- unique(vegTypeData$ecoregion)


shpStudyRegionFull <- SpaDES::Cache(shapefile, file.path(paths$inputPath,"shpLandWEB.shp"),
                                    cacheRepo = paths$cachePath)
shpStudyRegionFull$fireReturnInterval <- shpStudyRegionFull$LTHRC
shpStudyRegionFull@data <- shpStudyRegionFull@data[,!(names(shpStudyRegionFull) %in% "ECODISTRIC")]

crsKNNMaps <- CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
set.seed(853839)#set.seed(5567913)
if (studyArea == "SMALL") {
  areaKm2 <- 10000#700000#2000#600000#too big for laptop
} else if (studyArea == "VERYSMALL") {
  areaKm2 <- 1000 #700000#2000#600000#too big for laptop
} else if (studyArea == "MEDIUM") {
  areaKm2 <- 40000 #700000#2000#600000#too big for laptop
} else if (studyArea == "LARGE") {
  areaKm2 <- 80000 #700000#2000#600000#too big for laptop
}
minY <- 7778877 - 1.6e5
shpStudyRegionFull <- spTransform(shpStudyRegionFull, crsKNNMaps)
minX <- -1202250.2
maxX <- minX + sqrt(areaKm2 * 1e6)
maxY <- minY + sqrt(areaKm2 * 1e6)
meanY <- mean(c(minY, maxY))

# Add random noise to polygon
xAdd <- -3e5#round(runif(1,-5e5, 1.5e6))
yAdd <- 5e5#round(runif(1, 1e5, 5e5)) - xAdd / 2
nPoints <- 20
betaPar <- 0.6
X <- c(jitter(sort(rbeta(nPoints, betaPar, betaPar) * (maxX - minX) + minX)),
       jitter(sort(rbeta(nPoints, betaPar, betaPar) * (maxX - minX) + minX, decreasing = TRUE)))
Y <- c(jitter(sort(rbeta(nPoints / 2, betaPar, betaPar) * (maxY - meanY) + meanY)),
       jitter(sort(rbeta(nPoints, betaPar, betaPar) * (maxY - minY) + minY, decreasing = TRUE)),
       jitter(sort(rbeta(nPoints / 2, betaPar, betaPar) * (meanY - minY) + minY)))

Sr1 <- Polygon(cbind(X + xAdd, Y + yAdd))
Srs1 <- Polygons(list(Sr1), "s1")
inputMapPolygon <- SpatialPolygons(list(Srs1), 1L)
crs(inputMapPolygon) <- crsKNNMaps
shpStudyRegion <- raster::intersect(shpStudyRegionFull, inputMapPolygon)

ggStudyRegion <- ggvisFireReturnInterval(shpStudyRegion, shpStudyRegionFull)

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
ecodistrictsCan <- spTransform(ecodistrictsStudyRegion, crs(CanadaMap))
ecodistricts <- spTransform(ecodistrictsStudyRegion, crs(shpStudyRegion))

lflt <- "+init=epsg:4326"

# Available polygons
ecodistrictsDemoLFLT <- spTransform(ecodistricts, sp::CRS(lflt))
ecodistrictsFullLFLT <- spTransform(ecodistrictsFull, sp::CRS(lflt))
#AlbertaFMUDemoLFLT <- spTransform(AlbertaFMU, sp::CRS(lflt))
#AlbertaFMUFullLFLT <- spTransform(AlbertaFMUFull, sp::CRS(lflt))
ecodistrictsDemo <- ecodistricts
ecodistrictsFull <- ecodistrictsFull
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

polygonColours <- c(rep(c("red", "blue"), 2))
polygonIndivIdsColum <- list("ECODISTRIC", "FMU_NAME") %>% setNames(names(polygons[1:(length(polygons)/4)+(length(polygons)/4)*3]))

timeSinceFirePalette <- colorNumeric(
  c(rep("red", 10), paste0(colorRampPalette(c("light green", "dark green"))(100),"FF")),
  domain = NULL)
attr(timeSinceFirePalette, "colorArgs")$na.color <- "#00000000"

