labelColumn <- "shinyLabel"
lflt <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

reportingPolygons <- Cache(createReportingPolygons,
                            c("Alberta Ecozones", "National Ecozones", 
                            "National Ecodistricts", "Forest Management Areas", 
                            "Alberta FMUs", "Caribou Herds"))
  
########################################################
########################################################
### CURRENT CONDITION ##################################
message("Loading Current Condition Rasters")
dPath <- file.path(paths$inputPath, "CurrentCondition")
CCspeciesNames <- c("Pine", "Age", "BlackSpruce", "Deciduous", "Fir", "LandType", "WhiteSpruce")
rstCurrentConditionList <- Cache(loadCCSpecies, CCspeciesNames, 
                                 url = "https://drive.google.com/open?id=1JnKeXrw0U9LmrZpixCDooIm62qiv4_G1",
                                 dPath = dPath)

#############################################################################
#############################################################################


#availablePolygons <- c("ecodistricts")#, "AlbertaFMU")
if (FALSE) { #NOT USED
  availablePolygonAdjective <- c("Ecodistrict")#, "AlbertaFMU")
  
  rm(ecodistrictsFull)
  rm(ecodistrictsFullLFLT)
  
  polygonColours <- c(rep(c("red", "blue"), 2))
  polygonIndivIdsColum <- list("ECODISTRIC", "FMU_NAME") %>%
    setNames(names(polygons[1:(length(polygons) / 4) + (length(polygons) / 4) * 3]))
  
  colorVec <- diff(c(ageClassCutOffs[1],10,30,50,ageClassCutOffs[-(1:4)],maxAge))
  
  timeSinceFirePalette <- leaflet::colorNumeric(
    na.color = "transparent",
    c(rep("red", colorVec[1]), rep("orange", colorVec[2]), rep("yellow", colorVec[3]),
      paste0(colorRampPalette(c("light green", "dark green"))(colorVec[4]), "FF")),
    domain = NULL
  )
  
  fireReturnIntervalPalette <- leaflet::colorFactor(
    "Spectral", shpStudyRegionFull$fireReturnInterval
  )
  
  colorTableFile <<- file.path("www", studyArea, "color_table.txt")
  checkPath(dirname(colorTableFile), create = TRUE)
  color_tableFn <- function(timeSinceFirePalette, maxAge) {
    a <- t(sapply(timeSinceFirePalette(1:maxAge), col2rgb))
    rownames(a) <- NULL
    a <- rbind(rep(0,4), cbind(1:maxAge, a))
    write.table(a, file = colorTableFile, append = FALSE, row.names = FALSE, col.names = FALSE)
  }
  color_tableFn(timeSinceFirePalette, maxAge)
  
  
  
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

}
