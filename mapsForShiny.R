
labelColumn <- "shinyLabel"
lflt <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

## All FMAs - 
dPath <- file.path(paths$inputPath, "allFMAs")
allFMAsFilename <- asPath(file.path(dPath, "FMA_Boudary.shp"))
allFMAsFiles <- c("FMA_Boudary.CPG", "FMA_Boudary.dbf", "FMA_Boudary.prj", 
                     "FMA_Boudary.sbn", "FMA_Boudary.sbx", "FMA_Boudary.shp", "FMA_Boudary.shp.xml", 
                     "FMA_Boudary.shx")
shpAllFMAs <- Cache(prepInputs, userTags = "stable", 
                    url = "https://drive.google.com/open?id=1oCMiHRRT1bCWe0Uv69nRSrE1nsh-4Tic",
                    #targetFile = albertaFMUFilename,
                    #alsoExtract = albertaFMUFiles,
                    fun = "shapefile", 
                    destinationPath = dPath)
shpAllFMAs@data[[labelColumn]] <- shpAllFMAs$Name

# Alberta Ecozone
dPath <- asPath(file.path(paths$inputPath, "ecozones", "Alberta"))
albertaEcozoneFiles <- asPath(c("Natural_Regions_Subregions_of_Alberta.dbf", 
                                "Natural_Regions_Subregions_of_Alberta.lyr", "Natural_Regions_Subregions_of_Alberta.prj", 
                                "Natural_Regions_Subregions_of_Alberta.shp.xml", 
                                "Natural_Regions_Subregions_of_Alberta.shx", "natural_regions_subregions_of_alberta.zip", 
                                "nsr2005_final_letter.jpg", "nsr2005_final_letter.pdf"))
albertaEcozoneURL <- "https://www.albertaparks.ca/media/429607/natural_regions_subregions_of_alberta.zip"
albertaEcozoneFilename <- asPath("Natural_Regions_Subregions_of_Alberta.shp")
shpAlbertaEcozone <- Cache(prepInputs, userTags = "stable", 
                           url = albertaEcozoneURL, targetFile = albertaEcozoneFilename,
                           fun = "shapefile", destinationPath = dPath, alsoExtract = albertaEcozoneFiles)
shpAlbertaEcozone@data[[labelColumn]] <- shpAlbertaEcozone$NSRNAME

# Nationa Ecozone
dPath <- file.path(paths$inputPath, "ecozones", "National")
ecozoneFilename <-   file.path(dPath, "ecozones.shp")
ecozoneFiles <- c("ecozones.dbf", "ecozones.prj", 
                  "ecozones.sbn", "ecozones.sbx", "ecozones.shp", "ecozones.shx")
shpNationalEcozone <- Cache(prepInputs, userTags = "stable", 
                            url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
                            targetFile = asPath(ecozoneFilename),
                            alsoExtract = ecozoneFiles,
                            fun = "shapefile", destinationPath = dPath)
shpNationalEcozone@data[[labelColumn]] <- shpNationalEcozone$ZONE_NAME

# Nationa Ecodistrict
dPath <- file.path(paths$inputPath, "ecodistricts", "National")
ecodistrictFilename <-   file.path(dPath, "ecodistricts.shp")
ecodistrictFiles <- c("ecodistricts.dbf", "ecodistricts.prj", 
                      "ecodistricts.sbn", "ecodistricts.sbx", "ecodistricts.shp", "ecodistricts.shx")
shpNationalEcodistrict <- Cache(prepInputs, userTags = "stable", 
                                url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip",
                                targetFile = asPath(ecodistrictFilename),
                                alsoExtract = ecodistrictFiles,
                                fun = "shapefile", destinationPath = dPath)
shpNationalEcodistrict@data[[labelColumn]] <- shpNationalEcodistrict$ZONE_NAME


# Caribou Zones
dPath <- file.path(paths$inputPath, "Caribou")
caribouFilename <-   file.path(dPath, "LP_MASTERFILE_June62012.shp")
caribouFiles <- c("LP_MASTERFILE_June62012.dbf", "LP_MASTERFILE_June62012.prj", 
                  "LP_MASTERFILE_June62012.sbn", "LP_MASTERFILE_June62012.sbx", 
                  "LP_MASTERFILE_June62012.shp", "LP_MASTERFILE_June62012.shp.xml", 
                  "LP_MASTERFILE_June62012.shx")
CaribouZonesColumn <- "HERD"
shpCaribouZones <- Cache(prepInputs, userTags = "stable", 
                         url = "https://drive.google.com/file/d/1J38DKQQavjBV9F3z2gGzHNuNE0s2rmhh/view?usp=sharing",
                         targetFile = asPath(caribouFilename),
                         alsoExtract = caribouFiles,
                         fun = "shapefile", 
                         destinationPath = dPath)
shpCaribouZones@data[[labelColumn]] <- shpCaribouZones$HERD


## Alberta FMU - 

dPath <- file.path(paths$inputPath, "FMU_Alberta_2015-11")
albertaFMUFilename <- asPath(file.path(dPath, "FMU_Alberta_2015-11.shp"))
albertaFMUFiles <- c("FMU_Alberta_2015-11.cpg", "FMU_Alberta_2015-11.dbf", 
                     "FMU_Alberta_2015-11.prj", "FMU_Alberta_2015-11.sbn", 
                     "FMU_Alberta_2015-11.sbx", "FMU_Alberta_2015-11.shp", 
                     "FMU_Alberta_2015-11.shp.xml", "FMU_Alberta_2015-11.shx")
shpAlbertaFMU <- Cache(prepInputs, userTags = "stable", 
                       url = "https://drive.google.com/file/d/1JiCLcHh5fsBAy8yAx8NgtK7fxaZ4Tetl/view?usp=sharing",
                       targetFile = albertaFMUFilename,
                       alsoExtract = albertaFMUFiles,
                       fun = "shapefile", 
                       destinationPath = dPath)
shpAlbertaFMU@data[[labelColumn]] <- shpAlbertaFMU$FMU_NAME


# Put all polygons together in a list
# Polygons
availablePolygons <- grep("^shp.+", ls(), value = TRUE)
availablePolygons <- grep("Study", availablePolygons, invert = TRUE, value = TRUE) # remove shp
polygons <- mget(availablePolygons)

# Make them all crsStudyArea
polygons <- Cache(lapply, polygons, function(shp) {
  spTransform(shp, CRSobj = crsStudyArea)
}, userTags = "stable")

# Make SubRegion
polygonsSubRegion <- Cache(intersectListShps, polygons, shpStudyRegion, userTags = "stable")
names(polygonsSubRegion) <- paste0(names(polygonsSubRegion), "Demo")
polygons <- append(polygons, polygonsSubRegion)

#### Thin polygons
if (FALSE) {
message("Thinning polygons for faster plotting in leaflet")
polygons <- Cache(mapply, p = polygons, nam = names(polygons), userTags = "stable", 
                  function(p, nam) {
  print(nam)
  out <- Cache(rgeos::gSimplify, p, userTags = "stable", 
               tol = (xmax(p) - xmin(p))/10000, topologyPreserve = TRUE)
  #out <- suppressWarnings(thin(p))
  isSimp <- tryCatch(if(isTRUE(!all(rgeos::gIsSimple(out, byid = TRUE)))) FALSE else TRUE, 
                     error = function(xx) FALSE)
  browser(expr = "shpNationalEcodistrictDemo" %in% nam)
  #if (rgeos::gIsSimple(out)) out <- raster::buffer(out, width = 0, dissolve = FALSE)
  if (!isSimp) {
    out <- raster::buffer(out, width = 0, dissolve = FALSE)
  }
  out <- SpatialPolygonsDataFrame(out, data = p@data, match.ID = TRUE)
  
  return(out)
}) 
}

# Make Leaflet versions of all
message("Making leaflet versions of all reporting polygons")
polygonsLflt <- Cache(mapply, p = polygons, nam = names(polygons), userTags = "stable", 
                      function(p, nam) {
  message("  ", nam)
  out <- tryCatch(spTransform(p, CRSobj = CRS(lflt)), error = function(x) {
    p <- spChFIDs(p, as.character(seq(NROW(p))))
    spTransform(p, CRSobj = CRS(lflt))
  })
})
names(polygonsLflt) <- paste0(names(polygonsLflt), "LFLT")
polygons <- append(polygons, polygonsLflt)


availableScales <- c("Full", "Demo")
availableProjections <- c("", "LFLT") 

available <- data.frame(stringsAsFactors = FALSE,
                        expand.grid(stringsAsFactors = FALSE,
                                    polygons = availablePolygons,
                                    scales = availableScales,
                                    projections = availableProjections),
                        names = names(polygons)
)

#############################################################################
#############################################################################
########### OLD #############################################################
if (FALSE) {
  AlbertaFMUFull <- Cache(cacheRepo = paths$cachePath,
                          readSpTransform, userTags = "stable",
                          shapefilePath=file.path(paths$inputPath, "FMU_Alberta_2015-11", "FMU_Alberta_2015-11"),
                          crs = crs(shpStudyRegion))
  
  
  
  # out <- Cache(getEcoMaps, ecoDistrictPath=asPath(file.path(paths$modulePath,"Boreal_LBMRDataPrep", "data", "ecodistricts")),
  #              lfltEPSG=lflt, cacheRepo=paths$cachePath, digestPathContent = TRUE)
  # list2env(out, envir=.GlobalEnv)
}



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
  #lflt <- "+init=epsg:4326"
  
  # getEcoMaps <- function(ecoDistrictPath, cacheRepo, lfltEPSG) {
  #   ecodistricts <- shapefile(ecoDistrictPath)
  #   ecodistrictsFull <- shapefile(ecoDistrictPath)
  #   shpStudyRegionEco <- spTransform(shpStudyRegion, crs(ecodistricts))
  #   
  #   # There is a self intersection problem with ecodistricts file. This fixes it.
  #   ecodistricts <- raster::buffer(ecodistricts, width = 0, dissolve = FALSE)
  #   
  #   ecodistrictsStudyRegion <- crop(ecodistricts, shpStudyRegionEco)
  #   #ecodistrictsCan <- spTransform(ecodistrictsStudyRegion, crs(CanadaMap))
  #   ecodistricts <- spTransform(ecodistrictsStudyRegion, crs(shpStudyRegion))
  #   
  #   # Available polygons
  #   ecodistrictsDemoLFLT <- spTransform(ecodistricts, sp::CRS(lfltEPSG))
  #   ecodistrictsFullLFLT <- spTransform(ecodistrictsFull, sp::CRS(lfltEPSG))
  #   #AlbertaFMUDemoLFLT <- spTransform(AlbertaFMU, sp::CRS(lfltEPSG))
  #   #AlbertaFMUFullLFLT <- spTransform(AlbertaFMUFull, sp::CRS(lfltEPSG))
  #   ecodistrictsDemo <- ecodistricts
  #   #AlbertaFMUDemo <- AlbertaFMU
  #   #AlbertaFMUFull <- AlbertaFMUFull
  #   list(ecodistricts=ecodistricts,
  #        ecodistrictsDemo=ecodistrictsDemo,
  #        ecodistrictsFull=ecodistrictsFull,
  #        ecodistrictsDemoLFLT=ecodistrictsDemoLFLT,
  #        ecodistrictsFullLFLT=ecodistrictsFullLFLT
  #   )
  #   
  # }
  
}
