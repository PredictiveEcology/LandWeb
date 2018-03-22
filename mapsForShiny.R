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

getEcoMaps <- function(ecoDistrictPath, cacheRepo, lfltEPSG) {
  ecodistricts <- shapefile(ecoDistrictPath)
  ecodistrictsFull <- shapefile(ecoDistrictPath)
  shpStudyRegionEco <- spTransform(shpStudyRegion, crs(ecodistricts))

  # There is a self intersection problem with ecodistricts file. This fixes it.
  ecodistricts <- raster::buffer(ecodistricts, width = 0, dissolve = FALSE)

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
  list(ecodistricts = ecodistricts,
       ecodistrictsDemo = ecodistrictsDemo,
       ecodistrictsFull = ecodistrictsFull,
       ecodistrictsDemoLFLT = ecodistrictsDemoLFLT,
       ecodistrictsFullLFLT = ecodistrictsFullLFLT
       )
}


labelColumn <- "shinyLabel"
lflt <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Alberta Ecozone
dPath <- asPath(file.path(paths$inputPath, "ecozones", "Alberta"))
albertaEcozoneFiles <- asPath(c("Natural_Regions_Subregions_of_Alberta.dbf", 
                           "Natural_Regions_Subregions_of_Alberta.lyr", "Natural_Regions_Subregions_of_Alberta.prj", 
                           "Natural_Regions_Subregions_of_Alberta.shp.xml", 
                           "Natural_Regions_Subregions_of_Alberta.shx", "natural_regions_subregions_of_alberta.zip", 
                           "nsr2005_final_letter.jpg", "nsr2005_final_letter.pdf"))
albertaEcozoneURL <- "https://www.albertaparks.ca/media/429607/natural_regions_subregions_of_alberta.zip"
albertaEcozoneFilename <- asPath("Natural_Regions_Subregions_of_Alberta.shp")
shpAlbertaEcozone <- Cache(prepInputs, url = albertaEcozoneURL, targetFile = albertaEcozoneFilename,
           fun = "shapefile", destinationPath = dPath, alsoExtract = albertaEcozoneFiles)
shpAlbertaEcozone@data[[labelColumn]] <- shpAlbertaEcozone$NSRNAME

# Nationa Ecozone
dPath <- file.path(paths$inputPath, "ecozones", "National")
ecozoneFilename <-   file.path(dPath, "ecozones.shp")
ecozoneFiles <- c("ecozones.dbf", "ecozones.prj", 
                  "ecozones.sbn", "ecozones.sbx", "ecozones.shp", "ecozones.shx")
shpNationalEcozone <- Cache(prepInputs, url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
      targetFile = asPath(ecozoneFilename),
      alsoExtract = ecozoneFiles,
      fun = "shapefile", destinationPath = dPath)
shpNationalEcozone@data[[labelColumn]] <- shpNationalEcozone$ZONE_NAME

# Nationa Ecodistrict
dPath <- file.path(paths$inputPath, "ecodistricts", "National")
ecodistrictFilename <-   file.path(dPath, "ecodistricts.shp")
ecodistrictFiles <- c("ecodistricts.dbf", "ecodistricts.prj", 
                  "ecodistricts.sbn", "ecodistricts.sbx", "ecodistricts.shp", "ecodistricts.shx")
shpNationalEcodistrict <- Cache(prepInputs, url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip",
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
shpCaribouZones <- Cache(prepInputs, 
                         url = "https://drive.google.com/file/d/1J38DKQQavjBV9F3z2gGzHNuNE0s2rmhh/view?usp=sharing",
                         targetFile = asPath(caribouFilename),
                         alsoExtract = caribouFiles,
                         fun = "shapefile", 
                         destinationPath = dPath)
shpCaribouZones@data[[labelColumn]] <- shpCaribouZones$HERD


## Alberta FMU - only have local copy
dPath <- file.path(paths$inputPath, "FMU_Alberta_2015-11")
albertaFMUFilename <- asPath(file.path(dPath, "FMU_Alberta_2015-11.shp"))
albertaFMUFiles <- c("FMU_Alberta_2015-11.cpg", "FMU_Alberta_2015-11.dbf", 
                     "FMU_Alberta_2015-11.prj", "FMU_Alberta_2015-11.sbn", 
                     "FMU_Alberta_2015-11.sbx", "FMU_Alberta_2015-11.shp", 
                     "FMU_Alberta_2015-11.shp.xml", "FMU_Alberta_2015-11.shx")
shpAlbertaFMU <- Cache(prepInputs, 
                         url = "https://drive.google.com/file/d/1JiCLcHh5fsBAy8yAx8NgtK7fxaZ4Tetl/view?usp=sharing",
                         targetFile = albertaFMUFilename,
                         alsoExtract = albertaFMUFiles,
                         fun = "shapefile", 
                         destinationPath = dPath)
shpAlbertaFMU@data[[labelColumn]] <- shpAlbertaFMU$FMU_NAME

# Polygons
availablePolygons <- grep("^shp.+", ls(), value = TRUE)
names(availablePolygons) <- availablePolygons
lfltPolygons <- Cache(lapply, availablePolygons, function(shp) {
  spTransform(get(shp), CRSobj = CRS(lflt))
})

library(fastshp)
#aa <- read.shp(albertaFMUFilename, format = "list", close = TRUE)
lfltPolygonsDemo <- lapply(lfltPolygons, mask, )



#############################################################################
#############################################################################
########### OLD #############################################################
if (FALSE) {
  AlbertaFMUFull <- Cache(cacheRepo = paths$cachePath,
                          readSpTransform,
                          shapefilePath=file.path(paths$inputPath, "FMU_Alberta_2015-11", "FMU_Alberta_2015-11"),
                          crs = crs(shpStudyRegion))
  
  
  
  out <- Cache(getEcoMaps, ecoDistrictPath=asPath(file.path(paths$modulePath,"Boreal_LBMRDataPrep", "data", "ecodistricts")),
               lfltEPSG=lflt, cacheRepo=paths$cachePath, digestPathContent = TRUE)
  list2env(out, envir=.GlobalEnv)
}



#availablePolygons <- c("ecodistricts")#, "AlbertaFMU")
if (FALSE) { #NOT USED
  availablePolygonAdjective <- c("Ecodistrict")#, "AlbertaFMU")
}
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
  get(paste0(available$polygons[ii], available$scales[ii], available$projections[ii]))
}) %>%
  setNames(available$names)

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

#### Thin polygons
a <- thinPoly(shpNationalEcozone)

thinPoly <- function(polygon, tolerance = 1000) {
  browser()
  # For speed of plotting
  xy <- lapply(1:length(polygon), function(i) {
    lapply(polygon@polygons[[i]]@Polygons, function(j) {
      j@coords
    })
  })
  
  hole <- lapply(1:length(polygon), function(x) {
    lapply(polygon@polygons[[x]]@Polygons, function(x)
      x@hole)
  }) %>% unlist()
  
  ord <- polygon@plotOrder
  
  ordInner <- lapply(1:length(polygon), function(x) {
    polygon@polygons[[x]]@plotOrder
  })
  
  xyOrd.l <- lapply(ord, function(i) { # nolint
    xy[[i]][ordInner[[i]]]
  })
  
  idLength <- lapply(xyOrd.l, function(i) {
    lapply(i, length)
  }) %>%
    unlist() %>%
    `/`(., 2) %>%
    data.table(V1 = .)
  
  xyOrd <- do.call(rbind, lapply(xyOrd.l, function(i) {
    do.call(rbind, i)
  }))
  
  thinned <- data.table(
    thin = fastshp::thin(xyOrd[, 1], xyOrd[, 2],
                         tolerance = 1e-4)
  )
  thinned[, groups := rep(1:NROW(idLength), idLength$V1)]
  idLength <- thinned[, sum(thin), by = groups]
  xyOrd <- xyOrd[thinned$thin, ]
       
  SpatialPolygons 
  
}
      