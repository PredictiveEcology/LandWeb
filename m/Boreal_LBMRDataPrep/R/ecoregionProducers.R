ecoregionProducer <- function(studyAreaRaster, ecoregionMapFull, ecoregionName,
                              ecoregionActiveStatus, studyArea, rstStudyArea, maskFn) {
  # change the coordinate reference for all spatialpolygons
  message("ecoregionProducer 1: ", Sys.time())
  ecoregionMapInStudy <- raster::intersect(ecoregionMapFull, aggregate(studyArea))
  # ecoregions <- ecoregionMapInStudy@data[,ecoregionName]
  # ecoregionTable <- data.table(mapcode = numeric(),
  #                              ecoregion = character())
  # mapcode <- 1
  # for(ecoregion in unique(ecoregions)){
  # #  for(ecoregion in ecoregions){
  #     singleecoMapPoly <- ecoregionMapInStudy[ecoregionMapInStudy@data[,ecoregionName]==ecoregion,]
  #   studyAreaRaster <- setValues(studyAreaRaster, mapcode)
  #   singleecoMapRaster <- crop(studyAreaRaster, singleecoMapPoly)
  #   singleecoMapRaster <- suppressWarnings(maskFn(singleecoMapRaster, singleecoMapPoly))
  #   if(length(unique(getValues(singleecoMapRaster)))==1){
  #     if(is.na(unique(getValues(singleecoMapRaster)))){
  #       ecoregionTable <- rbind(ecoregionTable,
  #                               data.table(mapcode = NA,
  #                                          ecoregion = ecoregion))
  #     } else {
  #       ecoregionTable <- rbind(ecoregionTable,
  #                               data.table(mapcode = mapcode,
  #                                          ecoregion = ecoregion))
  #     }
  #   } else {
  #     ecoregionTable <- rbind(ecoregionTable,
  #                             data.table(mapcode = mapcode,
  #                                        ecoregion = ecoregion))
  #   }
  #
  #   if(mapcode == 1){
  #     ecoregionMap <- singleecoMapRaster
  #   } else {
  #     ecoregionMap <- merge(ecoregionMap, singleecoMapRaster)
  #   }
  #   mapcode <- mapcode + 1
  # }

  # Alternative
  message("ecoregionProducer fastRasterize: ", Sys.time())
  ecoregionMap <- fasterize::fasterize(sf::st_as_sf(ecoregionMapInStudy), studyAreaRaster, field = "ECODISTRIC")

  #ecoregionMap1 <- rasterize(ecoregionMapInStudy, studyAreaRaster, field = "ECODISTRIC")
  ecoregionFactorValues <- unique(ecoregionMap[])

  ecoregionTable <- data.table(mapcode = seq_along(ecoregionFactorValues[!is.na(ecoregionFactorValues)]),
                               ecoregion = as.numeric(ecoregionFactorValues[!is.na(ecoregionFactorValues)]))
  message("ecoregionProducer mapvalues: ", Sys.time())
  ecoregionMap[] <- plyr::mapvalues(ecoregionMap[], from = ecoregionTable$ecoregion, to = ecoregionTable$mapcode)
  ecoregionActiveStatus[, ecoregion := as.character(ecoregion)]
  ecoregionTable <- ecoregionTable[!is.na(mapcode),][, ecoregion := as.character(ecoregion)]
  message("ecoregionProducer dplyr_leftjoin: ", Sys.time())
  ecoregionTable <- dplyr::left_join(ecoregionTable,
                                     ecoregionActiveStatus,
                                     by = "ecoregion") %>%
    data.table
  ecoregionTable[is.na(active), active := "no"]
  ecoregionTable <- ecoregionTable[,.(active, mapcode, ecoregion)]
  return(list(ecoregionMap = ecoregionMap,
              ecoregion = ecoregionTable))
}

nonActiveEcoregionProducer <- function(nonactiveRaster, activeStatus, ecoregionMap,
                                       ecoregion, initialCommunityMap, initialCommunity) {
  browser()
  nonactiveRasterSmall <- crop(nonactiveRaster, ecoregionMap)
  nonecomapcode <- activeStatus[active == "no", ]$mapcode
  whNANonActiveRasterSmall <- which(nonactiveRasterSmall[] %in% nonecomapcode)
  initialCommunityMap[whNANonActiveRasterSmall] <- NA
  ecoregionMap[whNANonActiveRasterSmall] <- NA

  initialCommunity <- initialCommunity[mapcode %in% sort(unique(getValues(initialCommunityMap))), ]
  ecoregion <- ecoregion[mapcode %in% sort(unique(getValues(ecoregionMap))), ]
  return(list(ecoregionMap = ecoregionMap,
              ecoregion = ecoregion,
              initialCommunityMap = initialCommunityMap,
              initialCommunity = initialCommunity))
}
