fmaLP <- function(ml, runName, dataDir, canProvs) {
  dataDirLP <- file.path(dataDir, "LP")
  if (!dir.exists(dataDirLP)) dir.create(dataDirLP)

  ## There are 3 parts to the LP FMA: 2 in BC and one in MB. *Only need to do MB one right now.*
  manitoba <- canProvs[canProvs$NAME_1 %in% c("Manitoba"), ]
  west <- canProvs[canProvs$NAME_1 %in% c("British Columbia", "Alberta", "Saskatchewan"), ]
  lp <- ml$`FMA Boundaries Updated`[grepl("Fort St\\. John|Dawson Creek|Mountain",
                                          ml$`FMA Boundaries Updated`$Name), ]
  #plot(spTransform(west, crs(lp)))
  #plot(spTransform(manitoba, crs(lp)), add = TRUE)
  #plot(lp[, "Name"], main = "LP full", col = "lightblue", add = TRUE)

  shapefile(lp, filename = file.path(dataDirLP, "LP_full.shp"), overwrite = TRUE)

  if (grepl("LP_MB", runName)) {
    ## reportingPolygons
    lp_mb <- ml$`FMA Boundaries Updated`[grepl("Mountain", ml$`FMA Boundaries Updated`$Name), ]
    #plot(lp_mb, col = "purple", add = TRUE)
    shapefile(lp_mb, filename = file.path(dataDirLP, "LP_MB.shp"), overwrite = TRUE)

    lp_mb.caribou <- postProcess(ml$`Boreal Caribou Ranges`, studyArea = lp_mb, useSAcrs = TRUE,
                                 filename2 = file.path(dataDirLP, "LP_MB_caribou.shp"),
                                 overwrite = TRUE)
    #plot(lp_mb.caribou, col = "magenta", add = TRUE)

    ml <- mapAdd(lp_mb, ml, layerName = "LP MB", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "LP MB", isStudyArea = TRUE,
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(lp_mb.caribou, ml, layerName = "LP MB Caribou", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "LP MB Caribou",
                 columnNameForLabels = "Name", filename2 = NULL)

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    lp_mb_sr <- postProcess(ml$`LandWeb Study Area`,
                            studyArea = amc::outerBuffer(lp_mb, 50000), # 50 km buffer
                            useSAcrs = TRUE,
                            filename2 = file.path(dataDirLP, "LP_MB_SR.shp"),
                            overwrite = TRUE)
    #plot(lp_mb_sr)

    ml <- mapAdd(lp_mb_sr, ml, isStudyArea = TRUE, layerName = "LP MB SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  return(ml)
}
