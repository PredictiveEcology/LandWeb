fmaANC <- function(ml, runName, dataDir, canProvs) {
  dataDirANC <- file.path(dataDir, "ANC")
  if (!dir.exists(dataDirANC)) dir.create(dataDirANC)

  alberta <- canProvs[canProvs$NAME_1 %in% c("Alberta"), ]
  anc <- ml$`FMA Boundaries Updated`[grepl("ANC", ml$`FMA Boundaries Updated`$Name), ]
  #plot(spTransform(alberta, crs(anc)))
  #plot(anc, col = "lightblue", add = TRUE)

  shapefile(anc, filename = file.path(dataDirANC, "ANC.shp"), overwrite = TRUE)

  ## reportingPolygons
  anc.ansr <- postProcess(ml$`Alberta Natural Subregions`, studyArea = anc, useSAcrs = TRUE,
                          filename2 = file.path(dataDirANC, "ANC_ANSR.shp"),
                          overwrite = TRUE)
  #plot(anc.ansr, add = TRUE)

  anc.caribou <- postProcess(ml$`Boreal Caribou Ranges`, studyArea = anc, useSAcrs = TRUE,
                               filename2 = file.path(dataDirANC, "ANC_caribou.shp"),
                               overwrite = TRUE)
  #plot(anc.caribou, col = "magenta", add = TRUE)

  ml <- mapAdd(anc, ml, layerName = "ANC AB", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "ANC", isStudyArea = TRUE,
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(anc.ansr, ml, layerName = "ANC ANSR", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "ANC ANSR",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(anc.caribou, ml, layerName = "ANC Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "ANC Caribou",
               columnNameForLabels = "Name", filename2 = NULL)

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  anc_sr <- postProcess(ml$`LandWeb Study Area`,
                          studyArea = amc::outerBuffer(anc, 50000), # 50 km buffer
                          useSAcrs = TRUE,
                          filename2 = file.path(dataDirANC, "ANC_SR.shp"),
                          overwrite = TRUE)
  #plot(anc_sr)

  ml <- mapAdd(anc_sr, ml, isStudyArea = TRUE, layerName = "ANC SR",
               useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
               columnNameForLabels = "NSN", filename2 = NULL)

  return(ml)
}
