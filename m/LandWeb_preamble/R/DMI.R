fmaDMI <- function(ml, runName, dataDir, canProvs) {
  dataDirDMI <- file.path(dataDir, "DMI")
  if (!dir.exists(dataDirDMI)) dir.create(dataDirDMI)

  ## There are 3 parts to the DMI FMA: an East and two West areas (North and South)

  dmi <- ml$`FMA Boundaries Updated`[grepl("Daishowa-Marubeni International Ltd",
                                           ml$`FMA Boundaries Updated`$Name), ]
  #plot(dmi[, "Name"], main = "DMI full", col = "lightblue")

  dmi.full <- maptools::unionSpatialPolygons(dmi, rep(1, 2))
  shapefile(dmi.full, filename = file.path(dataDirDMI, "DMI_Full.shp"), overwrite = TRUE)

  dmi.e <- ml$`FMA Boundaries Updated`[grepl("Daishowa-Marubeni International Ltd.*East",
                                             ml$`FMA Boundaries Updated`$Name), ]
  #plot(dmi.e, col = "purple", add = TRUE)
  shapefile(dmi.e, filename = file.path(dataDirDMI, "DMI_East.shp"), overwrite = TRUE)

  dmi.w <- ml$`FMA Boundaries Updated`[grepl("Daishowa-Marubeni International Ltd.*West",
                                             ml$`FMA Boundaries Updated`$Name), ]
  shapefile(dmi.w, filename = file.path(dataDirDMI, "DMI_West.shp"), overwrite = TRUE)

  dmi.nw <- disaggregate(dmi.w)[2, ]
  #plot(dmi.nw, col = "lightgreen", add = TRUE)
  shapefile(dmi.nw, filename = file.path(dataDirDMI, "DMI_West_North.shp"), overwrite = TRUE)

  dmi.sw <- disaggregate(dmi.w)[1, ]
  #plot(dmi.sw, col = "orange", add = TRUE)
  shapefile(dmi.sw, filename = file.path(dataDirDMI, "DMI_West_South.shp"), overwrite = TRUE)

  ## reporting polygons ----------------------------------------------------- ##
  dmi.ansr <- postProcess(ml$`Alberta Natural Subregions`, studyArea = dmi, useSAcrs = TRUE,
                          filename2 = file.path(dataDirDMI, "DMI_ANSR.shp"))
  #plot(dmi.ansr)

  dmi.caribou <- postProcess(ml$`Boreal Caribou Ranges`, studyArea = dmi, useSAcrs = TRUE,
                             filename2 = file.path(dataDirDMI, "DMI_caribou.shp"))
  #plot(dmi.caribou)

  ml <- mapAdd(dmi, ml, layerName = "DMI AB Full", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "DMI Full", isStudyArea = TRUE,
               columnNameForLabels = "Name", filename2 = NULL)

  ml <- mapAdd(dmi.ansr, ml, layerName = "DMI AB ANSR", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "DMI ANSR",
               columnNameForLabels = "Name", filename2 = NULL)

  ml <- mapAdd(dmi.caribou, ml, layerName = "DMI AB Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "DMI Caribou",
               columnNameForLabels = "Name", filename2 = NULL)

  ml <- mapAdd(dmi.nw, ml, layerName = "DMI AB NW", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "DMI AB NW",
               columnNameForLabels = "Name", filename2 = NULL)

  ml <- mapAdd(dmi.sw, ml, layerName = "DMI AB SW", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "DMI AB SW",
               columnNameForLabels = "Name", filename2 = NULL)

  ml <- mapAdd(dmi.e, ml, layerName = "DMI AB E", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "DMI AB E",
               columnNameForLabels = "Name", filename2 = NULL)

  ## buffered study area (needs to have LTHFC data) ---------------------------#
  dmi.sr <- postProcess(ml$`LandWeb Study Area`,
                        studyArea = amc::outerBuffer(dmi.full, 50000), ## 50 km buffer
                        useSAcrs = TRUE,
                        filename2 = file.path(dataDirDMI, "DMI_SR.shp"))

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  ml <- mapAdd(dmi.sr, ml, layerName = "DMI AB SR Full", isStudyArea = TRUE,
               useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
               columnNameForLabels = "NSN", filename2 = NULL)

  return(ml)
}
