fmaTolko <- function(ml, runName, dataDir, canProvs) {
  dataDirTolko <- file.path(dataDir, "Tolko")
  if (!dir.exists(dataDirTolko)) dir.create(dataDirTolko)

  ## There are 3 parts to the Tolko FMA in AB and one in SK
  west <- canProvs[canProvs$NAME_1 %in% c("British Columbia", "Alberta", "Saskatchewan"), ]
  tolko <- ml$`FMA Boundaries Updated`[grepl("Tolko|Meadow Lake OSB",
                                             ml$`FMA Boundaries Updated`$Name), ]
  #plot(spTransform(west, crs(tolko)), main = "Tolko full")
  #plot(tolko[, "Name"], col = "lightblue", add = TRUE)

  #colours <- c("purple", "orange", "blue", "limegreen", "magenta")
  #for (i in 1:5) plot(tolko[i,], col = colours[i], add = TRUE)

  tolko.full <- maptools::unionSpatialPolygons(tolko, rep(1, 5))
  shapefile(tolko.full, filename = file.path(dataDirTolko, "Tolko_Full.shp"), overwrite = TRUE)



  if (grepl("tolko_AB_N", runName)) {
    ## reporting polygons

    tolko_ab_n <- tolko[4, ]
    #plot(tolko_ab_n, add = TRUE, col = colours[4])
    shapefile(tolko_ab_n, filename = file.path(dataDirTolko, "Tolko_AB_N.shp"), overwrite = TRUE)

    tolko_ab_n.ansr <- postProcess(ml$`Alberta Natural Subregions`, studyArea = tolko_ab_n, useSAcrs = TRUE,
                                   filename2 = file.path(dataDirTolko, "Tolko_AB_N_ANSR.shp"),
                                   overwrite = TRUE)
    #plot(tolko_ab_n.ansr)

    tolko_ab_n.caribou <- postProcess(ml$`Boreal Caribou Ranges`, studyArea = tolko_ab_n, useSAcrs = TRUE,
                                      filename2 = file.path(dataDirTolko, "Tolko_AB_N_caribou.shp"),
                                      overwrite = TRUE)
    #plot(tolko_ab_n.caribou)

    ml <- mapAdd(tolko_ab_n, ml, layerName = "Tolko AB North", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Tolko AB North", isStudyArea = TRUE,
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(tolko_ab_n.ansr, ml, layerName = "Tolko AB North ANSR", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Tolko AB North ANSR",
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(tolko_ab_n.caribou, ml, layerName = "Tolko AB North Caribou", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Tolko AB North Caribou",
                 columnNameForLabels = "Name", filename2 = NULL)

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    tolko_ab_n_sr <- postProcess(ml$`LandWeb Study Area`,
                                 studyArea = amc::outerBuffer(tolko_ab_n, 50000), # 50 km buffer
                                 useSAcrs = TRUE,
                                 filename2 = file.path(dataDirTolko, "Tolko_AB_N_SR.shp"),
                                 overwrite = TRUE)

    ml <- mapAdd(tolko_ab_n_sr, ml, isStudyArea = TRUE, layerName = "Tolko AB North SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  } else if (grepl("tolko_AB_S", runName)) {
    ## reportingPolygons
    tolko_ab_s <- tolko[c(2, 3, 5), ]
    #plot(tolko_ab_s, col = colours[c(2, 3, 5)])
    shapefile(tolko_ab_s, filename = file.path(dataDirTolko, "Tolko_AB_S.shp"), overwrite = TRUE)

    tolko_ab_s.ansr <- postProcess(ml$`Alberta Natural Subregions`, studyArea = tolko_ab_s, useSAcrs = TRUE,
                                   filename2 = file.path(dataDirTolko, "Tolko_AB_S_ANSR.shp"),
                                   overwrite = TRUE)
    #plot(tolko_ab_s.ansr)

    tolko_ab_s.caribou <- postProcess(ml$`Boreal Caribou Ranges`, studyArea = tolko_ab_s, useSAcrs = TRUE,
                                      filename2 = file.path(dataDirTolko, "Tolko_AB_S_caribou.shp"),
                                      overwrite = TRUE)
    #plot(tolko_ab_s.caribou)

    ml <- mapAdd(tolko_ab_s, ml, layerName = "Tolko AB South", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Tolko AB South", isStudyArea = TRUE,
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(tolko_ab_s.ansr, ml, layerName = "Tolko AB South ANSR", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Tolko AB South ANSR",
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(tolko_ab_s.caribou, ml, layerName = "Tolko AB South Caribou", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Tolko AB South Caribou",
                 columnNameForLabels = "Name", filename2 = NULL)

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    tolko_ab_s_sr <- postProcess(ml$`LandWeb Study Area`,
                                 studyArea = amc::outerBuffer(tolko_ab_s, 50000), # 50 km buffer
                                 useSAcrs = TRUE,
                                 filename2 = file.path(dataDirTolko, "Tolko_AB_S_SR.shp"),
                                 overwrite = TRUE)

    ml <- mapAdd(tolko_ab_s_sr, ml, isStudyArea = TRUE, layerName = "Tolko AB South SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  } else if (grepl("tolko_SK", runName)) {
    ## reportingPolygons
    tolko_sk <- tolko[1, ]
    #plot(tolko_sk, col = colours[1])
    shapefile(tolko_sk, filename = file.path(dataDirTolko, "Tolko_SK.shp"), overwrite = TRUE)

    # NOTE: no ANSR in SK ;)

    tolko_sk.caribou <- postProcess(ml$`Boreal Caribou Ranges`, studyArea = tolko_sk, useSAcrs = TRUE,
                                    filename2 = file.path(dataDirTolko, "Tolko_SK_caribou.shp"),
                                    overwrite = TRUE)
    #plot(tolko_sk.caribou)

    ml <- mapAdd(tolko_sk, ml, layerName = "Tolko SK", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Tolko SK", isStudyArea = TRUE,
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(tolko_sk.caribou, ml, layerName = "Tolko SK Caribou", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Tolko SK Caribou",
                 columnNameForLabels = "Name", filename2 = NULL)

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    tolko_sk_sr <- postProcess(ml$`LandWeb Study Area`,
                               studyArea = amc::outerBuffer(tolko_sk, 50000), # 50 km buffer
                               useSAcrs = TRUE,
                               filename2 = file.path(dataDirTolko, "Tolko_SK_SR.shp"),
                               overwrite = TRUE)

    ml <- mapAdd(tolko_sk_sr, ml, isStudyArea = TRUE, layerName = "Tolko SK SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  return(ml)
}
