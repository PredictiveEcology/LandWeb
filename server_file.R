

## paths for sim
rctCPath <- reactive({
  if (session$userData$userAuthorized()) {
    file.path("cache", paste0(subStudyRegionName, "_AUTH"))
  } else {
    file.path("cache", paste0(subStudyRegionName))
  }
})

## list of polygons to use for simulation and app
rctReportingPolygons <- reactive({
  if (session$userData$userAuthorized()) {
    reportingPolygonsFree[names(reportingPolygonsProprietary)] <- reportingPolygonsProprietary
  } else {
    reportingPolygonsFree
  }
})


