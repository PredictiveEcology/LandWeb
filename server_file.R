
modelType <- reactive({
  if (session$userData$userAuthorized()) {
    "Proprietary"
  } else {
    "Free"
  }
})


rctTsf <- reactive(tsf[[modelType]])

rctRasterList <- reactive(tsfRasters[[modelType]])

rctPolygonList <- reactive(reportingPolygons[[modelType]])

rctSim <- reactive(mySimOuts[[modelType]][[1]])