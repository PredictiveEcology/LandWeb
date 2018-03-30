## logged in and autheticated users can see the proprietary model outputs
modelType <- reactive({
  if (session$userData$userAuthorized()) {
    "Proprietary"
  } else {
    "Free"
  }
})

rctTsf <- reactive(tsfs[[modelType()]])
rctVtm <- reactive(vtms[[modelType()]])

rctRasterList <- reactive(tsfRasters[[modelType()]])

rctPolygonList <- reactive(reportingPolygons[[modelType()]])

rctSim <- reactive(mySimOuts[[modelType()]][[1]])
