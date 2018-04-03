## logged in and autheticated users can see the proprietary model outputs
rctAuthenticationType <- reactive({
  if (session$userData$userAuthorized()) {
    "Proprietary"
  } else {
    "Free"
  }
})

rctTsf <- reactive(tsfs[[rctAuthenticationType()]])

rctVtm <- reactive(vtms[[rctAuthenticationType()]])

rctPaths4sim <- reactive(paths4sim[[rctAuthenticationType()]])

rctRasterList <- reactive(tsfRasters[[rctAuthenticationType()]])

rctPolygonList <- reactive(reportingPolygons[[rctAuthenticationType()]])

rctSim <- reactive(mySimOuts[[rctAuthenticationType()]][[1]])

rctUrlTemplate <- reactive(tsfRasterTilePaths[[rctAuthenticationType()]])
