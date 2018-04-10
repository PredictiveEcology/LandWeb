## these reactives are listed alphabetically
## logged in and autheticated users can see the proprietary model outputs

rctAuthenticationType <- reactive({
  if (session$userData$userAuthorized()) {
    "Proprietary"
  } else {
    "Free"
  }
})

rctLeadingDTlist <- reactive(leading[[rctAuthenticationType()]])

rctLeadingDTlistCC <- reactive(leadingCC[[rctAuthenticationType()]])

rctLrgPatches <- reactive(lrgPatches[[rctAuthenticationType()]])

rctLrgPatchesCC <- reactive(lrgPatchesCC[[rctAuthenticationType()]])

rctPaths4sim <- reactive(getAllIfExists(paths4sim, ifNot = rctAuthenticationType()))

rctPolygonList <- reactive(reportingPolygons[[rctAuthenticationType()]])

rctRasterList <- reactive(getAllIfExists(tsfRasters, ifNot = rctAuthenticationType()))

rctSim <- reactive(getAllIfExists(mySimOuts, ifNot = rctAuthenticationType()))

rctTsf <- reactive(getAllIfExists(tsfs, ifNot = rctAuthenticationType()))

rctUrlTemplate <- reactive(getAllIfExists(tsfRasterTilePaths, ifNot = rctAuthenticationType()))

rctVtm <- reactive(getAllIfExists(vtms, ifNot = rctAuthenticationType()))
