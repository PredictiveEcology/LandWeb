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

rctPaths4sim <- reactive(paths4sim[[rctAuthenticationType()]])

rctPolygonList <- reactive(reportingPolygons[[rctAuthenticationType()]])

rctRasterList <- reactive(tsfRasters[[rctAuthenticationType()]])

rctSim <- reactive(mySimOuts[[rctAuthenticationType()]][[1]])

rctTsf <- reactive(tsfs[[rctAuthenticationType()]])

rctUrlTemplate <- reactive(tsfRasterTilePaths[[rctAuthenticationType()]])

rctVtm <- reactive(vtms[[rctAuthenticationType()]])
