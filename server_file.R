## these reactives are listed alphabetically
## logged in and autheticated users can see the proprietary model outputs

rctAuthenticationType <- reactive({
  if (session$userData$userAuthorized()) {
    "Proprietary"
  } else {
    "Free"
  }
})

rctLeadingDTlist <- reactive(sim2$leading[[rctAuthenticationType()]])
rctLeadingDTlistCC <- reactive(sim2$leadingCC[[rctAuthenticationType()]])

rctLrgPatches <- reactive(sim2$lrgPatches[[rctAuthenticationType()]])
rctLrgPatchesCC <- reactive(sim2$lrgPatchesCC[[rctAuthenticationType()]])

rctPaths4sim <- reactive(sim2$getAllIfExists(paths4sim, ifNot = rctAuthenticationType()))

rctPolygonList <- reactive(sim2$reportingPolygons[[rctAuthenticationType()]])

rctRasterList <- reactive(sim2$getAllIfExists(sim2$tsfRasters, ifNot = rctAuthenticationType()))

rctSim <- reactive(sim2$getAllIfExists(mySimOuts, ifNot = rctAuthenticationType())[[1]])

rctTsf <- reactive(sim2$getAllIfExists(sim2$tsfs, ifNot = rctAuthenticationType()))

rctUrlTemplate <- reactive(sim2$getAllIfExists(sim2$tsfRasterTilePaths, ifNot = rctAuthenticationType()))

rctVtm <- reactive(sim2$getAllIfExists(sim2$vtms, ifNot = rctAuthenticationType()))

rctStudyArea <- reactive(rctPolygonList()[["shpStudyArea"]]$crsSR) ## SMALL, LARGE, FULL, etc.
