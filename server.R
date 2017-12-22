function(input, output, session) {
  currentPolygon <- callModule(leafletMap, "leafletMap")

  callModule(simInfo, "simInfoTabs", mySimOut[[1]])
  callModule(moduleInfo, "modInfoBoxes", mySimOut[[1]])

  output$timeSinceFireUI <- renderUI({
    tabBox(width = 12,
           tabPanel("Time Since Fire maps", tabName = "timeSinceFireTab",
                    fluidRow(timeSinceFireUI("timeSinceFire", length(tsf)))
           )
    )
  })

  output$studyRegionUI <- renderUI({
    tabBox(width = 12,
           tabPanel("Time Since Fire maps", tabName = "timeSinceFireTab",
                    fluidRow(studyRegionModUI("studyRegion"))
           )
    )
  })

  callModule(timeSinceFire, "timeSinceFire", rasters = globalRasters, polygonsList = polygons,
             shpStudyRegionFull, colorTableFile, timeSinceFirePalette, maxAge)

  clumpMod2Args <- list(
    currentPolygon = polygons[[1 + length(polygons)/4]],
    tsf = tsf, vtm = vtm,
    cl = if(exists("cl")) cl,
    ageClasses = ageClasses, cacheRepo = paths$cachePath,
    largePatchesFn = largePatchesFn, countNumPatches = countNumPatches)
  clumpMod2Args <- clumpMod2Args[!unlist(lapply(clumpMod2Args, is.null))]

  callModule(largePatches, "largePatches", numberOfSimulationTimes = lenTSF, clumpMod2Args)


  output$speciesInputs <- renderDataTable({
    landisInputs
  })#, digits = 1)

  output$speciesEcoregionInputs <- renderDataTable({
    spEcoReg
  })#, digits = 1)

  noLongerWaiting()
  message("  Finished global.R")
}
