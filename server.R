function(input, output, session) {
  callModule(timeSinceFire, "timeSinceFire", rasters = globalRasters, polygonsList = polygons,
             shpStudyRegionFull, colorTableFile, timeSinceFirePalette, maxAge)
  callModule(largePatches, "largePatches", numberOfSimulationTimes = lenTSF, clumpMod2Args)
  callModule(simInfo, "simInfo", mySimOut[[1]])
  callModule(moduleInfo, "moduleInfo", mySimOut[[1]])
  callModule(inputTables, "inputTables")
}
