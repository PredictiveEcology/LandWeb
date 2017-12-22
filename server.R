function(input, output, session) {
  callModule(simInfo, "simInfoTabs", mySimOut[[1]])
  callModule(moduleInfo, "modInfoBoxes", mySimOut[[1]])
  callModule(timeSinceFire, "timeSinceFire", rasters = globalRasters, polygonsList = polygons,
             shpStudyRegionFull, colorTableFile, timeSinceFirePalette, maxAge)
  callModule(largePatches, "largePatches", numberOfSimulationTimes = lenTSF, clumpMod2Args)
  callModule(inputTables, "inputTables")
}
