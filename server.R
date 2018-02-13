function(input, output, session) {
  ## run additonal server code from server_file.R
  if (file.exists("server_file.R")) source("server_file.R")

  ## module calls
  callModule(timeSinceFire, "timeSinceFire", rasters = globalRasters, polygonsList = polygons, shpStudyRegionFull, colorTableFile, timeSinceFirePalette, maxAge, sim = mySim)
callModule(largePatches, "largePatches", numberOfSimulationTimes = lenTSF, clumpMod2Args)
callModule(simInfo, "simInfo", mySimOut[[1]])
callModule(moduleInfo, "moduleInfo", mySimOut[[1]])
callModule(inputTables, "inputTables")
}
