function(input, output, session) {
  ## run additonal server code from server_file.R
  if (file.exists("server_file.R")) source("server_file.R")

  ## module calls
  callModule(timeSeriesofRasters, "timeSinceFire", rasters = globalRasters, polygonsList = polygons, shpStudyRegionFull, colorTableFile, timeSinceFirePalette, maxAge, sim = mySim)
  callModule(largePatches, "largePatches", numberOfSimulationTimes = lenTSF, clumpMod2Args)
  callModule(simInfo, "simInfo", mySimOut[[1]])
  callModule(moduleInfo, "moduleInfo", mySimOut[[1]])
  callModule(inputTables, "inputTables")
  
  ## copyright footer (see ?copyrightFooter)
  callModule(copyrightFooter, "copyright", "Her Majesty the Queen in Right of Canada, as represented by the Minister of Natural Resources Canada.")
}

