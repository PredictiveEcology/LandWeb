rm(list=ls())
workingPath <- "~/GitHub/LandWeb/landWebDataPrep/outputs"
load(file.path(workingPath, paste("simulationInputs_Tile", 17, ".RData", sep = "")))
modules <- list("biomassSuccessionLANDIS")
path <- list(modulePath=file.path("~/GitHub/LandWeb"),
             outputPath="~/output")
times <- list(start = 0, end = 2000)

objects <- list("initialCommunities"=initialCommunities,
                "species"=speciesTable,
                "ecoregionMap"=ecoregionMap,
                "initialCommunitiesMap"=initialCommunitiesMap,
                "spinupMortalityfraction"=spinupMortalityfraction,
                "speciesEcoregion"=speciesEcoregion,
                "ecoregion"=ecoregionTable,
                "minRelativeB"=minRelativeB,
                "sufficientLight"=sufficientLight,
                "successionTimestep"=successionTimestep,
                "cellSize"=cellSize,
                "seedingAlgorithm"=seedingAlgorithm,
                "useCache"=useCache,
                "calibrate" = calibrate)
parameters <- list(.progress=list(type="graphical", interval=1),
                   .globals=list(verbose=FALSE),
                   biomassSuccessionLANDIS=list( .saveInitialTime=NA))

mySim <- simInit(times=times, params=parameters, modules=modules,
                 objects=objects, paths=path)
simutime <- system.time(mySim <- spades(mySim, debug=FALSE))

simuTimeR <- mySim$timeRecorder
simuTimeR$FinT <- shift(simuTimeR[,.(systemTime)], fill = NA,
                        type = "lead")
simuTimeR <- simuTimeR[!is.na(FinT),][, runningTime := FinT-systemTime]
save.image("oneTilesimu.RData")


