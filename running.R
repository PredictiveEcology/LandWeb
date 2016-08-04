rm(list=ls())
library(SpaDES)
library(raster)
objects <- list()
parameters <- list()
times <- list(start = 0, end = 1)
modules <- list("landWebParent")
path <- list(modulePath = file.path("~/GitHub/landWebParent"),
             outputPath="~/output")

mySim <- simInit(times=times, params=parameters, modules=modules,
                 objects=objects, paths=path)

mySim <- spades(mySim, debug=F)

