rm(list=ls())
library(SpaDES)
library(raster)
library(data.table)
library(dplyr)
objects <- list()
parameters <- list()
times <- list(start = 0, end = 8)
modules <- list("landWebDataPrep")
path <- list(modulePath = file.path("~/GitHub/LandWeb"),
             outputPath="~/output")

mySim <- simInit(times=times, params=parameters, modules=modules,
                 objects=objects, paths=path)
# 1. how does checksum work, why it downloads files everytime, 


mySim <- spades(mySim, debug=F)

