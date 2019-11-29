################################################################################
## Set paths for each part of the simulation
################################################################################

## preamble
paths1 <- list(
  ## use same cachePath for all data-prep steps before dynamic simulation
  cachePath = file.path("cache", "dataPrepGIS", "preamble"),
  modulePath = "m", # short name because shinyapps.io can't handle longer than 100 characters
  inputPath = "inputs",
  outputPath = file.path("outputs", runName)
)

## species layers
paths2 <- list(
  ## use same cachePath for all data-prep steps before dynamic simulation
  cachePath = file.path("cache", "dataPrepGIS", "speciesLayers"),
  modulePath = "m", # short name because shinyapps.io can't handle longer than 100 characters
  inputPath = "inputs",
  outputPath = file.path("outputs", runName)
)

## main simulation
paths3 <- list(
  ## NOTE: use separate cachePath for each dynamic simulation
  cachePath = file.path("cache", runName),
  modulePath = "m", # short name because shinyapps.io can't handle longer than 100 characters
  inputPath = "inputs",
  outputPath = file.path("outputs", runName)
)

## tile path (same for all)
tilePath <- file.path(paths1$outputPath, "tiles")
