################################################################################
## Set paths for each part of the simulation
################################################################################

## preamble
paths1 <- list(
  ## use same cachePath for all data-prep steps before dynamic simulation
  cachePath = file.path(cacheDir, "dataPrepGIS", "preamble"),
  modulePath = "m", # short name because shinyapps.io can't handle longer than 100 characters
  inputPath = "inputs",
  outputPath = file.path("outputs", runName)
)

## species layers
paths2 <- paths1
paths2[["cachePath"]] <- file.path(cacheDir, "dataPrepGIS", "speciesLayers")

## boreal data prep
paths2a <- paths2
paths2a[["cachePath"]] <- file.path(cacheDir, "dataPrepGIS", "borealDataPrep")

## main simulation
paths3 <- paths1
paths3[["cachePath"]] <- file.path(cacheDir, runName)

## tile path (same for all)
scratchDir <- checkPath(scratchDir, create = TRUE) ## from config.yml
tilePath <- file.path(paths1[["outputPath"]], "tiles")
