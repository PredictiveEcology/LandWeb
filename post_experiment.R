message("  Identify which files were created during simulation")
# outputs() function reports on any files that were created during the simulation
filesFromOutputs <- reactive({
  lapply(seq_along(mySimOut), function(x) {
    outputs(mySimOut[[x]])$file
  })
})

mySimOut_postExpt <- reactive({
  postExpt <- mySimOut()
  for (simNum in seq_along(postExpt)) {
    postExpt[[simNum]]@outputs$file <- lapply(
      strsplit(outputs(postExpt[[simNum]])$file,
               split = paste0(outputPath(postExpt[[simNum]]), "[\\/]+")),
      function(f) {
        f[[2]]
      }) %>%
      unlist() %>%
      file.path(outputPath(postExpt[[simNum]]), .)
  }
  postExpt
})

print(isolate(mySimOut_postExpt()))

message("  Load rasters from disk, reproject them to leaflet projection")
rastersFromOutputs <- reactive({
  lapply(seq_along(mySimOut_postExpt()), function(x) {
    grep(pattern = ".grd$|.tif$", outputs(mySimOut_postExpt()[[x]])$file, value = TRUE)
  }) %>% unlist()
})

# Look for all files named rstTimeSinceFire -- these are several rasters each with a filename
#   that represents the simulation "time" when it was created, e.g., 10, 20, 30 years
tsf <- reactive({
  grep(pattern = "rstTimeSinceFire", rastersFromOutputs(), value = TRUE)
})

# These are several rasters indicating vegetation type, again each one coming from a specific
#   simulation time ... a time series of rasters...
vtm <- reactive({
  grep(pattern = "vegTypeMap", rastersFromOutputs(), value = TRUE)
})

rasterResolution <- reactive({
  raster::raster(tsf()[1]) %>% res()
})

lfltFN <- reactive({
  gsub(tsf(), pattern = ".grd$|.tif$", replacement = "LFLT.tif")
})

globalRasters <- reactive({
  Cache(reprojectRasts, lapply(tsf(), asPath), digestPathContent = .quickCheck,
        lfltFN(), sp::CRS(lflt),  cacheRepo = cachePath(mySim()),
        flammableFile = asPath(file.path(outputPath(mySim()), "rstFlammable.grd")))
})

leading <- reactive({
  message("  Determine leading species by age class, by polygon (loading 2 rasters, summarize by polygon)")
  args <- list(leadingByStage, tsf(), vtm(),
               polygonToSummarizeBy = ecodistricts,
               cl = if (exists("cl")) cl,
               omitArgs = "cl", digestPathContent = .quickCheck,
               ageClasses = ageClasses, cacheRepo = cachePath(mySim()))
  args <- args[!unlist(lapply(args, is.null))]
  out <- do.call(Cache, args)
  rm(args)

  out
})

message("  Determine number of large patches, by polygon (loading 2 rasters, summarize by polygon)")
# Large patches
polygonsWithData <- reactive({
  leading()[, unique(polygonNum[!is.na(proportion)]), by = ageClass]
})

vegLeadingTypes <- reactive({
  c(unique(leading()$vegType))
})

vegLeadingTypesWithAllSpecies <- reactive({
  c(vegLeadingTypes(), "All species")
})

clumpMod2Args <- reactive({
  args <- list(
    currentPolygon = polygons[[1 + length(polygons) / 4]],
    tsf = tsf(),
    vtm = vtm(),
    cl = if (exists("cl")) cl,
    ageClasses = ageClasses,
    cacheRepo = cachePath(mySim()),
    largePatchesFn = largePatchesFn,
    countNumPatches = countNumPatches
  )
  args <- args[!unlist(lapply(args, is.null))]
  args
})

message("  Finished post_experiment.R")
