
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "LandWebShiny",
  description = NA, #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9009", LandWebShiny = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "LandWebShiny.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "mySimOuts", objectClass = "list", desc = "A list of simLists, from an experiment call, with files saved and described with outputs(sim)"),
    expectsInput("paths", "character", ""),
    expectsInput("studyAreaName", "character", "The name of the studyArea, in early stages of this module, these were SMALL, MEDIUM, LARGE, EXTRALARGE, FULL"),
    expectsInput("cacheIdName", "Character string", "Used with cacheIdEnv to get the cacheId list. Not passed in as a list directly to avoid false failures in Caching"),
    expectsInput("cacheIdEnv", "Environment", "Used with cacheIdName to get the cacheId list. Not passed in as a list directly to avoid false failures in Caching"),
    expectsInput("shpStudyArea", "SpatialPolygon", "The Study Area. This polygon is used to clip or intersect all others provided")
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "tsfs", objectClass = NA, desc = NA),
    createsOutput("leading", "list", ""),
    createsOutput("leadingCC", "list", ""),
    createsOutput("lrgPatches", "list", ""),
    createsOutput("lrgPatchesCC", "list", ""),
    createsOutput("reportingPolygons", "list", ""),
    createsOutput("tsfRasterTilePaths", "list", ""),
    createsOutput("tsfRasters", "list", ""),
    createsOutput("tsfs", "list", ""),
    createsOutput("tsfsCC", "list", ""),
    createsOutput("vtms", "list", ""),
    createsOutput("getAllIfExists", "function", "")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.LandWebShiny = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)

      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "LandWebShiny", "save")
    },
    save = {
      sim <- Save(sim)
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {

  cacheId <- get(sim$cacheIdName, envir = sim$cacheIdEnv)
  ##### POST Experiment
  rastersFromOutputs <- lapply(sim$outputs, function(output) {
    lapply(output, function(x) {
      grep(pattern = ".grd$|.tif$", x$file, value = TRUE)
    }) %>% unlist()
  })

  extractFilepaths <- function(filename, rastersFromOutput) {
    grep(pattern = filename, rastersFromOutput, value = TRUE)
  }

  # convert paths to local machine -- this will do nothing if prior runs were on this machine
  pathConversions <- list(pattern = c("outputsFULL", "/home/emcintir/Documents/"),
                          replacement = c("outputs/FULL_Proprietary", getwd()))
  sim$tsfs <- lapply(rastersFromOutputs, function(rastersFromOutput) {
    fps <- grep(pattern = "rstTimeSinceFire", rastersFromOutput, value = TRUE)
    fps <- convertPaths(fps, pattern = pathConversions$pattern,
                        replacement = pathConversions$replacement)
    asPath(fps, 2) # the tsfs, if used in a Cache situation, should be Cached to 2 parent directories
  })

  sim$vtms <- lapply(rastersFromOutputs, function(rastersFromOutput) {
    fps <- grep(pattern = "vegTypeMap", rastersFromOutput, value = TRUE)
    fps <- convertPaths(fps, pattern = pathConversions$pattern,
                        replacement = pathConversions$replacement)
    asPath(fps, 2)
  })

  tsfLFLTFilenames <- lapply(sim$tsfs, function(tsf) SpaDES.tools::.suffix(tsf, "LFLT") )

  rasterResolutions <- lapply(sim$tsfs, function(x) raster(x[1]) %>% res(.))
  rasterResolutions <- lapply(sim$tsfs, function(x) {
    raster(x[1]) %>% res(.)
  }
  )

  flammableFiles <- lapply(sim$outputPaths, function(op) {
    fps <- file.path(op[[1]], "rstFlammable.grd")
    fps <- convertPaths(fps, pattern = pathConversions$pattern,
                        replacement = pathConversions$replacement)
    asPath(fps, 2)
  })

  sim$tsfRasters <- Cache(Map, tsf = sim$tsfs,
                      userTags = c("reprojectRasts", "tsf", "tsfs"),
                      cacheId = cacheId$tsfRasters,
                      lfltFN = tsfLFLTFilenames, flammableFile = flammableFiles,
                      reprojectRasts, MoreArgs = list(crs = sp::CRS(SpaDES.shiny::proj4stringLFLT)))

  sim$tsfRasters <- convertRasterFileBackendPath(sim$tsfRasters,
                                             pattern = pathConversions$pattern,
                                             replacement = pathConversions$replacement)


  sim$tsfRasterTilePaths <- Cache(Map, rst = sim$tsfRasters, modelType = names(sim$tsfRasters),
                              userTags = c("gdal2TilesWrapper", "tsf", "tsfs"),
                              cacheId = cacheId$tsfRasterTilePaths,
                              MoreArgs = list(zoomRange = 1:10, colorTableFile = asPath(colorTableFile)),
                              function(rst, modelType, zoomRange, colorTableFile) {
                                outputPath <- file.path("www", modelType, sim$studyAreaName, "map-tiles")
                                filenames <- unlist(lapply(rst$crsLFLT, function(ras) filename(ras)))
                                lfltDirNames <- gsub(file.path(outputPath, paste0("out", basename(filenames))), pattern = "\\.tif$", replacement = "")
                                filenames <- #if (!(all(dir.exists(lfltDirNames))))  {
                                  Cache(gdal2Tiles, rst$crsLFLT, outputPath = asPath(outputPath, 3),
                                        userTags = c("gdal2Tiles", "tsf", "tsfs"),
                                        zoomRange = zoomRange, colorTableFile = colorTableFile)
                                #} else {
                                #  lfltDirNames
                                #}
                                return(filenames)
                              })


  vtmsTifs <- Cache(lapply, sim$vtms, #notOlderThan = Sys.time(),
                    cacheId = cacheId$vtmsTifs,
                    userTags = c("writeRaster", "tifs"),
                    function(vtmsInner) {
                      vtmTifs <- lapply(vtmsInner, function(vtm) {
                        vtmRas <- raster(vtm)
                        vtmRas <- writeRaster(vtmRas, file = gsub(".grd", ".tif", filename(vtmRas)), overwrite = TRUE)
                      })
                      return(unlist(lapply(vtmTifs, filename)))
                    })
  vtmLFLTFilenames <- lapply(vtmsTifs, function(vtm) SpaDES.tools::.suffix(vtm, "LFLT") )

  vtmRasters <- Cache(Map, tsf = vtmsTifs, userTags = c("reprojectRasts", "vtms", "vtm"),
                      cacheId = cacheId$vtmRasters,
                      lfltFN = vtmLFLTFilenames, flammableFile = flammableFiles,
                      reprojectRasts, MoreArgs = list(crs = sp::CRS(SpaDES.shiny::proj4stringLFLT)))

  vtmRasters <- convertRasterFileBackendPath(vtmRasters,
                                             pattern = pathConversions$pattern,
                                             replacement = pathConversions$replacement)

  vtmRasterTilePaths <- Cache(Map, rst = vtmRasters, modelType = names(vtmRasters),
                              userTags = c("gdal2Tiles", "vtm", "vtms"),
                              cacheId = cacheId$vtmRasterTilePaths,
                              MoreArgs = list(zoomRange = 1:10, colorTableFile = asPath(colorTableFile)),
                              function(rst, modelType, zoomRange, colorTableFile) {
                                outputPath <- file.path("www", modelType, sim$studyAreaName, "map-tiles")
                                filenames <- gdal2Tiles(rst$crsLFLT, outputPath = outputPath,
                                                        zoomRange = zoomRange, colorTableFile = colorTableFile)
                                return(filenames)
                              })


  ########################################################
  # formerly in mapsForShiny.R
  # Reporting polygons
  if (isTRUE(useParallelCluster)) {
    library(parallel)
    message("  Closing existing cluster for raster::extract")
    try(raster::endCluster(), silent = TRUE)
    message("  Starting ",numClusters, "  node cluster for raster::extract")
    raster::beginCluster(min(numClusters, parallel::detectCores() / 4))

    numClus <- 6
    message("  Also starting a cluster with ", numClus," threads")
    if (!exists("cl6")) {
      cl6 <- parallel::makeForkCluster(numClus)
    }
  } else {
    cl6 <- NULL
  }


  ### CURRENT CONDITION ##################################
  message("Loading Current Condition Rasters")

  dPath <- dataPath(sim)
  CCspeciesNames <- list(Free = c(),
                         Proprietary = c("Pine", "Age", "BlackSpruce", "Deciduous", "Fir", "LandType", "WhiteSpruce"))
  CCspeciesNames <- CCspeciesNames[names(authenticationType)] # make sure it has the names in authenticationType
  CurrentConditions <- Cache(Map, createCCfromVtmTsf, CCspeciesNames = CCspeciesNames,
                             userTags = c("createCCfromVtmTsf", "CurrentConditions"),
                             cacheId = cacheId$currentCondition,
                             MoreArgs = list(vtmRasters = vtmRasters,
                                             dPath = dPath,
                                             loadCCSpeciesFn = loadCCSpecies,
                                             shpStudyArea = sim$shpStudyArea,
                                             tsfRasters = sim$tsfRasters,
                                             vegLeadingPercent = sim$vegLeadingPercent))
  CurrentConditions <- convertRasterFileBackendPath(CurrentConditions, pattern = pathConversions$pattern,
                                                    replacement = pathConversions$replacement)
  sim$tsfsCC <- lapply(CurrentConditions, function(x) {if (!is.null(x)) {
    fps <- filename(x$CCtsf)
    fps <- convertPaths(fps, pattern = pathConversions$pattern,
                        replacement = pathConversions$replacement)
    asPath(fps, 2)
  }
  })
  vtmsCC <- lapply(CurrentConditions, function(x) {if (!is.null(x)) {
    fps <- filename(x$CCvtm)
    fps <- convertPaths(fps, pattern = pathConversions$pattern,
                        replacement = pathConversions$replacement)
    asPath(fps, 2)
  }
  })


  namedUrlsLabelColumnNames <- list(
    "Mountain Northern Caribou Ranges" = list(
      url = "https://drive.google.com/file/d/1Oz2vSor3oIKf2uGv3KRtLoLRWEfX5Mas/view?usp=sharing",
      labelColumnName = "Name"
    ),
    "Provincial Parks" =  list(
      url = "https://drive.google.com/file/d/1GHgTI4JY-YhAXvWkgV20vugbvLNqEEGH/view?usp=sharing",
      labelColumnName = "Name"
    ),
    "NWT Ecoregions" = list(
      url = "https://drive.google.com/file/d/1iRAQfARkmS6-XVHFnTkB-iltzMNPAczC/view?usp=sharing",
      labelColumnName = "Name"
    ),
    "National Parks" = list(
      url = "https://drive.google.com/file/d/1B3VUU8PDn4NPveAyF76OBPY0vZkxScEt/view?usp=sharing",
      labelColumnName = "Name"
    ),
    "AB Natural Sub Regions" = list(
      url = "https://drive.google.com/file/d/1mCEynahKnFkStJUJC8ho5ndRD41olz9F/view?usp=sharing",
      labelColumnName = "Name"
    ),
    # "LP MASTERFILE June62012" =
    #   list(url = "https://drive.google.com/file/d/1J38DKQQavjBV9F3z2gGzHNuNE0s2rmhh/view?usp=sharing",
    #        labelColumnName = "Name"),
    "BC Bio Geoclimatic Zones" =  list(
      url = "https://drive.google.com/file/d/1VAwsax63l2akOM2j_O4Je9p0ZiYg8Hl-/view?usp=sharing",
      labelColumnName = "ZONE_NAME"
    ),
    "FMU Alberta 2015-11" = list(
      url = "https://drive.google.com/file/d/1JiCLcHh5fsBAy8yAx8NgtK7fxaZ4Tetl/view?usp=sharing",
      labelColumnName = "FMU_NAME"
    ),
    "FMA Boundary Updated" = list(
      url = "https://drive.google.com/file/d/1nTFOcrdMf1hIsxd_yNCSTr8RrYNHHwuc/view?usp=sharing",
      labelColumnName = "Name"
    ),
    "Boreal Caribou Ranges" = list(
      url = "https://drive.google.com/file/d/1PYLou8J1wcrme7Z2tx1wtA4GvaWnU1Jy/view?usp=sharing",
      labelColumnName = "Name"
    )
  )

  freeReportingPolygonNames <- c("Alberta Ecozones", "National Ecozones", "National Ecodistricts",
                                 "Provincial Parks", "NWT Ecoregions", "National Parks",
                                 "BC Bio Geoclimatic Zones", "AB Natural Sub Regions")
  proprietaryReportingPolygonNames <- c("FMA Boundary Updated", "FMU Alberta 2015-11", "Boreal Caribou Ranges",
                                        "Mountain Northern Caribou Ranges")

  # Do two steps together, making Cache a bit faster
  reportingAndLeading <- Cache(reportingAndLeading,
                               # Used in reportingAndLeading
                               createReportingPolygonsAllFn = createReportingPolygonsAll, # pass function in so Caching captures function
                               calculateLeadingVegTypeFn = calculateLeadingVegType,

                               # Passed into createReportingPolygonsAllFn
                               createReportingPolygonsFn = createReportingPolygons,
                               userTags = c("leading", "reportingPolygons"),
                               freeReportingPolygonNames = freeReportingPolygonNames,
                               proprietaryReportingPolygonNames = proprietaryReportingPolygonNames,
                               authenticationType = authenticationType,

                               # Passed into createReportingPolygonsFn
                               prepInputsFromSilvacomFn = prepInputsFromSilvacom,
                               shpLandWebSA = sim$shpLandWebSA,
                               shpStudyArea = sim$shpStudyArea,
                               namedUrlsLabelColumnNames = namedUrlsLabelColumnNames,
                               labelColumn = sim$labelColumn,

                               # Passed into calculateLeadingVegTypeFn
                               leadingByStageFn = leadingByStage,
                               tsfs = sim$tsfs, vtms = sim$vtms, cl = cl6,
                               ageClass = ageClasses, ageClassCutOffs = ageClassCutOffs,
                               lapplyFn = lapplyFn,
                               destinationPath = dataPath(sim),

                               # Used by Cache
                               cacheId = cacheId$ReportingAndLeadingFn)
  list2env(reportingAndLeading, envir = envir(sim)) # puts leading and reportingPolygons into .GlobalEnv
  
  
  
  sim$leadingCC <- Cache(calculateLeadingVegType, sim$reportingPolygons, #calculateLeadingVegType = calculateLeadingVegType,
                     tsfs = sim$tsfsCC, vtms = vtmsCC, cl = cl6,
                     ageClasses = ageClasses,
                     ageClassCutOffs = ageClassCutOffs,
                     leadingByStageFn = leadingByStage)

  vegCoverLabels <- raster::levels(CurrentConditions$Proprietary$CCvtm)[[1]]
  sim$leadingCC$Proprietary <- lapply(sim$leadingCC$Proprietary, function(poly) {
    if (!is.null(poly)) {
      poly$vegCover <- as.character(vegCoverLabels$Factor[as.numeric(poly$vegCover)])
    }
    poly
  })

  sim$leadingCC$Free <- NULL


  #########################

  message(paste("Running largePatchesFn"))
  rp4LrgPatches <- lapply(sim$reportingPolygons, function(rpAll) {
    lapply(rpAll, function(rp) {
      rp$crsSR
    })
  })


  # some of the args oare for largePatchesFn, some passed through to largePatchesInnerFn,

  lrgPatchesArgs <- list(largePatchesFn,
                         reportingPolygons = rp4LrgPatches,
                         authenticationType = authenticationType,
                         largePatchesInnerFn = largePatchesInnerFn,

                         # Passed through to largePatchesInnerFn
                         largePatchesInner2Fn = largePatchesInner2Fn,
                         cellNumbersForPolygonFn = cellNumbersForPolygon,

                         # Passed through to largePatchesInner2Fn
                         ageClassCutOffs = ageClassCutOffs,
                         ageClasses = ageClasses,
                         cl = cl6, lapplyFn = lapplyFn, # this is passed to lapply on timeSinceFireFiles
                         countNumPatchesFn = countNumPatches,

                         omitArgs = c("cl", "lapplyFn")
  )


  # Only pass unique arguments here -- all tsfs & vtms and cacheId
  sim$lrgPatches <- do.call(Cache, append(list(
    timeSinceFireFiles = getAllIfExists(sim$tsfs, ifNot = "Proprietary"),
    vegTypeMapFiles = getAllIfExists(sim$vtms, ifNot = "Proprietary"),
    cacheId = cacheId$lrgPatches
  ),
  lrgPatchesArgs))

  # Only pass unique arguments here -- all sim$tsfsCC & vtmsCC and cacheId
  sim$lrgPatchesCC <- do.call(Cache, append(list(
    timeSinceFireFiles = getAllIfExists(sim$tsfsCC, ifNot = "Proprietary"),
    vegTypeMapFiles = getAllIfExists(vtmsCC, ifNot = "Proprietary"),
    cacheId = cacheId$lrgPatchesCC
  ),
  lrgPatchesArgs))

  message(paste("Finished largePatchesFn"))

  ##### TODO: remove this??
  if (FALSE) {
    polygonsWithData <- lapply(leading, function(polyWData) {
      lapply(polyWData, function(dt) {
        dt[, unique(polygonNum[!is.na(proportion)]), by = ageClass]
      })
    })

    #  vegLeadingTypes NEEDED?

    vegLeadingTypesWithAllSpecies <- lapply(leading, function(polyWData) {
      lapply(polyWData, function(dt) {
        c(unique(dt$vegType), "All species")
      })
    })
  }
  ################################################################################
  # Write all Proprietary input shapefiles to disk


  sim$getAllIfExists <- getAllIfExists
  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  polySubDir <- file.path(getAllIfExists(oPaths, ifNot = "Proprietary"), "Polygons")
  dir.create(polySubDir, showWarnings = FALSE)
  out <- Cache(Map, polys = lapply(getAllIfExists(sim$reportingPolygons, ifNot = "Proprietary"),
                                   function(p) p$crsSR$shpStudyRegion),
               namesPolys = names(getAllIfExists(sim$reportingPolygons, "Proprietary")),
               cacheId = cacheId$writeShapefiles,
               function(polys, namesPolys) {
                 tryCatch(raster::shapefile(polys,
                                            filename = file.path(polySubDir, namesPolys),
                                            overwrite = TRUE), error = function(x) NULL)
               })
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create an named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$defaultColor <- 'red'
  # }
  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
