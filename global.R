if (FALSE) { # For pushing to shinyapps.io
  allFiles <- dir(recursive = TRUE)
  allFiles <- grep(allFiles, pattern = "R-Portable", invert = TRUE, value = TRUE)
  rsconnect::deployApp(appName = "LandWebDemo", appFiles = allFiles, appTitle = "LandWeb Demo",
                       contentCategory = "application")  
}

#.libPaths("TEMP")
if (FALSE) {
  pkgNamespaces <- c("shiny", "shinydashboard", "BH", "RCurl", "RandomFieldsUtils", "R.oo", "R.methodsS3", "SpaDES",
                     "visNetwork", "rgexf", "influenceR", "DBI", "viridis", "htmlwidgets", "bit", "parallel",
                     "devtools", "raster", "rgeos", "RSQLite", "magrittr", "raster", "sp", "dplyr", "ggplot2", "maptools",
                     "broom", "ggvis", "rgdal", "grid", "leaflet", "plotly", "VGAM")
  lapply(pkgNamespaces, function(p) {
    if (!require(p, quietly = TRUE, character.only = TRUE)) {
      install.packages(p, dependencies = TRUE)
    }
  })
  if (!require("RandomFieldsUtils", character.only = TRUE)) install.packages("RandomFieldsUtils")
  if (!require("RandomFields", character.only = TRUE)) install.packages("RandomFields")
  if (tryCatch(packageVersion("SpaDES") < "1.3.1.9041", error = function(x) TRUE))
    devtools::install_github("PredictiveEcology/SpaDES@development")  
}
pkgs <- c("shiny", "shinydashboard", "broom", "rgeos", "raster", "rgdal", "grid", "ggplot2","VGAM",
          "maptools", "dplyr", "data.table", "plotly", "magrittr", "SpaDES", "parallel", "leaflet", "SpaDES",
          "ggvis")
lapply(pkgs, require, quietly = TRUE, character.only = TRUE)

if (FALSE) { # For shinyapps.io -- needs to see explicit require statements
  require(shiny)
  require(shinydashboard)
  require(BH)
  require(RCurl)
  require(RandomFieldsUtils)
  require(R.oo)
  require(R.methodsS3)
  require(SpaDES)
  require(visNetwork)
  require(rgexf)
  require(influenceR)
  require(DBI)
  require(viridis)
  require(htmlwidgets)
  require(bit)
  require(devtools)
  require(raster)
  require(rgeos)
  require(RSQLite)
  require(magrittr)
  require(raster)
  require(sp)
  require(VGAM)
  require(dplyr)
  require(ggplot2)
  require(maptools)
  require(broom)
  require(ggvis)
  require(rgdal)
  require(grid)
  require(data.table)
  require(plotly)
  require(leaflet)
  require(parallel)
}
#library(shiny)
#library(broom)
#library(shinydashboard)

# to produce figure one

#library(rgeos); library(raster); library(SpaDES); library(rgdal);library(ggplot2)
#library(grid);
#library(maptools); library(dplyr); library(data.table)
#library(plotly)

curDir <- getwd()
setwd(curDir)
#inputDir <- "~/Dropbox/Projects/Landweb/Sim/inputs/"

#CanadaMap <- shapefile(file.path(inputDir, "Canada.shp"))
#saveRDS(CanadaMap, file = "CanadaMap.rds")


#library(SpaDES)
#library(magrittr)
#inputDir <- file.path(tempdir(), "inputs") %>% checkPath(create = TRUE)

paths <- list(
  #cachePath = file.path(curDir, "cache"),
  cachePath = "appCache",
  modulePath = "m", # short name because shiny can't handle longer than 100 characters
  inputPath = "inputs",
  outputPath = "outputs"
)

CanadaMap <- readRDS(file.path(paths$inputPath, "CanadaMap.rds"))
landisInputs <- readRDS(file.path(paths$inputPath, "landisInputs.rds"))
spEcoReg <- readRDS(file.path(paths$inputPath, "SpEcoReg.rds"))

seralStageData <- readRDS(file.path(paths$inputPath, "seralStageData.rds"))
vegTypeData <- readRDS(file.path(paths$inputPath, "vegTypeData.rds"))
availableRegions <- unique(vegTypeData$ecoregion)


shpStudyRegionFull <- SpaDES::Cache(shapefile, file.path(paths$inputPath, "shpLandWEB.shp"),
                                    cacheRepo = paths$cachePath)
shpStudyRegionFull$fireReturnInterval <- shpStudyRegionFull$LTHRC
shpStudyRegionFull@data <- shpStudyRegionFull@data[, !(names(shpStudyRegionFull) %in% "ECODISTRIC")]

#biomassMap <- raster(file.path(paths$modulePath, "landWebDataPrep", "data", 
#                               "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.tif"))
crsKNNMaps <- CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
studyArea <- "SMALL"
set.seed(2)#set.seed(5567913)
if (studyArea == "SMALL") {
  # #smallExt <- clickExtent()
  # smallExt <- new("Extent", xmin = -983660, xmax = -899973, 
  #                 ymin = 8007529, ymax = 8085986)
  # #smallExt <- new("Extent", xmin = -1276390, xmax = -1181880, 
  # #                ymin = 7637258, ymax = 7706827)
  # shpStudyRegion2 <- spTransform(shpStudyRegion, crsKNNMaps)
  # shpStudyRegion <- crop(shpStudyRegion2, smallExt)
  # shpStudyRegionCan <- spTransform(shpStudyRegion, crs(CanadaMap))
  areaKm2 <- 10000#700000#2000#600000#too big for laptop
  minY <- 7508877 - 1.6e5
} else if (studyArea == "medium") {
  areaKm2 <- 80000 #700000#2000#600000#too big for laptop
  minY <- 7008877 - 1.6e5
}
shpStudyRegionFull <- spTransform(shpStudyRegionFull, crsKNNMaps)
minX <- -1202250.2
maxX <- minX + sqrt(areaKm2 * 1e6)
maxY <- minY + sqrt(areaKm2 * 1e6)
meanY <- mean(c(minY, maxY))

# Add random noise to polygon
xAdd <- round(runif(1, -5e5, 1.5e6))
yAdd <- round(runif(1, 1e5, 5e5)) - xAdd / 2
nPoints <- 20
betaPar <- 0.6
X <- c(jitter(sort(rbeta(nPoints, betaPar, betaPar) * (maxX - minX) + minX)),
      jitter(sort(rbeta(nPoints, betaPar, betaPar) * (maxX - minX) + minX, decreasing = TRUE)))
Y <- c(jitter(sort(rbeta(nPoints/2, betaPar, betaPar) * (maxY - meanY) + meanY)),
      jitter(sort(rbeta(nPoints, betaPar, betaPar) * (maxY - minY) + minY, decreasing = TRUE)),
      jitter(sort(rbeta(nPoints/2, betaPar, betaPar) * (meanY - minY) + minY)))

Sr1 <- Polygon(cbind(X + xAdd, Y + yAdd))
Srs1 <- Polygons(list(Sr1), "s1")
inputMapPolygon <- SpatialPolygons(list(Srs1), 1L)
crs(inputMapPolygon) <- crsKNNMaps
shpStudyRegion <- raster::intersect(shpStudyRegionFull, inputMapPolygon)

prepare1 <- function(shpStudyRegion, shpStudyRegionFull) {
  shpStudyAreaFort <- tidy(shpStudyRegion, region = 'Name_1') 
  shpStudyAreaFort <- left_join(shpStudyAreaFort, shpStudyRegion@data[, c("Name_1", "fireReturnInterval")],
                                by = c("id" = "Name_1"))
  shpStudyAreaOrigFort <- tidy(shpStudyRegionFull, region = 'Name_1') 
  shpStudyAreaOrigFort <- left_join(shpStudyAreaOrigFort, shpStudyRegionFull@data[, c("Name_1", "fireReturnInterval")],
                                    by = c("id" = "Name_1"))
  #shpStudyAreaOrigFort<-shpStudyAreaOrigFort[order(shpStudyAreaOrigFort$order), ]
  
  # ggplotStudyRegionSmall <- ggplot() +
  #   geom_polygon(data=shpStudyAreaOrigFort,
  #                aes(long, lat, group=id),
  #                colour='black',fill=NA) +
  #   geom_polygon(data=shpStudyAreaFort,
  #              aes(long, lat, group=id),
  #              colour='red',fill=NA) +
  #   coord_equal() +
  #   theme_bw()
  
  
  #map <- ggplot2::fortify(maine, region="name")
  
  wdth <- 650
  ht <- wdth * (ymax(shpStudyRegionFull) - ymin(shpStudyRegionFull)) /
    (xmax(shpStudyRegionFull) - xmin(shpStudyRegionFull))
  df2 <- data.frame(fri = unique(shpStudyAreaOrigFort$fireReturnInterval),
                    colrs = colorRampPalette(c("orange", "dark green"))(diff(range(shpStudyAreaOrigFort$fireReturnInterval)))[
                      unique(shpStudyAreaOrigFort$fireReturnInterval) - min(shpStudyAreaOrigFort$fireReturnInterval) + 1]
  )
  shpStudyAreaOrigFort <- left_join(shpStudyAreaOrigFort, df2, by = c("fireReturnInterval" = "fri"))
  
  a <- shpStudyAreaOrigFort %>%
    ggvis(~long, ~lat) %>%
    group_by(group, id) %>%
    layer_paths(strokeOpacity := 0.5, stroke := "#7f7f7f",
                fill := ~fireReturnInterval) %>%
    add_tooltip(function(data){
      paste0("Fire Return Interval: ", data$fireReturnInterval)
    }, "hover") %>% 
    layer_paths(data = shpStudyAreaFort %>% group_by(group, id),
                stroke := "red") %>%
    #hide_legend("fill") %>%
    hide_axis("x") %>% hide_axis("y") %>%
    #set_options(width=400, height=800)#, keep_aspect=TRUE)
    set_options(keep_aspect = TRUE, width = wdth, height = ht)
  
}
#ggStudyRegion <- Cache(prepare1, shpStudyRegion, 
#                       shpStudyRegionFull, cacheRepo = paths$cachePath)
ggStudyRegion <- prepare1(shpStudyRegion, shpStudyRegionFull)


#shpStudyAreaFort<-shpStudyAreaFort[order(shpStudyAreaFort$order), ]
#shpStudyAreaFort <- data.table(shpStudyAreaFort, key="id")
#tmp <- countryCodes[,CountryCode:=as.character(CountryCode)]
#setkey(countryCodes, "CountryCode")
#shpStudyAreaFort<-shpStudyAreaFort[countryCodes]




if (TRUE) {
  AlbertaFMUFull <- Cache(shapefile, file.path(curDir,"FMU_Alberta_2015-11", "FMU_Alberta_2015-11"),
                          cacheRepo = paths$cachePath)
  AlbertaFMUFull <- Cache(spTransform, AlbertaFMUFull, crs(shpStudyRegion), cacheRepo = paths$cachePath)
  AlbertaFMU <- Cache(crop, AlbertaFMUFull, shpStudyRegion, cacheRepo = paths$cachePath)
}

ecodistricts <- Cache(shapefile, file.path(paths$modulePath,"LW_LBMRDataPrep", "data", "ecodistricts"),
                      cacheRepo = paths$cachePath)
ecodistrictsFull <- Cache(shapefile, file.path(paths$modulePath,"LW_LBMRDataPrep", "data", "ecodistricts"),
                          cacheRepo = paths$cachePath)
shpStudyRegionEco <- spTransform(shpStudyRegion, crs(ecodistricts))
ecodistrictsStudyRegion <- Cache(crop, ecodistricts, shpStudyRegionEco, cacheRepo = paths$cachePath)
ecodistrictsCan <- spTransform(ecodistrictsStudyRegion, crs(CanadaMap))
ecodistricts <- spTransform(ecodistrictsStudyRegion, crs(shpStudyRegion))

#modules <- list("landWebDataPrep", "initBaseMaps", "fireDataPrep", "fireNull", "LW_LBMRDataPrep", "LBMR")

#modules <- list("landWebDataPrep", "initBaseMaps", "fireDataPrep", "fireNull")
#modules <- list("landWebDataPrep", "initBaseMaps", "LandMine")

modules <- list("landWebDataPrep", "initBaseMaps", "fireDataPrep", "LandMine", "LW_LBMRDataPrep", "LBMR", "timeSinceFire", "LandWebOutput")

fireModules <- list("landWebDataPrep", "initBaseMaps", "LandMine", "timeSinceFire", "LandWebOutput")

successionTimestep <- 2
summaryInterval <- 2

times <- list(start = 0, end = 4)
objects <- list("shpStudyRegionFull" = shpStudyRegionFull,
                "shpStudySubRegion" = shpStudyRegion,
                "successionTimestep" = successionTimestep,
                "summaryPeriod" = c(pmax((times$end - times$start) / 2), times$end))
parameters <- list(fireNull = list(burnInitialTime = 1,
                                   returnInterval = 1,
                                   .statsInitialTime = 1),
                   LandWebOutput = list(summaryInterval = summaryInterval),
                   LBMR = list(.plotInitialTime = times$start,
                               .saveInitialTime = NA),
                   initBaseMaps = list(.useCache = FALSE))
outputs <- data.frame(stringsAsFactors = FALSE,
                      expand.grid(
                        objectName = c("rstTimeSinceFire", "seralStageMap", "vegTypeMap", "oldBigPatch"),
                        saveTime = seq(objects$summaryPeriod[1], objects$summaryPeriod[2], 
                                       by = parameters$LandWebOutput$summaryInterval)),
                      fun = "writeRaster", package = "raster")
outputs2 <- data.frame(stringsAsFactors = FALSE,
                       expand.grid(
                         objectName = c("simulationOutput"),
                         saveTime = times$end), fun = "saveRDS", package = "base" )

outputs$arguments <- I(rep(list(list(overwrite = TRUE, progress = FALSE)), NROW(outputs)))
outputs <- as.data.frame(rbindlist(list(outputs, outputs2), fill = TRUE))

if (TRUE) {
  if (exists("mySim")) {
    if (readRDS(file = "mySimDigestSaved.rds") == digest::digest(mySim)) {
      needMySim <- FALSE
    } else {
      needMySim <- TRUE
    }
  } else {
    needMySim <- TRUE
  }
  if (needMySim) {
    mySim <- simInit(times = times, params = parameters, modules = modules,
                     objects = objects, paths = paths, outputs = outputs)
    # 
    # # Do an initial run for each given study area so that all the data prep can be done once only
    #initialRun1 <- spades(Copy(mySim), debug = TRUE)
    # 5 minutes for 6e3 km2
    # 30 minutes for 6e4 km2
    initialRun <- Cache(spades, sim = Copy(mySim), debug = TRUE, objects = "shpStudyRegion", 
                        cacheRepo = file.path(cachePath(mySim), "studyRegion"))
    try(silent = TRUE, {
      filesPresent <- dir(unique(dirname(outputs(initialRun)$file)))
      filesPresentFull <- dir(unique(dirname(outputs(initialRun)$file)), full.names = TRUE)
      filesToRemove <- unlist(lapply(strsplit(basename(outputs(initialRun)$file), split = "\\."), function(x) x[1])) %>%
        lapply(function(y) grep(filesPresent, pattern = y)) %>%
        unlist()
      file.remove(filesPresentFull[filesToRemove])
    })
    
    saveRDS(digest::digest(mySim), file = "mySimDigestSaved.rds")
  }
}
# 
library(parallel)
# try(stopCluster(cl), silent = TRUE)
if (!exists("cl")) {
 cl <- makeCluster(detectCores() - 1)
 clusterExport(cl = cl, varlist = list("objects", "shpStudyRegion"))
}
# try(stopCluster(cl4), silent = TRUE)
# cl4 <- makeCluster(4)
# 
message("Running Experiment")
mySimOut <- Cache(experiment, mySim, replicates = 3, debug = TRUE, #cache = TRUE, 
                                    cl = cl, 
                  .plotInitialTime = NA,
                  clearSimEnv = TRUE#, 
                  #notOlderThan = Sys.time()
                  )

grds <- unlist(lapply(seq_along(mySimOut), function(x) {
  grep(pattern = ".grd$", outputs(mySimOut[[x]])$file, value = TRUE)
}))

tsf <- grep(pattern = "rstTimeSinceFire", grds, value = TRUE)
vtm <- grep(pattern = "vegTypeMap", grds, value = TRUE)

# timeSinceFire <- Cache(cacheRepo = paths$cachePath, lapply, tsf, function(x) {
#   xrast <- raster(x)
#   sum(xrast[]>80)/ncell(xrast)
# })





# timeSinceFireHist <- hist(unlist(timeSinceFire), plot = FALSE)
# 
# leading <- Cache(cacheRepo = paths$cachePath, lapply, vtm, function(x) {
#   xrast <- raster(x)
#   #table1 <- table(factorValues(xrast, xrast[])) # slower than tabulate directly
#   nonNACells <- na.omit(xrast[])
#   vals <- tabulate(nonNACells, max(levels(xrast)[[1]]$ID))
#   names(vals)[levels(xrast)[[1]]$ID] <- levels(xrast)[[1]]$Factor
#   vals <- vals[!is.na(names(vals))]
#   props <- vals/length(nonNACells)
# })
# 
# spruce <- hist(unlist(lapply(leading, function(x) x["Spruce leading"])), plot = FALSE)
# mixed <- hist(unlist(lapply(leading, function(x) x["Mixed"])), plot = FALSE)
# deciduous <- hist(unlist(lapply(leading, function(x) x["Deciduous leading"])), plot = FALSE)

#Cache(cacheRepo = paths$cachePath, 


ageClasses <- c("Young", "Immature", "Mature", "Old")

leadingByStage <- function(timeSinceFireFiles, vegTypeMapFiles, polygonToSummarizeBy, polygonNames, 
                           ageCutoffs = c(0, 40, 80, 120), 
                           ageClasses, cl) {
  
  
  if (missing(cl)) {
    lapplyFn <- "lapply" 
  } else {
    lapplyFn <- "parLapplyLB"
    clusterExport(cl = cl, varlist = list("timeSinceFireFiles", "vegTypeMapFiles", "polygonToSummarizeBy"),
                  envir = environment())
    clusterEvalQ(cl = cl, {
      library(raster)
    })
  }
  out <- lapply(ageCutoffs, function(ages) {
    y <- match(ages, ageCutoffs)
    if (tryCatch(is(cl, "cluster"), error = function(x) FALSE)) {
      startList <- list(cl = cl)
    } else {
      startList <- list()
    }
    startList <- append(startList, list(y = y))
    
    out1 <- Cache(cacheRepo = paths$cachePath, 
                  do.call, lapplyFn, append(startList, list(X = timeSinceFireFiles, function(x, ...) {
                    x <- match(x, timeSinceFireFiles)
                    timeSinceFireFilesRast <- raster(timeSinceFireFiles[x])
                    leadingRast <- raster(vegTypeMapFiles[x])
                    leadingRast[timeSinceFireFilesRast[]<ageCutoffs[y]] <- NA
                    if ((y+1) < length(ageCutoffs))
                      leadingRast[timeSinceFireFilesRast[]>=ageCutoffs[y+1]] <- NA
                    
                    aa <- extract(leadingRast, polygonToSummarizeBy, fun = function(x, ...) {
                      nonNACells <- na.omit(x)
                      vals <- tabulate(nonNACells, max(levels(leadingRast)[[1]]$ID))
                      names(vals)[levels(leadingRast)[[1]]$ID] <- levels(leadingRast)[[1]]$Factor
                      vals <- vals[!is.na(names(vals))]
                      props <- vals/length(nonNACells)  
                    })
                  })))
    names(out1) <- paste(basename(dirname(tsf)), basename(tsf), sep = "_")
    out1
  })
  names(out) <- ageClasses
  out
}

## TO DO: move this out of global init
message("Running leadingByStage")
leading <- Cache(leadingByStage, tsf, vtm, ecodistricts, polygonNames = ecodistricts$ECODISTRIC, 
                 #cl=cl, 
                 ageClasses = ageClasses, cacheRepo = paths$cachePath)


# Large patches

largePatchSizeOptions <- c(100, 200, 500, 1000)


countNumPatches <- function(ras, patchSize, ...) {
  clumpedRas <- clump(ras, ...)
  freqTable <- data.table(freq(clumpedRas))[!is.na(value), ][
    , area := count * (res(clumpedRas)[1] ^ 2) / 10000]
  largeEnoughPatches <- freqTable[area >= patchSize, ][, newValue := as.numeric(as.factor(value))]
  clumpedRas[!(clumpedRas %in% largeEnoughPatches$value)] <- NA
  list(ras = clumpedRas, count = largeEnoughPatches)
}

largePatchesFn <- function(timeSinceFireFiles, vegTypeMapFiles, polygonToSummarizeBy, #polygonNames, 
                           ageCutoffs = c(0, 40, 80, 120), patchSize = 1000, 
                           ageClasses, cl, notOlderThan = Sys.time() - 1e7) {
  if (missing(cl)) {
    lapplyFn <- "lapply"
  } else {
    lapplyFn <- "parLapplyLB"
    clusterExport(cl = cl, varlist = list("timeSinceFireFiles", "vegTypeMapFiles", "polygonToSummarizeBy",
                                          "countNumPatches", "paths", "ageCutoffs", "patchSize",
                                          "ageClasses"),
                  envir = environment())
    clusterEvalQ(cl = cl, {
      library(raster)
      library(magrittr)
      library(SpaDES)
      library(data.table)
    })
  }
  
  out <- lapply(ageCutoffs, function(ages) {
    y <- match(ages, ageCutoffs)
    if (tryCatch(is(cl, "cluster"), error = function(x) FALSE)) {
      startList <- list(cl = cl)
    } else {
      startList <- list()
    }
    startList <- append(startList, list(y = y))
    out1 <- Cache(cacheRepo = paths$cachePath, #notOlderThan = Sys.time(),
                  do.call, lapplyFn, append(startList, list(X = timeSinceFireFiles, function(x, ...) {
                    x <- match(x, timeSinceFireFiles)
                    timeSinceFireFilesRast <- raster(timeSinceFireFiles[x])
                    leadingRast <- raster(vegTypeMapFiles[x])
                    leadingRast[timeSinceFireFilesRast[]<ageCutoffs[y]] <- NA
                    if ((y+1) < length(ageCutoffs))
                      leadingRast[timeSinceFireFilesRast[]>=ageCutoffs[y+1]] <- NA
                    
                    clumpedRasts <- lapply(levels(leadingRast)[[1]]$ID, function(ID) {
                      spRas <- leadingRast
                      spRas[spRas == ID] <- NA
                      #Cache(cacheRepo = paths$cachePath, notOlderThan = Sys.time(),
                      countNumPatches(spRas, patchSize, directions = 8)
                    })
                    names(clumpedRasts) <- levels(leadingRast)[[1]]$Factor
                    clumpedRasts <- append(clumpedRasts,
                                           list(speciesAgnostic =
                                                  #Cache(notOlderThan = Sys.time(),
                                                  countNumPatches(leadingRast, patchSize, directions = 8
                                                                  #, cacheRepo = paths$cachePath)
                                                  )
                                           ))
                    
                    out2 <- lapply(clumpedRasts, function(ras) {
                      aa <- #Cache(notOlderThan = Sys.time(),
                        extract(ras[[1]], polygonToSummarizeBy, fun = function(x, ...) {
                          nonNACells <- na.omit(x)
                          length(unique(nonNACells))
                        }, cacheRepo = paths$cachePath)
                    }) %>% setNames(names(clumpedRasts))
                    out2
                    
                  })))
    names(out1) <- paste(basename(dirname(timeSinceFireFiles)),basename(timeSinceFireFiles),sep="_")
    out1
  }
  )
  names(out) <- ageClasses
  out
}

if (FALSE) {
  for(iii in largePatchSizeOptions[3]) {
    message("Running largePatches")
    largePatches <- Cache(largePatchesFn, timeSinceFireFiles=tsf, 
                          vegTypeMapFiles=vtm, 
                          polygonToSummarizeBy = ecodistricts
                          #, polygonNames = ecodistricts$ECODISTRIC 
                          #, cl=cl
                          , cacheRepo = paths$cachePath#, notOlderThan = Sys.time() 
                          , ageClasses = ageClasses, patchSize = as.integer(iii)
    )
  }
}

omitted <- lapply(leading, function(x) lapply(x, function(y) attr(na.omit(y), "na.action")))
polygonsWithData <- lapply(seq_along(leading), function(x) {
    unlist(lapply(x, function(y) {
      if (!is.null(omitted[[x]][[y]])) {
        seq_len(NROW(leading[[x]][[y]]))[-omitted[[x]][[y]]]
      } else {
        seq_len(NROW(leading[[x]][[y]]))
      }
    }))
  }) %>%
  setNames(ageClasses)
vegLeadingTypes <- unique(unlist(lapply(leading, function(x) lapply(x, function(y) colnames(y)))))

if (FALSE) {
  initialCommunityMap <- raster("initialCommunitiesMap.tif")
  ecoregionMap <- raster("landtypes_BP.tif")
  
  spruceleadingpatchmapRegion1_5000 <- readRDS("spruceleadingpatchmapRegion1_5000.rds")
  spruceleadingpatchmapRegion2_5000 <- readRDS("spruceleadingpatchmapRegion2_5000.rds")
  aspenleadingpatchmapRegion1_5000 <- readRDS("aspenleadingpatchmapRegion1_5000.rds")
  aspenleadingpatchmapRegion2_5000 <- readRDS("aspenleadingpatchmapRegion2_5000.rds")
  overoldpatchmapRegion1_5000 <- readRDS("overoldpatchmapRegion1_5000.rds")
  overoldpatchmapRegion2_5000 <- readRDS("overoldpatchmapRegion2_5000.rds")
  
  #CanadaMap <- spTransform(CanadaMap, crs(ecoregionMap))
  #studyArea <- crop(CanadaMap, ecoregionMap)
  
  #CanadaMap <- fortify(CanadaMap, region = "NAME") 
  #CanadaMap <- data.table(CanadaMap)
  #saveRDS(CanadaMap, file = "CanadaMap.rds")
  #CanadaMap <-readRDS("CanadaMap.rds")
  #CanadaMap1 <- CanadaMap[long<=1.5e+06, ] %>%
  #  data.frame
  
  # figure1 <- ggplot(CanadaMap, aes(x = long, y = lat, col = "grey")) +
  #   geom_rect(aes(xmin = xmin(studyArea), xmax = xmax(studyArea),
  #                 ymin = ymin(studyArea), ymax = ymax(studyArea)),
  #             fill = "white", col = "red")+
  #   geom_path(aes(group = group))+
  #   scale_color_manual(values = "grey", label = "border") +
  #   #geom_rect(aes(xmin = 0.8e+06, xmax = 3e+06,
  #   #          ymin = 8.5e+06, ymax = 1.09e+07), fill = "white", col = "white")+
  #   #geom_rect(aes(xmin = 0.8e+06, xmax = 3e+06,
  #   #              ymin = 6e+06, ymax = 8.4e+06), fill = "white", col = "white")#+
  #   
  #   #annotation_raster(as.raster(ecoregionMap), xmin = 0.8e+06, xmax = 3e+06,
  #   #                  ymin = 8.5e+06, ymax = 1.05e+07)+
  #   #annotation_raster(as.raster(initialCommunityMap), xmin = 0.8e+06, xmax = 3e+06,
  #   #                  ymin = 6e+06, ymax = 8e+06)+
  #   #annotate("text", x = 1.9e+06, y = 1.075e+7, label = "Ecoregions (N=163)", size = 5)+
  #   #annotate("text", x = 1.9e+06, y = 8.25e+6, label = "Initial Communities (N=442)", size = 5)+
  # theme_bw()+
  #   theme(panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         panel.border = element_blank(),
  #         axis.title = element_blank(),
  #         axis.text = element_blank(),
  #         axis.ticks = element_blank(),
  #         legend.position = "none")
  # figure1
  
  
  #ecoRegionFig <- as.raster(ecoregionMap)
  #saveRDS(ecoRegionFig, file = "ecoregionFig.rds")
  #ecoregionFig <- readRDS("ecoregionFig.rds")
  
  
  #initialCommunityMap <- as.raster(initialCommunityMap)
  #saveRDS(initialCommunityMap, file = "initialCommunityMap.rds")
  #initialCommunityMap <- readRDS("initialCommunityMap.rds")
  
  crsICM <- crs(initialCommunityMap)
  extInit <- extent(initialCommunityMap)
  X = extInit[c(1,1,2,2,1)]
  Y = extInit[c(3,4,4,3,3)]
  
  Sr1 <- Polygon(cbind(X,Y))
  Srs1 = Polygons(list(Sr1), "s1")
  inputMapPolygon <- SpatialPolygons(list(Srs1), 1L)
  crs(inputMapPolygon) <- crsICM
  crsCanadaMap <- crs(CanadaMap)
  inputMapPolygon <- spTransform(inputMapPolygon, crsCanadaMap)
  
  
  
  #landisInputs <- read.table(file = "clipboard")
  #colnames(landisInputs) <- c("Longevity", "Sexual Maturity", "Shade tolerance", "Effective Seed Distance", "Maximum Seed Distance")
  #landisInputs <- data.frame(Species = landisInputsRowNames$V1, landisInputs)
  #saveRDS(landisInputs, file = "landisInputs.rds")
  # SpEcoReg <- read.table(file = "clipboard", header=TRUE, stringsAsFactors = FALSE)
  # saveRDS(SpEcoReg, "SpEcoReg.rds")
  
  seralStageData <- readRDS("seralStageData.rds")
  seralStageDataFig5 <- readRDS("seralStageDataFig5.rds")
  vegTypeDataFig6 <- readRDS("vegTypeDataFig6.rds")
  
  ### Fig 2
  
  MapRegion1 <- readRDS("MapRegion1.rds")
  MapRegion2 <- readRDS("MapRegion2.rds")
  #saveRDS(output, "outputFig2.rds")
  outputFig2 <- readRDS("outputFig2.rds")
  
  NRVFig <- ggplot(data = outputFig2, aes(x = Year, y = totalB, col = chain))+
    geom_rect(aes(xmin = 800, xmax = 2000, ymin = 6000, ymax = 7200),
              fill = "white", col = "red")+
    annotate("text", x = 1400, y = 7150, label = "Summary Regions", size = 6)+
    annotation_raster(as.raster(MapRegion1), xmin = 850, xmax = 1375,
                      ymin = 6050, ymax = 7000)+
    annotation_raster(as.raster(MapRegion2), xmin = 1425, xmax = 1950,
                      ymin = 6050, ymax = 7000)+
    geom_rect(aes(xmin = 1400, xmax = 2000, ymin = 7500, ymax = 8000),
              fill = "white", col = "red")+
    annotate("text", x = 1700, y = 7650, label = "Summary Period", size = 6)+
    geom_line()+
    scale_x_continuous("Year", limits = c(-0, 2000), breaks = seq(0, 2000, by = 400))+
    scale_y_continuous(name = expression(paste("Aboveground biomass (g . ", m^-2, ")",
                                               sep = "")),
                       limits = c(6000, 9000), breaks = seq(6000, 9000, by = 500))+
    theme_bw()+
    guides(colour = guide_legend(title = "Simulations", title.position = "top"))+
    
    # labs(colour = "Simulation")+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "black", size = 1),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 15),
          legend.title.align = 0.5,
          legend.key = element_rect(fill = "white", colour = "white"),
          legend.key.width = unit(60, "points"),
          legend.key.height = unit(35, "points"),
          legend.position = c(0.7, 0.8),
          legend.direction = "horizontal",
          legend.text = element_text(size = 15))
  
  
  #### Fig 3
  vegTypeMapExampleRegion1 <- readRDS("vegTypeMapExampleRegion1.rds")
  vegTypeMapExampleRegion2 <- readRDS("vegTypeMapExampleRegion2.rds")
  seralMapExampleRegion1 <- readRDS("seralMapExampleRegion1.rds")
  seralMapExampleRegion2 <- readRDS("seralMapExampleRegion2.rds")
  seralStageData <- readRDS("seralStageData.rds")
  vegTypeDataFig6 <- readRDS("vegTypeDataFig6.rds")
  
  availableRegions <- unique(vegTypeData$ecoregion)
}



######### Modules


vegAgeModUI <- function(id) {
  #decidOldModUI <- function(id) {
  ns <- NS(id)
  
  ids <- strsplit(id, split = "_")[[1]]
  i <- as.numeric(ids[1])
  j <- as.numeric(ids[2])
  k <- as.numeric(ids[3])
  tagList(
    box(width = 4, solidHeader = TRUE, collapsible = TRUE, 
        title = paste0(ageClasses[i],", ", vegLeadingTypes[k], ", in ", ecodistricts$ECODISTRIC[j]),
        #splitLayout(cellWidths=c("75%","25%"),
        plotOutput(ns("g"), height = 300)#,
        #radioButtons(ns("radio"),label = "buttons",
        #             choices = list("with intercept"=1,"without intersept"=2),
        #             selected = 1))      
    )
  )
  
} 

vegAgeMod <- function(input, output, server, listOfProportions, indivPolygonIndex, 
                      polygonLayer, vegLeadingType) {
  output$g <- renderPlot(height = 300, 
                         {
                           withProgress(message = 'Calculation in progress',
                                        detail = 'This may take a while...',value = 0,
                                        {
                                          
                                          
                                          #clearPlot()
                                          actualPlot <- #Cache(cacheRepo = paths$cachePath,
                                            ggplot(data = data.frame(x = unlist(lapply(
                                              listOfProportions, function(x) x[indivPolygonIndex, vegLeadingType]))),
                                              aes(x = x)) + 
                                            stat_bin(bins = 30) + 
                                            xlab("") + #xlab("Proportion of polygon") + 
                                            theme_bw() + 
                                            theme(text = element_text(size=16)) + 
                                            ylab("Frequency")
                                          
                                          # If want base plot histogram -- faster
                                          # actualPlot <- 
                                          #    try(hist(unlist(lapply(listOfProportions, function(x) x[indivPolygonIndex, "Deciduous leading"])), 
                                          #             plot = FALSE))
                                          #if(!(is(actualPlot, "try-error")))
                                          #   actualPlot
                                          #Plot(actualPlot, new = TRUE, visualSqueeze = 1, gpText = gpar(fontsize = 16), 
                                          #      title = "", 
                                          #      addTo = paste0("actualPlot_dist",polygonLayer$ECODISTRIC[indivPolygonIndex]))
                                          setProgress(1)
                                        })
                           actualPlot
                         })
}


clumpModOutput <- function(id) {
  #decidOldModUI <- function(id) {
  ns <- NS(id)
  
  ids <- strsplit(id, split = "_")[[1]]
  i <- as.numeric(ids[1])
  j <- as.numeric(ids[2])
  k <- as.numeric(ids[3])
  #tagList(
  box(width = 4, solidHeader = TRUE, collapsible = TRUE, 
      title = paste0(ageClasses[i],", ", vegLeadingTypes[k], ", in ", ecodistricts$ECODISTRIC[j]),
      #splitLayout(cellWidths=c("75%","25%"),
      plotOutput(ns("h"), height = 300)#,
      #radioButtons(ns("radio"),label = "buttons",
      #             choices = list("with intercept"=1,"without intersept"=2),
      #             selected = 1))      
  )
  
} 


clumpMod2Input <- function(id, label = "CSV file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  #tagList(
  selectInput(ns("PatchSize33"), "Patch Size Here", selectize = FALSE,
              choices = largePatchSizeOptions, selected = largePatchSizeOptions[3]
  )
  #)
}

clumpMod2 <- function(input, output, server, tsf, vtm, ecodistricts, #polygonNames = ecodistricts$ECODISTRIC, 
                      #cl=cl, 
                      ageClasses = ageClasses,
                      patchSize,
                      cacheRepo = paths$cachePath,
                      id, indivPolygonIndex) {
  
  Clumps <- reactive({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...',value = 0,
                 {
                   largePatches <- Cache(largePatchesFn, timeSinceFireFiles=tsf, 
                                         vegTypeMapFiles=vtm, 
                                         polygonToSummarizeBy = ecodistricts, 
                                         #polygonNames = polygonNames, 
                                         #cl=cl, 
                                         ageClasses = ageClasses, patchSize = as.integer(input$PatchSize33),
                                         cacheRepo = cacheRepo
                   )
                   setProgress(1)
                 })
    
    largePatches
  })
  return(Clumps)
}


clumpMod <- function(input, output, server, Clumps, id
                     #tsf, vtm, ecodistricts, polygonNames = ecodistricts$ECODISTRIC, 
                     #cl=cl, 
                     #Clumps,
                     #ageClasses = ageClasses,
                     #cacheRepo = paths$cachePath,
                     #id#,
                     #indivPolygonIndex
) {
  
  output$h <- renderPlot( {
    #browser()
    a <- Clumps()
    ids <- strsplit(id, split = "_")[[1]]
    i <- as.numeric(ids[1])
    j <- as.numeric(ids[2])
    k <- as.numeric(ids[3])
    
    forHist <- unlist(lapply(a, function(x) lapply(x, function(y) {
      y[[k]][j,1]
    }
    )))
    
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...',value = 0,
                 {
                   actualPlot <- #Cache(cacheRepo = paths$cachePath,
                     ggplot(data = data.frame(x = forHist),
                            aes(x = x)) + 
                     stat_bin(bins = 30) + 
                     xlab("") + #xlab("Proportion of polygon") + 
                     theme_bw() + 
                     theme(text = element_text(size=16)) + 
                     ylab("Frequency")
                   setProgress(1)
                 })
    
    actualPlot
  }
  )
}


leafletMapUI <- #bootstrapPage(
  #tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  function(id) {
    #decidOldModUI <- function(id) {
    ns <- NS(id)
    #browser()
    #ids <- strsplit(id, split = "_")[[1]]
    #i <- as.numeric(ids[1])
    #j <- as.numeric(ids[2])
    #k <- as.numeric(ids[3])
    tagList(
      box(width = 12, 
          solidHeader = TRUE, collapsible = TRUE, 
          title = "Area covered by this demo, within the LandWeb study area",
          #splitLayout(cellWidths=c("75%","25%"),
          leafletOutput(ns("leafletMap"), height = 600),
          selectInput(ns("leafletMapPolygons"), "Other layers to show summaries with", 
                      choices = names(loadedPolygons), selected = names(loadedPolygons)[[1]])
          
          #radioButtons(ns("radio"),label = "buttons",
          #             choices = list("with intercept"=1,"without intersept"=2),
          #             selected = 1))      
      )
    )
  }
#)

lflt = "+init=epsg:4326"

ecodistrictsLFLT <- spTransform(ecodistricts, sp::CRS(lflt))
ecodistrictsFullLFLT <- spTransform(ecodistrictsFull, sp::CRS(lflt))
AlbertaFMULFLT <- spTransform(AlbertaFMU, sp::CRS(lflt))
AlbertaFMUFullLFLT <- spTransform(AlbertaFMUFull, sp::CRS(lflt))
ecodistrictsFullLFLT <- spTransform(ecodistrictsFull, sp::CRS(lflt))

loadedPolygons <- list("Ecodistrict" = ecodistrictsLFLT, "Alberta FMUs" = AlbertaFMULFLT)

leafletMap <- function(input, output, session, ecodistrictsFullLFLT) {
  
  # filteredData <- reactive({
  #   quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  # })
  # 
  # # This reactive expression represents the palette function,
  # # which changes as the user makes selections in UI.
  # colorpal <- reactive({
  #   colorNumeric(input$colors, quakes$mag)
  # })
  #browser()
  
  output$leafletMap <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...',value = 0,
                 {
                   poly <- polygonInput()
                   a <- leaflet() %>% addTiles(group = "OSM (default)") %>%
                     addPolygons(data = ecodistrictsFullLFLT, smoothFactor=2,
                                 group = "Ecodistricts", popup=paste("Ecodistrict",ecodistrictsFullLFLT$ECODISTRIC)) %>%
                     addPolygons(data = AlbertaFMUFullLFLT, smoothFactor=2,
                                 group = "Alberta FMUs",  popup=paste("FMU",AlbertaFMUFullLFLT$FMU_NAME)) %>%
                     addLayersControl(
                       #baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
                       baseGroups = c("Ecodistricts", "Alberta FMUs"),
                       options = layersControlOptions(collapsed = FALSE)
                     ) %>%
                     setView(-115, 58, zoom=5) %>%
                     addPolygons(data = poly, color = "red")  
                   # fitBounds(xmin(ecodistrictsFullLFLT),ymin(ecodistrictsFullLFLT),
                   #           xmax(ecodistrictsFullLFLT),ymax(ecodistrictsFullLFLT))
                   #mean(xmax(ecodistrictsFullLFLT),xmin(ecodistrictsFullLFLT)), 
                   #mean(ymax(ecodistrictsFullLFLT),ymin(ecodistrictsFullLFLT)), 
                   #ymin(ecodistrictsFullLFLT), 
                   #zoom = 5)
                   setProgress(1)
                 })   
    a
    
  })
  
  polygonInput <- reactive({
    switch(input$leafletMapPolygons,
           "Ecodistrict" = ecodistrictsLFLT, 
           "Alberta FMUs" = AlbertaFMULFLT
    )
  })
  
  # observe({
  # 
  #   poly <- polygonInput()
  #   leafletProxy("leafletMap", data = poly) %>%
  #     clearShapes() %>%
  #     addPolygons(color = "red")
  # 
  #   })
  
  #leafletProxy()
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  # observe({
  #   pal <- colorpal()
  #   
  #   leafletProxy("map", data = filteredData()) %>%
  #     clearShapes() %>%
  #     addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
  #                fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
  #     )
  # })
  # 
  # # Use a separate observer to recreate the legend as needed.
  # observe({
  #   proxy <- leafletProxy("map", data = quakes)
  #   
  #   # Remove any existing legend, and only if the legend is
  #   # enabled, create a new one.
  #   proxy %>% clearControls()
  #   if (input$legend) {
  #     pal <- colorpal()
  #     proxy %>% addLegend(position = "bottomright",
  #                         pal = pal, values = ~mag
  #     )
  #   }
  # })
}
  