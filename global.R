#### Some variables
largePatchSizeOptions <- c(500, 1000, 2000)
largePatchesFnLoop <- length(largePatchSizeOptions) - 1 # The number is how many to run, e.g., 1 would be run just 1000
ageClasses <- c("Young", "Immature", "Mature", "Old")
experimentReps <- 3 # was 4
maxNumClusters <- 0#35 # use 0 to turn off # otherwise detectCPUs() - 1
if(!exists("globalRasters")) globalRasters <- list()
endTime <- 400 # was 4
studyArea <- "MEDIUM"
#studyArea <- "SMALL"
successionTimestep <- 10 # was 2
summaryInterval <- 10#endTime/2 # was 2
summaryPeriod <- c(200, endTime)

#### Some variables
largePatchSizeOptions <- c(100, 200, 400)
largePatchesFnLoop <- length(largePatchSizeOptions) - 3 # The number is how many to run, e.g., 1 would be run just 1000
ageClasses <- c("Young", "Immature", "Mature", "Old")
experimentReps <- 2 # was 4
maxNumClusters <- 0#35 # use 0 to turn off # otherwise detectCPUs() - 1
if(!exists("globalRasters")) globalRasters <- list()
endTime <- 100 # was 4
#studyArea <- "MEDIUM"
studyArea <- "SMALL"
successionTimestep <- 10 # was 2
summaryInterval <- 10#endTime/2 # was 2
summaryPeriod <- c(10, endTime)

##########
message("Started at ", Sys.time())


if(FALSE) { # THese are all "dangerous" in the sense that they should never be run inadvertently
  SpaDES::clearCache(cacheRepo = "appCache")
  SpaDES::clearCache(cacheRepo = "appCache/studyRegion/")
  rm(mySim)
  rm(cl)
  file.remove(dir("outputs", recursive = TRUE, full.names = TRUE))
  file.remove("mySimDigestSaved.rds", "mySimSaved.rds")
}

# To rerun the spades initial call, delete the mySim object in the .GlobalEnv ##

if (FALSE) { # For pushing to shinyapps.io
  message("Started at: ",Sys.time())
  allFiles <- dir(recursive = TRUE)
  allFiles <- grep(allFiles, pattern = "^R-Portable", invert = TRUE, value = TRUE)
  #allFiles <- grep(allFiles, pattern = "^appCache", invert = TRUE, value = TRUE)
  #allFiles <- grep(allFiles, pattern = "^outputs", invert = TRUE, value = TRUE)
  print(paste("Total size:", sum(unlist(lapply(allFiles, function(x) file.info(x)[, "size"]))) / 1e6, "MB"))
  rsconnect::deployApp(appName = "LandWebDemo", appFiles = allFiles, appTitle = "LandWeb Demo",
                       contentCategory = "application")  
  rsconnect::deployApp(appName = "LandWebDemoDev", appFiles = allFiles, 
                       appTitle = "LandWeb Demo",
                       contentCategory = "application")  
  
}

print(getwd())
#.libPaths("TEMP")
if (FALSE) {
  pkgNamespaces <- c("htmlwidgets", "shiny", "shinydashboard", "shinyBS", "leaflet",
                     "BH", "RCurl", "RandomFieldsUtils", "R.oo", "R.methodsS3", "SpaDES", "markdown",
                     "visNetwork", "rgexf", "influenceR", "DBI", "viridis", "bit", "parallel",
                     "devtools", "raster", "rgeos", "RSQLite", "magrittr", "raster", "sp",
                     "dplyr", "ggplot2", "maptools", "broom", "ggvis", "rgdal", "grid", "VGAM")
  lapply(pkgNamespaces, function(p) if (!require(p, quietly = TRUE, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
  })
  if (!require("RandomFieldsUtils", character.only = TRUE)) install.packages("RandomFieldsUtils")
  if (!require("RandomFields", character.only = TRUE)) install.packages("RandomFields")
}

if (tryCatch(packageVersion("SpaDES") < "1.3.1.9047", error = function(x) TRUE))
  devtools::install_github("PredictiveEcology/SpaDES@development")  

pkgs <- c("shiny", "shinydashboard", "shinyBS", "leaflet", #"plotly", 
          "broom", "rgeos", "raster", "rgdal", "grid", "ggplot2", "VGAM", "maptools",
          "dplyr", "data.table", "magrittr", "parallel", "SpaDES", "ggvis", "markdown")
lapply(pkgs, require, quietly = TRUE, character.only = TRUE)

## For shinyapps.io -- needs to see explicit require statements
if (FALSE) {
  require(shiny)
  require(shinydashboard)
  require(shinyBS)
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
  require(leaflet)
  require(parallel)
  require(markdown)
}

curDir <- getwd()
setwd(curDir)



#### Maps
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


shpStudyRegionFull <- SpaDES::Cache(shapefile, file.path(paths$inputPath,"shpLandWEB.shp"),
                                    cacheRepo = paths$cachePath)
shpStudyRegionFull$fireReturnInterval <- shpStudyRegionFull$LTHRC
shpStudyRegionFull@data <- shpStudyRegionFull@data[,!(names(shpStudyRegionFull) %in% "ECODISTRIC")]

crsKNNMaps <- CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
set.seed(2)#set.seed(5567913)
if (studyArea == "SMALL") {
  # #smallExt <- clickExtent()
  # smallExt <- new("Extent" , xmin = -983660, xmax = -899973, 
  #                 ymin = 8007529, ymax = 8085986)
  # #smallExt <- new("Extent" , xmin = -1276390, xmax = -1181880, 
  # #                ymin = 7637258, ymax = 7706827)
  # shpStudyRegion2 <- spTransform(shpStudyRegion, crsKNNMaps)
  # shpStudyRegion <- crop(shpStudyRegion2, smallExt)
  # shpStudyRegionCan <- spTransform(shpStudyRegion, crs(CanadaMap))
  areaKm2 <- 10000#700000#2000#600000#too big for laptop
} else if (studyArea == "MEDIUM") {
  areaKm2 <- 40000 #700000#2000#600000#too big for laptop
}
minY <- 7778877 - 1.6e5
shpStudyRegionFull <- spTransform(shpStudyRegionFull, crsKNNMaps)
minX <- -1202250.2
maxX <- minX + sqrt(areaKm2 * 1e6)
maxY <- minY + sqrt(areaKm2 * 1e6)
meanY <- mean(c(minY, maxY))

# Add random noise to polygon
xAdd <- round(runif(1,-5e5, 1.5e6))
yAdd <- round(runif(1, 1e5, 5e5)) - xAdd / 2
nPoints <- 20
betaPar <- 0.6
X <- c(jitter(sort(rbeta(nPoints, betaPar, betaPar) * (maxX - minX) + minX)),
       jitter(sort(rbeta(nPoints, betaPar, betaPar) * (maxX - minX) + minX, decreasing = TRUE)))
Y <- c(jitter(sort(rbeta(nPoints / 2, betaPar, betaPar) * (maxY - meanY) + meanY)),
       jitter(sort(rbeta(nPoints, betaPar, betaPar) * (maxY - minY) + minY, decreasing = TRUE)),
       jitter(sort(rbeta(nPoints / 2, betaPar, betaPar) * (meanY - minY) + minY)))

Sr1 <- Polygon(cbind(X + xAdd, Y + yAdd))
Srs1 <- Polygons(list(Sr1), "s1")
inputMapPolygon <- SpatialPolygons(list(Srs1), 1L)
crs(inputMapPolygon) <- crsKNNMaps
shpStudyRegion <- raster::intersect(shpStudyRegionFull, inputMapPolygon)

prepare1 <- function(shpStudyRegion, shpStudyRegionFull) {
  shpStudyAreaFort <- tidy(shpStudyRegion, region = 'Name_1') 
  shpStudyAreaFort <- left_join(shpStudyAreaFort, shpStudyRegion@data[, c("Name_1", "fireReturnInterval")], by = c("id" = "Name_1"))
  shpStudyAreaOrigFort <- tidy(shpStudyRegionFull, region = 'Name_1') 
  shpStudyAreaOrigFort <- left_join(shpStudyAreaOrigFort, shpStudyRegionFull@data[, c("Name_1", "fireReturnInterval")], by = c("id" = "Name_1"))
  #shpStudyAreaOrigFort<-shpStudyAreaOrigFort[order(shpStudyAreaOrigFort$order), ]

  #map <- ggplot2::fortify(maine, region="name")
  
  wdth <- 650
  ht <- wdth * (ymax(shpStudyRegionFull) - ymin(shpStudyRegionFull)) /
    (xmax(shpStudyRegionFull) - xmin(shpStudyRegionFull))
  df2 <- data.frame(fri = unique(shpStudyAreaOrigFort$fireReturnInterval), 
                    colrs = colorRampPalette(c("orange", "dark green"))(diff(range(shpStudyAreaOrigFort$fireReturnInterval)))[unique(shpStudyAreaOrigFort$fireReturnInterval) - min(shpStudyAreaOrigFort$fireReturnInterval) + 1]
  )
  shpStudyAreaOrigFort <- left_join(shpStudyAreaOrigFort, df2, by = c("fireReturnInterval" = "fri"))
  # 
  a <- shpStudyAreaOrigFort %>%
    ggvis(~long, ~lat) %>%
    group_by(group, id) %>%
    layer_paths(strokeOpacity := 0.5, stroke := "#7f7f7f", fill := ~fireReturnInterval) %>%
    add_tooltip(function(data) {
      paste0("Fire Return Interval: ", data$fireReturnInterval)
    }, "hover") %>% 
    layer_paths(data = shpStudyAreaFort %>% group_by(group, id),
                stroke := "red") %>%
    #hide_legend("fill") %>%
    hide_axis("x") %>% hide_axis("y") %>%
    #set_options(width = 400, height = 800)#, keep_aspect = TRUE)
    set_options(width = wdth, height = ht, keep_aspect = TRUE)
  
}
#ggStudyRegion <- Cache(prepare1, shpStudyRegion, 
#                       shpStudyRegionFull, cacheRepo = paths$cachePath)
ggStudyRegion <- prepare1(shpStudyRegion, shpStudyRegionFull)

if (FALSE) {
  readSpTransform <- function(shapefilePath, crs, cacheRepo){
    AlbertaFMUFull <- shapefile(shapefilePath)
    AlbertaFMUFull <- spTransform(AlbertaFMUFull, crs)
  }
  AlbertaFMUFull <- Cache(cacheRepo = paths$cachePath,
                          readSpTransform, 
                          shapefilePath=file.path(paths$inputPath, "FMU_Alberta_2015-11", "FMU_Alberta_2015-11"),
                          crs = crs(shpStudyRegion))
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

lflt <- "+init=epsg:4326"

# Available polygons
ecodistrictsDemoLFLT <- spTransform(ecodistricts, sp::CRS(lflt))
ecodistrictsFullLFLT <- spTransform(ecodistrictsFull, sp::CRS(lflt))
#AlbertaFMUDemoLFLT <- spTransform(AlbertaFMU, sp::CRS(lflt))
#AlbertaFMUFullLFLT <- spTransform(AlbertaFMUFull, sp::CRS(lflt))
ecodistrictsDemo <- ecodistricts
ecodistrictsFull <- ecodistrictsFull
#AlbertaFMUDemo <- AlbertaFMU
#AlbertaFMUFull <- AlbertaFMUFull

availablePolygons <- c("ecodistricts")#, "AlbertaFMU")
availableProjections <- c("", "LFLT")
availableScales <- c("Full", "Demo")
available <- data.frame(stringsAsFactors = FALSE,
                        expand.grid(stringsAsFactors = FALSE,
                                    polygons = availablePolygons,
                                    scales = availableScales,
                                    projections = availableProjections),
                        names = rep(c("Ecodistricts Full", #"Alberta FMUs Full", 
                                      "Ecodistricts Demo"#, "Alberta FMUs Demo"
                                      ), 2))
polygons <- lapply(seq_len(NROW(available)), function(ii) {
  get(paste0(available$polygons[ii], available$scales[ii], available$projections[ii]))}) %>%
  setNames(available$names)

polygonColours <- c(rep(c("red", "blue"), 2))
polygonIndivIdsColum <- list("ECODISTRIC", "FMU_NAME") %>% setNames(names(polygons[1:(length(polygons)/4)+(length(polygons)/4)*3]))

timeSinceFirePalette <- colorNumeric(
  c(rep("red", 10), paste0(colorRampPalette(c("light green", "dark green"))(100),"FF")),
  domain = NULL)
attr(timeSinceFirePalette, "colorArgs")$na.color <- "#00000000"

## Create mySim
modules <- list("landWebDataPrep", "initBaseMaps", "fireDataPrep", "LandMine",
                "LW_LBMRDataPrep", "LBMR", "timeSinceFire", "LandWebOutput")


times <- list(start = 0, end = endTime)
objects <- list("shpStudyRegionFull" = shpStudyRegionFull,
                "shpStudySubRegion" = shpStudyRegion,
                "successionTimestep" = successionTimestep,
                "summaryPeriod" = summaryPeriod)
parameters <- list(fireNull = list(burnInitialTime = 1,
                                   returnInterval = 1,
                                   .statsInitialTime = 1),
                   LandWebOutput = list(summaryInterval = summaryInterval),
                   LBMR = list(.plotInitialTime = times$start,
                               .saveInitialTime = NA),
                   initBaseMaps = list(.useCache = FALSE))
objectNamesToSave <- c("rstTimeSinceFire", "vegTypeMap")
outputs <- data.frame(stringsAsFactors = FALSE,
                      expand.grid(
                        objectName = objectNamesToSave,#, "oldBigPatch"),
                        saveTime = seq(objects$summaryPeriod[1], objects$summaryPeriod[2], 
                                       by = parameters$LandWebOutput$summaryInterval)),
                      fun = "writeRaster", package = "raster", 
                      file = paste0(objectNamesToSave, c(".tif", ".grd")))
outputs2 <- data.frame(stringsAsFactors = FALSE,
                       expand.grid(
                         objectName = c("simulationOutput"),
                         saveTime = times$end), fun = "saveRDS", package = "base" )

outputs$arguments <- I(rep(list(list(overwrite = TRUE, progress = FALSE, datatype="INT2U", format = "GTiff"),
                                list(overwrite = TRUE, progress = FALSE, datatype="INT1U", format = "raster")), 
                           times=NROW(outputs)/length(objectNamesToSave)))

outputs <- as.data.frame(rbindlist(list(outputs, outputs2), fill = TRUE))

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
  saveRDS(digest::digest(mySim), file = "mySimDigestSaved.rds")
} 

if (maxNumClusters > 0) {
  if (!exists("cl")) {
    library(parallel)
    # try(stopCluster(cl), silent = TRUE)
    ncores <- if (Sys.info()[["user"]] == "achubaty") {
      pmin(maxNumClusters, detectCores() / 2)
    } else {
      maxNumClusters
    } 
    
    ncores <-  pmin(ncores, detectCores() - 1) 
    
    message("Spawning ", ncores, " threads")
    if (Sys.info()[["sysname"]] == "Windows") {
      clusterType = "SOCK"
    } else {
      clusterType = "FORK"
    }
    cl <- makeCluster(ncores, type = clusterType)
    if (Sys.info()[["sysname"]] == "Windows") {
      clusterExport(cl = cl, varlist = list("objects", "shpStudyRegion"))
    }
    message("  Finished Spawning multiple threads")
  }
}

######### Modules

vegAgeModUI <- function(id, vegLeadingTypes) {
  ns <- NS(id)
  
  ids <- strsplit(id, split = "_")[[1]]
  i <- as.numeric(ids[1])
  j <- as.numeric(ids[2])
  k <- as.numeric(ids[3])
  tagList(
    box(width = 4, solidHeader = TRUE, collapsible = TRUE, 
        title = paste0(ageClasses[i],", ", vegLeadingTypes[k], ", in Ecodistrict ", 
                       ecodistricts$ECODISTRIC[j]),
        plotOutput(ns("g"), height = 300)
    )
  )
} 

vegAgeMod <- function(input, output, server, listOfProportions, indivPolygonIndex, 
                      #polygonLayer, 
                      vegLeadingType) {
  
  output$g <- renderPlot(height = 300, {
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0, {
                   actualPlot <- 
                     #ggplot(data = data.frame(x = unlist(lapply(
                      #listOfProportions, function(x) x[indivPolygonIndex, vegLeadingType]))),
                    ggplot(data = data.frame(x = listOfProportions),
                     aes(x = x)) +
                     #stat_bin(bins = 30) +
                     stat_bin(aes(y=..density..),#bins = max(6, max(a)+2), 
                              fill="grey", colour="darkgrey", size = 1,
                              binwidth = 0.1) + 
                     xlab("") + #xlab("Proportion of polygon") +
                     xlim(-0.1,1.1) +
                     theme_bw() +
                     theme(text = element_text(size = 16)) +
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

clumpModOutput <- function(id, vegLeadingTypes) {
  #decidOldModUI <- function(id) {
  ns <- NS(id)
  
  ids <- strsplit(id, split = "_")[[1]]
  i <- as.numeric(ids[1])
  j <- as.numeric(ids[2])
  k <- as.numeric(ids[3])
  #tagList(
  box(width = 4, solidHeader = TRUE, collapsible = TRUE, 
      title = paste0(ageClasses[i],", ", vegLeadingTypes[k], ", in ", ecodistricts$ECODISTRIC[j]),
      plotOutput(ns("h"), height = 300)
  )
  
} 


clumpMod2Input <- function(id, label = "CSV file") {
  ns <- NS(id)
  
  selectInput(ns("PatchSize33"), "Patch Size Here", selectize = FALSE,
              choices = largePatchSizeOptions, selected = largePatchSizeOptions[4])
}

clumpMod2 <- function(input, output, server, session, tsf, vtm, currentPolygon, 
                      #polygonNames = currentPolygon$ECODISTRIC,
                      cl, 
                      ageClasses = ageClasses,
                      patchSize,
                      cacheRepo = paths$cachePath,
                      id, indivPolygonIndex,
                      largePatchesFn) {
  
  lastOne <- FALSE
  Clumps <- reactive({
    # Pre-run all patch sizes automatically.
    if (largePatchesFnLoop < (length(largePatchSizeOptions) )) {
      invalidateLater(50)
      largePatchesFnLoop <<- largePatchesFnLoop + 1
      patchSize <- as.integer(largePatchSizeOptions[largePatchesFnLoop])
    } else {
      patchSize <- as.integer(input$PatchSize33)
      lastOne <<- TRUE
    }
    
    message(paste("Running largePatchesFn for patch size:", patchSize))
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0, {
                   args <- list(largePatchesFn, timeSinceFireFiles = tsf,
                                vegTypeMapFiles = vtm,
                                cl = if (tryCatch(is(cl, "cluster"), error = function(x) FALSE)) cl,
                                polygonToSummarizeBy = currentPolygon,
                                #polygonToSummarizeBy = isolate(currentPolygon()),
                                ageClasses = ageClasses, patchSize = patchSize,
                                cacheRepo = cacheRepo)
                   args <- args[!unlist(lapply(args, is.null))]
                   largePatches <- do.call(Cache, args)
                   
                   
                   #largePatches <- do.call(Cache, args)
                   # largePatches <- Cache(largePatchesFn, timeSinceFireFiles = tsf,
                   #                       vegTypeMapFiles = vtm,
                   #                       cl = cl,
                   #                       polygonToSummarizeBy = currentPolygon,
                   #                       ageClasses = ageClasses, patchSize = patchSize,
                   #                       cacheRepo = cacheRepo)
                 setProgress(1)
    })
    message(paste("  Finished largePatchesFn for patch size:", patchSize))
    
    
    if(lastOne) {
      #updateTabItems(session = session, inputId = "TimeSinceFire", selected = TRUE)
    }
    
    largePatches
  })
  return(Clumps)
}

clumpMod <- function(input, output, server, Clumps, id, ageClasses, vegLeadingTypes) {
  output$h <- renderPlot({
    
    a <- Clumps()
    #browser()
    ids <- strsplit(id, split = "_")[[1]]
    i <- as.numeric(ids[1])
    j <- as.numeric(ids[2])
    k <- as.numeric(ids[3])
  
    # i is age
    # j is polygon index
    # k is Veg type
    indicesForHist <- grep(colnames(a), pattern = ageClasses[i], value = TRUE) %>% 
      agrep(pattern = vegLeadingTypes[k], value = TRUE)
    
    forHist <- unname(a[j,indicesForHist])

    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0, {
                   #browser()
                   actualPlot <- ggplot(data = data.frame(x = forHist), aes(x = x)) + 
                     stat_bin(aes(y=..density..),#bins = max(6, max(a)+2), 
                              fill="grey", colour="darkgrey", size = 1,
                              binwidth = 1) + 
                     xlab("") + #xlab("Proportion of polygon") + 
                     xlim(-1,max(6,max(a)+1)) +
                     #ggthemes::theme_fivethirtyeight() +
                     #ggthemes::scale_color_fivethirtyeight() +
                     theme_bw() + 
                     theme(text = element_text(size = 16)) + 
                     ylab("Proportion of landscape")
                   setProgress(1)
    })
    
    actualPlot
  })
}

leafletMapUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = 12, 
        solidHeader = TRUE, collapsible = TRUE, 
        title = "Area covered by this demo (in red), within the LandWeb study area (blue)",
        leafletOutput(ns("leafletMap1"), height = 600),
        selectInput(ns("leafletMapPolygons"), "Other layers to show summaries with", 
                    choices = names(polygons[1:(length(polygons)/4)+(length(polygons)/4)*3]), 
                    selected = names(polygons[1:(length(polygons)/4)+(length(polygons)/4)*3])[[1]])
    )
  )
}

leafletMap <- function(input, output, session) {
  output$leafletMap1 <- renderLeaflet({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0, {
                   polyNum <- polygonInput()
                   polyDemo <- polygons[[polyNum + (length(polygons)/4)*3]]#6]] # leaflet projection, DEMO scale
                   polyFull <- polygons[[polyNum + (length(polygons)/4)*2]] # leaflet projection, Full scale
                   a <- leaflet() %>% addTiles(group = "OSM (default)") %>%
                     addPolygons(data = polyFull, color = "blue", group = "Full",
                                 fillOpacity = 0.2, weight = 1,
                                 popup = paste(polyFull[[polygonIndivIdsColum[[polyNum]]]])) %>%
                     addPolygons(data = polyDemo, color = "red", group = "Demo",
                                 fillOpacity = 0.6, weight = 3,
                                 popup = paste(polyDemo[[polygonIndivIdsColum[[polyNum]]]]))  %>%
                     setView(mean(c(xmin(polyDemo),xmax(polyDemo))), 
                             mean(c(ymin(polyDemo),ymax(polyDemo))), 
                             zoom = 5) 
                   setProgress(1)
    })
    
    a
  })
  
  polygonInput <- reactive({
    switch(input$leafletMapPolygons,
           "Ecodistricts Demo" = 1#, 
           #"Alberta FMUs Demo" = 2
    )
  })
  
  return(polygonInput)
}

timeSinceFireMod <- function(input, output, session, rasts) {
  output$timeSinceFire1 <- renderLeaflet({
    ras1 <- rasterInput()
    pol <- polygons[[(length(polygons)/4)*4]]
    leafZoom <- if(is.null(input$timeSinceFire1_zoom)) 7 else input$timeSinceFire1_zoom
    a <- leaflet() %>% addTiles(group = "OSM (default)") %>%
      addRasterImage(x = ras1, group = "timeSinceFireRasts", opacity = 0.7, 
                     colors = timeSinceFirePalette, project = FALSE)  %>%
      addPolygons(data = pol, fillOpacity = 0, weight = 1) %>%
      addLegend(position = "bottomright", pal = timeSinceFirePalette, 
                values = na.omit(ras1[]), title = "Time since fire (years)") %>%
      addLayersControl(options = layersControlOptions(autoZIndex = TRUE)) %>%
     # setView(-117.8, 58.7, zoom = 7) 
     setView(mean(c(xmin(pol),xmax(pol))), 
             mean(c(ymin(pol),ymax(pol))), 
             zoom = leafZoom
             ) 
    
    a
  })
  
  rasterInput <- reactive({
    r <- rasts[[input$timeSinceFire1Slider]]
    if (ncell(r) > 2e5)
      r <- sampleRegular(r, size = 2e5, asRaster = TRUE)
    r
  })
}

timeSinceFireModUI <- function(id, tsf) {
  ns <- NS(id)
  tagList(
    box(width = 12, solidHeader = TRUE, collapsible = TRUE, 
        #title = "Time Since Fire maps",
        h4(paste("Below are a sequence of snapshots of the landscape, showing the natural range of",
                 "variation in time since fire. Click on the 'play' button at the bottom right to animate")),
        leafletOutput(ns("timeSinceFire1"), height = 600),
        sliderInput(ns("timeSinceFire1Slider"), 
                    "Individual snapshots of time since fire maps. Use play button (bottom right) to animate.", 
                    min = 1, max = length(tsf), value = 1, step = 1, 
                    animate = animationOptions(interval = 2500, loop = TRUE))
    )
  )
}

### simulation overview diagrams
simInfoUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    tabBox(width = 12,
      tabPanel("Module Diagram", simModuleDiagramUI(ns("moduleDiagram"))),
      tabPanel("Object Diagram", simObjectDiagramUI(ns("objectDiagram"))),
      tabPanel("Event Diagram", simEventDiagramUI(ns("eventDiagram")))
    )
  )
}

simInfo <- function(input, output, session, sim) {
  callModule(simModuleDiagram, "moduleDiagram", sim)
  callModule(simObjectDiagram, "objectDiagram", sim)
  callModule(simEventDiagram, "eventDiagram", sim)
}

simModuleDiagramUI <- function(id) {
  ns <- NS(id)
  
  ui_output <- tagList()
  
  ui_output$diagramTitle <- h3("Dependency graph (simplified)")
  
  ui_output$diagramDescription <- p(paste(
    "A network diagram illustrating the simplified module",
    "dependencies of a simulation.",
    "Arrows between modules indicate at least one data object",
    "passed from one module to the other."))
  
  ui_output$diagram <- imageOutput(ns("modDiag"), height = 750)
  
  return(ui_output)
}

simModuleDiagram <- function(input, output, session, sim) {
  output$modDiag <- renderPlot({
    moduleDiagram(sim, vertex.size = 30)
  })
}

simObjectDiagramUI <- function(id) {
  ns <- NS(id)
  
  ui_output <- tagList()
  
  ui_output$title <- h3("Summary of the data objects shared among modules.")
  
  ui_output$description <- p(paste(
    "A sequence diagram illustrating the data object dependencies",
    "of a simulation.",
    "Arrows between modules indicate at least one data object",
    "passed from one module to the other."
  ))
  
  ui_output$diagram <- DiagrammeR::DiagrammeROutput(ns("objectDiagram"), height = 1500)
  
  return(ui_output)
}

simObjectDiagram <- function(input, output, session, sim) {
  output$objectDiagram <- DiagrammeR::renderDiagrammeR({
    objectDiagram(sim)
  })
}

simEventDiagramUI <- function(id) {
  ns <- NS(id)
  
  out <- tagList()
  
  out$title <-  h3("Summary of the simulation event sequence.")
  
  out$description <- p(paste(
    "Simulation time is presented on the x-axis.",
    "Each module appears in a color-coded row,",
    "within which each event for that module is displayed",
    "corresponding to the sequence of events for that module.",
    "Note that only the start time of the event is meaningful is",
    "this figure:",
    "the width of the bar associated with a particular module's",
    "event DOES NOT correspond to an event's 'duration'."
  ))
  
  out$diagram <- DiagrammeR::DiagrammeROutput(ns("eventDiagram"), height = 1500)

  return(out)
}

simEventDiagram <- function(input, output, session, sim) {
  output$eventDiagram <- DiagrammeR::renderDiagrammeR({
    eventDiagram(sim)
  })
}

### detailed module info
moduleInfoUI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("allModuleInfo"))
}

moduleInfo <- function(input, output, session, sim) {
  output$allModuleInfo <- renderUI({
    fluidRow(
      tagList(lapply(modules(sim), function(module) {
        m <- slot(depends(sim), "dependencies")[[module]]
        rmdFile <- file.path("m", module, paste0(module, ".Rmd"))
        box(title = module, width = 12, status = "success", collapsible = TRUE,
            div(
              p(paste("Description:", slot(m, "description"))),
              p(paste("Keywords:", paste(slot(m, "keywords"), collapse = ", "))),
              p(paste("Authors:", paste(slot(m, "authors"), collapse = "; "))),
              p(paste("Version:", slot(m, "version"))),
              p("Documentation:", actionLink(paste0(module, "_Rmd"), "Rmd"))
              ## TO DO: add more metadata as required
            ),
            bsModal(module, basename(rmdFile),
                    trigger = paste0(module, "_Rmd"), size = "large",
                    includeMarkdown(rmdFile))
        )
      }))
    )
  })
}

### footers
copyrightFooter <- function() {
  copyrightInfo <- paste(
    shiny::icon("copyright",  lib = "font-awesome"), "Copyright ",
    format(Sys.time(), "%Y"),
    paste("Her Majesty the Queen in Right of Canada,",
          "as represented by the Minister of Natural Resources Canada.")
  )
  
  HTML(paste(
    "<footer>", "<div id=\"copyright\">", copyrightInfo, "</div>", "</footer>"
  ))
}

sidebarFooter <- function() {
  HTML(paste(
    "<footer>",
    "<div id=\"sidebar\">",
    "Powered by <a href=\"http://SpaDES.PredictiveEcology.org\", target=\"_blank\">\u2660 SpaDES</a> ",
    "and <a href=\"http://shiny.rstudio.com/\", target=\"_blank\">shiny</a>",
    "</div>",
    "</footer>"
  ))
}
