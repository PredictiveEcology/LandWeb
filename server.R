function(input, output, session) {
  sessionStartTime <- Sys.time()
  
  session$onSessionEnded(function() {
    if(needWorking) {
      checkoutDev(checkoutCondition) # put git back to Development branch
      .libPaths(origLibPaths) # get out of checkpoint
    }
    
    if(TRUE)
      if(Sys.info()["nodename"]=="W-VIC-A105388") {
        keepCache(mySim, after = appStartTime)
        if(rsyncToAWS)
          system(paste0("rsync -ruv --exclude '.git' --exclude '.Rproj.user' --exclude '.checkpoint' --delete -e 'ssh -i ",
                        path.expand('~'),
                        "/.ssh/laptopTesting.pem' ~/Documents/GitHub/LandWeb/ emcintir@ec2-52-26-180-235.us-west-2.compute.amazonaws.com:/srv/shiny-server/Demo/"))
      }
    
    message("The app started at ", appStartTime)
    message("The session started at ", sessionStartTime)
  })
  
  if(needWorking) {
    keepArtifacts <<- unique(showCache(paths$cachePath, after = startCacheTime)$artifact)
    archivist::addTagsRepo(keepArtifacts,
                           repoDir = paths$cachePath,
                           tags = paste0("LandWebVersion:", LandWebVersion))
  }
  #react <- reactiveValues()
  seed <- sample(1e8,1)
  set.seed(seed)
  message("Current seed is: ", seed)
  spadesAndExperiment <- function(mySim, experimentReps) {
    
    if (TRUE) {
      # # Do an initial run for each given study area so that all the data prep can be done once only
      #initialRun1 <- spades(Copy(mySim), debug = TRUE)
      # 5 minutes for 6e3 km2
      # 30 minutes for 6e4 km2
      mySimCopy <- Copy(mySim)
      end(mySimCopy) <- 0
      message("Running Initial spades call")
      initialRun <<- Cache(spades, sim = mySimCopy, #notOlderThan = Sys.time(),
                           debug = "paste(Sys.time(), paste(unname(current(sim)), collapse = ' '))", 
                           objects = "shpStudyRegion", 
                           #cacheRepo = cachePath(mySim), 
                           .plotInitialTime = NA, 
                           omitArgs = c("debug", ".plotInitialTime"),
                           debugCache="complete")
    }
    
    ##########
    raster::endCluster()
    seed <- sample(1e8,1)
    #seed <- 792282
    set.seed(seed)
    message("Current seed is: ", seed)
    #startTime <<- st <<- Sys.time()
    message("Running Experiment, starting at time: ", appStartTime)
    objectsToHash <- grep("useParallel", ls(mySim@.envir, all.names = TRUE), value=TRUE, invert=TRUE)
    args <- list(experiment, mySim, replicates = experimentReps, 
                 objects = objectsToHash,
                 debug = "paste(Sys.time(), format(Sys.time() - appStartTime, digits = 2), 
                 paste(unname(current(sim)), collapse = ' '))",#,
                 # {lsObj <- ls(envir=sim@.envir); keep <- 1:1; a <- format(big.mark = ',',
                 #           sort(unlist(lapply(lsObj, function(x) object.size(get(x, envir=sim@.envir)))) %>%
                 #           setNames(lsObj), decreasing = TRUE))[keep];
                 #           paste(names(a)[keep], collapse=' ')},
                 # paste(a[keep], collapse=' '),
                 # 'NROW cohortData:', NROW(sim$cohortData), 'Num PixelGroups: ',
                 # uniqueN(sim$cohortData,by='pixelGroup'), 'PixelGroups:ncell:',
                 # round(uniqueN(sim$cohortData,by='pixelGroup')/ncell(sim$pixelGroupMap),4),
                 # {st <<- Sys.time()})",
                 # debug = "paste(paste(unname(current(sim)), collapse = ' '), 'is sim$useParallel a cluster:', 
                 #          is(sim$useParallel, 'cluster'))", #cache = TRUE, 
                 #cl = if(exists("cl")) cl, 
                 .plotInitialTime = NA,
                 #notOlderThan = Sys.time(), # uncomment if want to rerun without Cached copy
                 clearSimEnv = TRUE,
                 debugCache="complete",
                 omitArgs = c("debug", ".plotInitialTime"))
    args <- args[!unlist(lapply(args, is.null))]
    #profvis::profvis(interval = 0.5, {mySimOut <- do.call(Cache, args)})
    mySimOut <- do.call(Cache, args)
    message(attr(mySimOut, "tags"))
    mySimOut
  }
  
  objectsToHash <- grep("useParallel", ls(mySim@.envir, all.names=TRUE), value=TRUE, invert=TRUE)
  mySimOut <<- Cache(spadesAndExperiment, mySim, experimentReps, debugCache = "complete",
                     objects = objectsToHash)
  
  callModule(simInfo, "simInfoTabs", mySimOut[[1]])
  callModule(moduleInfo, "modInfoBoxes", mySimOut[[1]])
  
  
  if(needWorking) {
    keepArtifacts3 <- unique(showCache(paths$cachePath, after = startCacheTime)$artifact)
    keepArtifacts <<- setdiff(keepArtifacts3, keepArtifacts)
    archivist::addTagsRepo(keepArtifacts,
                           repoDir = paths$cachePath,
                           tags = paste0("LandWebVersion:", LandWebVersion))
  }
  
  # mySimOut <- Cache(experiment, mySim, replicates = experimentReps, debug = TRUE, cache = TRUE,
  #                   #cl = cl,
  #                   .plotInitialTime = NA,
  #                   clearSimEnv = TRUE#,
  #                   #notOlderThan = Sys.time()
  # )
  message("  Finished Experiment")
  
  filesFromOutputs <- lapply(seq_along(mySimOut), function(x) {
    outputs(mySimOut[[x]])$file
  })
  
  #if(any(!file.exists(unlist(filesFromOutputs)))) {
  for(simNum in seq_along(mySimOut)) {
    mySimOut[[simNum]]@outputs$file <- 
      
      lapply(strsplit(outputs(mySimOut[[simNum]])$file, split = paste0(outputPath(mySimOut[[simNum]]),"[\\/]+")), function(f) {
        f[[2]]
      }) %>%
      unlist() %>%
      file.path(paths$outputPath, .)
  }
  #}
  
  rastersFromOutputs <- lapply(seq_along(mySimOut), function(x) {
    grep(pattern = ".grd$|.tif$", outputs(mySimOut[[x]])$file, value = TRUE)
  })
  
  rastersFromOutputs <- unlist(rastersFromOutputs)
  tsf <- grep(pattern = "rstTimeSinceFire", rastersFromOutputs, value = TRUE)
  vtm <- grep(pattern = "vegTypeMap", rastersFromOutputs, value = TRUE)
  lenTSF <- length(tsf)
  rasterResolution <<- raster(tsf[1]) %>% res()
  
  lfltFN <- gsub(tsf, pattern = ".grd$|.tif$", replacement = "LFLT.tif")
  #lfltFN <- gsub(lfltFN, pattern = ".grd", replacement = ".tif")
  
  #if (!(length(globalRasters) == length(tsf))) {
  globalRasters <<- Cache(reprojectRasts, lapply(tsf, asPath), digestPathContent = TRUE,
                          lfltFN, sp::CRS(lflt), end(mySim), cacheRepo = paths$cachePath,
                          flammableFile = asPath(file.path(paths$outputPath, "rstFlammable.grd")))
  
  message("Running leadingByStage")
  args <- list(leadingByStage, tsf, vtm, 
               polygonToSummarizeBy = ecodistricts,
               #polygonNames = ecodistricts$ECODISTRIC, 
               cl = if(exists("cl")) cl, 
               omitArgs = "cl",
               ageClasses = ageClasses, cacheRepo = paths$cachePath)
  args <- args[!unlist(lapply(args, is.null))]
  leading <- do.call(Cache, args)
  message("  Finished leadingByStage")
  
  if(needWorking) {
    keepArtifacts3 <- unique(showCache(paths$cachePath, after = startCacheTime)$artifact)
    keepArtifacts <<- setdiff(keepArtifacts3, keepArtifacts)
    archivist::addTagsRepo(keepArtifacts,
                           repoDir = paths$cachePath,
                           tags = paste0("LandWebVersion:", LandWebVersion))
  }
  
  # Large patches
  polygonsWithData <- leading[,unique(polygonNum[!is.na(proportion)]),by=ageClass]
  
  vegLeadingTypes <- c(unique(leading$vegType))
  vegLeadingTypesWithAllSpecies <- c(vegLeadingTypes, "All species")
  
  message("  Finished global.R")
  
  # Large patch size section, i.e., clumps
  observe({
    lapply(ageClasses, function(ageClass) {
      output[[paste0("Clumps",ageClass,"UI")]] <- renderUI({
        ns <- session$ns
        ageClassIndex <- pmatch(ageClass, ageClasses)
        polygonIDs <- seq_along(ecodistricts)[polygonsWithData[ageClass==ageClasses[ageClassIndex]]$V1]
        
        myTabs <- lapply(polygonIDs, function(polygonID) {
          tabPanel(paste(availablePolygonAdjective,ecodistricts$ECODISTRIC[polygonID]), 
                   tabName = paste0("Poly",ageClass,polygonID),
                   fluidRow(
                     lapply(seq_along(vegLeadingTypesWithAllSpecies), function(k) {
                       clumpModOutput(paste0(ageClassIndex, "_", polygonID, "_", k, "_clumps"),
                                      vegLeadingTypes = vegLeadingTypesWithAllSpecies)
                     })
                   )
          )               
        })
        fluidPage(
          column(width = 12, h2("NRV of number of 'large' (",strong(ClumpsReturn()$patchSize," hectares"),"), ",
                                strong(tolower(ageClasses[ageClassIndex]))," patches")),
          column(width = 12, h4("These figures show the NRV of the probability distribution",
                                "of ",tolower(ageClasses[ageClassIndex])," patches that are",ClumpsReturn()$patchSize," hectares",
                                "or larger, for each given combination of Age Class, ",
                                "Leading Vegetation, and Polygon.",
                                "To change the patch size that defines these, type a new value",
                                "in the menu at the left.")
          ),
          do.call(tabsetPanel, myTabs)
        )
      })
    })
  })
  
  observe({
    lapply(ageClasses, function(ageClass) {
      output[[paste0(ageClass,"UI")]] <- renderUI({
        ageClassIndex <- pmatch(ageClass, ageClasses)
        polygonIDs <- seq_along(ecodistricts)[polygonsWithData[ageClass==ageClasses[ageClassIndex]]$V1]
        
        myTabs <- lapply(polygonIDs, function(polygonID) {
          tabPanel(paste(availablePolygonAdjective,ecodistricts$ECODISTRIC[polygonID]), 
                   tabName = paste0("Poly",ageClass,polygonID),
                   fluidRow(
                     lapply(seq_along(vegLeadingTypes), function(k) {
                       vegAgeModUI(paste0(ageClassIndex, "_", polygonID, "_", k),
                                   vegLeadingTypes = vegLeadingTypes)
                     })
                   )
          )               
        })
        fluidPage(
          column(width = 12, h2("NRV of ",strong(tolower(ageClasses[ageClassIndex])), "(",ageClassZones[ageClassIndex],"years )",
                                " forest, by leading vegetation")),
          column(width = 12, h4("These figures show the NRV of the proportion of",strong(tolower(ageClasses[ageClassIndex])),
                                "forests each polygon that are in each leading vegetation type, as labeled in the plots.",
                                "The proportions are proportions",em("within"),"age class. In any given",
                                "replicate, the numbers below sum to 1."
          )),
          do.call(tabsetPanel, myTabs)
        )
      })
    })
  })
  
  
  currentPolygon <- callModule(leafletMap, "leafletMap")
  # returns a number 1 or 2 ... up to number of unique polygons in dataset
  
  output$timeSinceFireUI <- renderUI({
    tabBox(width = 12,
           tabPanel("Time Since Fire maps", tabName = "timeSinceFireTab",
                    fluidRow(timeSinceFireModUI("timeSinceFire", tsf = tsf))
           )
    )
  })
  
  output$studyRegionUI <- renderUI({
    tabBox(width = 12,
           tabPanel("Time Since Fire maps", tabName = "timeSinceFireTab",
                    fluidRow(studyRegionModUI("studyRegion"))
           )
    )
  })
  
  callModule(timeSinceFireMod, "timeSinceFire", rasts = globalRasters)
  
  args <- list(clumpMod2, "id1", session = session, 
               currentPolygon = polygons[[1 + length(polygons)/4]], 
               tsf = tsf, vtm = vtm,
               cl = if(exists("cl")) cl, 
               ageClasses = ageClasses, cacheRepo = paths$cachePath,
               patchSize = reactive({input$PatchSize33}),
               largePatchesFn = largePatchesFn)
  args <- args[!unlist(lapply(args, is.null))]
  ClumpsReturn <- do.call(callModule, args )
  
  lapply(seq_along(ageClasses), function(ageClassIndex) { # ageClassIndex is age
    lapply(polygonsWithData[ageClass==ageClasses[ageClassIndex]]$V1, function(j) { # j is polygon index
      lapply(seq_along(vegLeadingTypesWithAllSpecies), function(k) { # k is Veg type
        callModule(clumpMod, paste0(ageClassIndex, "_", j, "_", k, "_clumps"),
                   Clumps = reactive({ClumpsReturn()$Clumps}),
                   id = paste0(ageClassIndex, "_", j, "_", k, "_clumps"),
                   ageClasses = ageClasses,
                   vegLeadingTypes = vegLeadingTypesWithAllSpecies,
                   numReps = lenTSF
        )  
      })
    })
  })
  
  lapply(seq_along(ageClasses), function(ageClassIndex) {
    #  ageClassIndex <- match(ages, seq_along(ageClasses))
    lapply(polygonsWithData[ageClass==ageClasses[ageClassIndex]]$V1, function(j) {
      lapply(seq_along(vegLeadingTypes), function(k) {
        callModule(vegAgeMod, paste0(ageClassIndex, "_", j, "_", k), 
                   listOfProportions = leading[ageClass==ageClasses[ageClassIndex] & 
                                                 polygonNum==j & 
                                                 vegType==vegLeadingTypes[k]]$proportion
        )
      })
    })
  })
  
  
  # There is double code here, allowing for ggplotly version (currently commented out) and ggvis version
  #output$StudyRegion <- renderPlot(
  if(FALSE) {
    bind_shiny(plot_id = "StudyRegion",
               #height = 525, 
               #width = 800,
               #  {
               #ggplotly(ggStudyRegion )
               ggStudyRegion 
               
               #   g[[jj]]  <- ggplot() +
               #     geom_raster(data=packSaturationMapPts[[jj]],
               #                 aes(long, lat, fill=Probability, text=CountryName),
               #                 alpha = 0.98) +
               #     geom_polygon(data=shape.fort, 
               #                  aes(long, lat, group=group), 
               #                  colour='black',fill=NA) +
               #     coord_equal() +
               #     scale_fill_gradientn(colors=c('blue','blue','light blue', 'light blue','orange','red'),
               #                          #scale_fill_gradientn(colors=c('#ffffd4','#fee391','#fec44f','#fe9929','#d95f0e','#993404'),
               #                          name='Probability of a pack',  na.value = NA) + 
               #     theme(
               #       legend.position = 'right',
               #       legend.key.size = unit(1, "cm")
               #     ) +
               #     scale_x_continuous(expand=c(0,0)) + 
               #     scale_y_continuous(expand=c(0,0)) +
               #     labs(x='Easting', y='Northing', title=paste("Probability of pack presence in 2097, based on",
               #                                                 length(numPacks2AByTime[[jj]]), "replicate simulations, \n",
               #                                                 "with", jj, "starting locations"))
               #   
               #   #g1 <- g
               #}
               # gridExtra::grid.arrange(grobs = g)
               #ggplotly(g, tooltip = c("CountryName", "Probability"))
               
               #clearPlot()
               #Plot(shpStudyRegionOrig, title = "Proportion of leading spruce")
               #Plot(shpStudyRegion, addTo = "shpStudyRegionOrig")
               #}
    )
  }
  
  output$speciesInputs <- renderDataTable({
    landisInputs
  })#, digits = 1)
  
  output$speciesEcoregionInputs <- renderDataTable({
    spEcoReg
  })#, digits = 1)

  Cache(workingShas, cacheRepo = paths$cachePath)#, userTags = "workingShas")
}
