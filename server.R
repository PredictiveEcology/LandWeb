function(input, output, session) {
  
  #react <- reactiveValues()
  
  if (TRUE) {
      # # Do an initial run for each given study area so that all the data prep can be done once only
      #initialRun1 <- spades(Copy(mySim), debug = TRUE)
      # 5 minutes for 6e3 km2
      # 30 minutes for 6e4 km2
      mySimCopy <- Copy(mySim)
      end(mySimCopy) <- 1
      message("Running Initial spades call")
      initialRun <- Cache(spades, sim = mySimCopy, debug = TRUE, objects = "shpStudyRegion", 
                          cacheRepo = file.path(cachePath(mySim), "studyRegion"), .plotInitialTime = NA)
      # try(silent = TRUE, {
      #   filesPresent <- dir(unique(dirname(outputs(initialRun)$file)))
      #   filesPresentFull <- dir(unique(dirname(outputs(initialRun)$file)), full.names = TRUE)
      #   filesToRemove <- unlist(lapply(strsplit(basename(outputs(initialRun)$file), split = "\\."), function(x) x[1])) %>%
      #     lapply(function(y) grep(filesPresent, pattern = y)) %>%
      #     unlist()
      #   file.remove(filesPresentFull[filesToRemove])
      # })
  }
  
  callModule(simInfo, "simInfoTabs", initialRun)
  callModule(moduleInfo, "modInfoBoxes", initialRun)
  
  message("Running Experiment")
  args <- list(experiment, mySim, replicates = experimentReps, debug = TRUE, #cache = TRUE, 
               cl = if(exists("cl")) cl, 
               .plotInitialTime = NA,
               #notOlderThan = Sys.time(),
               clearSimEnv = TRUE)
  args <- args[!unlist(lapply(args, is.null))]
  mySimOut <- do.call(Cache, args)
  message(attr(mySimOut, "tags"))
  
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
  
 # browser()
  if(any(!file.exists(unlist(filesFromOutputs)))) {
    for(simNum in seq_along(mySimOut)) {
      mySimOut[[simNum]]@outputs$file <- 
      
        lapply(strsplit(outputs(mySimOut[[simNum]])$file, split = paste0(outputPath(mySimOut[[simNum]]),"[\\/]+")), function(f) {
          f[[2]]
        }) %>%
          unlist() %>%
          file.path(paths$outputPath, .)
    }
  }
  
  rastersFromOutputs <- lapply(seq_along(mySimOut), function(x) {
    grep(pattern = ".grd$|.tif$", outputs(mySimOut[[x]])$file, value = TRUE)
  })
  
  rastersFromOutputs <- unlist(rastersFromOutputs)
  tsf <- grep(pattern = "rstTimeSinceFire", rastersFromOutputs, value = TRUE)
  vtm <- grep(pattern = "vegTypeMap", rastersFromOutputs, value = TRUE)
  lenTSF <- length(tsf)
  
  lfltFN <- gsub(tsf, pattern = ".grd$|.tif$", replacement = "LFLT.tif")
  #lfltFN <- gsub(lfltFN, pattern = ".grd", replacement = ".tif")
  
  if (!(length(globalRasters) == length(tsf))) {
    message("Reprojecting rasters & loading into RAM")
    globalRasters <<- lapply(seq_along(tsf), function(FN) {
      if (file.exists(lfltFN[FN])) {
        r <- raster(lfltFN[FN])
      } else {
        r <- raster(tsf[FN])
        r <- projectRaster(r, crs = sp::CRS(lflt), method = "ngb",
                           filename = lfltFN[FN], overwrite = TRUE,
                           datatype = "INT2U")
      }
      if ((ncell(r) < 5e5) & (length(tsf) < 30)) r[] <- r[] 
      r
    })
    message("  Finished reprojecting rasters & loading into RAM")
  } 
  
  message("Running leadingByStage")
  args <- list(leadingByStage, tsf, vtm, 
               polygonToSummarizeBy = ecodistricts,
               #polygonNames = ecodistricts$ECODISTRIC, 
               cl = if(exists("cl")) cl, 
               ageClasses = ageClasses, cacheRepo = paths$cachePath)
  args <- args[!unlist(lapply(args, is.null))]
  leading <- do.call(Cache, args)
  message("  Finished leadingByStage")
  
  # Large patches
  SpaDES::checkPath(file.path(paths$cachePath, "scratch"), create = TRUE)

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
  
  
  output$speciesInputs <- renderDataTable({
    landisInputs
  })#, digits = 1)
  
  output$speciesEcoregionInputs <- renderDataTable({
    spEcoReg
  })#, digits = 1)
  
  
}
