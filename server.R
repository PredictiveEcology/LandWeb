function(input, output, session) {
  
  #react <- reactiveValues()
  
  if (TRUE) {
      # # Do an initial run for each given study area so that all the data prep can be done once only
      #initialRun1 <- spades(Copy(mySim), debug = TRUE)
      # 5 minutes for 6e3 km2
      # 30 minutes for 6e4 km2
      message("Running Initial spades call")
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
      
  }
  
  callModule(simInfo, "simInfoTabs", initialRun)
  callModule(moduleInfo, "modInfoBoxes", initialRun)
  
  message("Running Experiment")
  mySimOut <- Cache(experiment, mySim, replicates = 3, debug = TRUE, cache = TRUE, 
                    #cl = cl, 
                    .plotInitialTime = NA,
                    clearSimEnv = TRUE#, 
                    #notOlderThan = Sys.time()
  )
  message("  Finished Experiment")
  
  grds <- unlist(lapply(seq_along(mySimOut), function(x) {
    grep(pattern = ".grd$", outputs(mySimOut[[x]])$file, value = TRUE)
  }))
  
  tsf <- grep(pattern = "rstTimeSinceFire", grds, value = TRUE)
  vtm <- grep(pattern = "vegTypeMap", grds, value = TRUE)
  
  message("Reprojecting rasters")
  lfltFN <- gsub(tsf, pattern = ".grd", replacement = "LFLT.grd")
  lfltFN <- gsub(lfltFN, pattern = ".grd", replacement = ".tif")
  
  rasts <- lapply(seq_along(tsf), function(FN) {
    if (file.exists(lfltFN[FN])) {
      r <- raster(lfltFN[FN])
    } else {
     r <- raster(tsf[FN])
     r <- projectRaster(r, crs = sp::CRS(lflt), method = "ngb",
                        filename = lfltFN[FN], overwrite = TRUE,
                        datatype = "INT1U")
    }
    if ((ncell(r) < 5e5) & (length(tsf) < 30)) r[] <- r[] 
    r
  })
  message("  Finished reprojecting rasters")

  leadingByStage <- function(timeSinceFireFiles, vegTypeMapFiles,
                             polygonToSummarizeBy, polygonNames, 
                             ageCutoffs = c(0, 40, 80, 120),  ageClasses, cl) {
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
                      leadingRast[timeSinceFireFilesRast[] < ageCutoffs[y]] <- NA
                      if ((y + 1) < length(ageCutoffs))
                        leadingRast[timeSinceFireFilesRast[] >= ageCutoffs[y + 1]] <- NA
                      
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
  
  message("Running leadingByStage")
  leading <- Cache(leadingByStage, tsf, vtm, ecodistricts,
                   polygonNames = ecodistricts$ECODISTRIC, 
                   #cl=cl, 
                   ageClasses = ageClasses, cacheRepo = paths$cachePath)
  message("  Finished leadingByStage")
  
  # Large patches
  countNumPatches <- function(ras, patchSize, ...) {
    clumpedRas <- clump(ras, ...)
    freqTable <- data.table(freq(clumpedRas))[!is.na(value), ][
      , area := count * (res(clumpedRas)[1] ^ 2) / 10000]
    largeEnoughPatches <- freqTable[area >= patchSize, ][, newValue := as.numeric(as.factor(value))]
    clumpedRas[!(clumpedRas %in% largeEnoughPatches$value)] <- NA
    list(ras = clumpedRas, count = largeEnoughPatches)
  }
  
  largePatchesFn <- function(timeSinceFireFiles, vegTypeMapFiles,
                             polygonToSummarizeBy, #polygonNames, 
                             ageCutoffs = c(0, 40, 80, 120), patchSize = 1000, 
                             ageClasses, cl, notOlderThan = Sys.time() - 1e7) {
    if (missing(cl)) {
      lapplyFn <- "lapply"
    } else {
      lapplyFn <- "parLapplyLB"
      clusterExport(cl = cl,
                    varlist = list("timeSinceFireFiles", "vegTypeMapFiles",
                                   "polygonToSummarizeBy", "countNumPatches",
                                   "paths", "ageCutoffs", "patchSize", "ageClasses"),
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
                      leadingRast[timeSinceFireFilesRast[] < ageCutoffs[y]] <- NA
                      if ((y + 1) < length(ageCutoffs))
                        leadingRast[timeSinceFireFilesRast[] >= ageCutoffs[y + 1]] <- NA
                      
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
      names(out1) <- paste(basename(dirname(timeSinceFireFiles)), basename(timeSinceFireFiles), sep = "_")
      out1
    }
    )
    names(out) <- ageClasses
    out
  }

  if (FALSE) {
    for (iii in largePatchSizeOptions[3]) {
      message("Running largePatches")
      largePatches <- Cache(largePatchesFn, timeSinceFireFiles = tsf, 
                            vegTypeMapFiles = vtm,
                            polygonToSummarizeBy = ecodistricts,
                            #polygonNames = ecodistricts$ECODISTRIC,
                            #cl=cl,
                            cacheRepo = paths$cachePath,
                            #notOlderThan = Sys.time(),
                            ageClasses = ageClasses, patchSize = as.integer(iii)
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
  
  message("  Finished global.R")

  output$ClumpsYoungUI <- renderUI({
    tabBox(width = 12,
      tabPanel("Young, Deciduous", tabName = "ClumpsYoung_Deciduous2",
        fluidRow(
          lapply(pmatch("Young", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[[i]]], function(j) {
              clumpModOutput(paste0(i, "_", j, "_", pmatch("Deciduous", vegLeadingTypes), "_clumps"),
                             vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Young, Spruce", tabName = "ClumpsYoung_Spruce2",
        fluidRow(
          lapply(pmatch("Young", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[[i]]], function(j) {
              clumpModOutput(paste0(i, "_", j, "_", pmatch("Spruce", vegLeadingTypes), "_clumps"),
                             vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Young, Mixed", tabName = "ClumpsYoung_Mixed2",
        fluidRow(
          lapply(pmatch("Young", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[[i]]], function(j) {
              clumpModOutput(paste0(i, "_", j, "_", pmatch("Mixed", vegLeadingTypes), "_clumps"),
                             vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      )
    )
  })
  
  output$ClumpsImmatureUI <- renderUI({
    tabBox(width = 12,
      tabPanel("Immature, Deciduous", tabName = "Immature_Deciduous2",
        fluidRow(
          lapply(pmatch("Immature", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[[i]]], function(j) {
              clumpModOutput(paste0(i, "_", j, "_", pmatch("Deciduous", vegLeadingTypes), "_clumps"),
                             vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Immature, Spruce", tabName = "Immature_Spruce2",
        fluidRow(
          lapply(pmatch("Immature", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[[i]]], function(j) {
              clumpModOutput(paste0(i, "_", j, "_", pmatch("Spruce", vegLeadingTypes), "_clumps"),
                             vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Immature, Mixed", tabName = "Immature_Mixed2",
        fluidRow(
          lapply(pmatch("Immature", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[[i]]], function(j) {
              clumpModOutput(paste0(i, "_", j, "_", pmatch("Mixed", vegLeadingTypes), "_clumps"),
                             vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      )
    )
  })
  
  output$ClumpsMatureUI <- renderUI({
    tabBox(width = 12,
      tabPanel("Mature, Deciduous", tabName = "Mature_Deciduous2",
        fluidRow(
          lapply(pmatch("Mature", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[[i]]], function(j) {
              clumpModOutput(paste0(i, "_", j, "_", pmatch("Deciduous", vegLeadingTypes), "_clumps"),
                             vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Mature, Spruce", tabName = "Mature_Spruce2",
        fluidRow(
          lapply(pmatch("Mature", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[[i]]], function(j) {
              clumpModOutput(paste0(i, "_", j, "_", pmatch("Spruce", vegLeadingTypes), "_clumps"),
                             vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Mature, Mixed", tabName = "Mature_Mixed2",
        fluidRow(
          lapply(pmatch("Mature", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[[i]]], function(j) {
              clumpModOutput(paste0(i, "_", j, "_", pmatch("Mixed", vegLeadingTypes), "_clumps"),
                             vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      )
    )
  })
  
  output$ClumpsOldUI <- renderUI({
    tabBox(width = 12,
      tabPanel("Old, Deciduous", tabName = "Old_Deciduous2",
        fluidRow(
          lapply(pmatch("Old", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[[i]]], function(j) {
              clumpModOutput(paste0(i, "_", j, "_", pmatch("Deciduous", vegLeadingTypes), "_clumps"),
                             vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Old, Spruce", tabName = "Old_Spruce2",
        fluidRow(
          lapply(pmatch("Old", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[[i]]], function(j) {
              clumpModOutput(paste0(i, "_", j, "_", pmatch("Spruce", vegLeadingTypes), "_clumps"),
                             vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Old, Mixed", tabName = "Old_Mixed2",
        fluidRow(
          lapply(pmatch("Old", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[[i]]], function(j) {
              clumpModOutput(paste0(i, "_", j, "_", pmatch("Mixed", vegLeadingTypes), "_clumps"),
                             vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      )
    )
  })
  
  output$YoungUI <- renderUI({
    tabBox(width = 12,
      tabPanel("Young, Deciduous", tabName = "Young_Deciduous",
        fluidRow(
          lapply(pmatch("Young", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[[i]]], function(j) {
              vegAgeModUI(paste0(i, "_", j, "_", pmatch("Deciduous", vegLeadingTypes)),
                          vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Young, Spruce", tabName = "Young_Spruce",
        fluidRow(
          lapply(pmatch("Young", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[[i]]], function(j) {
              vegAgeModUI(paste0(i, "_", j, "_", pmatch("Spruce", vegLeadingTypes)),
                          vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Young, Mixed", tabName = "Young_Mixed",
        fluidRow(
          lapply(pmatch("Young", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[[i]]], function(j) {
              vegAgeModUI(paste0(i, "_", j, "_", pmatch("Mixed", vegLeadingTypes)),
                          vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      )
    )
  })

  output$ImmatureUI <- renderUI({
    tabBox(width = 12,
      tabPanel("Immature, Deciduous", tabName = "Immature_Deciduous",
        fluidRow(
          lapply(pmatch("Immature", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[[i]]], function(j) {
              vegAgeModUI(paste0(i, "_", j, "_", pmatch("Deciduous", vegLeadingTypes)),
                          vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Immature, Spruce", tabName = "Immature_Spruce",
        fluidRow(
          lapply(pmatch("Immature", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[[i]]], function(j) {
              vegAgeModUI(paste0(i, "_", j, "_", pmatch("Spruce", vegLeadingTypes)),
                          vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Immature, Mixed", tabName = "Immature_Mixed",
        fluidRow(
          lapply(pmatch("Immature", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[[i]]], function(j) {
              vegAgeModUI(paste0(i, "_", j, "_", pmatch("Mixed", vegLeadingTypes)),
                          vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      )
    )
  })
  
  output$MatureUI <- renderUI({
    tabBox(width = 12,
      tabPanel("Mature, Deciduous", tabName = "Mature_Deciduous",
        fluidRow(
          lapply(pmatch("Mature", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[[i]]], function(j) {
              vegAgeModUI(paste0(i, "_", j, "_", pmatch("Deciduous", vegLeadingTypes)),
                          vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Mature, Spruce", tabName = "Mature_Spruce",
        fluidRow(
          lapply(pmatch("Mature", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[[i]]], function(j) {
              vegAgeModUI(paste0(i, "_", j, "_", pmatch("Spruce", vegLeadingTypes)),
                          vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Mature, Mixed", tabName = "Mature_Mixed",
        fluidRow(
          lapply(pmatch("Mature", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[[i]]], function(j) {
              vegAgeModUI(paste0(i, "_", j, "_", pmatch("Mixed", vegLeadingTypes)),
                          vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      )
    )
  })
  
  output$OldUI <- renderUI({
    tabBox(width = 12,
      tabPanel("Old, Deciduous", tabName = "Old_Deciduous",
        fluidRow(
          lapply(pmatch("Old", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[[i]]], function(j) {
              vegAgeModUI(paste0(i, "_", j, "_", pmatch("Deciduous", vegLeadingTypes)),
                          vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Old, Spruce", tabName = "Old_Spruce",
        fluidRow(
          lapply(pmatch("Old", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[[i]]], function(j) {
              vegAgeModUI(paste0(i, "_", j, "_", pmatch("Spruce", vegLeadingTypes)),
                          vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Old, Mixed", tabName = "Old_Mixed",
        fluidRow(
          lapply(pmatch("Old", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[[i]]], function(j) {
              vegAgeModUI(paste0(i, "_", j, "_", pmatch("Mixed", vegLeadingTypes)),
                          vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      )
    )
  })
  
  #patchSizeReact <- reactive({input$PatchSize33})
  
  currentPolygon <- callModule(leafletMap, "leafletMap",
                               ecodistrictsFullLFLT = ecodistrictsFullLFLT)
  # returns a number 1 or 2 ... up to number of unique polygons in dataset
  
  output$timeSinceFireUI <- renderUI({
    tabBox(width = 12,
         tabPanel("Time Since Fire maps", tabName = "timeSinceFireTab",
                  fluidRow(timeSinceFireModUI("timeSinceFire", tsf = tsf))
         )
    )
  })
  
  callModule(timeSinceFireMod, "timeSinceFire", rasts = rasts)
  
  Clumps <- callModule(clumpMod2, "id1", 
                       tsf = tsf, vtm = vtm, 
                       #currentPolygon = polygons[[reactive({currentPolygon()})+6]], 
                       currentPolygon = polygons[[1 + 2]], 
                       #polygonNames = ecodistricts$ECODISTRIC, 
                       #cl=cl, 
                       ageClasses = ageClasses, cacheRepo = paths$cachePath,
                       patchSize = reactive({input$patchSize33}),
                       largePatchesFn = largePatchesFn)
  
  lapply(seq_along(polygonsWithData), function(i) { # i is age
    lapply(polygonsWithData[[i]], function(j) {
      lapply(seq_along(vegLeadingTypes), function(k) {
        callModule(clumpMod,paste0(i, "_", j, "_", k, "_clumps"),
                   Clumps = reactive({Clumps()}),
                   id = paste0(i, "_", j, "_", k, "_clumps")
        )  
      })
    })
  })
  
  lapply(seq_along(polygonsWithData), function(i) {
    lapply(polygonsWithData[[i]], function(j) {
      lapply(seq_along(vegLeadingTypes), function(k) {
        callModule(vegAgeMod, paste0(i, "_", j, "_", k), indivPolygonIndex = j,
                   polygonLayer = ecodistricts,
                   listOfProportions = leading[[i]],
                   vegLeadingType = vegLeadingTypes[k]
        )
      })
    })
  })
  
  # lapply(seq_along(ecodistricts)[polygonsWithData[[1]]], function(i) {
  #   #callModule(decidOldMod,i,seed=i)
  #   callModule(decidOldMod,i,indivPolygonIndex=i,polygonLayer = ecodistricts, listOfProportions = leading[[1]])
  # })
  
  # lapply(seq_along(ecodistricts)[polygonsWithData[[2]]], function(i) {
  #   #callModule(decidOldMod,i,seed=i)
  #   callModule(decidOldMod,i,indivPolygonIndex=i,polygonLayer = ecodistricts, listOfProportions = leading[[2]])
  # })
  # 
  # lapply(seq_along(ecodistricts)[polygonsWithData[[3]]], function(i) {
  #   #callModule(decidOldMod,i,seed=i)
  #   callModule(decidOldMod,i,indivPolygonIndex=i,polygonLayer = ecodistricts, listOfProportions = leading[[3]])
  # })
  # 
  # lapply(seq_along(ecodistricts)[polygonsWithData[[4]]], function(i) {
  #   #callModule(decidOldMod,i,seed=i)
  #   callModule(decidOldMod,i,indivPolygonIndex=i,polygonLayer = ecodistricts, listOfProportions = leading[[4]])
  # })
  
  # lapply(seq_along(ecodistricts), function(i) {
  #   callModule(oldDecidByPolyMod,i,indivPolygonIndex=i,polygonLayer = ecodistricts, listOfProportions = leadingOld)
  # })
  
  
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
  
  # output$initialCommunityMap <- renderPlot({
  #   clearPlot()
  #   Plot(deciduous, addTo = "deciduous", title = "Proportion deciduous leading")
  # })
  # 
  # output$seralStagePctCoverMap <- renderPlot({
  #   clearPlot()
  #   Plot(mixed, addTo = "mixed", title = "Proportion mixed leading")
  # })
  
  # output$ecoregionFig <- renderPlot({
  #   #zoom <- input$inputZoom
  #   ext <- extent(ecoregionMap)
  #   # halfRangex <- diff(range(ext[1],ext[2]))/2
  #   # halfRangey <- diff(range(ext[3],ext[4]))/2
  #   # extNew <- numeric(4)
  #   # extNew[1] <- ext[1]+halfRangex  - halfRangex/zoom
  #   # extNew[2] <- ext[1]+halfRangex  + halfRangex/zoom
  #   # extNew[3] <- ext[3]+halfRangey  - halfRangey/zoom
  #   # extNew[4] <- ext[3]+halfRangey  + halfRangey/zoom
  #   react$newExtent <- extent(ext)
  #   ecoregionMapNew <- crop(ecoregionMap, react$newExtent)
  #   Plot(ecoregionMapNew, title = FALSE, new = TRUE, speedup = 10)
  # })
  
  # output$initialCommunityMap <- renderPlot({
  #   initialCommunityMapNew <- crop(initialCommunityMap, react$newExtent)
  #   Plot(initialCommunityMapNew, title = FALSE, new = TRUE, speedup = 10)
  # })
  
  output$speciesInputs <- renderDataTable({
    landisInputs
  })#, digits = 1)
  
  output$speciesEcoregionInputs <- renderDataTable({
    spEcoReg
  })#, digits = 1)
  
  
  # output$seralStagePctCoverMap <- renderPlot({
  #   browser()
  #   if(is.null(input$regionSelector)) regionNum <- 139 else regionNum <- as.numeric(input$regionSelector)
  #   num <- match(regionNum, availableRegions)
  #   toMap <- get(paste0("seralMapExampleRegion",num))
  #   Plot(toMap, title = FALSE, new=TRUE, speedup = 10, cols = rev(c("forestgreen", "olivedrab3",
  #                                                                   "goldenrod3", "grey")))
  # })
  # 
  # output$seralStagePctCover <- renderPlot({
  #   if(is.null(input$regionSelector)) regionNum <- 139 else regionNum <- as.numeric(input$regionSelector)
  #   ggplot(data = seralStageData[ecoregion == regionNum,],
  #          aes(fill = as.factor(seralStageData[ecoregion == regionNum,]$type))) + 
  #     #annotation_custom(sub1Title)+
  #     #annotation_raster(as.raster(seralMapExampleRegion1), xmin = 0.7, xmax = 3.7,
  #     #                  ymin = 25, ymax = 55)+
  #     geom_rect(aes(x = type,
  #                   xmin = as.numeric(type)-0.2, 
  #                   xmax = as.numeric(type)+0.2,
  #                   ymin = minAreaPercentage,
  #                   ymax = maxAreaPercentage)) +
  #     scale_fill_manual(name = "Seral Stage", values = c("forestgreen", "olivedrab3",
  #                                                        "goldenrod3", "grey"),
  #                       labels = c("Overold (>100)", "Old (80-100)",
  #                                  "Mature (40-80)", "Young (< 40)"),
  #                       guide = guide_legend(reverse = TRUE))+
  #     scale_y_continuous("Percentage in forested landscape (%)", 
  #                        breaks = c(seq(0, 20, by = 5), seq(80, 100, by = 5)),
  #                        labels = c(seq(0, 20, by = 5), seq(80, 100, by = 5)))+
  #     
  #     theme_bw()+
  #     theme(panel.grid.major.y = element_blank(),
  #           panel.grid.minor = element_blank(),
  #           panel.border = element_rect(colour = "black", size = 1),
  #           axis.text.x = element_text(size = 12),
  #           axis.text.y = element_blank(),
  #           axis.title.x = element_text(size = 15),
  #           axis.title.y = element_blank(),
  #           axis.ticks.length = unit(5, "points"),
  #           axis.ticks.y = element_blank(),
  #           legend.position = c(0.7, 0.6),
  #           legend.title = element_text(size = 15),
  #           legend.key = element_rect(fill = "white", colour = "white"),
  #           legend.margin = unit(50, "points"),
  #           legend.key.width = unit(20, "points"),
  #           legend.key.height = unit(20, "points"),
  #           legend.text = element_text(size = 12))+
  #     coord_flip()
  #   
  # })
  # 
  # 
  # 
  # output$vegTypePctCoverMap <- renderPlot({
  #   if(is.null(input$regionSelector)) regionNum <- 139 else regionNum <- as.numeric(input$regionSelector)
  #   num <- match(regionNum, availableRegions)
  #   toMap <- get(paste0("vegTypeMapExampleRegion",num))
  #   Plot(toMap, title = FALSE, new=TRUE, speedup = 10, cols = rev(c("white", "yellow3",
  #                                                                   "grey70", "white")))
  # })
  # 
  # output$vegTypePctCover <- renderPlot({
  #   if(is.null(input$regionSelector)) regionNum <- 139 else regionNum <- as.numeric(input$regionSelector)
  #   ggplot(data = vegTypeData[ecoregion == regionNum,],
  #          aes(fill = as.factor(vegTypeData[ecoregion == regionNum,]$type))) + 
  #     #annotation_custom(sub1Title)+
  #     geom_rect(aes(x = type,
  #                   xmin = as.numeric(type)-0.2, 
  #                   xmax = as.numeric(type)+0.2,
  #                   ymin = minAreaPercentage,
  #                   ymax = maxAreaPercentage)) +
  #     scale_fill_manual(name = "Vegetation Type", values = c("white", "yellow3",
  #                                                            "grey70", "white"),
  #                       labels = c(expression(paste("Mixed (", B[none], " > 50%)", sep = "")),
  #                                  expression(paste("Spruce Leading (",
  #                                                   B[spruce], " > 50%)", sep = "")),
  #                                  expression(paste("Aspen Leading (",
  #                                                   B[aspen], " > 50%)", sep = "")),
  #                                  expression(paste("Pine Leading (",
  #                                                   B[pine], " > 50%)", sep = ""))),
  #                       guide = guide_legend(reverse = TRUE))+
  #     scale_y_continuous("Percentage in forested landscape (%)", limits = c(0, 60),
  #                        breaks = c(seq(40, 60, by = 5)),
  #                        labels = c(seq(40, 60, by = 5)))+
  #     
  #     theme_bw()+
  #     theme(panel.grid.major.y = element_blank(),
  #           panel.grid.minor = element_blank(),
  #           panel.border = element_rect(colour = "black", size = 1),
  #           axis.text.x = element_text(size = 12),
  #           axis.title.x = element_text(size = 15),
  #           axis.text.y = element_blank(),
  #           axis.title.y = element_blank(),
  #           axis.ticks.length = unit(5, "points"),
  #           axis.ticks.y = element_blank(),
  #           legend.position = c(0.45, 0.6),
  #           legend.title = element_text(size = 15),
  #           legend.key = element_rect(fill = "white", colour = "white"),
  #           legend.margin = unit(50, "points"),
  #           legend.key.width = unit(20, "points"),
  #           legend.key.height = unit(20, "points"),
  #           legend.text = element_text(size = 12),
  #           legend.text.align = 0)+
  #     coord_flip()
  #   
  # })
  # 
  # 
  # output$patchSizesFig5 <- renderPlot({
  #   if(is.null(input$regionSelector)) regionNum <- 139 else regionNum <- as.numeric(input$regionSelector)
  #   
  #   figuredata <- seralStageDataFig5[ecoregion == regionNum & patchClasses == "[0,1e+03)",]
  #   figuredata[,':='(minNofPatch = minNofPatch/100, maxNofPatch = maxNofPatch/100)]
  #   figure5_1_1 <- ggplot(data = figuredata,
  #                         aes(fill = as.factor(landType)))+
  #     geom_rect(aes(x = landType,
  #                   xmin = as.numeric(as.factor(landType))-0.2, 
  #                   xmax = as.numeric(as.factor(landType))+0.2,
  #                   ymin = minNofPatch,
  #                   ymax = maxNofPatch))+
  #     scale_fill_manual(name = "Seral stage", values = figuredata$colour,
  #                       labels = figuredata$labels)+
  #     scale_y_log10("Number of patches",
  #                   breaks = c(9, 10, 13, 16, seq(120, 240, by = 60)),
  #                   labels = c(9, 10, 13, 16, seq(120, 240, by = 60)))+
  #     theme_bw()+
  #     theme(panel.grid.major.y = element_blank(),
  #           panel.grid.minor = element_blank(),
  #           panel.border = element_rect(colour = "black", size = 1),
  #           axis.text.y = element_blank(),
  #           axis.text.x = element_text(size = 12),
  #           axis.title = element_blank(),
  #           axis.ticks.length = unit(5, "points"),
  #           axis.ticks.y = element_blank(),
  #           legend.position = "none")+
  #     coord_flip()
  #   figure5_1_1gp <- ggplotGrob(figure5_1_1)
  #   rm(figuredata)
  #   
  #   
  #   # region 1 and class 2
  #   figuredata <- seralStageDataFig5[ecoregion == regionNum & patchClasses == "[1e+03,3e+03)",]
  #   figure5_1_2 <- ggplot(data = figuredata,
  #                         aes(fill = as.factor(figuredata$landType),
  #                             group = patchClasses))+
  #     geom_rect(aes(x = landType,
  #                   xmin = as.numeric(as.factor(landType))-0.2, 
  #                   xmax = as.numeric(as.factor(landType))+0.2,
  #                   ymin = minNofPatch,
  #                   ymax = maxNofPatch))+
  #     scale_fill_manual(name = "Seral stage", values = figuredata$colour,
  #                       labels = figuredata$labels)+
  #     theme_bw()+
  #     theme(panel.grid.major.y = element_blank(),
  #           panel.grid.minor = element_blank(),
  #           panel.border = element_rect(colour = "black", size = 1),
  #           axis.text.y = element_blank(),
  #           axis.text.x = element_text(size = 12),
  #           axis.title = element_blank(),
  #           axis.ticks.length = unit(5, "points"),
  #           axis.ticks.y = element_blank(),
  #           legend.position = "none")+
  #     coord_flip()
  #   figure5_1_2gp <- ggplotGrob(figure5_1_2)
  #   rm(figuredata)
  #   
  #   
  #   # region 1 and class 3
  #   figuredata <- seralStageDataFig5[ecoregion == regionNum & patchClasses == "[3e+03,5e+03)",]
  #   figure5_1_3 <- ggplot(data = figuredata,
  #                         aes(fill = as.factor(figuredata$landType),
  #                             group = patchClasses))+
  #     geom_rect(aes(x = landType,
  #                   xmin = as.numeric(as.factor(landType))-0.2, 
  #                   xmax = as.numeric(as.factor(landType))+0.2,
  #                   ymin = minNofPatch,
  #                   ymax = maxNofPatch))+
  #     scale_fill_manual(name = "Seral stage", values = figuredata$colour,
  #                       labels = figuredata$labels)+
  #     theme_bw()+
  #     theme(panel.grid.major.y = element_blank(),
  #           panel.grid.minor = element_blank(),
  #           panel.border = element_rect(colour = "black", size = 1),
  #           axis.text.y = element_blank(),
  #           axis.text.x = element_text(size = 12),
  #           axis.title = element_blank(),
  #           axis.ticks.length = unit(5, "points"),
  #           axis.ticks.y = element_blank(),
  #           legend.position = "none")+
  #     coord_flip()
  #   figure5_1_3gp <- ggplotGrob(figure5_1_3)
  #   rm(figuredata)
  #   
  #   
  #   # region 1 and class 4
  #   figuredata <- seralStageDataFig5[ecoregion == regionNum & (patchClasses == "[5e+03,4.88e+06)" |
  #                                                                patchClasses == "[5e+03,7.05e+06)"),]
  #   figure5_1_4 <- ggplot(data = figuredata,
  #                         aes(fill = as.factor(figuredata$landType),
  #                             group = patchClasses))+
  #     geom_rect(aes(x = landType,
  #                   xmin = as.numeric(as.factor(landType))-0.2, 
  #                   xmax = as.numeric(as.factor(landType))+0.2,
  #                   ymin = minNofPatch,
  #                   ymax = maxNofPatch))+
  #     scale_fill_manual(name = "Seral stage", values = figuredata$colour,
  #                       labels = figuredata$labels)+
  #     theme_bw()+
  #     theme(panel.grid.major.y = element_blank(),
  #           panel.grid.minor = element_blank(),
  #           panel.border = element_rect(colour = "black", size = 1),
  #           axis.text.y = element_blank(),
  #           axis.text.x = element_text(size = 12),
  #           axis.title = element_blank(),
  #           axis.ticks.length = unit(5, "points"),
  #           axis.ticks.y = element_blank(),
  #           legend.position = "none")+
  #     coord_flip()
  #   figure5_1_4gp <- ggplotGrob(figure5_1_4)
  #   
  #   rm(figuredata)
  #   
  #   figure5basedata <- data.table(x = seq(0, 1.99, length = 4), y = seq(0, 4, length = 4))
  #   
  #   figure5 <- ggplot(data = figure5basedata, aes(x = x, y = y))+
  #     geom_abline(, col = "white")+
  #     scale_x_continuous("", limits = c(-0.35, 1.99), breaks = 0:1,
  #                        labels = 0:1)+
  #     scale_y_continuous("", limits = c(-0.4, 4.3), breaks = 1:4,
  #                        labels = 1:4)+
  #     theme_bw()+
  #     theme(panel.grid.major = element_line(colour = "grey"),
  #           panel.grid.minor = element_blank(),
  #           panel.border = element_rect(colour = "black"),
  #           axis.text = element_blank(),
  #           axis.title = element_blank(),
  #           axis.ticks = element_blank())
  #   
  #   figure5 <- figure5+annotation_custom(grob = figure5_1_1gp,
  #                                        xmin = 0.01, xmax = 1.99,
  #                                        ymin = 3.05, ymax = 3.99)+
  #     annotation_custom(grob = figure5_1_2gp,
  #                       xmin = 0.01, xmax = 1.99,
  #                       ymin = 2.05, ymax = 2.99)+
  #     annotation_custom(grob = figure5_1_3gp,
  #                       xmin = 0.01, xmax = 1.99,
  #                       ymin = 1.05, ymax = 1.99)+
  #     annotation_custom(grob = figure5_1_4gp,
  #                       xmin = 0.01, xmax = 1.99,
  #                       ymin = 0.05, ymax = 0.99)#+
  #   #annotation_custom(grob = figure5_2_1gp,
  #   #                  xmin = 1.01, xmax = 1.99,
  #   #                  ymin = 3.05, ymax = 3.99)+
  #   #annotation_custom(grob = figure5_2_2gp,
  #   #                  xmin = 1.01, xmax = 1.99,
  #   #                  ymin = 2.05, ymax = 2.99)+
  #   #annotation_custom(grob = figure5_2_3gp,
  #   #                  xmin = 1.01, xmax = 1.99,
  #   #                  ymin = 1.05, ymax = 1.99)+
  #   #annotation_custom(grob = figure5_2_4gp,
  #   #                  xmin = 1.01, xmax = 1.99,
  #   #                  ymin = 0.05, ymax = 0.99)
  #   a <- 0.35
  #   figure5 <- figure5+
  #     #annotate("text", x = 0.5, y = 4.25, label = "Region 1", size = 5)+
  #     #annotate("text", x = 1.5, y = 4.25, label = "Region 2", size = 5)+
  #     annotate("text", x = -0.18, y = 4.25, label = "Patch\nsize", size = 5)+
  #     annotate("text", x = -0.18, y = 3.5, label = "Class 1 \n< 1000ha", size = 5)+
  #     annotate("text", x = -0.18, y = 2.5, label = "Class 2 \n1000-3000ha", size = 5)+
  #     annotate("text", x = -0.18, y = 1.5, label = "Class 3 \n3000-5000ha", size = 5)+
  #     annotate("text", x = -0.18, y = 0.5, label = "Class 4 \n> 5000ha", size = 5)+
  #     #annotate("text", x = 0.5, y = 3.15, label = "(x100)", size = 5)+
  #     annotate("text", x = 1, y = 3.15, label = "(x100)", size = 5)+
  #     geom_rect(aes(xmin = 0.25, xmax = 1.9, ymin = -0.4, ymax = 0.0), fill = "white", col = "black")+
  #     geom_rect(aes(xmin = 0.55, xmax = 0.65, ymin = -0.3, ymax = -0.1), fill = "red", col = "red")+
  #     annotate("text", x = 0.65+0.1, y = -0.2, label = "Young", size = 5)+
  #     geom_rect(aes(xmin = 0.55+a, xmax = 0.65+a, ymin = -0.3, ymax = -0.1), fill = "green", col = "green")+
  #     annotate("text", x = 0.65+a+0.1, y = -0.2, label = "Mature", size = 5)+
  #     geom_rect(aes(xmin = 0.55+2*a, xmax = 0.65+2*a, ymin = -0.3, ymax = -0.1), fill = "blue", col = "blue")+
  #     annotate("text", x = 0.65+2*a+0.1, y = -0.2, label = "Old", size = 5)+
  #     geom_rect(aes(xmin = 0.55+3*a, xmax = 0.65+3*a, ymin = -0.3, ymax = -0.1), fill = "brown", col = "brown")+
  #     annotate("text", x = 0.65+3*a+0.1, y = -0.2, label = "Overold", size = 5)+
  #     annotate("text", x = 0.4, y = -0.2, label = "Seral stage:", size = 5)
  #   
  #   figure5
  #   
  # })
  # 
  # 
  # 
  # 
  # 
  # output$patchSizesFig6 <- renderPlot({
  #   if(is.null(input$regionSelector)) regionNum <- 139 else regionNum <- as.numeric(input$regionSelector)
  #   
  #   
  #   figuredata <- vegTypeDataFig6[ecoregion == regionNum & patchClasses == "[0,1e+03)",]
  #   figure6_1_1 <- ggplot(data = figuredata,
  #                         aes(fill = as.factor(landType)))+
  #     geom_rect(aes(x = landType,
  #                   xmin = as.numeric(as.factor(landType))-0.2, 
  #                   xmax = as.numeric(as.factor(landType))+0.2,
  #                   ymin = minNofPatch,
  #                   ymax = maxNofPatch))+
  #     scale_fill_manual(name = "Seral stage", values = figuredata$colour,
  #                       labels = figuredata$labels)+
  #     scale_y_log10("", breaks = c(1, 5, 10, 20, 50, 10000),
  #                   label = c(1, 5, 10, 20, 50, 10000))+
  #     # scale_y_log10("Number of patches",
  #     #               breaks = c(9, 10, 13, 16, seq(120, 240, by = 60)),
  #     #               labels = c(9, 10, 13, 16, seq(120, 240, by = 60)))+
  #     theme_bw()+
  #     theme(panel.grid.major.y = element_blank(),
  #           panel.grid.minor = element_blank(),
  #           panel.border = element_rect(colour = "black", size = 1),
  #           axis.text.y = element_blank(),
  #           axis.text.x = element_text(size = 12),
  #           axis.title = element_blank(),
  #           axis.ticks.length = unit(5, "points"),
  #           axis.ticks.y = element_blank(),
  #           legend.position = "none")+
  #     coord_flip()
  #   figure6_1_1gp <- ggplotGrob(figure6_1_1)
  #   rm(figuredata)
  #   
  #   
  #   # region 1 and class 2
  #   figuredata <- vegTypeDataFig6[ecoregion == regionNum & patchClasses == "[1e+03,3e+03)",]
  #   figure6_1_2 <- ggplot(data = figuredata,
  #                         aes(fill = as.factor(figuredata$landType),
  #                             group = patchClasses))+
  #     geom_rect(aes(x = landType,
  #                   xmin = as.numeric(as.factor(landType))-0.2, 
  #                   xmax = as.numeric(as.factor(landType))+0.2,
  #                   ymin = minNofPatch,
  #                   ymax = maxNofPatch))+
  #     scale_fill_manual(name = "Seral stage", values = figuredata$colour,
  #                       labels = figuredata$labels)+
  #     theme_bw()+
  #     theme(panel.grid.major.y = element_blank(),
  #           panel.grid.minor = element_blank(),
  #           panel.border = element_rect(colour = "black", size = 1),
  #           axis.text.y = element_blank(),
  #           axis.text.x = element_text(size = 12),
  #           axis.title = element_blank(),
  #           axis.ticks.length = unit(5, "points"),
  #           axis.ticks.y = element_blank(),
  #           legend.position = "none")+
  #     coord_flip()
  #   figure6_1_2gp <- ggplotGrob(figure6_1_2)
  #   rm(figuredata)
  #   
  #   
  #   # region 1 and class 3
  #   figuredata <- vegTypeDataFig6[ecoregion == regionNum & patchClasses == "[3e+03,5e+03)",]
  #   figure6_1_3 <- ggplot(data = figuredata,
  #                         aes(fill = as.factor(figuredata$landType),
  #                             group = patchClasses))+
  #     geom_rect(aes(x = landType,
  #                   xmin = as.numeric(as.factor(landType))-0.2, 
  #                   xmax = as.numeric(as.factor(landType))+0.2,
  #                   ymin = minNofPatch,
  #                   ymax = maxNofPatch))+
  #     scale_fill_manual(name = "Seral stage", values = figuredata$colour,
  #                       labels = figuredata$labels)+
  #     theme_bw()+
  #     theme(panel.grid.major.y = element_blank(),
  #           panel.grid.minor = element_blank(),
  #           panel.border = element_rect(colour = "black", size = 1),
  #           axis.text.y = element_blank(),
  #           axis.text.x = element_text(size = 12),
  #           axis.title = element_blank(),
  #           axis.ticks.length = unit(5, "points"),
  #           axis.ticks.y = element_blank(),
  #           legend.position = "none")+
  #     coord_flip()
  #   figure6_1_3gp <- ggplotGrob(figure6_1_3)
  #   rm(figuredata)
  #   
  #   
  #   # region 1 and class 4
  #   figuredata <- vegTypeDataFig6[ecoregion == regionNum & (patchClasses == "[5e+03,4.88e+06)" |
  #                                                             patchClasses == "[5e+03,7.05e+06)"),]
  #   figure6_1_4 <- ggplot(data = figuredata,
  #                         aes(fill = as.factor(figuredata$landType),
  #                             group = patchClasses))+
  #     geom_rect(aes(x = landType,
  #                   xmin = as.numeric(as.factor(landType))-0.2, 
  #                   xmax = as.numeric(as.factor(landType))+0.2,
  #                   ymin = minNofPatch,
  #                   ymax = maxNofPatch))+
  #     scale_fill_manual(name = "Seral stage", values = figuredata$colour,
  #                       labels = figuredata$labels)+
  #     theme_bw()+
  #     theme(panel.grid.major.y = element_blank(),
  #           panel.grid.minor = element_blank(),
  #           panel.border = element_rect(colour = "black", size = 1),
  #           axis.text.y = element_blank(),
  #           axis.text.x = element_text(size = 12),
  #           axis.title = element_blank(),
  #           axis.ticks.length = unit(5, "points"),
  #           axis.ticks.y = element_blank(),
  #           legend.position = "none")+
  #     coord_flip()
  #   figure6_1_4gp <- ggplotGrob(figure6_1_4)
  #   rm(figuredata)
  #   
  #   
  #   
  #   figure6basedata <- data.table(x = seq(0, 1.99, length = 4), y = seq(0, 4, length = 4))
  #   figure6 <- ggplot(data = figure6basedata, aes(x = x, y = y))+
  #     geom_abline(, col = "white")+
  #     scale_x_continuous("", limits = c(-0.35, 1.99), breaks = 0:1,
  #                        labels = 0:1)+
  #     scale_y_continuous("", limits = c(-0.4, 4.3), breaks = 1:4,
  #                        labels = 1:4)+
  #     theme_bw()+
  #     theme(panel.grid.major = element_line(colour = "grey"),
  #           panel.grid.minor = element_blank(),
  #           panel.border = element_rect(colour = "black"),
  #           axis.text = element_blank(),
  #           axis.title = element_blank(),
  #           axis.ticks = element_blank())
  #   
  #   figure6 <- figure6+annotation_custom(grob = figure6_1_1gp,
  #                                        xmin = 0.01, xmax = 1.99,
  #                                        ymin = 3.05, ymax = 3.99)+
  #     annotation_custom(grob = figure6_1_2gp,
  #                       xmin = 0.01, xmax = 1.99,
  #                       ymin = 2.05, ymax = 2.99)+
  #     annotation_custom(grob = figure6_1_3gp,
  #                       xmin = 0.01, xmax = 1.99,
  #                       ymin = 1.05, ymax = 1.99)+
  #     annotation_custom(grob = figure6_1_4gp,
  #                       xmin = 0.01, xmax = 1.99,
  #                       ymin = 0.05, ymax = 0.99)
  #   intervl <- 0.32
  #   start1 <- 0.34
  #   
  #   figure6a <- figure6+
  #     #annotate("text", x = 0.5, y = 4.25, label = "Region 1", size = 5)+
  #     #annotate("text", x = 1.5, y = 4.25, label = "Region 2", size = 5)+
  #     annotate("text", x = -0.18, y = 4.25, label = "Patch\nsize", size = 5)+
  #     annotate("text", x = -0.18, y = 3.5, label = "Class 1 \n< 1000ha", size = 5)+
  #     annotate("text", x = -0.18, y = 2.5, label = "Class 2 \n1000-3000ha", size = 5)+
  #     annotate("text", x = -0.18, y = 1.5, label = "Class 3 \n3000-5000ha", size = 5)+
  #     annotate("text", x = -0.18, y = 0.5, label = "Class 4 \n> 5000ha", size = 5)+
  #     geom_rect(aes(xmin = 0.1, xmax = 1.95, ymin = -0.4, ymax = 0.0), fill = "white", col = "black")+
  #     geom_rect(aes(xmin = start1, xmax = start1+0.1, ymin = -0.3, ymax = -0.1), fill = "red", col = "red")+
  #     annotate("text", x = start1+0.05+intervl/2, y = -0.2, label = "Pine", size = 5)+
  #     geom_rect(aes(xmin = start1+intervl, xmax = start1+0.1+intervl, ymin = -0.3, ymax = -0.1), fill = "green", col = "green")+
  #     annotate("text", x = start1+0.05+intervl*3/2, y = -0.2, label = "Aspen", size = 5)+
  #     geom_rect(aes(xmin = start1+2*intervl, xmax = start1+0.1+2*intervl, ymin = -0.3, ymax = -0.1), fill = "blue", col = "blue")+
  #     annotate("text", x = start1+0.05+intervl*5/2, y = -0.2, label = "Spruce", size = 5)+
  #     geom_rect(aes(xmin = start1+3*intervl, xmax = start1+0.1+3*intervl, ymin = -0.3, ymax = -0.1), fill = "brown", col = "brown")+
  #     annotate("text", x = start1+0.05+intervl*7/2, y = -0.2, label = "Mixed", size = 5)#+
  #     #annotate("text", x = 0.28, y = -0.2, label = "Vegetation type:", size = 5)
  #   
  #   figure6a
  #   
  # })
  # 
  # 
  # output$patchSizesFig5Raster <- renderPlot({
  #   if(is.null(input$regionSelector)) regionNum <- 139 else regionNum <- as.numeric(input$regionSelector)
  #   
  #   num <- match(regionNum, availableRegions)
  #   overold <- get(paste0("overoldpatchmapRegion",num, "_5000"))
  #   
  #   Plot(overold, legend = FALSE, title = FALSE, new=TRUE)
  #   seekViewport("overold")
  #   grid.text(label = paste("N =", length(unique(getValues(overold)))-1),
  #             x = 0.1, y = 0.05, gp = gpar(fontsize = 15))
  #   
  # })
  # 
  # output$patchSizesFig6Raster <- renderPlot(height = 800,  {
  #   if(is.null(input$regionSelector)) regionNum <- 139 else regionNum <- as.numeric(input$regionSelector)
  #   
  #   num <- match(regionNum, availableRegions)
  #   aspenLeading <- get(paste0("aspenleadingpatchmapRegion",num, "_5000"))
  #   spruceLeading <- get(paste0("spruceleadingpatchmapRegion",num, "_5000"))
  #   
  #   Plot(aspenLeading, spruceLeading, new=TRUE, 
  #        legend = FALSE, title = FALSE)
  #   seekViewport("aspenLeading")
  #   grid.text(label = paste("N =", length(unique(getValues(aspenLeading)))-1),
  #             x = 0.1, y = 0.05, gp = gpar(fontsize = 15))
  #   seekViewport("spruceLeading")
  #   grid.text(label = paste("N =", length(unique(getValues(spruceLeading)))-1),
  #             x = 0.1, y = 0.05, gp = gpar(fontsize = 15))
  #   
  # })
  
  output$numMapUnits <- renderValueBox({
    valueBox(
      value = 163,
      subtitle = "Number of unique map units"
    )
  })
  
  output$numInitialCommunities <- renderValueBox({
    valueBox(
      value = 442,
      subtitle = "Number of unique initial communities"
    )
  })
}
