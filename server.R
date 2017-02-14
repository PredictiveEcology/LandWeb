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
  
  message("Running Experiment")
  mySimOut <- Cache(experiment, mySim, replicates = 6, debug = TRUE, cache = TRUE, 
                    cl = cl, 
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
  
  message("Reprojecting rasters & loading into RAM")
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
    #if ((ncell(r) < 5e5) & (length(tsf) < 30)) r[] <- r[] 
    r
  })
  message("  Finished reprojecting rasters & loading into RAM")

  leadingByStage <- function(timeSinceFireFiles, vegTypeMapFiles,
                             polygonToSummarizeBy, polygonNames, 
                             ageCutoffs = c(0, 40, 80, 120),  ageClasses, cl) {
    if (missing(cl)) {
      lapplyFn <- "lapply" 
    } else {
      lapplyFn <- "parLapplyLB"
      #clusterExport(cl = cl, varlist = list("timeSinceFireFiles", "vegTypeMapFiles", "polygonToSummarizeBy"),
      if(Sys.info()[["sysname"]]=="Windows") {
        clusterExport(cl = cl, varlist = list(ls()),
                    envir = environment())
        clusterEvalQ(cl = cl, {
          library(raster)
        })
      }
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
                      
                      aa <- raster::extract(leadingRast, polygonToSummarizeBy, fun = function(x, ...) {
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
                   cl=cl, 
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
                             cl = cl, #countNumPatches,
                             ageCutoffs = c(0, 40, 80, 120), patchSize = 1000, 
                             ageClasses, notOlderThan = Sys.time() - 1e7) {
    if (missing(cl)) {
      lapplyFn <- "lapply"
    } else {
      lapplyFn <- "parLapplyLB"
      if(Sys.info()[["sysname"]]=="Windows") {
        
        clusterExport(cl = cl,
                      varlist = list(c(ls(), "countNumPatches")),
                      # varlist = list("timeSinceFireFiles", "vegTypeMapFiles",
                      #                "polygonToSummarizeBy", "countNumPatches",
                      #                "paths", "ageCutoffs", "patchSize", "ageClasses"),
                      envir = environment())
        clusterEvalQ(cl = cl, {
          library(raster)
          library(magrittr)
          library(SpaDES)
          library(data.table)
        })
      }
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
                          raster::extract(ras[[1]], polygonToSummarizeBy, fun = function(x, ...) {
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
                       cl=cl, 
                       ageClasses = ageClasses, cacheRepo = paths$cachePath,
                       patchSize = reactive({input$patchSize33}),
                       largePatchesFn = largePatchesFn,
                       countNumPatches=countNumPatches)
  
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
