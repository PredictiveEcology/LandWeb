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
  
  rastersFromOutputs <- lapply(seq_along(mySimOut), function(x) {
    grep(pattern = ".grd$|.tif$", outputs(mySimOut[[x]])$file, value = TRUE)
  })
  
 # browser()
  if(any(!file.exists(unlist(rastersFromOutputs)))) {
    rastersFromOutputs2 <- 
      lapply(rastersFromOutputs, function(ras) {
        unlist(lapply(strsplit(ras, split = outputPath(mySimOut[[1]])), 
                    function(x) file.path(paths$outputPath, gsub(x[[2]], pattern = "^/", replacement = ""))))})
    for(simNum in seq_along(mySimOut)) {
      mySimOut[[simNum]]@outputs$file[outputs(mySimOut[[simNum]])$file %in% rastersFromOutputs[[simNum]] ] <- 
        rastersFromOutputs2[[simNum]]
    }
  }
  
  rastersFromOutputs <- unlist(rastersFromOutputs)
  tsf <- grep(pattern = "rstTimeSinceFire", rastersFromOutputs, value = TRUE)
  vtm <- grep(pattern = "vegTypeMap", rastersFromOutputs, value = TRUE)
  
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
  

  leadingByStage <- function(timeSinceFireFiles, vegTypeMapFiles,
                             polygonToSummarizeBy, 
                             #polygonToSummarizeByRas, 
                             polygonNames, 
                             ageCutoffs = c(0, 40, 80, 120),  ageClasses, cl) {
    if (missing(cl)) {
      lapplyFn <- "lapply" 
    } else {
      lapplyFn <- "parLapplyLB"
      #clusterExport(cl = cl, varlist = list("timeSinceFireFiles", "vegTypeMapFiles", "polygonToSummarizeBy"),
      if (Sys.info()[["sysname"]] == "Windows") {
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
      
      out1 <- #Cache(cacheRepo = paths$cachePath, 
                    do.call(lapplyFn, append(startList, list(X = timeSinceFireFiles, function(x, ...) {
                      x <- match(x, timeSinceFireFiles)
                      timeSinceFireFilesRast <- raster(timeSinceFireFiles[x])
                      leadingRast <- raster(vegTypeMapFiles[x])
                      leadingRast[timeSinceFireFilesRast[] < ageCutoffs[y]] <- 0
                      if ((y + 1) < length(ageCutoffs))
                        leadingRast[timeSinceFireFilesRast[] >= ageCutoffs[y + 1]] <- 0
                      leadingRast
                    })))
      names(out1) <- gsub(paste(basename(dirname(tsf)), basename(tsf), sep = "_"), 
                          replacement = "_", pattern = "\\.")
      out1
      })
    names(out) <- ageClasses
      allStack <- stack(unlist(lapply(out, function(ageClasses) {
        out <- lapply(ageClasses, function(rep) {
          #lapply(rep, function(vegType) {
            rep
          #}) 
        })
        #names(out) <- 
        out
      })))
      IDs <- levels(out[[1]][[1]])[[1]]$ID
      Factors <- levels(out[[1]][[1]])[[1]]$Factor
      ii <- 3
      aa <- raster::extract(allStack, polygonToSummarizeBy)#, #df = TRUE,
                          #   fun = function (x, ...) {
                          #     
                          #     nonNACells <- na.omit(x) 
                          #     out <- table(nonNACells)
                          #     #out <- tabulate(nonNACells, 4)
                          #     out2 <- as.numeric(out) %>% setNames(names(out))
                          #     browser(expr=ii<9)
                          #     ii <<- ii + 1
                          #     out3 <- rep(0,max(IDs))
                          #     if(length(out2)) {
                          #       out3[as.numeric(names(out))] <- out2
                          #     }
                          #     out3
                          #     #out
                          # })
      
      aa1 <- lapply(aa, function(x,  ...) {
        if(!is.null(x)) {
          apply(x, 2, function(y) {
            nonNACells <- na.omit(y)
            vals <- tabulate(nonNACells, max(IDs))
            #                  vals <- tabulate(nonNACells, max(IDs))
                              names(vals)[IDs] <- Factors
                              vals <- vals[!is.na(names(vals))]
                              if(sum(vals)) {
                                vals / sum(vals)
                              } else {
                                vals
                              }
          })
        } else {
          NULL
        }
      })
      
      
      nonNulls <- unlist(lapply(aa1, function(x) !is.null(x)))
      #aa <- cbind(zone=rep(ecodistricts$ECODISTRIC, each = length(Factors)), aa) 
      vegType <- unlist(lapply(aa1[nonNulls], rownames))
      aa1 <- lapply(aa1, function(a) {
        rownames(a) <- NULL
        a
      })
      
      aadf <- data.frame(zone=rep(ecodistricts$ECODISTRIC[nonNulls], each = length(Factors)), 
                         polygonNum = rep(seq_along(ecodistricts$ECODISTRIC)[nonNulls], each = length(Factors)),
                         vegType = vegType, do.call(rbind, aa1[nonNulls]),
                         stringsAsFactors = FALSE)
      
      temp <- list()
      for(ages in ageClasses) {
        temp[[ages]] <- aadf %>%
          dplyr::select(starts_with(ages) , zone:vegType) %>%
          tidyr::gather(key = "label", value = "proportion", -(zone:vegType)) %>%
          mutate(ageClass = unlist(lapply(strsplit(label, split = "\\."), function(x) x[[1]])))
      }
      
      aa <- rbindlist(temp)
      aa
  }
  #ecodistrictsRas <- Cache(cacheRepo = paths$cachePath,
  #                         rasterize, ecodistricts, raster(tsf[1]))
  message("Running leadingByStage")
  args <- list(leadingByStage, tsf, vtm, 
               polygonToSummarizeBy = ecodistricts,
               #polygonToSummarizeByRas = ecodistrictsRas,
               polygonNames = ecodistricts$ECODISTRIC, 
               cl = if(exists("cl")) cl, 
               ageClasses = ageClasses, cacheRepo = paths$cachePath)
  args <- args[!unlist(lapply(args, is.null))]
  leading <- do.call(Cache, args)
  # leading <- Cache(leadingByStage, tsf, vtm, ecodistricts,
  #                  polygonNames = ecodistricts$ECODISTRIC, 
  #                  cl = cl,
  #                  ageClasses = ageClasses, cacheRepo = paths$cachePath)
  message("  Finished leadingByStage")
  
  # Large patches
  countNumPatches <- function(ras, patchSize, ...) {
    clumpedRas <- clump(ras, gaps=FALSE, ...)
    # freqTable <- data.table(freq(clumpedRas))[!is.na(value), ][
    #   , area := count * (res(clumpedRas)[1] ^ 2) / 10000]
    # largeEnoughPatches <- freqTable[area >= patchSize, ][, newValue := as.numeric(as.factor(value))]
    if(!is.na(maxValue(clumpedRas))) {
      largeEnoughPatches <- which((tabulate(na.omit(clumpedRas[]), nbins = maxValue(clumpedRas))*
                                   ((res(clumpedRas)[1] ^ 2) / 10000))>patchSize)
    } else {
      largeEnoughPatches <- integer(0)
    }
    clumpedRas[!(clumpedRas %in% largeEnoughPatches)] <- NA
    list(ras = clumpedRas, count = largeEnoughPatches)
  }
  
  largePatchesFn <- function(timeSinceFireFiles, vegTypeMapFiles,
                             polygonToSummarizeBy, #polygonNames,
                             cl, #countNumPatches,
                             ageCutoffs = c(0, 40, 80, 120), patchSize = 1000, 
                             ageClasses, notOlderThan = Sys.time() - 1e7) {
    if (missing(cl)) {
      lapplyFn <- "lapply"
    } else {
      lapplyFn <- "parLapplyLB"
      if (Sys.info()[["sysname"]] == "Windows") {
        
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
      out1 <- #Cache(cacheRepo = paths$cachePath, #notOlderThan = Sys.time(),
                    do.call(lapplyFn, append(startList, list(X = timeSinceFireFiles, function(x, ...) {
                      x <- match(x, timeSinceFireFiles)
                      timeSinceFireFilesRast <- raster(timeSinceFireFiles[x])
                      leadingRast <- raster(vegTypeMapFiles[x])
                      leadingRast[timeSinceFireFilesRast[] < ageCutoffs[y]] <- NA
                      if ((y + 1) < length(ageCutoffs))
                        leadingRast[timeSinceFireFilesRast[] >= ageCutoffs[y + 1]] <- NA
                      
                      clumpedRasts <- lapply(levels(leadingRast)[[1]]$ID, function(ID) {
                        spRas <- leadingRast
                        spRas[spRas != ID] <- NA
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
                      clumpedRasts
                    })))
      out1
    }
    )

    out <- setNames(out, ageClasses)
    allStack <- unlist(out, recursive = FALSE) %>%
      unlist(recursive = FALSE) %>%
      lapply(function(x) x$ras) %>%
      stack()

    # allStack <- stack(unlist(lapply(out, function(ageClasses) {
    #   lapply(ageClasses, function(rep) {
    #     lapply(rep, function(vegType) {
    #       vegType[[1]]
    #     }) 
    #   })
    # })))
    out2 <- raster::extract(allStack, 
                            y = polygonToSummarizeBy, 
                            fun = function(x, ...) {
      nonNACells <- na.omit(x)
      length(unique(nonNACells))
    })
    out2
    #out3 <- lapply(as.list(data.frame(out2)), function(x) as.matrix(x))
                      #%>% setNames(names(clumpedRasts))
                      
                      # 
                      # out2 <- lapply(clumpedRasts, function(ras) {
                      #   aa <- #Cache(notOlderThan = Sys.time(),
                      #     raster::extract(ras[[1]], polygonToSummarizeBy, fun = function(x, ...) {
                      #       nonNACells <- na.omit(x)
                      #       length(unique(nonNACells))
                      #     }, cacheRepo = paths$cachePath)
                      # }) %>% setNames(names(clumpedRasts))
    #                  out2
      #names(out2) <- paste(basename(dirname(timeSinceFireFiles)), basename(timeSinceFireFiles), sep = "_")
      #out2
    #names(out) <- ageClasses
    #out3
  }

  #omitted <- lapply(leading, function(x) lapply(x, function(y) attr(na.omit(y), "na.action")))
  # polygonsWithData <- lapply(seq_along(leading), function(x) {
  #     unlist(lapply(x, function(y) {
  #       if (!is.null(omitted[[x]][[y]])) {
  #         seq_len(NROW(leading[[x]][[y]]))[-omitted[[x]][[y]]]
  #       } else {
  #         seq_len(NROW(leading[[x]][[y]]))
  #       }
  #     }))
  #   }) %>%
  #   setNames(ageClasses)

  
  #zoneCol <- which(colnames(leading)=="zone")
  
  polygonsWithData <- leading[,unique(polygonNum[!is.na(proportion)]),by=ageClass]
  
  # polygonsWithData <- apply(
  #   apply(leading[,-zoneCol],2,
  #             function(x) which(!is.na(x)))
  #   , 2, unique)
  #polygonsWithData <- as.list(data.frame(polygonsWithData))
  
  # polygonsWithData <- lapply(seq_along(leading), function(x) {
  #   unique(unlist(lapply(seq_along(leading[[x]]), function(y) {
  #     if (!is.null(omitted[[x]][[y]])) {
  #       seq_len(NROW(leading[[x]][[y]]))[-omitted[[x]][[y]]]
  #     } else {
  #       seq_len(NROW(leading[[x]][[y]]))
  #     }
  #   })))
  # }) %>%
  #names(polygonsWithData) <- ageClasses

  vegLeadingTypes <- unique(leading$vegType)
  
  message("  Finished global.R")
  
  ##
  ageClassTextTitle <- h2("NRV of Large Patches")
  ageClassText <- h4(paste("These figures show the NRV of the number of 'large' patches,",
                           "by Age Class, Leading Vegetation, and Polygon. ",
                           "If this is blank, it means there was no vegetation in this age class, ",
                           "so 'a large patch' doesn't make any sense and is not shown."))
  vegText <- h4(paste("These figures show the NRV of the proportion of each polygon in each Age Class,",
                      "and Leading Vegetation type.",
                      "The totals sum to 1 across Leading Vegetation type, within each Age Class."
                      ))
  vegTextTitle <- h2("NRV of Vegetation Cover")
  
  ##
  
  output$ClumpsYoungUI <- renderUI({
    
    tabBox(width = 12,
      tabPanel("Young, Deciduous", tabName = "ClumpsYoung_Deciduous2",
        fluidRow(
          column(width = 12, ageClassTextTitle),
          column(width = 12, ageClassText),
          lapply(pmatch("Young", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[ageClass==ageClasses[i]]$V1], function(j) {
              clumpModOutput(paste0(i, "_", j, "_", pmatch("Deciduous", vegLeadingTypes), "_clumps"),
                             vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Young, Spruce", tabName = "ClumpsYoung_Spruce2",
        fluidRow(
          column(width = 12, ageClassTextTitle),
          column(width = 12, ageClassText),
          lapply(pmatch("Young", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[ageClass==ageClasses[i]]$V1], function(j) {
              clumpModOutput(paste0(i, "_", j, "_", pmatch("Spruce", vegLeadingTypes), "_clumps"),
                             vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Young, Mixed", tabName = "ClumpsYoung_Mixed2",
        fluidRow(
          column(width = 12, ageClassTextTitle),
          column(width = 12, ageClassText),
          lapply(pmatch("Young", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[ageClass==ageClasses[i]]$V1], function(j) {
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
          column(width = 12, ageClassTextTitle),
          column(width = 12, ageClassText),
          lapply(pmatch("Immature", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[ageClass==ageClasses[i]]$V1], function(j) {
              clumpModOutput(paste0(i, "_", j, "_", pmatch("Deciduous", vegLeadingTypes), "_clumps"),
                             vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Immature, Spruce", tabName = "Immature_Spruce2",
        fluidRow(
          column(width = 12, ageClassTextTitle),
          column(width = 12, ageClassText),
          lapply(pmatch("Immature", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[ageClass==ageClasses[i]]$V1], function(j) {
              clumpModOutput(paste0(i, "_", j, "_", pmatch("Spruce", vegLeadingTypes), "_clumps"),
                             vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Immature, Mixed", tabName = "Immature_Mixed2",
        fluidRow(
          column(width = 12, ageClassTextTitle),
          column(width = 12, ageClassText),
          lapply(pmatch("Immature", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[ageClass==ageClasses[i]]$V1], function(j) {
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
          column(width = 12, ageClassTextTitle),
          column(width = 12, ageClassText),
          lapply(pmatch("Mature", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[ageClass==ageClasses[i]]$V1], function(j) {
              clumpModOutput(paste0(i, "_", j, "_", pmatch("Deciduous", vegLeadingTypes), "_clumps"),
                             vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Mature, Spruce", tabName = "Mature_Spruce2",
        fluidRow(
          column(width = 12, ageClassTextTitle),
          column(width = 12, ageClassText),
          lapply(pmatch("Mature", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[ageClass==ageClasses[i]]$V1], function(j) {
              clumpModOutput(paste0(i, "_", j, "_", pmatch("Spruce", vegLeadingTypes), "_clumps"),
                             vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Mature, Mixed", tabName = "Mature_Mixed2",
        fluidRow(
          column(width = 12, ageClassTextTitle),
          column(width = 12, ageClassText),
          lapply(pmatch("Mature", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[ageClass==ageClasses[i]]$V1], function(j) {
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
          column(width = 12, ageClassTextTitle),
          column(width = 12, ageClassText),
          lapply(pmatch("Old", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[ageClass==ageClasses[i]]$V1], function(j) {
              clumpModOutput(paste0(i, "_", j, "_", pmatch("Deciduous", vegLeadingTypes), "_clumps"),
                             vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Old, Spruce", tabName = "Old_Spruce2",
        fluidRow(
          column(width = 12, ageClassTextTitle),
          column(width = 12, ageClassText),
          lapply(pmatch("Old", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[ageClass==ageClasses[i]]$V1], function(j) {
              clumpModOutput(paste0(i, "_", j, "_", pmatch("Spruce", vegLeadingTypes), "_clumps"),
                             vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Old, Mixed", tabName = "Old_Mixed2",
        fluidRow(
          column(width = 12, ageClassTextTitle),
          column(width = 12, ageClassText),
          lapply(pmatch("Old", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[ageClass==ageClasses[i]]$V1], function(j) {
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
          column(width = 12, vegTextTitle),
          column(width = 12, vegText),
          lapply(pmatch("Young", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[ageClass==ageClasses[i]]$V1], function(j) {
              vegAgeModUI(paste0(i, "_", j, "_", pmatch("Deciduous", vegLeadingTypes)),
                          vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Young, Spruce", tabName = "Young_Spruce",
        fluidRow(
          column(width = 12, vegTextTitle),
          column(width = 12, vegText),
          lapply(pmatch("Young", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[ageClass==ageClasses[i]]$V1], function(j) {
              vegAgeModUI(paste0(i, "_", j, "_", pmatch("Spruce", vegLeadingTypes)),
                          vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Young, Mixed", tabName = "Young_Mixed",
        fluidRow(
          column(width = 12, vegTextTitle),
          column(width = 12, vegText),
          lapply(pmatch("Young", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[ageClass==ageClasses[i]]$V1], function(j) {
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
          column(width = 12, vegTextTitle),
          column(width = 12, vegText),
          lapply(pmatch("Immature", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[ageClass==ageClasses[i]]$V1], function(j) {
              vegAgeModUI(paste0(i, "_", j, "_", pmatch("Deciduous", vegLeadingTypes)),
                          vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Immature, Spruce", tabName = "Immature_Spruce",
        fluidRow(
          column(width = 12, vegTextTitle),
          column(width = 12, vegText),
          lapply(pmatch("Immature", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[ageClass==ageClasses[i]]$V1], function(j) {
              vegAgeModUI(paste0(i, "_", j, "_", pmatch("Spruce", vegLeadingTypes)),
                          vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Immature, Mixed", tabName = "Immature_Mixed",
        fluidRow(
          column(width = 12, vegTextTitle),
          column(width = 12, vegText),
          lapply(pmatch("Immature", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[ageClass==ageClasses[i]]$V1], function(j) {
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
          column(width = 12, vegTextTitle),
          column(width = 12, vegText),
          lapply(pmatch("Mature", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[ageClass==ageClasses[i]]$V1], function(j) {
              vegAgeModUI(paste0(i, "_", j, "_", pmatch("Deciduous", vegLeadingTypes)),
                          vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Mature, Spruce", tabName = "Mature_Spruce",
        fluidRow(
          column(width = 12, vegTextTitle),
          column(width = 12, vegText),
          lapply(pmatch("Mature", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[ageClass==ageClasses[i]]$V1], function(j) {
              vegAgeModUI(paste0(i, "_", j, "_", pmatch("Spruce", vegLeadingTypes)),
                          vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Mature, Mixed", tabName = "Mature_Mixed",
        fluidRow(
          column(width = 12, vegTextTitle),
          column(width = 12, vegText),
          lapply(pmatch("Mature", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[ageClass==ageClasses[i]]$V1], function(j) {
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
          column(width = 12, vegTextTitle),
          column(width = 12, vegText),
          lapply(pmatch("Old", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[ageClass==ageClasses[i]]$V1], function(j) {
              vegAgeModUI(paste0(i, "_", j, "_", pmatch("Deciduous", vegLeadingTypes)),
                          vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Old, Spruce", tabName = "Old_Spruce",
        fluidRow(
          column(width = 12, vegTextTitle),
          column(width = 12, vegText),
          lapply(pmatch("Old", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[ageClass==ageClasses[i]]$V1], function(j) {
              vegAgeModUI(paste0(i, "_", j, "_", pmatch("Spruce", vegLeadingTypes)),
                          vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      ),
      tabPanel("Old, Mixed", tabName = "Old_Mixed",
        fluidRow(
          column(width = 12, vegTextTitle),
          column(width = 12, vegText),
          lapply(pmatch("Old", ageClasses), function(i) {
            lapply(seq_along(ecodistricts)[polygonsWithData[ageClass==ageClasses[i]]$V1], function(j) {
              vegAgeModUI(paste0(i, "_", j, "_", pmatch("Mixed", vegLeadingTypes)),
                          vegLeadingTypes = vegLeadingTypes)
            })
          })
        )
      )
    )
  })
  
  #patchSizeReact <- reactive({input$PatchSize33})
  
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
               #currentPolygon = reactive({polygons[[currentPolygon()+2]]}), 
               currentPolygon = polygons[[1 + length(polygons)/4]], 
               tsf = tsf, vtm = vtm,
               #polygonNames = ecodistricts$ECODISTRIC, 
               cl = if(exists("cl")) cl, 
               ageClasses = ageClasses, cacheRepo = paths$cachePath,
               patchSize = reactive({input$patchSize33}),
               largePatchesFn = largePatchesFn)
  args <- args[!unlist(lapply(args, is.null))]
  Clumps <- do.call(callModule, args )
  
  # Clumps <- callModule(clumpMod2, "id1", 
  #                      #currentPolygon = polygons[[reactive({currentPolygon()})+6]], 
  #                      currentPolygon = polygons[[1 + 2]], 
  #                      tsf = tsf, vtm = vtm,
  #                      #polygonNames = ecodistricts$ECODISTRIC, 
  #                      cl = cl, 
  #                      ageClasses = ageClasses, cacheRepo = paths$cachePath,
  #                      patchSize = reactive({input$patchSize33}),
  #                      largePatchesFn = largePatchesFn
  #                      )
  # 
  lapply(seq_along(ageClasses), function(i) { # i is age
    lapply(polygonsWithData[ageClass==ageClasses[i]]$V1, function(j) { # j is polygon index
      lapply(seq_along(vegLeadingTypes), function(k) { # k is Veg type
        callModule(clumpMod, paste0(i, "_", j, "_", k, "_clumps"),
                   Clumps = reactive({Clumps()}),
                   id = paste0(i, "_", j, "_", k, "_clumps"),
                   ageClasses = ageClasses,
                   vegLeadingTypes = vegLeadingTypes
        )  
      })
    })
  })
  
  lapply(seq_along(ageClasses), function(i) {
    #  i <- match(ages, seq_along(ageClasses))
    lapply(polygonsWithData[ageClass==ageClasses[i]]$V1, function(j) {
      lapply(seq_along(vegLeadingTypes), function(k) {
        callModule(vegAgeMod, paste0(i, "_", j, "_", k), 
                   #indivPolygonIndex = j,
                   #polygonLayer = ecodistricts,
                   listOfProportions = leading[ageClass==ageClasses[i] & 
                                                 polygonNum==j & 
                                                 vegType==vegLeadingTypes[k]]$proportion
                   #vegLeadingType = vegLeadingTypes[k]
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
