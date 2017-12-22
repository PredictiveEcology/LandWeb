function(input, output, session) {
  currentPolygon <- callModule(leafletMap, "leafletMap")

  callModule(simInfo, "simInfoTabs", mySimOut[[1]])
  callModule(moduleInfo, "modInfoBoxes", mySimOut[[1]])

  output$timeSinceFireUI <- renderUI({
    tabBox(width = 12,
           tabPanel("Time Since Fire maps", tabName = "timeSinceFireTab",
                    fluidRow(timeSinceFireUI("timeSinceFire", length(tsf)))
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

  callModule(timeSinceFire, "timeSinceFire", rasters = globalRasters, polygonsList = polygons,
             shpStudyRegionFull, colorTableFile, timeSinceFirePalette, maxAge)

  args <- list(clumpMod2, "id1", session = session,
               currentPolygon = polygons[[1 + length(polygons)/4]],
               tsf = tsf, vtm = vtm,
               cl = if(exists("cl")) cl,
               ageClasses = ageClasses, cacheRepo = paths$cachePath,
               patchSize = reactive({input$PatchSize33}),
               largePatchesFn = largePatchesFn)
  args <- args[!unlist(lapply(args, is.null))]
  ClumpsReturn <- do.call(callModule, args )
  rm(args)

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

  clumpMod2Args <- list(
    currentPolygon = polygons[[1 + length(polygons)/4]],
    tsf = tsf, vtm = vtm,
    cl = if(exists("cl")) cl,
    ageClasses = ageClasses, cacheRepo = paths$cachePath,
    largePatchesFn = largePatchesFn, countNumPatches = countNumPatches)
  clumpMod2Args <- clumpMod2Args[!unlist(lapply(clumpMod2Args, is.null))]

  callModule(largePatches, "largePatches", numberOfSimulationTimes = lenTSF, clumpMod2Args)


  output$speciesInputs <- renderDataTable({
    landisInputs
  })#, digits = 1)

  output$speciesEcoregionInputs <- renderDataTable({
    spEcoReg
  })#, digits = 1)

  noLongerWaiting()
  message("  Finished global.R")
}
