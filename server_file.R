# THIS IS DANGEROUS, BUT NECESSARY FOR GUARANTEED RUNNING --
#    THIS MEANS that any values of objects will be OK and will trigger a cached return
#    Only shpStudySubRegion and non-object arguments to simInit will make a new run
guaranteedRun <- ifelse(any(c("emcintir") %in% Sys.info()["user"]), TRUE, FALSE)

experimentReps <- reactive({
  1 # Currently, only using 1 -- more than 1 may not work
})

# simInit objects
times4sim <- reactive({
  list(start = 0, end = endTime)
})

modules4sim <- reactive({
  list("landWebDataPrep", "initBaseMaps", "fireDataPrep", "LandMine",
       "Boreal_LBMRDataPrep", "LBMR", "timeSinceFire", "LandWebOutput")#, "makeLeafletTiles")
})

objects4sim <- reactive({
  list("shpStudyRegionFull" = shpStudyRegionFull,
       "shpStudySubRegion" = shpStudyRegion,
       "summaryPeriod" = summaryPeriod,
       "useParallel" = 2) #6
})

parameters4sim <- reactive({
  list(
    LandWebOutput = list(summaryInterval = summaryInterval),
    landWebDataPrep = list(.useCache = eventCaching),
    landWebProprietaryData = list(.useCache = eventCaching),
    Boreal_LBMRDataPrep = list(.useCache = eventCaching),
    LandMine = list(biggestPossibleFireSizeHa = 5e5,
                    fireTimestep = fireTimestep,
                    burnInitialTime = fireTimestep,
                    .plotInitialTime = NA,
                    .useCache = eventCaching),
    LBMR = list(successionTimestep = successionTimestep,
                .plotInitialTime = times4sim()$start,
                .saveInitialTime = NA,
                .useCache = eventCaching),
    initBaseMaps = list(.useCache = eventCaching),
    timeSinceFire = list(startTime = fireTimestep,
                         .useCache = eventCaching),
    fireDataPrep = list(.useCache = eventCaching)
  )
})

outputs4sim <- reactive({
  objectNamesToSave <- c("rstTimeSinceFire", "vegTypeMap")
  outputs <- data.frame(stringsAsFactors = FALSE,
                        expand.grid(
                          objectName = objectNamesToSave,#, "oldBigPatch"),
                          saveTime = seq(objects4sim()$summaryPeriod[1], objects4sim()$summaryPeriod[2],
                                         by = parameters4sim()$LandWebOutput$summaryInterval)),
                        fun = "writeRaster", package = "raster",
                        file = paste0(objectNamesToSave, c(".tif", ".grd")))

  outputs2 <- data.frame(stringsAsFactors = FALSE,
                         expand.grid(objectName = c("simulationOutput"), saveTime = times4sim()$end),
                         fun = "saveRDS",
                         package = "base")

  outputs$arguments <- I(rep(list(list(overwrite = TRUE, progress = FALSE, datatype = "INT2U", format = "GTiff"),
                                  list(overwrite = TRUE, progress = FALSE, datatype = "INT1U", format = "raster")),
                             times = NROW(outputs) / length(objectNamesToSave)))

  as.data.frame(data.table::rbindlist(list(outputs, outputs2), fill = TRUE))
})

## paths for sim
cpath <- reactive({
  if (session$userData$userAuthorized()) {
    paste0("appCache", studyArea, "_AUTH")
  } else {
    paste0("appCache", studyArea)
  }
})

paths4sim <- reactive({
  # debugging/testing paths
  print(paste("User logged in:", session$userData$userLoggedIn()))
  print(paste("User authorized:", session$userData$userAuthorized()))
  print(cpath())

  list(
    cachePath = cpath(),
    modulePath = "m",
    inputPath = "inputs",
    outputPath = paste0("outputs", studyArea)
  )
})

seed <- sample(1e8, 1)
