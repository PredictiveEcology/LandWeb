debugCache <- "complete"

objectsToHash <- reactive({
  if (guaranteedRun) {
    "shpStudySubRegion" # basically only cache on non-.envir objects plus study area
  } else {
    grep("useParallel", ls(mySim()@.envir, all.names = TRUE), value = TRUE, invert = TRUE)
  }
})
