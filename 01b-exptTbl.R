## TODO: implement exptTbl stuff to pass values to config
if (FALSE) {
  fExptTbl <- file.path(prjDir, "experimentTable.csv")
  if (!file.exists(fExptTbl)) {
    exptTbl <- expand.grid(
      .studyAreaName = c("ANC", "AlPac", "BlueRidge", "DMI", "Edson", "FMANWT",
                         "LandWeb", "LP_BC", "LP_MB",
                         "Manning", "MillarWestern", "Mistik", "SprayLake", "Sundre", "Tolko", ## TODO: check e.g. Tolko_SK etc.
                         "Vanderwell", "WeyCo", "WestFraser",
                         "provAB", "provMB", "provNWT", "provSK"),
      delayStart = TRUE,
      dispersalType = c("default"),
      endTime = 1000,
      forceResprout = c(FALSE),
      friMultiple = c(1L),
      pixelSize = 250,
      rep = c(1L:15L, NA_integer_), ## NA for postprocessing runs
      ROStype = c("default"),
      succession = c(TRUE)
    )
    exptTbl$postProcessOnly <- FALSE

    ## postprocessing runs -----------
    exptTbl[is.na(exptTbl$rep), ]$postProcessOnly <- TRUE
    exptTbl[is.na(exptTbl$rep), ]$delayStart <- FALSE

    ## scheduling --------------------
    exptTbl$._targetMachine <- NA
    exptTbl[exptTbl$.studyAreaName %in% c("LandWeb", "provMB"), ]$._targetMachine <- "pseudotsuga.for-cast.ca"

    exptTbl$._targetMemory <- NA

    ## status and tracking -----------
    ## TODO: populate these from completed sims for MB
    exptTbl$._status <- NA ## "queued", "started", "completed", "error"
    exptTbl$._runtime <- NA
    exptTbl$._memory <- NA

    write.csv(exptTbl, fExptTbl)
  } else {
    ## TODO: local csv; google sheet; database
    #exptTbl <- getExperimentTable(fExptTbl)
    exptTable <- read.csv(fExptTbl)
  }
}
