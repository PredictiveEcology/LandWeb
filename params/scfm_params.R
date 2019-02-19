defaultInterval <- NA
defaultPlotInterval <- NA
defaultInitialSaveTime <- NA

scfmParams <- list(
  #.progress = list(type = "text", interval = 1),
  scfmIgnition = list(
    pIgnition = 0.0001,
    returnInterval = 1, ## annual fires
    startTime = times$start,
    .plotInitialTime = NA,
    .plotInterval = NA,
    .saveInitialTime = NA,
    .saveInterval = NA),
  scfmEscape = list(
    p0 = 0.05,
    returnInterval = defaultInterval,
    startTime = times$start,
    .plotInitialTime = NA,
    .plotInterval = defaultPlotInterval,
    .saveInitialTime = defaultInitialSaveTime,
    .saveInterval = defaultInterval),
  scfmSpread = list(
    pSpread = 0.235,
    returnInterval = defaultInterval,
    startTime = times$start,
    .plotInitialTime = times$start,
    .plotInterval = defaultPlotInterval,
    .saveInitialTime = defaultInitialSaveTime,
    .saveInterval = defaultInterval)
)
