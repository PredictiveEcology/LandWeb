################################################################################
## species layers
################################################################################

do.call(SpaDES.core::setPaths, paths$paths2a)

objects2a <- list(
  cloudFolderID = config.get(config, c("cloud", "cacheDir")),
  rstLCC = simOutPreamble[["LCC"]],
  rasterToMatch = simOutPreamble[["rasterToMatch"]],
  rasterToMatchLarge = simOutPreamble[["rasterToMatchLarge"]],
  speciesLayers = simOutSpeciesLayers[["speciesLayers"]],
  speciesParams = simOutPreamble[["speciesParams"]],
  speciesTable = simOutPreamble[["speciesTable"]],
  speciesTableAreas = c("BSW", "BP", "MC"), ## TODO: should we remove BP? MC?
  sppColorVect = simOutPreamble[["sppColorVect"]],
  sppEquiv = simOutPreamble[["sppEquiv"]],
  standAgeMap = simOutPreamble[["CC TSF"]],
  studyArea = simOutPreamble[["studyArea"]],
  studyAreaLarge = simOutPreamble[["studyAreaLarge"]]
)

dataPrepFile <- file.path(Paths$inputPath, paste0("simOutDataPrep_", substr(config.get(config, c("runInfo", "runName")), 1, 8), ".qs"))
debug(LandR::statsModel) ## TODO: filter out terms in singles that are not in formula
simOutDataPrep <- Cache(simInitAndSpades,
                        times = list(start = 0, end = 1),
                        params = config$params,
                        modules = config$modules$dataPrep,
                        objects = objects2a,
                        omitArgs = c("debug", "paths", ".plotInitialTime"),
                        useCache = TRUE, ## TODO: use param useCache??
                        useCloud = config.get(config, c("args", "cloud", "useCloud")),
                        cloudFolderID = config.get(config, c("args", "cloud", "cacheDir")),
                        ## make .plotInitialTime an argument, not a parameter:
                        ##  - Cache will see them as unchanged regardless of value
                        .plots = config.get(config, c("params", ".globals", ".plots")),
                        paths = paths$paths2a,
                        debug = 1)
simOutDataPrep@.xData[["._sessionInfo"]] <- projectSessionInfo(prjDir)
saveSimList(simOutDataPrep, dataPrepFile, fileBackend = 2)
