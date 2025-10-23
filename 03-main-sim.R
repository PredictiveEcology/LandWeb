## main simulation ---------------------------------------------------------------------------------

times3 <- list(start = 0, end = config$args[["simYears"]][["end"]])

modules3 <- list(
  "Biomass_core",
  "LandMine",
  "Biomass_regeneration",
  "LandWeb_output",
  "timeSinceFire"
)

parameters3 <- list(
  .globals = config$params[[".globals"]],
  Biomass_core = config$params[["Biomass_core"]],
  Biomass_regeneration = config$params[["Biomass_regeneration"]],
  LandMine = config$params[["LandMine"]],
  LandWeb_output = config$params[["LandWeb_output"]],
  timeSinceFire = config$params[["timeSinceFire"]]
)

## check pixel resolution
# stopifnot(unique(res(simOutSpeciesLayers[["speciesLayers"]])) %==% config$params[["LandWeb_preamble"]][["pixelSize"]])

objects3 <- list(
  biomassMap = simOutDataPrep[["biomassMap"]],
  cohortData = simOutDataPrep[["cohortData"]],
  # ecoDistrict = simOutDataPrep[["ecoDistrict"]], ## TODO: unused??
  ecoregion = simOutDataPrep[["ecoregion"]],
  ecoregionMap = simOutDataPrep[["ecoregionMap"]],
  fireReturnInterval = simOutPreamble[["fireReturnInterval"]],
  minRelativeB = simOutDataPrep[["minRelativeB"]],
  pixelGroupMap = simOutDataPrep[["pixelGroupMap"]],
  rawBiomassMap = simOutDataPrep[["rawBiomassMap"]],
  rasterToMatch = simOutDataPrep[["rasterToMatch"]],
  rasterToMatch_biomassParam = simOutDataPrep[["rasterToMatch_biomassParam"]],
  ROSTable = simOutPreamble[["ROSTable"]],
  rstFlammable = simOutPreamble[["rstFlammable"]],
  rstLCC = simOutDataPrep[["rstLCC"]],
  rstTimeSinceFire = terra::crop(simOutDataPrep[["standAgeMap"]], simOutDataPrep[["rasterToMatch"]], mask = TRUE),
  species = simOutDataPrep[["species"]],
  speciesEcoregion = simOutDataPrep[["speciesEcoregion"]],
  speciesLayers = simOutDataPrep[["speciesLayers"]],
  speciesParams = simOutDataPrep[["speciesParams"]],
  speciesTable = simOutDataPrep[["speciesTable"]],
  sppColorVect = simOutDataPrep[["sppColorVect"]],
  sppEquiv = simOutDataPrep[["sppEquiv"]],
  standAgeMap = simOutDataPrep[["standAgeMap"]],
  studyArea = simOutDataPrep[["studyArea"]],
  studyArea_biomassParam = simOutDataPrep[["studyArea_biomassParam"]],
  studyAreaReporting = simOutPreamble[["studyAreaReporting"]],
  sufficientLight = simOutDataPrep[["sufficientLight"]]
)

analysesOutputsTimes <- LandWebUtils::analysesOutputsTimes(
  config$params[[".globals"]][["summaryPeriod"]], config$params[[".globals"]][["summaryInterval"]]
)

objectNamesToSave <- c("cohortData", "pixelGroupMap", "standAgeMap", "rstTimeSinceFire", "vegTypeMap")

outputs3a <- data.frame(
  expand.grid(
    objectName = objectNamesToSave,
    saveTime = c(config$args[["timeSeriesTimes"]], analysesOutputsTimes)
  ),
  fun = c("saveRDS", "writeRaster", "writeRaster", "writeRaster", "writeRaster"),
  package = c("base", "terra", "terra", "terra", "terra"),
  file = paste0(objectNamesToSave, c(".rds", ".tif", ".tif", ".tif", ".tif")),
  stringsAsFactors = FALSE
)
outputs3a$arguments <- I(rep(list(
  list(),
  list(overwrite = TRUE, progress = FALSE, filetype = "GTiff"),
  list(overwrite = TRUE, progress = FALSE, datatype = "INT2U", filetype = "GTiff"),
  list(overwrite = TRUE, progress = FALSE, datatype = "INT2U", filetype = "GTiff"),
  list(overwrite = TRUE, progress = FALSE, datatype = "INT1U", filetype = "GTiff")
), times = NROW(outputs3a) / length(objectNamesToSave)))

outputs3b <- data.frame(
  expand.grid(objectName = c("simulationOutput"), saveTime = times3$end),
  fun = c("saveRDS"),
  package = c("base"),
  stringsAsFactors = FALSE
)

outputs3c <- data.frame(
  expand.grid(objectName = c("rstCurrentBurnCumulative", "rstFlammable"), saveTime = times3$end),
  fun = c("writeRaster", "writeRaster"),
  package = c("terra", "terra"),
  arguments = I(
    list(
      list(overwrite = TRUE, progress = FALSE, datatype = "INT2U", filetype = "GTiff"),
      list(overwrite = TRUE, progress = FALSE, datatype = "INT1U", filetype = "GTiff")
    )
  ),
  stringsAsFactors = FALSE
)

outputs3 <- list(outputs3a, outputs3b, outputs3c) |>
  data.table::rbindlist(fill = TRUE) |>
  as.data.frame()

fseed <- file.path(config$paths[["outputPath"]], "seed.rds")
fseed2 <- tools::file_path_sans_ext(fseed) |> paste0(".txt")
if (file.exists(fseed)) {
  seed <- readRDS(fseed)
} else {
  seed <- sample(1e4, 1)
  saveRDS(seed, fseed)
}
print(paste("random seed:", seed))
cat(paste("Setting seed in 03-main-sim.R:", seed), file = fseed2, sep = "\n")
set.seed(seed)
writeRNGInfo(fseed2, append = TRUE)

data.table::setDTthreads(config$params[[".globals"]][[".useParallel"]])

## remove previous log files
f_elog <- file.path(config$paths[["logPath"]], "errors_sim.log")
f_wlog <- file.path(config$paths[["logPath"]], "warnings_sim.log")

if (file.exists(f_elog)) {
  try(unlink(f_elog))
}

if (file.exists(f_wlog)) {
  try(unlink(f_wlog))
}

mainSimFile <- simFile(
  name = "mainSim",
  path = config$paths[["outputPath"]],
  time = config$args[["endTime"]],
  ext = config$args[["fsimext"]]
)

tryCatch(
  withCallingHandlers({
    mainSim <- simInitAndSpades(
      times = times3,
      params = parameters3, ## TODO: use config$params
      modules = modules3, ## TODO: use config$modules
      outputs = outputs3,
      objects = objects3,
      paths = SpaDES.config::paths4spades(config$paths),
      loadOrder = unlist(modules3), ## TODO: use config$modules
      debug = list(
        file = list(
          file = file.path(config$paths[["logPath"]], "03-sim.log"),
          append = TRUE
        ),
        debug = 1
      )
    )
  },
  warning = function(w) {
    cat(w$message, file = f_wlog, append = TRUE)
    invokeRestart("muffleWarning")
  },
  error = function(e) {
    cat(paste("ERROR: ", e$message), file = f_elog, sep = "\n")
    cat("---------------------------------------------------------",
        file = f_elog, append = TRUE, sep = "\n")
    sys.calls() |>
      capture.output(file = f_elog, append = TRUE, split = TRUE)

    cli::col_red(e$message)
  }),
  error = function(e) {
    if (requireNamespace("notifications") & file.exists("~/.rgooglespaces")) {
      notifications::notify_google(
        paste0("ERROR in simulation `", config$context[["runName"]],
               "` on host `", config$context[["machine"]], "`.\n",
               "```\n", e$message, "\n```")
      )
    }
    stop(e$message)
  }
)

mainSim@.xData[["._sessionInfo"]] <- workflowtools::projectSessionInfo(prjDir)

saveSimList(
  mainSim,
  mainSimFile,
  inputs = FALSE,
  outputs = FALSE,
  cache = FALSE,
  files = FALSE
)

# save simulation stats -----------------------------------------------------------------------
elapsed <- elapsedTime(mainSim)
data.table::fwrite(elapsed, file.path(config$paths[["outputPath"]], "elapsedTime.csv"))
saveRDS(elapsed, file.path(config$paths[["outputPath"]], "elapsedTime.rds"))

if (!isFALSE(getOption("spades.memoryUseInterval"))) {
  memory <- memoryUse(mainSim, max = TRUE)
  data.table::fwrite(memory, file.path(config$paths[["outputPath"]], "memoryUsed.csv"))
  saveRDS(memory, file.path(config$paths[["outputPath"]], "memoryUsed.rds"))
}

# end-of-sim cleanup --------------------------------------------------------------------------

## ensure any previously-created ggplot objects get removed from disk; they can be >100GB in size!!
gg_qs <- file.path(config$paths[["outputPath"]], "figures") |>
  list.files(pattern = "_gg[.]qs$", full.names = TRUE)

if (length(gg_qs)) {
  unlink(gg_qs)
}

## cleanup
gc()
terra::tmpFiles(remove = TRUE)

# end-of-sim notifications --------------------------------------------------------------------

if (requireNamespace("notifications") & file.exists("~/.rgooglespaces")) {
  notifications::notify_google(
    paste0("Simulation `", config$context[["runName"]],
           "` completed on host `", config$context[["machine"]], "`",
           if (nzchar(Sys.getenv("STY"))) paste0(" (screen `", Sys.getenv("STY"), "`)"), ".")
  )
}
