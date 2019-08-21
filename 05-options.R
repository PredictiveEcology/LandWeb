################################################################################
## Options
################################################################################

rep <- as.integer(substr(runName, nchar(runName) - 1, nchar(runName)))
.plotInitialTime <- if (is.na(rep)) NA else if (user("emcintir")) 0 else if (user("achubaty") && rep == 1) 0 else NA

maxMemory <- if (grepl("LandWeb", runName)) 5e+12 else 5e+9
scratchDir <- if (dir.exists(computeCanadaScratch)) {
  computeCanadaScratch
} else {
  file.path("/tmp/scratch/LandWeb")
}

rasterOptions(default = TRUE)
opts <- options(
  "fftempdir" = scratchDir,
  "future.globals.maxSize" = 1000*1024^2,
  "LandR.assertions" = if (user("emcintir")) FALSE else FALSE,
  "LandR.verbose" = if (user("emcintir")) 1 else 1,
  "map.dataPath" = normPath(paths1$inputPath), # not used yet
  "map.overwrite" = TRUE,
  "map.tilePath" = tilePath,
  "map.useParallel" = mapParallel,
  "rasterMaxMemory" = maxMemory,
  "rasterTmpDir" = scratchDir,
  "reproducible.destinationPath" = normPath(paths1$inputPath),
  #"reproducible.devMode" = if (user("emcintir")) TRUE else FALSE,
  "reproducible.futurePlan" = if (.Platform$OS.type != "windows" && user("emcintir")) FALSE else FALSE,
  "reproducible.inputPaths" = if (user("emcintir")) path.expand("~/data") else NULL,
  "reproducible.overwrite" = TRUE,
  "reproducible.quick" = FALSE,
  "reproducible.showSimilar" = TRUE,
  "reproducible.useCache" = if (pemisc::user("emcintir")) TRUE else TRUE,
  "reproducible.useCloud" = TRUE,
  "reproducible.useGDAL" = FALSE, ## NOTE: gdal is faster, but mixing gdal with raster causes inconsistencies
  "reproducible.useMemoise" = ifelse(isTRUE(batchMode), FALSE, if (pemisc::user("emcintir")) FALSE else TRUE),
  "reproducible.useGDAL" = FALSE,
  "reproducible.useNewDigestAlgorithm" = TRUE,
  "spades.moduleCodeChecks" = FALSE,
  "spades.recoveryMode" = FALSE,
  "spades.useRequire" = FALSE # Don't use Require... meaning assume all pkgs installed
)

httr::set_config(httr::config(http_version = 0))
if (dir.exists(computeCanadaScratch)) {
  if (utils::packageVersion("googledrive") < "1.0.0")
    googledrive::drive_auth(service_token = file.path(activeDir, "landweb-82e0f9f29fbc.json"))
  else
    googledrive::drive_auth(path = file.path(activeDir, "landweb-82e0f9f29fbc.json"))
}

