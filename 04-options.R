################################################################################
## Options
################################################################################

## cache database connection (requires reproducbile >= 1.0.0)
cacheDBconn <- if (config::get("cachedb") == "sqlite") {
  DBI::dbConnect(drv = RSQLite::SQLite())
} else if (config::get("cachedb") == "postgresql") {
  DBI::dbConnect(drv = RPostgres::Postgres())
} else {
  stop("Unsupported cache database type '", config::get("cachedb"), "'")
}

rep <- config::get("rep")
.plotInitialTime <- if (isTRUE(config::get("plot"))) 0 else NA

maxMemory <- if (grepl("LandWeb", runName)) 5e+12 else 5e+9
scratchDir <- config::get("paths")[["scratchdir"]]

rasterOptions(default = TRUE)
opts <- options(
  "fftempdir" = scratchDir,
  "future.globals.maxSize" = 1000*1024^2,
  "LandR.assertions" = FALSE,
  "LandR.verbose" = 1,
  "map.dataPath" = normPath(paths1$inputPath), # not used yet
  "map.overwrite" = TRUE,
  "map.tilePath" = tilePath,
  "map.useParallel" = mapParallel,
  "rasterMaxMemory" = maxMemory,
  "rasterTmpDir" = scratchDir,
  "reproducible.conn" = cacheDBconn,
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
  "reproducible.useMemoise" = FALSE,
  "reproducible.useGDAL" = FALSE,
  "reproducible.useNewDigestAlgorithm" = TRUE,
  "spades.moduleCodeChecks" = FALSE,
  "spades.nThreads" = 4,
  "spades.recoveryMode" = FALSE,
  "spades.restartR.restartDir" = paths3$outputPath,
  "spades.useRequire" = FALSE # Don't use Require... meaning assume all pkgs installed
)

library(googledrive)

httr::set_config(httr::config(http_version = 0))

token <- if (Sys.info()['nodename'] == "landweb") {
  file.path(activeDir, "landweb-e3147f3110bf.json")
} else {
  NA_character_
} %>%
  normPath(.)

if (is.na(token) || !file.exists(token))
  message(crayon::red("no Google service token found"))

drive_auth(email = config::get("cloud")[["googleuser"]])
#drive_auth(use_oob = quickPlot::isRstudioServer())

message(crayon::silver("Authenticating as: "), crayon::green(drive_user()$emailAddress))
