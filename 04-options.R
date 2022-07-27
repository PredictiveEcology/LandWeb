################################################################################
## Options
################################################################################

## cache database connection (requires reproducbile >= 1.0.0)
cacheDBconn <- if (config::get("cachedb") == "sqlite") {
  Require("RSQLite")
  NULL ## should default to sqlite
} else if (config::get("cachedb") == "postgresql") {
  Require("RPostgres")
  DBI::dbConnect(drv = RPostgres::Postgres(),
                 host = Sys.getenv("PGHOST"),
                 port = Sys.getenv("PGPORT"),
                 dbname = Sys.getenv("PGDATABASE"),
                 user = Sys.getenv("PGUSER"),
                 password = Sys.getenv("PGPASSWORD"))
} else {
  stop("Unsupported cache database type '", config::get("cachedb"), "'")
}

maxMemory <- if (grepl("LandWeb", runName)) 5e+12 else 5e+9

raster::rasterOptions(default = TRUE)
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
  "reproducible.cachePath" = file.path(scratchDir, "cache"),
  "reproducible.cacheSaveFormat" = "rds", ## can be "qs" or "rds"
  "reproducible.conn" = cacheDBconn,
  "reproducible.destinationPath" = normPath(paths1$inputPath),
  "reproducible.futurePlan" = if (.Platform$OS.type != "windows" && peutils::user("emcintir")) FALSE else FALSE,
  "reproducible.inputPaths" = if (peutils::user("emcintir")) normPath("~/data") else NULL,
  "reproducible.nThreads" = 2,
  "reproducible.overwrite" = TRUE,
  "reproducible.polygonShortcut" = FALSE, ## TODO: work with Eliot to debug why this is needed
  "reproducible.quick" = FALSE,
  "reproducible.showSimilar" = TRUE,
  "reproducible.useCache" = TRUE,
  "reproducible.useCloud" = TRUE,
  "reproducible.useGDAL" = FALSE, ## NOTE: gdal is faster, but mixing gdal with raster causes inconsistencies
  "reproducible.useMemoise" = FALSE,
  "reproducible.useNewDigestAlgorithm" = 2, ## this should solve most/all file-backed raster issues
  "reproducible.useRequire" = FALSE,
  #"reproducible.useTerra" = TRUE, ## TODO: update + test with terra
  "spades.messagingNumCharsModule" = 36,
  "spades.moduleCodeChecks" = FALSE,
  "spades.nThreads" = 4,
  "spades.recoveryMode" = FALSE,
  "spades.restartR.restartDir" = paths3$outputPath,
  "spades.useRequire" = FALSE # Don't use Require... meaning assume all pkgs installed
)

httr::set_config(httr::config(http_version = 0))

token <- Require::normPath(list.files(".", "landweb-.*[.]json")[1])
haveToken <- all(isTRUE(length(token) == 1), !is.na(token))

if (haveToken) {
  drive_auth(path = token)
} else {
  message(crayon::red("No Google service account token found. Trying user authentication..."))

  drive_auth(email = config::get("cloud")[["googleuser"]], use_oob = quickPlot::isRstudioServer())
}

message(crayon::silver("Authenticating as: "), crayon::green(drive_user()$emailAddress))
