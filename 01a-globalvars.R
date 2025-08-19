# global variables ----------------------------------------------------------------------------

.ncores <- min(parallelly::availableCores(), 24L)
.nodename <- Sys.info()[["nodename"]]
.user <- Sys.info()[["user"]]

## allow setting run context info from outside this script (e.g., bash script)

if (exists(".mode", .GlobalEnv)) {
  stopifnot(.mode %in% c("development", "postprocess", "production", "profile"))
} else {
  .mode <- "development"
}

if (exists(".rep", .GlobalEnv)) {
  .rep <- if (.mode == "postprocess") NA_integer_ else as.integer(.rep)
} else {
  .rep <- if (.mode == "postprocess") NA_integer_ else 1L
}

if (exists(".res", .GlobalEnv)) {
  stopifnot(.res %in% c(240, 120))
} else {
  .res <- 240
}

if (!exists(".studyAreaName", .GlobalEnv)) {
  .studyAreaName <- "LandWeb"
}

if (exists(".version", .GlobalEnv)) {
  .version <- as.integer(.version)
  stopifnot(.version %in% c(3L))
} else {
  .version <- 3L
}

if (!exists(".upload", .GlobalEnv)) {
  .upload <- FALSE
}
