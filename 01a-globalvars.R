# global variables ----------------------------------------------------------------------------

.ncores <- min(parallel::detectCores() / 2, 24L)
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
  stopifnot(.res %in% c(50, 125, 250))
} else {
  .res <- 250
}

if (!exists(".studyAreaName", .GlobalEnv)) {
  .studyAreaName <- "LandWeb"
}

if (exists(".version", .GlobalEnv)) {
  .version <- as.integer(.version)
  stopifnot(.version %in% c(2L, 3L))
} else {
  .version <- 2L ## 3L
}

if (.version == 2L) {
  if (exists(".dispersalType", .GlobalEnv)) {
    stopifnot(.dispersalType %in% c("default", "aspen", "high", "none"))
  } else {
    .dispersalType <- "high"
  }

  if (exists(".ROStype", .GlobalEnv)) {
    stopifnot(.ROStype %in% c("default", "burny", "equal", "log"))
  } else {
    .ROStype <- "log"
  }
}
