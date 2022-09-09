if (file.exists(".Renviron")) readRenviron(".Renviron")

.nodename <- Sys.info()[["nodename"]]
.starttime <- Sys.time()
.user <- Sys.info()[["user"]]

options(Ncpus = min(parallel::detectCores() / 2, 120))

# install and load packages -------------------------------------------------------------------

## TODO: move helper function to SpaDES.project or SpaDES.install
setProjPkgDir <- function(lib.loc = "packages") {
  pkgDir <- Sys.getenv("PRJ_PKG_DIR")
  if (!nzchar(pkgDir)) {
    pkgDir <- lib.loc ## default: use subdir within project directory
  }
  pkgDir <- normalizePath(
    file.path(pkgDir, version$platform, paste0(version$major, ".", strsplit(version$minor, "[.]")[[1]][1])),
    winslash = "/",
    mustWork = FALSE
  )

  if (!dir.exists(pkgDir)) {
    dir.create(pkgDir, recursive = TRUE)
  }

  .libPaths(pkgDir)
  message("Using libPaths:\n", paste(.libPaths(), collapse = "\n"))
}

setProjPkgDir("packages")

if (!require("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

Require.version <- "PredictiveEcology/Require@archivedPkg" ## testing
if (!"Require" %in% rownames(installed.packages())) {
  remotes::install_github(Require.version)
} else if (packageVersion("Require") < "0.1.0.9000") {
  remotes::install_github(Require.version)
}
library(Require)

.spatialPkgs <- c("lwgeom", "rgdal", "rgeos", "sf", "sp", "raster", "terra")

Require("PredictiveEcology/SpaDES.install@development (>= 0.0.9.9002)")

setLinuxBinaryRepo()
installSpaDES(dontUpdate = .spatialPkgs)

if (FALSE) {
  installSpatialPackages() # repos = "https://cran.r-project.org"
  # install.packages("pak")
  # pak::pkg_install(.spatialPkgs)
  # install.packages(.spatialPkgs, repos = "https://cran.r-project.org")
  # install.packages(c("raster", "terra"), repos = "https://rspatial.r-universe.dev")
  sf::sf_extSoftVersion() ## want at least GEOS 3.9.0, GDAL 3.2.1, PROJ 7.2.1
}

modulePkgs <- makeSureAllPackagesInstalled(modulePath = "m", doInstalls = FALSE)
otherPkgs <- c("animation", "archive", "assertthat", "config", "crayon", "devtools", "DBI", "s-u/fastshp",
               "lhs", "logging", "parallel", "qs", "RCurl", "RPostgres", "scales", "slackr", "XML")

Require(unique(c(modulePkgs, otherPkgs)), require = FALSE, upgrade = FALSE, standAlone = TRUE)

.packageLoadStartTime <- Sys.time()

## NOTE: always load packages LAST, after installation above;
##       ensure plyr loaded before dplyr or there will be problems
Require(c("data.table", "plyr", "pryr",
          "PredictiveEcology/LandR@development (>= 1.0.7.9003)",
          "PredictiveEcology/SpaDES.core@development (>= 1.1.0.9000)",
          "archive", "googledrive", "httr", "magrittr", "slackr"), upgrade = FALSE, standAlone = TRUE)

# configure project ---------------------------------------------------------------------------
stopifnot(exists("runName", envir = .GlobalEnv)) ## run name should be set
source("02-config.R")

message(crayon::red(runName))

# define simulation paths ---------------------------------------------------------------------
stopifnot(identical(checkPath(config$paths$projectPath), getwd()))

config$paths <- Require::modifyList2(
  config$paths,
  list(
    outputPath = file.path(config$paths$outputPath, runName),
    tilePath = file.path("outputs", runName, "tiles")
  )
)

config$paths <- lapply(config$paths, function(p) {
  if (!is.null(p)) checkPath(p, create = TRUE) else NULL
})

## extract only the subset accepted by SpaDES.core::setPaths()
paths4spades <- function(paths) {
  want <- grep("Path", names(formals(SpaDES.core::setPaths)), value = TRUE)
  paths[which(names(paths) %in% want)]
}

paths <- list(
  paths1 = paths4spades(config.get(config, "paths")),  ## preamble
  paths2 = paths4spades(config.get(config, "paths")),  ## species layers
  paths2a = paths4spades(config.get(config, "paths")), ## boreal data prep
  paths3 = paths4spades(config.get(config, "paths"))   ## main simulation
)

paths$paths2[["cachePath"]] <- file.path(config.get(config, c("paths", "cachePath")), "dataPrepGIS", "speciesLayers")
paths$paths2a[["cachePath"]] <- file.path(config.get(config, c("paths", "cachePath")), "dataPrepGIS", "borealDataPrep")
paths$paths3[["cachePath"]] <- file.path(config.get(config, c("paths", "cachePath")), config$runInfo$runName)

# set package options -------------------------------------------------------------------------
raster::rasterOptions(default = TRUE)
opts <- options(config.get(config, "options"))
httr::set_config(httr::config(http_version = 0))

## TODO: move these helper functions to package SpaDES.project
findToken <- function(name) {
  Require::normPath(list.files(".", paste0(name, "-.*[.]json")[1]))
}
hasToken <- function(name) {
  token <- findToken(name)
  all(isTRUE(length(token) == 1), !is.na(token))
}

if (hasToken("landweb")) {
  drive_auth(path = findToken("landweb"))
} else {
  message(crayon::red("No Google service account token found. Trying user authentication..."))
  drive_auth(email = config$cloud$googleuser, use_oob = quickPlot::isRstudioServer())
}

message(crayon::silver("Authenticating as: "), crayon::green(drive_user()$emailAddress))

if (config.get(config, "delayStart") > 0) {
  message(crayon::green("\nStaggered job start: delaying by", as.integer(config.get(config, "delayStart")), "minutes."))
  Sys.sleep(config.get(config, "delayStart")*60)
}

# run pre-sim data prep modules ---------------------------------------------------------------
source("06-preamble.R")
source("07-speciesLayers.R")

message(crayon::red(config.get(config, c("runInfo", "runName"))))

if (isFALSE(config.get(config, "postProcessOnly"))) {
  source("08-borealDataPrep.R")
  source("09-pre-sim.R")

  if (isFALSE(config.get(config, c("POM", "usePOM")))) {
    source("10-main-sim.R")
    #source("11-post-sim.R")
  } else {
    source("10a-POM.R") ## TODO: may not work out-of-the-box; untested!!
  }
} else {
  #mySimOut <- loadSimList(simFile("mySimOut", Paths$outputPath, 1000))
  source("12-postprocessing.R")
}

#source("11-post-sim.R")

if (FALSE) {
  source("13-old.R")
}
