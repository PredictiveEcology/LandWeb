# project basics ------------------------------------------------------------------------------

if (file.exists(".Renviron")) readRenviron(".Renviron") ## GITHUB_PAT and database credentials

.nodename <- Sys.info()[["nodename"]]
.starttime <- Sys.time()
.user <- Sys.info()[["user"]]

prjDir <- "~/GitHub/LandWeb"

options(
  Ncpus = min(parallel::detectCores() / 2, 120),
  repos = c(CRAN = "https://cran.rstudio.com"),
  Require.RPackageCache = "default", ## will use default package cache directory: `RequirePkgCacheDir()`
  Require.usepak = FALSE ## pkg deps too complicated for pak
)

# install and load packages -------------------------------------------------------------------

pkgDir <- file.path("packages", version$platform, substr(getRversion(), 1, 3))
dir.create(pkgDir, recursive = TRUE, showWarnings = FALSE)
.libPaths(pkgDir, include.site = FALSE)
message("Using libPaths:\n", paste(.libPaths(), collapse = "\n"))

if (!"remotes" %in% rownames(installed.packages(lib.loc = .libPaths()[1]))) {
  install.packages("remotes")
}

Require.version <- "PredictiveEcology/Require@development"
if (!"Require" %in% rownames(installed.packages(lib.loc = .libPaths()[1])) ||
    packageVersion("Require", lib.loc = .libPaths()[1]) < "0.1.2") {
  remotes::install_github(Require.version)
}
library(Require)

setLinuxBinaryRepo()

Require("PredictiveEcology/SpaDES.project@transition (>= 0.0.7)", ## TODO: use development once merged
        upgrade = FALSE, standAlone = TRUE)

if (FALSE) {
  .spatialPkgs <- c("raster", "sf", "sp", "terra", "rgdal", "s2", "units", "lwgeom")
  # install.packages("pak")
  # pak::pkg_install(.spatialPkgs)
  # install.packages(.spatialPkgs, repos = "https://cran.r-project.org")
  # install.packages(c("raster", "terra"), repos = "https://rspatial.r-universe.dev")
  sf::sf_extSoftVersion() ## want at least GEOS 3.9.0, GDAL 3.2.1, PROJ 7.2.1
}

modulePkgs <- unname(unlist(packagesInModules(modulePath = file.path(prjDir, "m"))))
otherPkgs <- c("animation", "archive", "assertthat", "config", "crayon", "devtools", "DBI",
               "s-u/fastshp",
               "PredictiveEcology/LandR@development (>= 1.1.0.9001)",
               "PredictiveEcology/LandWebUtils@development",
               "lhs", "logging", "parallel", "qs", "RCurl", "RPostgres",
               "PredictiveEcology/SpaDES.core@development (>= 1.1.0.9000)",
               "scales", "slackr", "XML")

Require(unique(c(modulePkgs, otherPkgs)), require = FALSE, standAlone = TRUE, upgrade = FALSE)

## NOTE: always load packages LAST, after installation above;
##       ensure plyr loaded before dplyr or there will be problems
Require(c("data.table", "plyr", "pryr", "SpaDES.core",
          "archive", "googledrive", "httr", "magrittr", "slackr"), upgrade = FALSE, standAlone = TRUE)

# configure project ---------------------------------------------------------------------------
stopifnot(exists("runName", envir = .GlobalEnv)) ## run name should be set
source("02-config.R")

# print run info ------------------------------------------------------------------------------

message(
  config.get(config, c("runInfo", "runName"))
  #"Run information:\n",
  #lapply(names(config$runInfo), function(x) {
  #  paste(paste0("  ", x, ":\t"), config$runInfo[[x]], "\n")
  #})
)

# define simulation paths ---------------------------------------------------------------------
stopifnot(identical(checkPath(config.get(config, c("paths", "projectPath"))), getwd()))

config$paths <- lapply(config.get(config, "paths"), function(p) {
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
  paths3 = paths4spades(config.get(config, "paths")),   ## main simulation
  paths4 = paths4spades(config.get(config, "paths"))   ## post-processing
)

paths$paths2[["cachePath"]] <- file.path(config.get(config, c("paths", "cachePath")), "dataPrepGIS", "speciesLayers")
paths$paths2a[["cachePath"]] <- file.path(config.get(config, c("paths", "cachePath")), "dataPrepGIS", "borealDataPrep")
paths$paths3[["cachePath"]] <- file.path(config.get(config, c("paths", "cachePath")), config.get(config, c("runInfo", "runName")))
paths$paths4[["cachePath"]] <- file.path(config.get(config, c("paths", "cachePath")), "postprocessing")
paths$paths4[["outputPath"]] <- checkPath(file.path("outputs", config.get(config, c("runInfo", "runNamePostProcess"))), create = TRUE)

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
  source("10-main-sim.R")
} else {
  #mySimOut <- loadSimList(simFile("mySimOut", Paths$outputPath, 1000))
  source("12-postprocessing.R")
}

SpaDES.project::reproducibilityReceipt()

#source("11-post-sim.R")
