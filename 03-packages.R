####################################################################################################
## Packages for global.R
## don't need to load packages for modules; done automatically, but ensure they are installed
####################################################################################################

# install.packages("rgdal", repos="http://R-Forge.R-project.org")
stopifnot(packageVersion("rgdal") >= package_version("1.5-17")) ## need development version

Require("assertthat")
Require("DBI")
Require("plyr"); Require("dplyr") ## ensure plyr loaded before dplyr or there will be problemas
Require("data.table")
Require("magrittr")
Require("parallel")
Require("qs")
Require("raster")

packageLoadStartTime <- Sys.time()
SpaDESPkgs <- c(
  "PredictiveEcology/Require@development",
  "PredictiveEcology/quickPlot@development",
  "PredictiveEcology/reproducible@development",
  "PredictiveEcology/SpaDES.core@development",
  "PredictiveEcology/SpaDES.tools@development",
  "PredictiveEcology/map@development",
  "PredictiveEcology/pemisc@development",
  "PredictiveEcology/map@development",
  "PredictiveEcology/LandR@LandWeb",
  "PredictiveEcology/LandWebUtils@development",
  "achubaty/amc@development"
)
shinyPkgs <- c("gdalUtils", "leaflet", "leaflet.extras", "parallel", "raster", "reactlog", "rgeos",
               "shiny", "shinyBS", "shinycssloaders", "shinydashboard", "shinyjs", "shinyWidgets")
googleAuthPkgs <- c("googleAuthR", "googledrive", "MarkEdmondson1234/googleID")
otherPkgs <- c("animation", "logging", "slackr", "jimhester/archive")

moduleRqdPkgs <- lapply(basename(dir("m")), function(m) {
  SpaDES.core::packages(modules = m, paths = paths1$modulePath)
}) %>%
  unlist() %>%
  unname() %>%
  unique() %>%
  sort()

allPkgs <- unique(c(SpaDESPkgs, shinyPkgs, googleAuthPkgs, otherPkgs, moduleRqdPkgs))

fromCRAN <- names(which(!pemisc::isGitHubPkg(allPkgs))) %>%
  sapply(., function(x) strsplit(x, " ")[[1]][[1]]) %>%
  unname() %>%
  unique()

fromGitHub <- names(which(pemisc::isGitHubPkg(allPkgs))) %>%
  sapply(., function(x) strsplit(x, " ")[[1]][[1]]) %>%
  unname() %>%
  gsub(pattern = "LandR@development", replacement = "LandR@LandWeb", x = .) %>%
  unique()

Require(fromCRAN)
Require(fromGitHub)
