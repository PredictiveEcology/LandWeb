####################################################################################################
## Packages for global.R
## don't need to load packages for modules; done automatically, but ensure they are installed
####################################################################################################

library(plyr); library(dplyr) ## ensure plyr loaded before dplyr or there will be problemas
library(data.table)
library(magrittr)
library(parallel)
library(qs)

library(raster)
library(SpaDES.core)
library(pemisc)
library(map) #load_all(file.path(gitPkgPath, "map"))
library(LandR) #devtools::load_all(file.path(gitPkgPath, "LandR"))
library(LandWebUtils) #devtools::load_all(file.path(gitPkgPath, "LandWebUtils"))

#devtools::install_github("achubaty/amc@development")
library(amc)

packageLoadStartTime <- Sys.time()
SpaDESPkgs <- c(
  "PredictiveEcology/quickPlot@development",
  "PredictiveEcology/reproducible@development",
  "PredictiveEcology/SpaDES.core@development",
  "PredictiveEcology/SpaDES.tools@development",
  "PredictiveEcology/map@development",
  "PredictiveEcology/pemisc@development",
  "PredictiveEcology/LandR@development",
  "PredictiveEcology/LandWebUtils@development",
  #"PredictiveEcology/SpaDES.shiny@generalize-modules", ## do this after running the model, before app
  "raster"
)
shinyPkgs <- c("gdalUtils", "leaflet", "leaflet.extras", "parallel", "raster", "reactlog", "rgeos",
               "shiny", "shinyBS", "shinycssloaders", "shinydashboard", "shinyjs", "shinyWidgets")
googleAuthPkgs <- c("googleAuthR", "googledrive", "googleID")

moduleRqdPkgs <- lapply(basename(dir("m")), function(m) {
  packages(modules = m, paths = "m")
}) %>%
  unlist() %>%
  unname() %>%
  unique() %>%
  sort()

fromCRAN <- names(which(!pemisc::isGitHubPkg(moduleRqdPkgs)))
fromGitHub <- names(which(pemisc::isGitHubPkg(moduleRqdPkgs)))

if (any(!(fromCRAN %in% installed.packages()[, "Package"]))) {
  pkgIds <- which(!(fromCRAN %in% installed.packages()[, "Package"]))
  install.packages(fromCRAN[pkgIds])
}

if (any(!(pemisc::ghPkgName(fromGitHub) %in% installed.packages()[, "Package"]))) {
  pkgIds <- which(!(pemisc::ghPkgName(fromGitHub) %in% installed.packages()[, "Package"]))
  lapply(fromGitHub[pkgIds], devtools::install_github)
}
