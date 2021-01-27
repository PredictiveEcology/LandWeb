####################################################################################################
## Packages for global.R
## don't need to load packages for modules; done automatically, but ensure they are installed
####################################################################################################

# install.packages("rgdal", repos="http://R-Forge.R-project.org")
stopifnot(packageVersion("rgdal") >= package_version("1.5-17")) ## need development version

library(assertthat)
library(DBI)
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
  "PredictiveEcology/Require@development",
  "PredictiveEcology/quickPlot@development",
  "PredictiveEcology/reproducible@development",
  "PredictiveEcology/SpaDES.core@development",
  "PredictiveEcology/SpaDES.tools@development",
  "PredictiveEcology/map@development",
  "PredictiveEcology/pemisc@development",
  "PredictiveEcology/LandR@LandWeb",
  "PredictiveEcology/LandWebUtils@development",
  #"PredictiveEcology/SpaDES.shiny@generalize-modules", ## do this after running the model, before app
  "raster"
)
shinyPkgs <- c("gdalUtils", "leaflet", "leaflet.extras", "parallel", "raster", "reactlog", "rgeos",
               "shiny", "shinyBS", "shinycssloaders", "shinydashboard", "shinyjs", "shinyWidgets")
googleAuthPkgs <- c("googleAuthR", "googledrive", "MarkEdmondson1234/googleID")

moduleRqdPkgs <- lapply(basename(dir("m")), function(m) {
  packages(modules = m, paths = paths1$modulePath)
}) %>%
  unlist() %>%
  unname() %>%
  unique() %>%
  sort()

allPkgs <- unique(c(SpaDESPkgs, shinyPkgs, googleAuthPkgs, moduleRqdPkgs))

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
