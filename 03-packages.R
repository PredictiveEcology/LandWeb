####################################################################################################
## Packages for global.R
## don't need to load packages for modules; done automatically, but ensure they are installed
####################################################################################################

library(plyr); library(dplyr) ## ensure plyr loaded before dplyr or there will be problemas
library(data.table)
library(magrittr)
library(parallel)

library(raster)
library(SpaDES.core)
library(pemisc)
library(map) #load_all(file.path(gitLocalPath, "map"))
library(LandR) #devtools::load_all(file.path(gitLocalPath, "LandR"))
library(LandWebUtils) #devtools::load_all(file.path(gitLocalPath, "LandWebUtils"))

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

isGHpkg <- Vectorize(function(x) {
  if (length(strsplit(x, "/")[[1]]) == 1) FALSE else TRUE
})

ghPkgName <- Vectorize(function(x) {
  y <- strsplit(x, "/")[[1]]

  if (length(y) == 1) {
    repo <- NULL
    pkg <- y
    branch <- NULL
  } else {
    z <- strsplit(y[2], "@")[[1]]

    repo <- y[1]
    pkg <- z[1]
    branch <- z[2]
    if (is.na(branch)) branch <- "master"
  }

  return(pkg)
})

moduleRqdPkgs <- lapply(basename(dir(Paths$modulePath)), function(m) {
  packages(modules = m, paths = Paths$modulePath)
}) %>%
  unlist() %>%
  unname() %>%
  unique() %>%
  sort()

fromCRAN <- names(which(!isGHpkg(moduleRqdPkgs)))
fromGitHub <- names(which(isGHpkg(moduleRqdPkgs)))

if (any(!(fromCRAN %in% installed.packages()[, "Package"]))) {
  pkgIds <- which(!(fromCRAN %in% installed.packages()[, "Package"]))
  vapply(fromCRAN[ids], install.packages, NULL)
}

if (any(!(ghPkgName(fromGitHub) %in% installed.packages()[, "Package"]))) {
  pkgIds <- which(!(ghPkgName(fromGitHub) %in% installed.packages()[, "Package"]))
  vapply(fromGitHub[ids], devtools::install_github, NULL)
}
