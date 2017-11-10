
## This follows a reproducible work flow:
# 1. need reproducible package 
# 2. change .libPaths to a project specific one
instPack <- installed.packages(.libPaths())["reproducible", "Version"]
if(instPack < "0.1.3.9004") {
  devtools::install_github("PredictiveEcology/reproducible@reproduciblePackages", upgrade_dependencies = FALSE)
}
library(reproducible)
newLibPaths("Packages")
instPkgs(c("RcppCore/Rcpp",
           "achubaty/amc@development", 
           "PredictiveEcology/reproducible@sideEffectAsPath", 
           "PredictiveEcology/SpaDES.core@downloadData",
           "PredictiveEcology/SpaDES.tools@randomPolygon"))

Require(#notOlderThan = Sys.time(),
  c("RcppCore/Rcpp",
    "devtools",
    "data.table",
    "raster",
    "magrittr",
    "rgeos",
    "dplyr",
    "leaflet",
    "shiny",
    "shinydashboard",
    "shinyBS",
    "shinyjs",
    "shinycssloaders",
    "VGAM",
    if (Sys.info()["sysname"] == "Windows") "snow",# Required internally inside "parallel" package for Windows SOCK clusters
    "purrr",
    "gdalUtils",
    "achubaty/amc@development", 
    "PredictiveEcology/reproducible@sideEffectAsPath", 
    "PredictiveEcology/SpaDES.core@downloadData",
    "PredictiveEcology/SpaDES.tools@randomPolygon"))


if(FALSE) # only do this when you want a new snapshot taken of the packages installed
  pkgSnapshot(".packageVersions.txt")



updatePkg <- function(pkg, pkgHash, repo) {
  PkgDescr <- try(read.dcf(system.file(package = pkg, "DESCRIPTION")))
  needPkg <- TRUE
  if ("GithubSHA1" %in% colnames(PkgDescr)) {
    if (grepl(paste0("^",pkgHash), PkgDescr[,"GithubSHA1"])) needPkg <- FALSE
  }
  if (needPkg) install_github(paste0(file.path(repo,pkg),"@", pkgHash))
}
