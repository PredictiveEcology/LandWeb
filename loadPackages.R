
## This follows a reproducible work flow:
# 1. need reproducible package 
# 2. change .libPaths to a project specific one
packageLibrary <- "Packages2"
dir.create(packageLibrary)
.libPaths(packageLibrary)
instPack <- installed.packages(.libPaths()[1])
needReproducible <- FALSE
if(NROW(instPack)>0) {
  instPack <- instPack["reproducible", "Version"]
  if(instPack < "0.1.3.9006") {
    needReproducible <- TRUE
  }
} else {
  needReproducible <- TRUE
}
if(needReproducible)
  devtools::install_github("PredictiveEcology/reproducible@reproduciblePackages", upgrade_dependencies = TRUE, local = FALSE)

library(reproducible)
Require(libPath = packageLibrary, 
        c("Rcpp",
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
          "tidyr",
          "VGAM",
          if (Sys.info()["sysname"] == "Windows") "snow",# Required internally inside "parallel" package for Windows SOCK clusters
          "purrr",
          "gdalUtils",
          "achubaty/amc@development"#, 
          #"PredictiveEcology/reproducible@reproduciblePackages", 
          #"PredictiveEcology/SpaDES.core@downloadData",
          #"PredictiveEcology/SpaDES.tools@randomPolygon"
          ),
        packageVersionFile = ".packageVersions.txt")


if(FALSE) # only do this when you want a new snapshot taken of the packages installed
  pkgSnapshot(".packageVersions.txt", libPath = packageLibrary)

reproducible::newLibPaths(packageLibrary)

updatePkg <- function(pkg, pkgHash, repo) {
  PkgDescr <- try(read.dcf(system.file(package = pkg, "DESCRIPTION")))
  needPkg <- TRUE
  if ("GithubSHA1" %in% colnames(PkgDescr)) {
    if (grepl(paste0("^",pkgHash), PkgDescr[,"GithubSHA1"])) needPkg <- FALSE
  }
  if (needPkg) install_github(paste0(file.path(repo,pkg),"@", pkgHash))
}
