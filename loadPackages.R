
## This follows a reproducible work flow:
# 1. need reproducible package 
# 2. change .libPaths to a project specific one
if(!require(devtools)) install.packages("devtools", dependencies = TRUE)
library(devtools)
install_github("PredictiveEcology/reproducible@development", 
               dependencies = TRUE, upgrade_dependencies = FALSE, local=FALSE)
install_github("PredictiveEcology/SpaDES.core@development", 
               dependencies = TRUE, upgrade_dependencies = FALSE, local=FALSE)
packageLibrary <- "Packages1"
dir.create(packageLibrary)
.libPaths(c(packageLibrary, .libPaths()))

library(reproducible) # important to load the one in the libPaths -- or else there will be conflicts 
Require(libPath = packageLibrary, 
        c("devtools",
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
          "PredictiveEcology/SpaDES.core@development",
          "PredictiveEcology/SpaDES.tools@randomPolygon"
        ))#,
#packageVersionFile = ".packageVersions.txt")


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
