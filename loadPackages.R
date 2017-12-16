
## This follows a reproducible work flow:
packageLibrary <- file.path(dirname(tempdir()), "Packages")
alreadyHasPackageLibrary <- grepl(packageLibrary, .libPaths())
if(any(alreadyHasPackageLibrary)) .libPaths(.libPaths()[!alreadyHasPackageLibrary])
#.libPaths(c(packageLibrary, .libPaths()))
if(!require(devtools)) install.packages("devtools", dependencies = TRUE)
library(devtools)
install_github("PredictiveEcology/reproducible@development", 
               dependencies = TRUE, upgrade_dependencies = FALSE, local=FALSE)
install_github("PredictiveEcology/SpaDES.core@development", 
               dependencies = TRUE, upgrade_dependencies = FALSE, local=FALSE)
suppressWarnings(dir.create(packageLibrary))

library(reproducible) # important to load the one in the libPaths -- or else there will be conflicts 

.libPaths(c(packageLibrary, .libPaths()))
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
          "VGAM",
          if (Sys.info()["sysname"] != "Windows") "Cairo",
          if (Sys.info()["sysname"] == "Windows") "snow",# Required internally inside "parallel" package for Windows SOCK clusters
          "purrr",
          "gdalUtils",
          "achubaty/amc@development", 
          "ecohealthalliance/fasterize",
          "PredictiveEcology/SpaDES.core@development",
          "PredictiveEcology/SpaDES.tools@randomPolygon"
        ), packageVersionFile = file.path(packageLibrary, ".packageVersions.txt"))

data.table::setDTthreads(6)

if(FALSE) # only do this when you want a new snapshot taken of the packages installed
  pkgSnapshot(".packageVersions.txt", libPath = packageLibrary, standAlone = FALSE)
