source("01-packages-libPath.R")

Require.version <- "PredictiveEcology/Require@archivedPkg" ## testing
if (!"Require" %in% rownames(installed.packages())) {
  remotes::install_github(Require.version)
} else if (packageVersion("Require") < "0.1.0.9014") {
  remotes::install_github(Require.version)
}
library(Require)

.spatialPkgs <- c("lwgeom", "rgdal", "rgeos", "sf", "sp", "raster", "terra")

## TODO: SpaDES.install will be defunct -- use SpaDES.project
Require("PredictiveEcology/SpaDES.install@development (>= 0.0.9.9002)", standAlone = TRUE, upgrade = FALSE)

if (FALSE) {
  installSpatialPackages() # repos = "https://cran.r-project.org"
  #install.packages(c("raster", "terra"), repos = "https://rspatial.r-universe.dev")
  sf::sf_extSoftVersion() ## want at least GEOS 3.9.0, GDAL 3.2.1, PROJ 7.2.1
}

modPkgs <- makeSureAllPackagesInstalled(modulePath = "m", doInstalls = FALSE)
otherPkgs <- c("animation", "archive", "assertthat", "config", "crayon", "devtools", "DBI",
               "s-u/fastshp",
               "PredictiveEcology/LandWebUtils@development",
               "lhs", "logging", "parallel", "qs", "RCurl", "RPostgres", "scales", "slackr", "XML")
Require(c(modPkgs, otherPkgs), require = FALSE, standAlone = TRUE, upgrade = FALSE)

packageLoadStartTime <- Sys.time()

## NOTE: always load packages LAST, after installation above;
##       ensure plyr loaded before dplyr or there will be problems
Require(c("data.table", "plyr", "pryr",
          "PredictiveEcology/LandR@development (>= 1.0.7.9003)",
          "PredictiveEcology/SpaDES.core@development (>= 1.1.0)",
          "archive", "googledrive", "httr", "magrittr", "slackr"), standAlone = TRUE, upgrade = FALSE)
