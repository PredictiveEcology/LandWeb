source("01-packages-libPath.R")

if (!require("Require", quietly = TRUE)) {
  install.packages("Require")
  library(Require)
}

.spatialPkgs <- c("lwgeom", "rgdal", "rgeos", "sf", "sp", "raster", "terra")

Require("PredictiveEcology/SpaDES.install@development")
installSpaDES(dontUpdate = .spatialPkgs)

if (FALSE) {
  installSpatialPackages() # repos = "https://cran.r-project.org"
  #install.packages(c("raster", "terra"), repos = "https://rspatial.r-universe.dev")
  sf::sf_extSoftVersion() ## want at least GEOS 3.9.0, GDAL 3.2.1, PROJ 7.2.1
}

Require(c("animation", "archive", "assertthat", "config", "crayon", "devtools", "DBI",
          "s-u/fastshp",
          "PredictiveEcology/LandWebUtils@development",
          "lhs", "logging", "parallel", "qs", "RCurl", "RPostgres", "scales", "slackr", "XML"),
        require = FALSE)
makeSureAllPackagesInstalled(modulePath = "m")

packageLoadStartTime <- Sys.time()

## NOTE: always load packages LAST, after installation above;
##       ensure plyr loaded before dplyr or there will be problems
Require(c("data.table", "plyr", "pryr",
          "PredictiveEcology/LandR@development (>= 1.0.7.9003)",
          "PredictiveEcology/SpaDES.core@development (>= 1.1.0)",
          "archive", "googledrive", "httr", "magrittr", "slackr"), upgrade = FALSE)
