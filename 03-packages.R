####################################################################################################
## Packages for global.R
## don't need to load packages for modules; done automatically, but ensure they are installed
####################################################################################################

.spatialPkgs <- c("lwgeom", "rgdal", "rgeos", "sf", "sp", "raster", "terra")

packageLoadStartTime <- Sys.time()

Require("PredictiveEcology/SpaDES.install")

if (!all(.spatialPkgs %in% rownames(installed.packages()))) {
  installSpatialPackages()
}

Require(c("plyr", "dplyr")) ## ensure plyr loaded before dplyr or there will be problems
Require(c("animation", "archive", "assertthat", "devtools", "DBI", "s-u/fastshp",
          "logging", "parallel", "qs", "slackr"), require = FALSE)

shinyPkgs <- c("leaflet", "leaflet.extras", "parallel", "raster", "reactlog", "rgeos",
               "shiny", "shinyBS", "shinycssloaders", "shinydashboard", "shinyjs", "shinyWidgets")
googleAuthPkgs <- c("googleAuthR", "googledrive", "MarkEdmondson1234/googleID")

allPkgs <- unique(c(shinyPkgs, googleAuthPkgs))
Require(allPkgs, require = FALSE)

#installSpaDES()
makeSureAllPackagesInstalled(modulePath = paths1[["modulePath"]])
