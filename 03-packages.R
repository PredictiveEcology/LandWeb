####################################################################################################
## Packages for global.R
## don't need to load packages for modules; done automatically, but ensure they are installed
####################################################################################################

packageLoadStartTime <- Sys.time()

Require(c("plyr", "dplyr")) ## ensure plyr loaded before dplyr or there will be problems
Require(c("animation", "archive", "assertthat", "devtools", "DBI", "s-u/fastshp",
          "logging", "parallel", "qs", "slackr"), require = FALSE)

shinyPkgs <- c("leaflet", "leaflet.extras", "parallel", "raster", "reactlog", "rgeos",
               "shiny", "shinyBS", "shinycssloaders", "shinydashboard", "shinyjs", "shinyWidgets")
googleAuthPkgs <- c("googleAuthR", "googledrive", "MarkEdmondson1234/googleID")

allPkgs <- unique(c(shinyPkgs, googleAuthPkgs))
Require(allPkgs, require = FALSE)

Require("PredictiveEcology/SpaDES.install")

if (FALSE) {
  installSpatialPackages()
}

#installSpaDES()
makeSureAllPackagesInstalled(modulePath = paths1[["modulePath"]])
