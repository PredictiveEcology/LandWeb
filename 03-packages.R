####################################################################################################
## Packages for global.R
## don't need to load packages for modules; done automatically, but ensure they are installed
####################################################################################################

Require(c("plyr", "dplyr")) ## ensure plyr loaded before dplyr or there will be problems
Require(c("assertthat", "devtools", "DBI", "parallel", "qs", "raster", "rgdal"), require = FALSE)

packageLoadStartTime <- Sys.time()
SpaDESPkgs <- c(
  "PredictiveEcology/Require@development",
  "PredictiveEcology/quickPlot@development",
  "PredictiveEcology/reproducible@development",
  "PredictiveEcology/SpaDES.core@development (>= 1.0.6.9018)",
  "PredictiveEcology/SpaDES.tools@development",
  "PredictiveEcology/SpaDES.project@development",
  "PredictiveEcology/map@development",
  "PredictiveEcology/pemisc@development",
  "PredictiveEcology/map@development",
  "PredictiveEcology/LandR@LandWeb (>= 1.0.2)",
  "PredictiveEcology/LandWebUtils@development (>= 0.1.4)",
  "achubaty/amc@development"
)
shinyPkgs <- c("leaflet", "leaflet.extras", "parallel", "raster", "reactlog", "rgeos",
               "shiny", "shinyBS", "shinycssloaders", "shinydashboard", "shinyjs", "shinyWidgets")
googleAuthPkgs <- c("googleAuthR", "googledrive", "MarkEdmondson1234/googleID")
otherPkgs <- c("animation", "logging", "slackr", "jimhester/archive", "s-u/fastshp")

allPkgs <- unique(c(SpaDESPkgs, shinyPkgs, googleAuthPkgs, otherPkgs))
Require(allPkgs, require = FALSE)

## reinstall spatial packages from source
if (FALSE) {
  install.packages(c("lwgeom", "rgdal", "rgeos", "sf", "sp", "raster", "terra"),
                   repos = "https://cran.rstudio.com")
  rgeos::rgeos_extSoftVersion() ## want GEOS 3.9.0, GDAL 3.2.1, PROJ 7.2.1
}

moduleRqdPkgs <- lapply(basename(dir("m")), function(m) {
  SpaDES.core::packages(modules = m, paths = paths1$modulePath)
}) %>%
  unlist() %>%
  unname() %>%
  unique() %>%
  sort()
Require(moduleRqdPkgs, require = FALSE) ## ensure nistalled, but don't load yet

Require(c("data.table", "SpaDES.core"))
