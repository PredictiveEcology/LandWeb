if (FALSE) {
  try(detach("package:SpaDES.shiny", unload = TRUE));
  #try(detach("package:SpaDES.addins", unload = TRUE));
  try(detach("package:SpaDES.tools", unload = TRUE));
  try(detach("package:SpaDES.core", unload = TRUE));
  #try(detach("package:reproducible", unload = TRUE));
  #devtools::load_all("~/Documents/GitHub/reproducible")
  devtools::load_all("~/Documents/GitHub/SpaDES.core")
  devtools::load_all("~/GitHub/SpaDES.tools")
  #devtools::load_all("~/GitHub/SpaDES.shiny")
}

# Packages for global.R -- don't need to load packages for modules -- happens automatically
library(reproducible)

SpaDESPkgs <- c(
  "PredictiveEcology/SpaDES.core@development",
  "PredictiveEcology/SpaDES.tools@prepInputs",
  "PredictiveEcology/SpaDES.shiny@develop",
  "raster"
)
shinyPkgs <- c("leaflet", "gdalUtils", "rgeos", "raster",
               "shiny", "shinydashboard", "shinyBS", "shinyjs", "shinycssloaders")
Require(c(
  SpaDESPkgs,
  if (Sys.info()["sysname"] != "Windows") "Cairo",
  # `snow` required internally by `parallel` for Windows SOCK clusters
  if (Sys.info()["sysname"] == "Windows") "snow",
  # shiny app
  shinyPkgs
))

data.table::setDTthreads(6)

# only do this when you want a new snapshot taken of the packages installed
if (FALSE) {
  modulePkgs <- unique(unlist(SpaDES.core::packages(module = modules)))
  reproducible::Require(c(# modules
    modulePkgs,
    if (Sys.info()["sysname"] != "Windows") "Cairo",
    # `snow` required internally by `parallel` for Windows SOCK clusters
    if (Sys.info()["sysname"] == "Windows") "snow",
    # shiny app
    shinyPkgs
  ))#, ".packageVersions.txt")

  if (FALSE)
    b <- pkgSnapshot(file.path(getwd(),".packageVersions.txt"), libPath = .libPaths(), standAlone = FALSE)
}
