require <- function(package, forceInstall = FALSE) {
  if (package %in% c("amc")) {
    devtools::install_github(paste0("achubaty/", package, "@development"))
  } else if (package %in% c("reproducible")) {
    devtools::install_github(paste0("PredictiveEcology/", package, "@sideEffectAsPath"))
    #devtools::install(file.path("~","GitHub", package, "."))
  } else if (package %in% c("SpaDES.tools")) {
    devtools::install_github(paste0("PredictiveEcology/", package, "@randomPolygon"))
    #devtools::install(file.path("~","GitHub", package, "."), dependencies = FALSE)
  } else if (package %in% c("SpaDES.core")) {
    devtools::install_github(paste0("PredictiveEcology/", package, "@downloadData"))
    #devtools::install(file.path("~","GitHub", package, "."), dependencies = FALSE)
  }
  
  if (!base::require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)  
  }
  library(package, character.only = TRUE)
  
}

require("reproducible")
require("SpaDES.core")
require("SpaDES.tools")
require("devtools")
require("data.table")
require("raster")
require("magrittr")
require("rgeos")
if (Sys.info()["sysname"] == "Windows") require("snow")# required internally inside "parallel" package for Windows SOCK clusters
require("dplyr")
require("leaflet")
require("shiny")
require("shinydashboard")
require("shinyBS")
require("shinyjs")
require("shinycssloaders")
require("purrr")
require("gdalUtils")

if (FALSE) { # these are needed internally by packages. checkpoint needs to see these to install them
  require("htmlwidgets")
  require("shinydashboard")
  require("shinyBS")
  require("BH")
  require("RCurl")
  require("RandomFieldsUtils")
  require("R.oo")
  require("R.methodsS3")
  require("jsonlite")
  require("devtools")
  require("rgdal")
  require("RSQLite")
  require("sp")
  require("reproducible")
  require("quickPlot")
  require("bit")
  require("markdown")
  require("visNetwork")
  require("rgexf")
  require("influenceR")
  require("DBI")
  require("viridis")
  require("parallel")
  require("ggplot2")
  require("maptools")
  require("broom")
  require("ggvis")
  require("grid")
  require("VGAM")
  require("fpCompare")
  require("RandomFieldsUtils")
  require("RandomFields")
}
rm("require")

updatePkg <- function(pkg, pkgHash, repo) {
  PkgDescr <- try(read.dcf(system.file(package = pkg, "DESCRIPTION")))
  needPkg <- TRUE
  if ("GithubSHA1" %in% colnames(PkgDescr)) {
    if (grepl(paste0("^",pkgHash), PkgDescr[,"GithubSHA1"])) needPkg <- FALSE
  }
  if (needPkg) install_github(paste0(file.path(repo,pkg),"@", pkgHash))
}
