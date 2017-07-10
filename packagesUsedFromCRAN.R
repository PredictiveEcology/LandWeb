require <- function(package) {
  if(!base::require(package, character.only = TRUE)) install.packages(package, dependencies = TRUE)  
}

require("SpaDES.core")
require("data.table")
require("raster")
require("magrittr")
require("rgeos")
if(Sys.info()["sysname"]=="WindowsSpaDES") require("snow")# required internally inside "parallel" package for Windows SOCK clusters
require("dplyr")
require("leaflet")

if(FALSE) { # these are needed internally by packages. checkpoint needs to see these to install them
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
  require("purrr")
}
rm("require")


updatePkg <- function(pkg, pkgHash, repo) {
  PkgDescr <- try(read.dcf(system.file(package = pkg, "DESCRIPTION")))
  needPkg <- TRUE
  if("GithubSHA1" %in% colnames(PkgDescr)) {
    if(grepl(paste0("^",pkgHash), PkgDescr[,"GithubSHA1"])) needPkg <- FALSE
  }
  if(needPkg) install_github(paste0(file.path(repo,pkg),"@", pkgHash))
}
