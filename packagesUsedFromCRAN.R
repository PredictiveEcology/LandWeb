require <- function(package) {
  if(!base::require(package, character.only = TRUE)) install.packages(package, dependencies = TRUE)  
}

require("htmlwidgets")
require("shiny")
require("shinydashboard")
require("shinyBS")
require("leaflet")
require("BH")
require("RCurl")
require("RandomFieldsUtils")
require("R.oo")
require("R.methodsS3")
require("jsonlite")
require("SpaDES")
require("markdown")
require("visNetwork")
require("rgexf")
require("influenceR")
require("DBI")
require("viridis")
require("bit")
require("snow")# required internally inside "parallel" package for Windows SOCK clusters
require("parallel")
require("devtools")
require("raster")
require("rgeos")
require("RSQLite")
require("magrittr")
require("sp")
require("dplyr")
require("ggplot2")
require("maptools")
require("broom")
require("ggvis")
require("rgdal")
require("grid")
require("VGAM")
require("data.table")
require("fpCompare")
require("RandomFieldsUtils")
require("RandomFields")
require("purrr")
rm("require")


updatePkg <- function(pkg, pkgHash, repo) {
  PkgDescr <- try(read.dcf(system.file(package = pkg, "DESCRIPTION")))
  needPkg <- TRUE
  if("GithubSHA1" %in% colnames(PkgDescr)) {
    if(grepl(paste0("^",pkgHash), PkgDescr[,"GithubSHA1"])) needPkg <- FALSE
  }
  if(needPkg) install_github(paste0(file.path(repo,pkg),"@", pkgHash))
}
