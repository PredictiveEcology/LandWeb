if(TRUE) {
  #try(detach("package:SpaDES.shiny", unload = TRUE));
  # try(detach("package:SpaDES.addins", unload = TRUE));
  # try(detach("package:SpaDES.tools", unload = TRUE));
   try(detach("package:SpaDES.core", unload = TRUE));
  # try(detach("package:reproducible", unload = TRUE));
  # devtools::load_all("~/GitHub/reproducible")
   devtools::load_all("~/GitHub/SpaDES.core")
  # devtools::load_all("~/GitHub/SpaDES.tools")
  #devtools::load_all("~/GitHub/SpaDES.shiny")
 }
# Packages for global.R -- don't need to load packages for modules -- happens automatically
library(SpaDES)
library(SpaDES.shiny)
library(raster) 

shinyPkgs <- c("leaflet", "shiny", "shinydashboard", "shinyBS", "shinyjs", "shinycssloaders")
Require(c(# modules
        #modulePkgs,
        if (Sys.info()["sysname"] != "Windows") "Cairo",
        if (Sys.info()["sysname"] == "Windows") "snow",# Required internally inside "parallel" package for Windows SOCK clusters
        # shiny app
        shinyPkgs
        ))

data.table::setDTthreads(6)

if(FALSE) {# only do this when you want a new snapshot taken of the packages installed
  modulePkgs <- unique(unlist(SpaDES.core::packages(module = modules)))
  reproducible::Require(c(# modules
    modulePkgs,
    if (Sys.info()["sysname"] != "Windows") "Cairo",
    if (Sys.info()["sysname"] == "Windows") "snow",# Required internally inside "parallel" package for Windows SOCK clusters
    # shiny app
    shinyPkgs
  ))#, ".packageVersions.txt")
  if (FALSE)
    b <- pkgSnapshot(file.path(getwd(),".packageVersions.txt"), libPath = .libPaths(), standAlone = FALSE)
  
}
