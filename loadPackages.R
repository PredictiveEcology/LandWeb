
# reproducible work flow
# 1. Change .libPaths
# 2. Check packageVersions.txt
# 3. Compare packages in directories with packageVersions.csv
# 4. If any are different, install.versions
# 5. 
require <- function(package, forceInstall = FALSE) {
  if (suppressWarnings(!base::require(package, character.only = TRUE))) {
    install.packages(package, dependencies = TRUE)  
  }
  library(package, character.only = TRUE) # needed for the ones that weren's installed when base::require called
}

compareNA <- function(v1,v2) {
  # This function returns TRUE wherever elements are the same, including NA's,
  # and false everywhere else.
  same <- (v1 == v2)  |  (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

newLibPaths <- function(libPath) {
  .libPaths(.libPaths()[length(.libPaths())])
  suppressWarnings(dir.create(libPath))
  .libPaths(libPath)
  .libPaths()
}

if(FALSE) { # THis uses Rcpp to read the DESCRIPTION files from packages https://gist.github.com/hadley/6353939
            # providing a substantial speedup, instead of using the package called "versions" for this 
            # function called installed.versions 
  if(FALSE) {
    library(Rcpp)
    sourceCpp("read-file.cpp") 
  }
  
  installed.versions <- function (pkgs, lib) {
    if (missing(lib) || is.null(lib)) {
      lib <- .libPaths()[1L]
      if (length(.libPaths()) > 1L) 
        message(sprintf(ngettext(length(pkgs), "Checking package in %s\\n(as %s is unspecified)", 
                                 "Checking packages in %s\\n(as %s is unspecified)"), 
                        sQuote(lib), sQuote("lib")), domain = NA)
    }
    if (length(pkgs) > 1) {
      ans <- sapply(pkgs, installed.versions, lib)
      return(ans)
    }
    desc_path <- sprintf("%s/%s/DESCRIPTION", lib, pkgs)
    if (!file.exists(desc_path)) {
      return(NA)
    }
    else {
      lines <- read_file_cpp2(desc_path); 
      lines <- strsplit(lines, split = "\n")[[1]]; 
      vers_line <- lines[grep("^Version: *", lines)]
      vers <- gsub("Version: ", "", vers_line)
      return(vers)
    }
  }
}

instPkgs <- function(gitHubPackages, packageVersionFile, libPath) {
  libPaths1 <- if(missing(libPath)) .libPaths()[1] else libPath
  if(missing(packageVersionFile)) packageVersionFile <- ".packageVersions.txt"
  packageVersionFile <- file.path(libPaths1, packageVersionFile)
  if(file.exists(packageVersionFile)) {
    instPkgs <- dir(libPaths1)
    instVers <- installed.versions(instPkgs, libPaths1)
    inst <- data.frame(instPkgs, instVers, stringsAsFactors = FALSE)
    supposedToBe <- read.table(packageVersionFile, header = TRUE, stringsAsFactors = FALSE)
    together <- merge(supposedToBe, inst, by="instPkgs")
    needInst1 <- setdiff(supposedToBe$instPkgs,inst$instPkgs)
    needInst2 <- which(!compareNA(together$instVers.x, together$instVers.y))
    wh1 <- which(supposedToBe$instPkgs %in% needInst1)
    wh2 <- needInst2
    whPkgsNeeded <- sort(unique(c(wh1,wh2)))
    if(length(whPkgsNeeded)) {
      packages <- supposedToBe$instPkgs[whPkgsNeeded]
      ghPackages <- sapply(strsplit(sapply(strsplit(gitHubPackages, split="/"), function(x) x[2]), split = "@"),function(x) x[1])
      pkgsOmitted <- (ghPackages %in% packages)
      
      whPkgsNeededFromCran <- whPkgsNeeded[!(packages %in% ghPackages)]
      whPkgsNeededGH <- gitHubPackages[pkgsOmitted]
      if(length(whPkgsNeededFromCran))
        install.versions(supposedToBe$instPkgs[whPkgsNeededFromCran], 
                         supposedToBe$instVers[whPkgsNeededFromCran], dependencies = FALSE)
      if(length(whPkgsNeededGH)) {
        lapply(whPkgsNeededGH, function(pkg) {
          devtools::install_github(pkg, upgrade_dependencies = FALSE) 
        })
      }
    }
  } 
}

require("versions")
newLibPaths("Packages")
instPkgs(c("achubaty/amc@development", 
         "PredictiveEcology/reproducible@sideEffectAsPath", 
         "PredictiveEcology/SpaDES.core@downloadData",
         "PredictiveEcology/SpaDES.tools@randomPolygon"))


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
require("amc")
require("reproducible")
require("SpaDES.core")
require("SpaDES.tools")

pkgSnapshot <- function(packageVersionFile, libPath) {
  if(missing(libPath)) libPath <- .libPaths()[1]
  if(missing(packageVersionFile)) packageVersionFile <- ".packageVersions.txt"
  instPkgs <- dir(libPath)
  instVers <- installed.versions(instPkgs, libPath)
  inst <- data.frame(instPkgs, instVers, stringsAsFactors = FALSE)
  write.table(inst, file = file.path(libPath, packageVersionFile), row.names = FALSE)
}

if(FALSE) # only do this when you want a new snapshot taken of the packages installed
  pkgSnapshot(".packageVersions.txt")



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
