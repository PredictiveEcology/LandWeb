if (FALSE) { # these are FALSE for standard use, but individual cases may need to run them
  devtools::install_github("PredictiveEcology/quickPlot", ref = "development")
  if (packageVersion("reproducible") < "0.1.4.9004")
    devtools::install_github("PredictiveEcology/reproducible", ref = "development")
  devtools::install_github("PredictiveEcology/webDatabases", ref = "master")
  devtools::install_github("PredictiveEcology/SpaDES.tools", ref = "development")
  devtools::install_github("PredictiveEcology/SpaDES.core", ref = "development")
}
#devtools::install_github("PredictiveEcology/SpaDES.shiny", ref = "develop")
devtools::install_github("PredictiveEcology/SpaDES.shiny", ref = "generalize-modules", dependencies = FALSE)

if (FALSE)
  options(shiny.reactlog = TRUE)

if (FALSE) {
  ## test download of private data from Google Drive
  dataDir <- file.path("~/GitHub/LandWeb/m/Boreal_LBMRDataPrep/data")

  file.remove(c(
    list.files(dataDir, pattern = "SPP_1990_FILLED_100m_NAD83_LCC_BYTE_VEG", full.names = TRUE),
    list.files(dataDir, pattern = "CASFRI", full.names = TRUE)
  ))
}

library(dplyr)
library(gdalUtils)
commonLinuxGdalPath <- "/usr/bin"
commonWindowsGdalPath <- "C:/OSGEO4~1/bin/"
if (dir.exists(commonLinuxGdalPath)) {
  gdal_setInstallation(commonLinuxGdalPath)
} else {
  if (dir.exists(commonWindowsGdalPath)) {
    gdal_setInstallation(commonWindowsGdalPath)
    if (is.null(getOption("gdalUtils_gdalPath"))) {
      gdal_setInstallation(rescan = TRUE)  
    } 
  }
}
if (is.null(getOption("gdalUtils_gdalPath"))) {
  gdal_setInstallation(rescan = TRUE)  
}

  
library(SpaDES.shiny)

#load_all("~/GitHub/SpaDES.shiny")

Modules <- tribble(
  ~type,  ~name, ~id, ~parameters,
  #"shinymodule", "authGoogle", "auth_google", list("authFile = authFile", "appURL = appURL"),
  "shinyModule", "spades_simInit", "sim_init", list(),
  "shinyModule", "spades_expt", "sim_expt", list(),
  "shinyModule", "timeSeriesofRasters", "timeSinceFire", list("rasterList = globalRasters()", "polygonList = polygons()", "shpStudyRegionFull", "colorTable = colorTableFile", "palette = timeSinceFirePalette", "maxAge = maxAge", "sim = mySim()"),
  "shinyModule", "largePatches", "largePatches", list("nSimTimes = length(tsf())", "clumpMod2Args()"),
  "shinyModule", "simInfo", "simInfo", list("mySimOut()[[1]]"),
  "shinyModule", "moduleInfo", "moduleInfo", list("mySimOut()[[1]]"),
  "shinyModule", "inputTables", "inputTables", list()
)

Layout <- tribble( # TODO: add authGoogleUI to ui.R (it's not a menuItem!)
  ~tabName,  ~menuItemName, ~icon, ~moduleId, ~moduleUIParameters,
#  "authGoogle", NA_character_, NA_character_, "auth_google", list(),
  "timeSinceFire", "Maps - time since fire", "map-o", "timeSinceFire", list("length(tsf())"),
  "largePatches", "Large Patches", "bar-chart",  "largePatches", list(),
  "simInfo", "Overview Diagrams", "sitemap", "simInfo", list(),
  "moduleInfo", "Module Info", "puzzle-piece", "moduleInfo", list(),
  "inputTables", "LBMR (Succession) Model Inputs", "table", "inputTables", list()
)

appMetadata2 <- list(
  title = "LandWeb",
  copyright = paste("Her Majesty the Queen in Right of Canada,",
                    "as represented by the Minister of Natural Resources Canada."),
  layout = as.data.frame(Layout),
  modules = as.data.frame(Modules),
  sidebar = list(width = 300, footer = NULL)
)

newApp(getwd(), appMetadata2)

file.copy("global_file.R", "global.R", overwrite = TRUE)

options(shiny.error = browser)
shiny::runApp(".", launch.browser = TRUE, port = 5921)
