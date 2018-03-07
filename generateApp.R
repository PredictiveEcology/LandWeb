if (FALSE) { # these are FALSE for standard use, but individual cases may need to run them 
  devtools::install_github("PredictiveEcology/quickPlot", ref = "development") 
  devtools::install_github("PredictiveEcology/reproducible", ref = "development") 
  devtools::install_github("PredictiveEcology/webDatabases", ref = "master") 
  devtools::install_github("PredictiveEcology/SpaDES.tools", ref = "development") 
  devtools::install_github("PredictiveEcology/SpaDES.core", ref = "development") 
} 
devtools::install_github("PredictiveEcology/SpaDES.shiny", ref = "develop")

if (FALSE) {
  ## test download of private data from Google Drive
  dataDir <- file.path("~/GitHub/LandWeb/m/Boreal_LBMRDataPrep/data")

  file.remove(c(
    list.files(dataDir, pattern = "SPP_1990_FILLED_100m_NAD83_LCC_BYTE_VEG", full.names = TRUE),
    list.files(dataDir, pattern = "CASFRI", full.names = TRUE)
  ))
}

library(dplyr)
library(gdalUtils); gdal_setInstallation(rescan = TRUE)
library(SpaDES.shiny)

#load_all("~/GitHub/SpaDES.shiny")

# appMetadata <- list(
#   modules = data.frame(
#     type = c("shinyModule", "shinyModule", "shinyModule", "shinyModule", "shinyModule"),
#     name = c("timeSeriesofRasters", "largePatches", "simInfo", "moduleInfo", "inputTables"),
#     id = c("timeSinceFire", "largePatches", "simInfo", "moduleInfo", "inputTables"),
#     stringsAsFactors = FALSE
#   ),
#   layout = data.frame(
#     tabName = c("timeSinceFire", "largePatches", "simInfo", "moduleInfo", "inputTables"),
#     menuItemName = c("Maps - time since fire", "Large Patches", "Overview Diagrams", "Module Info", "LBMR (Succession) Model Inputs"),
#     icon = c("map-o", "bar-chart", "sitemap", "puzzle-piece", "table"),
#     moduleId = c("timeSinceFire", "largePatches", "simInfo", "moduleInfo", "inputTables"),
#     stringsAsFactors = FALSE
#   )
# )
# appMetadata$modules$parameters <- list(
#   list("rasters = globalRasters", "polygonsList = polygons", "shpStudyRegionFull",
#        "colorTableFile", "timeSinceFirePalette", "maxAge"),
#   list("numberOfSimulationTimes = lenTSF", "clumpMod2Args"),
#   list("mySimOut[[1]]"),
#   list("mySimOut[[1]]"),
#   list()
# )
# appMetadata$layout$moduleUIParameters <- list(list("length(tsf)"), list(), list(), list(), list())
#
# # newApp(getwd(), appMetadata)
# #
# # file.copy("global_file.R", "global.R", overwrite = TRUE)
# # setwd("~/GitHub/LandWeb/")
# # shiny::runApp(".")

Modules <- tribble(
  ~type,  ~name, ~id, ~parameters,
  #"shinymodule", "authGoogle", "auth_google", list("authFile = authFile", "appURL = appURL"),
  "shinyModule", "timeSeriesofRasters", "timeSinceFire", list("rasters = globalRasters", "polygonsList = polygons", "shpStudyRegionFull", "colorTableFile", "timeSinceFirePalette", "maxAge", "sim = mySim"),
  "shinyModule", "largePatches", "largePatches", list("numberOfSimulationTimes = lenTSF", "clumpMod2Args"),
  "shinyModule", "simInfo", "simInfo", list("mySimOut[[1]]"),
  "shinyModule", "moduleInfo", "moduleInfo", list("mySimOut[[1]]"),
  "shinyModule", "inputTables", "inputTables", list()
)

Layout <- tribble( # TODO: add authGoogleUI to ui.R (it's not a menuItem!)
  ~tabName,  ~menuItemName, ~icon, ~moduleId, ~moduleUIParameters,
#  "authGoogle", NA_character_, NA_character_, "auth_google", list(),
  "timeSinceFire", "Maps - time since fire", "map-o", "timeSinceFire", list("length(tsf)"),
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
shiny::runApp(".")
