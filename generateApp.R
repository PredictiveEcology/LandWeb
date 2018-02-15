devtools::install_github("PredictiveEcology/quickPlot", ref = "development")
devtools::install_github("PredictiveEcology/reproducible", ref = "development")
devtools::install_github("PredictiveEcology/SpaDES.core", ref = "development")
devtools::install_github("PredictiveEcology/SpaDES.tools", ref = "prepInputs")
devtools::install_github("PredictiveEcology/SpaDES.shiny", ref = "79-rename-timeSinceFire")
devtools::install_github("PredictiveEcology/webDatabases", ref = "master")

library(dplyr)
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
# appMetadata$modules$parameters <- list(list("rasters = globalRasters", "polygonsList = polygons", "shpStudyRegionFull", "colorTableFile", "timeSinceFirePalette", "maxAge"),
#                                        list("numberOfSimulationTimes = lenTSF", "clumpMod2Args"),
#                                        list("mySimOut[[1]]"),
#                                        list("mySimOut[[1]]"),
#                                        list())
# appMetadata$layout$moduleUIParameters <- list(list("length(tsf)"), list(), list(), list(), list())
#
# # newApp(getwd(), appMetadata)
# #
# # file.copy("global_file.R", "global.R", overwrite = TRUE)
# # setwd("~/GitHub/LandWeb/")
# # shiny::runApp(".")


Modules <- tribble(
  ~type,  ~name, ~id, ~parameters,
  "shinyModule", "timeSeriesofRasters", "timeSinceFire", list("rasters = globalRasters", "polygonsList = polygons", "shpStudyRegionFull", "colorTableFile", "timeSinceFirePalette", "maxAge", "sim = mySim"),
  "shinyModule", "largePatches", "largePatches", list("numberOfSimulationTimes = lenTSF", "clumpMod2Args"),
  "shinyModule", "simInfo", "simInfo", list("mySimOut[[1]]"),
  "shinyModule", "moduleInfo", "moduleInfo", list("mySimOut[[1]]"),
  "shinyModule", "inputTables", "inputTables", list()
)

Layout <- tribble(
  ~tabName,  ~menuItemName, ~icon, ~moduleId, ~moduleUIParameters,
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
  modules = as.data.frame(Modules),
  layout = as.data.frame(Layout)
)

newApp(getwd(), appMetadata2)

file.copy("global_file.R", "global.R", overwrite = TRUE)
setwd("~/GitHub/LandWeb/")
shiny::runApp(".")
