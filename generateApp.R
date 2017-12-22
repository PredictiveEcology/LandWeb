devtools::install_github("PredictiveEcology/SpaDES.shiny", ref = "develop")
library(SpaDES.shiny)

appMetadata <- list(
  modules = data.frame(
    type = c("shinyModule", "shinyModule", "shinyModule", "shinyModule", "shinyModule"),
    name = c("timeSinceFire", "largePatches", "simInfo", "moduleInfo", "inputTables"),
    id = c("timeSinceFire", "largePatches", "simInfo", "moduleInfo", "inputTables"),
    stringsAsFactors = FALSE
  ),
  layout = data.frame(
    tabName = c("timeSinceFire", "largePatches", "simInfo", "moduleInfo", "inputTables"),
    menuItemName = c("Maps - time since fire", "Large Patches", "Overview Diagrams", "Module Info", "LBMR (Succession) Model Inputs"),
    icon = c("map-o", "bar-chart", "sitemap", "puzzle-piece", "table"),
    moduleId = c("timeSinceFire", "largePatches", "simInfo", "moduleInfo", "inputTables"),
    stringsAsFactors = FALSE
  )
)

appMetadata$modules$parameters <- list(list("rasters = globalRasters", "polygonsList = polygons", "shpStudyRegionFull", "colorTableFile", "timeSinceFirePalette", "maxAge"),
                                       list("numberOfSimulationTimes = lenTSF", "clumpMod2Args"), list("mySimOut[[1]]"), list("mySimOut[[1]]"), list())
appMetadata$layout$moduleUIParameters <- list(list("length(tsf)"), list(), list(), list(), list())

newApp(getwd(), appMetadata)

file.copy("global_file.R", "global.R", overwrite = TRUE)

shiny::runApp("./")

