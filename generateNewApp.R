library(SpaDES.shiny)
library(dplyr)
#load_all("~/GitHub/SpaDES.shiny")

Modules <- tribble(
  ~type,  ~name, ~id, ~parameters,
  #"shinymodule", "authGoogle", "auth_google", list("authFile = authFile", "appURL = appURL"),
  "shinyModule", "spades_simInit", "sim_init", list(),
  "shinyModule", "spades_expt", "sim_expt", list(),
  "shinyModule", "timeSeriesofRasters", "timeSinceFire",
  list("rasterList = globalRasters()", # TODO: update this from server.R!!!
       "polygonList = polygons()",
       "shpStudyRegionFull",
       "colorTable = colorTableFile",
       "palette = timeSinceFirePalette",
       "maxAge = maxAge",
       "sim = mySim()",
       "mapLegend = paste0(\"Time since fire\", br(), \"(years)\")"),
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
