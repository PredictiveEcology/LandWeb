library(shiny)
library(shinydashboard)
library(shinyBS)

dashboardPage(skin = "green",
              dashboardHeader(title = "templateApp"),
              dashboardSidebar(width = 300,
                               sidebarMenu(id = "wholeThing",
                                           menuItem("Large Patches", tabName = "largePatches", icon = icon("bar-chart")),
menuItem("Maps - time since fire", tabName = "timeSinceFire", icon = icon("map-o")),
menuItem("Overview Diagrams", tabName = "simInfo", icon = icon("sitemap")),
menuItem("Module Info", tabName = "moduleInfo", icon = icon("puzzle-piece")),
menuItem("LBMR (Succession) Model Inputs", tabName = "inputTables", icon = icon("table"))
                               )
              ),
              dashboardBody(
                tabItems(
                  tabItem("largePatches", largePatchesUI("largePatches")),
tabItem("timeSinceFire", timeSinceFireUI("timeSinceFire", length(tsf))),
tabItem("simInfo", simInfoUI("simInfo")),
tabItem("moduleInfo", moduleInfoUI("moduleInfo")),
tabItem("inputTables", inputTablesUI("inputTables"))
                )
              )
)
