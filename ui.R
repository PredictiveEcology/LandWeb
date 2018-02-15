library(shiny)
library(shinydashboard)
library(shinyBS)

dashboardPage(
  skin = "green",
  dashboardHeader(title = "LandWeb"),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "wholeThing",
      menuItem("Maps - time since fire", tabName = "timeSinceFire", icon = icon("map-o")),
      menuItem("Large Patches", tabName = "largePatches", icon = icon("bar-chart")),
      menuItem("Overview Diagrams", tabName = "simInfo", icon = icon("sitemap")),
      menuItem("Module Info", tabName = "moduleInfo", icon = icon("puzzle-piece")),
      menuItem("LBMR (Succession) Model Inputs", tabName = "inputTables", icon = icon("table"))
    ),
    sidebarFooter()
  ),
  dashboardBody(
    tabItems(
      #tabItem("timeSinceFire", timeSeriesofRastersUI("timeSinceFire", length(tsf))),
      tabItem("largePatches", largePatchesUI("largePatches")),
      tabItem("simInfo", simInfoUI("simInfo")),
      tabItem("moduleInfo", moduleInfoUI("moduleInfo")),
      tabItem("inputTables", inputTablesUI("inputTables"))
    ),
	  copyrightFooterUI("copyright")
  )
)

