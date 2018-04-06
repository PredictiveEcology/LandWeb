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
      authGoogleUI("auth_google"), # TODO: have this automatically added!!
      br(), ## TODO: add this to generator
      h4(HTML("&nbsp;"), "Maps"),  ## TODO: add this to generator
      menuItem("Time since fire", tabName = "timeSinceFire", icon = icon("map-o")),
      br(), ## TODO: add this to generator
      h4(HTML("&nbsp;"), "NRV"),  ## TODO: add this to generator
      menuItem("Large Patches", tabName = "largePatches", icon = icon("bar-chart")),
      menuItem("Vegetation Cover", tabName = "vegArea", icon = icon("bar-chart")), ## TODO: add this to generator
      br(), ## TODO: add this to generator
      h4(HTML("&nbsp;"), "Model Details"),  ## TODO: add this to generator
      menuItem("Overview Diagrams", tabName = "simInfo", icon = icon("sitemap")),
      menuItem("Module Info", tabName = "moduleInfo", icon = icon("puzzle-piece")),
      menuItem("LBMR (Succession) Model Inputs", tabName = "inputTables", icon = icon("table")),
      br(),
      downloadOutputsUI("downloadOutputs") ## TODO: write this with generator
    ),
    sidebarFooterUI("sidebar")
  ),
  dashboardBody(
    includeCSS("www/style.css"), ## TODO: add this to generator
    useShinyjs(),
    tabItems(
      tabItem("timeSinceFire", timeSeriesofRastersUI("timeSinceFire")),
      tabItem("largePatches", largePatchesUI("largePatches")),
      tabItem("vegArea", vegAgeModUI("vegArea")), # TO DO: add this to generator
      tabItem("simInfo", simInfoUI("simInfo")),
      tabItem("moduleInfo", moduleInfoUI("moduleInfo")),
      tabItem("inputTables", inputTablesUI("inputTables"))
    ),
    copyrightFooterUI("copyright")
  )
)
