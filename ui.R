library(shiny)
library(shinydashboard)
library(shinyBS)

dashboardPage(
  skin = "green",
  dashboardHeader(title = "LandWeb"),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "wholeThing", ## TODO: update generator to add the various modules and headings!!
      authGoogleUI("auth_google"),
      br(),
      h4(HTML("&nbsp;"), "App Information"),
      menuItem("About", tabName = "appInfo", icon = icon("info-circle")),
      menuItem("Privacy Statement", tabName = "appPrivacy", icon = icon("eye-slash")),
      menuItem("Terms of Service", tabName = "appToS", icon = icon("file")),
      menuItem("Support", tabName = "appSupport", icon = icon("question-circle")),
      br(),
      h4(HTML("&nbsp;"), "Maps"),
      menuItem("Time since fire", tabName = "timeSinceFire", icon = icon("map-o"), selected = TRUE),
      br(),
      h4(HTML("&nbsp;"), "NRV"),
      menuItem("Large Patches", tabName = "largePatches", icon = icon("bar-chart")),
      menuItem("Vegetation Cover", tabName = "vegArea", icon = icon("bar-chart")),
      br(),
      h4(HTML("&nbsp;"), "Model Details"),
      menuItem("Overview Diagrams", tabName = "simInfo", icon = icon("sitemap")),
      menuItem("Module Info", tabName = "moduleInfo", icon = icon("puzzle-piece")),
      menuItem("LBMR (Succession) Model Inputs", tabName = "inputTables", icon = icon("table")),
      tags$hr(),
      downloadOutputsUI("downloadOutputs")
    ),
    sidebarFooterUI("sidebar")
  ),
  dashboardBody(
    includeCSS("www/style.css"), ## TODO: add this to generator
    useShinyjs(),
    tabItems(
      tabItem("appInfo", landwebAppInfoUI("appInfo")),          ## TODO: add this to generator
      tabItem("appPrivacy", landwebAppPrivacyUI("appPrivacy")), ## TODO: add this to generator
      tabItem("appToS", landwebAppToSUI("appToS")),             ## TODO: add this to generator
      tabItem("appSupport", landwebAppSupportUI("appSupport")), ## TODO: add this to generator

      tabItem("timeSinceFire", timeSeriesofRastersUI("timeSinceFire")),
      tabItem("largePatches", largePatchesUI("largePatches")),
      tabItem("vegArea", vegAgeModUI("vegArea")), ## TODO: add this to generator

      tabItem("simInfo", simInfoUI("simInfo")),
      tabItem("moduleInfo", moduleInfoUI("moduleInfo")),
      tabItem("inputTables", inputTablesUI("inputTables"))
    ),
    copyrightFooterUI("copyright")
  )
)
