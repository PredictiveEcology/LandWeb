library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(SpaDES.shiny)

dashboardPage(
  skin = "green",
  dashboardHeader(
    title = "LandWeb", titleWidth = 300,
    dropdownMenuOutput("notifications")
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar_menu", ## TODO: update generator to add the various modules and headings!!
      authGoogleUI("auth_google"),
      br(),
      h4(HTML("&nbsp;"), "App Information"),
      menuItem("About", tabName = "appInfo", icon = icon("info-circle")),
      menuItem("Privacy Policy",
               href = "https://friresearch.ca/content/friresearchca-privacy-policy",
               newtab = TRUE,
               icon = icon("eye-slash")),
      menuItem("Terms of Service", tabName = "appToS", icon = icon("file")),
      menuItem("Support", tabName = "appSupport", icon = icon("question-circle")),
      br(),
      h4(HTML("&nbsp;"), "NRV"),
      menuItem("Time Since Fire Maps", tabName = "timeSinceFire", icon = icon("map-o"), selected = TRUE),
      menuItem("Large Patches", tabName = "largePatches", icon = icon("bar-chart")),
      menuItem("Vegetation Cover (histograms)", tabName = "vegArea", icon = icon("bar-chart")),
      menuItem("Vegetation Cover (boxplots)", tabName = "vegArea2", icon = icon("area-chart")),
      br(),
      h4(HTML("&nbsp;"), "Model Details"),
      menuItem("Overview Diagrams", tabName = "simInfo", icon = icon("sitemap")),
      menuItem("Module Info", tabName = "moduleInfo", icon = icon("puzzle-piece")),
      menuItem("LBMR (Succession) Model Inputs", tabName = "inputTables", icon = icon("table")),

      downloadOutputsUI("downloadOutputs")
    ),
    sidebarFooterUI("sidebar")
  ),
  dashboardBody(
    tags$head(includeScript("google-analytics.js")),
    includeCSS("www/style.css"), ## TODO: add this to generator
    useShinyjs(),

    tabItems(
      tabItem("appInfo", landwebAppInfoUI("appInfo")),          ## TODO: add this to generator
      #tabItem("appPrivacy", privacyStatementUI("appPrivacy")), ## TODO: add this to generator
      tabItem("appToS", termsOfServiceUI("appToS")),            ## TODO: add this to generator
      tabItem("appSupport", landwebAppSupportUI("appSupport")), ## TODO: add this to generator

      tabItem("timeSinceFire", fluidRow(
        timeSeriesofRastersUI("timeSinceFire"),
        box(width = 8, solidHeader = TRUE, collapsible = TRUE, polygonChooserUI("polyDropdown"))
      )),
      tabItem("largePatches", largePatchesUI("largePatches")),  ## TODO: add this to generator
      tabItem("vegArea", vegAgeModUI("vegArea")),               ## TODO: add this to generator
      tabItem("vegArea2", vegAgeMod2UI("vegArea2")),            ## TODO: add this to generator

      tabItem("simInfo", simInfoUI("simInfo")),
      tabItem("moduleInfo", moduleInfoUI("moduleInfo")),
      tabItem("inputTables", inputTablesUI("inputTables"))
    ),
    copyrightFooterUI("copyright")
  )
)
