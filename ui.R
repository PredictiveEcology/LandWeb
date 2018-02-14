library(shiny)
library(shinydashboard)
library(shinyBS)

dashboardPage(
  skin = "green",
  dashboardHeader(title = "templateApp"),
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
    HTML(paste(
      "<footer>",
      "<div id=\"sidebar\" style='position: absolute; bottom: 5px; margin: 15px'>",
      " Built with ", # nolint
      "<a href=\"http://SpaDES.PredictiveEcology.org\">  <img src=\"http://predictiveecology.org/img/avatar.png\", height=25px>SpaDES, </a>",
      "<a href=\"http://shiny.rstudio.com/\", target=\"_blank\">shiny</a> and ",
      "<a href=\"https://appsilondatascience.com/\">  <img src=\"http://d3u4jj2f3q2139.cloudfront.net/logo-appsilon-data-science-transparent.png\", height=20px> </a>",
      "</div>",
      "</footer>"
    ))
  ),
  dashboardBody(
    tabItems(
      tabItem("timeSinceFire", timeSinceFireUI("timeSinceFire", length(tsf))),
      tabItem("largePatches", largePatchesUI("largePatches")),
      tabItem("simInfo", simInfoUI("simInfo")),
      tabItem("moduleInfo", moduleInfoUI("moduleInfo")),
      tabItem("inputTables", inputTablesUI("inputTables"))
    )
  )
)
