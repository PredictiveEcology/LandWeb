dashboardPage(skin = "green",
  dashboardHeader(title = "LandWeb"),
  dashboardSidebar(width = 300,
    sidebarMenu(id = "wholeThing",
      menuItem("Maps - time since fire", icon = icon("map-o"), tabName = "TimeSinceFire"),
      menuItem("Large Patches - new", tabName = "largePatchesSlicer", icon = icon("bar-chart")),
      menuItem("Overview Diagrams", tabName = "simDiagrams", icon = icon("sitemap")),
      menuItem("Module Info", tabName = "moduleInfo", icon = icon("puzzle-piece")),
      menuItem("LBMR (Succession) Model Inputs", tabName = "inputTables", icon = icon("table"))
    )
  ),
  dashboardBody(
    includeCSS("www/style.css"),
    useShinyjs(),
      div(
        id = "main_content",

        tabItems(
          tabItem("simDiagrams", simInfoUI("simInfoTabs")),
          tabItem("moduleInfo", moduleInfoUI("modInfoBoxes")),
          tabItem("largePatchesSlicer", largePatchesUI("largePatches")),
          tabItem("TimeSinceFire", timeSinceFireUI("timeSinceFire", length(tsf))),
          tabItem("inputTables", inputTablesUI("inputTables"))
        )
    )
  )
)

