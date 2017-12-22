dashboardPage(skin = "green",
  dashboardHeader(title = "LandWeb"),
  dashboardSidebar(width = 300,
    sidebarMenu(id = "wholeThing",
      menuItem("Maps", icon = icon("map-o"),
               menuSubItem("Time since fire", tabName = "TimeSinceFire", icon = icon("clock-o"), selected = TRUE)),
      br(),
      h4(HTML("&nbsp;"), "NRV plots"),
      menuItem("Large Patches - new", tabName = "largePatchesSlicer", icon = icon("bar-chart")),
      br(),
      h4(HTML("&nbsp;"), "Model details"),
      menuItem("Overview Diagrams", tabName = "simDiagrams", icon = icon("sitemap")),
      menuItem("Module Info", tabName = "moduleInfo", icon = icon("puzzle-piece")),
      menuItem("LBMR (Succession) Model Inputs", tabName = "inputTables", icon = icon("table")),
      br(),
      sidebarFooter() ## CSS rules push the footer to the bottom of the sidebar
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
          tabItem("TimeSinceFire",
                  fluidRow(uiOutput("timeSinceFireUI"))
          ),
          tabItem("inputTables",
                  fluidRow(
                    h3("Currently, these inputs are not changeable by specific regions"),
                    box(
                      width = 10,
                      solidHeader = TRUE,
                      status = "success",
                      title = "Species Inputs",
                      dataTableOutput("speciesInputs")
                    )
                  ),
                  fluidRow(
                    box(
                      width = 12,
                      solidHeader = TRUE,
                      status = "success",
                      title = "Geographically Varying Species Inputs. These are means (and SE) across all map regions",
                      dataTableOutput("speciesEcoregionInputs")
                    )
                  )
          )
        )
    )
  )
)

