library(shiny)
library(shinydashboard)
library(shinyBS)

dashboardPage(skin = "green",
  dashboardHeader(title = "LandWeb"),
  dashboardSidebar(width = 300,
    sidebarMenu(
      menuItem("Maps", icon = icon("map-o"),
               menuSubItem("Time since fire", tabName = "TimeSinceFire", icon = icon("clock-o")),
               menuSubItem("Change polygon layer", tabName = "Polygons", icon = icon("map-marker")),
               menuSubItem("Fire Return Interval Map", tabName = "StudyRegion",
                           icon = icon("map"))),
      br(),
      h4(HTML("&nbsp;"), "NRV plots"),
      menuItem("Change Patch Size that defines 'large'", tabName = "patchSize",
               icon = icon("arrows-alt"), clumpMod2Input("id1")),
      menuItem("Large Patches", icon = icon("bar-chart"),
               menuSubItem("Young", tabName = "ClumpsYoung", icon = icon("tree")),
               menuSubItem("Immature", tabName = "ClumpsImmature", icon = icon("tree")),
               menuSubItem("Mature", tabName = "ClumpsMature", icon = icon("tree")),
               menuSubItem("Old", tabName = "ClumpsOld", icon = icon("tree"))),
      menuItem("Proportion Area in Age Class By Cover", icon = icon("bar-chart"),
               menuSubItem("Young Forest", tabName = "Young", icon = icon("tree")),
               menuSubItem("Immature Forest", tabName = "Immature", icon = icon("tree")),
               menuSubItem("Mature Forest", tabName = "Mature", icon = icon("tree")),
               menuSubItem("Old Forest", tabName = "Old", icon = icon("tree"))),
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
    
    tabItems(
      tabItem("StudyRegion",
              fluidRow(
                box(
                  width = 12,
                  height = 900,
                  solidHeader = TRUE,
                  status = "success",
                  title = "Fire Return Interval, with sub region currently in Demo (hover mouse over for values)",
                  #plotOutput("StudyRegion", width = "100%")#, height = "100%")
                  #plotlyOutput("StudyRegion", width = "100%")#, height = "100%")
                  ggvisOutput("StudyRegion")
                )
              )
      ),
      tabItem("simDiagrams", simInfoUI("simInfoTabs")),
      tabItem("moduleInfo", moduleInfoUI("modInfoBoxes")),
      tabItem("ClumpsYoung",
              fluidRow(uiOutput("ClumpsYoungUI"))
      ),
      tabItem("ClumpsImmature",
              fluidRow(uiOutput("ClumpsImmatureUI"))
      ),
      tabItem("ClumpsMature",
              fluidRow(uiOutput("ClumpsMatureUI"))
      ),
      tabItem("Polygons",
              fluidRow(
                tabBox(width = 12,
                       tabPanel("Current Polygons", tabName = "Polygons1",
                                fluidRow(leafletMapUI("leafletMap"))
                       )
                )
              )
      ),
      tabItem("TimeSinceFire",
              fluidRow(uiOutput("timeSinceFireUI"))
      ),
      tabItem("ClumpsOld",
              fluidRow(uiOutput("ClumpsOldUI"))
      ),
      tabItem("Young",
              fluidRow(uiOutput("YoungUI"))
      ),
      tabItem("Mature",
              fluidRow(uiOutput("MatureUI"))
      ),
      tabItem("Old",
              fluidRow(uiOutput("OldUI"))
      ),
      
      tabItem("Immature",
              fluidRow(uiOutput("ImmatureUI"))
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
      ),
      tabItem("OldDecidByPoly",
              fluidRow(
                box(
                  width = 12,
                  height = 900,
                  solidHeader = TRUE,
                  status = "success",
                  title = "Initial Conditions Gone for Natural Range of Variation Calculation",
                  plotOutput("OldDecidByPoly")
                )
              )
      ),
      tabItem("seralStage1",
              fluidRow(
                box(
                  width = 6,
                  solidHeader = TRUE,
                  #width = 8, 
                  status = "success",
                  title = "% Cover by each seral stage",
                  plotOutput("seralStagePctCover")
                ),
                box(
                  width = 4,
                  solidHeader = TRUE,
                  status = "success",
                  title = "Seral stage for region",
                  plotOutput("seralStagePctCoverMap")
                )
              )
      ),
      tabItem("vegType",
              fluidRow(
                box(
                  width = 6,
                  solidHeader = TRUE,
                  status = "success",
                  title = "% Cover by each vegatation type",
                  plotOutput("vegTypePctCover")
                ),
                box(
                  width = 4,
                  solidHeader = TRUE,
                  status = "success",
                  title = "Vegetation types for region",
                  plotOutput("vegTypePctCoverMap")
                )
              )
      ),
      tabItem("patchSizes",
              fluidRow(
                box(
                  width = 6,
                  solidHeader = TRUE,
                  status = "success",
                  title = "Number of patches, by seral stage",
                  plotOutput("patchSizesFig5")
                ),
                box(
                  width = 5,
                  solidHeader = TRUE,
                  status = "success",
                  title = "Over old patches",
                  plotOutput("patchSizesFig5Raster")
                )
              )
      )
    ),
    copyrightFooter()
  )
)

