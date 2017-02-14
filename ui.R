library(shiny)

dashboardPage(skin = "green",
  dashboardHeader(title = "LandWeb"),
  dashboardSidebar(width = 300,
    sidebarMenu(
      menuItem("Large Patches", #tabName = "ClumpsYoung",
               icon = icon("area-chart"),
               menuSubItem("Young", tabName = "ClumpsYoung"),
               menuSubItem("Immature", tabName = "ClumpsImmature"),
               menuSubItem("Mature", tabName = "ClumpsMature"),
               menuSubItem("Old", tabName = "ClumpsOld")),
      menuItem("Change Patch Size that defines 'large'", clumpMod2Input("id1")),
      menuItem("Study Region & Fire Return Interval Map", tabName = "StudyRegion"),
      menuItem("Maps",
               menuSubItem("Time since fire", tabName = "TimeSinceFire"),
               menuSubItem("Change polygon layer", tabName = "Polygons")),
      menuItem("Proportion Area in Age Class By Cover",
               menuSubItem("Young Forest", tabName = "Young"),
               menuSubItem("Immature Forest", tabName = "Immature"),
               menuSubItem("Mature Forest", tabName = "Mature"),
               menuSubItem("Old Forest", tabName = "Old")),
      menuItem("Input Tables", tabName = "inputTables")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("StudyRegion",
              fluidRow(
                box(
                  width = 12,
                  height = 900,
                  solidHeader = TRUE,
                 status = "success",
                  title = "LandWeb Study Area, with sub region currently in Demo",
                  #plotOutput("StudyRegion", width = "100%")#, height = "100%")
                  #plotlyOutput("StudyRegion", width = "100%")#, height = "100%")
                  ggvisOutput("StudyRegion")
                )
              )
      ),
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
    )
  )
)

