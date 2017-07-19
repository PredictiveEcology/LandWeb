dashboardPage(skin = "green",
  dashboardHeader(title = "LandWeb"),
  dashboardSidebar(width = 300,
    sidebarMenu(id = "wholeThing", 
      menuItem("Maps", icon = icon("map-o"),
               menuSubItem("Time since fire", tabName = "TimeSinceFire", icon = icon("clock-o"), selected = TRUE)),
               #menuSubItem("Change polygon layer", tabName = "Polygons", icon = icon("map-marker"), selected = FALSE)),
               #menuSubItem("Fire Return Interval Map", tabName = "StudyRegion",
               #             icon = icon("map"))),
      br(),
      h4(HTML("&nbsp;"), "NRV plots"),
      #tags$head(tags$script(HTML('$(document).ready(function() {$(".treeview-menu").css("display", "block");})'))),
      menuItem("Large Patches", icon = icon("bar-chart"),
               menuSubItem(#text=tags$div("Type patch size in hectares",tags$br(),
                          #            "that defines 'Large'",tags$br(),
                          #            "(numbers below 100 will not work)",
                          #            style = "display: inline-block; vertical-align: middle;"), 
                        tabName = "patchSize",
                        #icon = icon("arrows-alt"), 
                        clumpMod2Input("id1")),
               menuSubItem("Young", tabName = "ClumpsYoung", icon = icon("tree"), selected = FALSE),
               menuSubItem("Immature", tabName = "ClumpsImmature", icon = icon("tree")),
               menuSubItem("Mature", tabName = "ClumpsMature", icon = icon("tree")),
               menuSubItem("Old", tabName = "ClumpsOld", icon = icon("tree"), selected = FALSE)),
      menuItem("Proportion Area in Age Class By Cover", icon = icon("bar-chart"),
               menuSubItem("Young Forest", tabName = "Young", icon = icon("tree"), selected = FALSE),
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
    useShinyjs(),
    #tags$head(tags$style(HTML(mycss))),
    div(
      id = "loading_page",
      h1("Loading, please wait ..."),
      tags$img(src = #"http://cdn.boatinternational.com/assets/spinner-2cd0a51f39b9c1d559af6c393f97a80c.gif",
               "http://s01.europapress.net/chance/loading.gif",
               id = "loading-spinner",
               width = "100px", height = "100px"),
      br(),
      
      style="text-align: center;"
    ),
    
    #hidden(
      div(
        id = "main_content",
    
        tabItems(
          # tabItem("StudyRegion",
          #         fluidRow(
          #           box(
          #             width = 12,
          #             height = 900,
          #             solidHeader = TRUE,
          #             status = "success",
          #             title = "Fire Return Interval, with sub region currently in Demo (hover mouse over for values)",
          #             #plotOutput("StudyRegion", width = "100%")#, height = "100%")
          #             #plotlyOutput("StudyRegion", width = "100%")#, height = "100%")
          #             ggvisOutput("StudyRegion")
          #           )
          #         )
          # ),
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
          tabItem("ClumpsOld",
                  fluidRow(uiOutput("ClumpsOldUI"))
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
          tabItem("Young",
                  fluidRow(uiOutput("YoungUI"))
          ),
          tabItem("Immature",
                  fluidRow(uiOutput("ImmatureUI"))
          ),
          tabItem("Mature",
                  fluidRow(uiOutput("MatureUI"))
          ),
          tabItem("Old",
                  fluidRow(uiOutput("OldUI"))
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
    #  ) # hidden
    ),
    
    copyrightFooter()
  )
)

