
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

dashboardPage(skin = "green",
  dashboardHeader(title = "LandWeb"),
  dashboardSidebar(width = 300,
    sidebarMenu(
      menuItem("Large Patches", #tabName = "ClumpsYoung", 
               icon = icon("area-chart"),
               menuSubItem(tabName = "ClumpsYoung", "Young"),
               menuSubItem(tabName = "ClumpsImmature", "Immature"),
               menuSubItem(tabName = "ClumpsMature", "Mature"),
               menuSubItem(tabName = "ClumpsOld", "Old"),
               # menuSubItem(selectInput("PatchSize33", "Patch Size",
               #                         choices = c(100, 200), selected = 100
               # ))
      #),
      menuItem("Change Patch Size that defines 'large'",
               #menuSubItem(
               clumpMod2Input("id1")
      )),
      menuItem("Study Region", tabName = "StudyRegion"),
      menuItem("Maps", 
               menuSubItem("Age since fire", tabName = "AgeSinceFire"),
               menuSubItem("Change polygon layer", tabName = "Polygons")),
      menuItem("Proportion Area in Age Class By Cover",
        menuSubItem("Young Forest", tabName = "Young"),
        #menuItem("Young Forest", tabName = "Young"),
        menuSubItem("Immature Forest", tabName = "Immature"),
        menuSubItem("Mature Forest", tabName = "Mature"),
        menuSubItem("Old Forest", tabName = "Old")
      ),
      #br(),
      menuItem("Input Tables", tabName = "inputTables")
      #menuItem("Old deciduous, by polygon", tabName = "OldDecidByPoly"),
      #menuItem("Map overview", tabName = "leafletMap"),
      #br(),
      
      #menuItem("By Region", 
      #   menuSubItem(selectInput("regionSelector", "Region Selector",
      #             choices = c(139, 149), selected = 139
      #   )),
      #   menuSubItem("Seral Stage", tabName = "seralStage1"),
      #   menuSubItem("Vegetation Type", tabName = "vegType"),
      #   menuSubItem("Patch Size", tabName = "patchSizes"),
      #   menuSubItem("Patch Size, leading species", tabName = "patchSizesLeadingSp")
      # )
      # 
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
                  status = "info",
                  title = "LandWeb Study Area, with sub region currently in Demo",
                  #plotOutput("StudyRegion", width = "100%")#, height = "100%")
                  #plotlyOutput("StudyRegion", width = "100%")#, height = "100%")
                  ggvisOutput("StudyRegion")
                )
              )
              
      ),
      tabItem("ClumpsYoung",
              fluidRow(
                tabBox(width = 16,
                       tabPanel("Young, Deciduous", tabName = "ClumpsYoung_Deciduous2",
                                lapply(pmatch("Young", ageClasses),function(i) {
                                  lapply(seq_along(ecodistricts)[polygonsWithData[[i]]],function(j) {
                                    clumpModOutput(paste0(i,"_",j, "_", pmatch("Deciduous", vegLeadingTypes),"_clumps"))
                                  })
                                })
                       ),
                       tabPanel("Young, Spruce", tabName = "ClumpsYoung_Spruce2",
                                lapply(pmatch("Young", ageClasses),function(i) {
                                  lapply(seq_along(ecodistricts)[polygonsWithData[[i]]],function(j) {
                                    clumpModOutput(paste0(i,"_",j, "_", pmatch("Spruce", vegLeadingTypes),"_clumps"))
                                  })
                                })
                       ),
                       tabPanel("Young, Mixed", tabName = "ClumpsYoung_Mixed2",
                                lapply(pmatch("Young", ageClasses),function(i) {
                                  lapply(seq_along(ecodistricts)[polygonsWithData[[i]]],function(j) {
                                    clumpModOutput(paste0(i,"_",j, "_", pmatch("Mixed", vegLeadingTypes),"_clumps"))
                                  })
                                })
                       )
                )
              )
              
      ),
      
      tabItem("Polygons",
              fluidRow(
                tabBox(width = 16,
                       tabPanel("Current Polygons", tabName = "Polygons1",
                                    leafletMapUI("leafletMap")
                       )
                )
              )
              
      ),
      tabItem("AgeSinceFire",
              fluidRow(
                tabBox(width = 16,
                       tabPanel("Age Since Fire maps", tabName = "ageSinceFireTab",
                                ageSinceFireModUI("ageSinceFire")
                       )
                )
              )
              
      ),
      
      tabItem("ClumpsImmature",
              fluidRow(
                tabBox(width = 16,
                       tabPanel("Immature, Deciduous", tabName = "Immature_Deciduous2",
                                lapply(pmatch("Immature", ageClasses),function(i) {
                                  lapply(seq_along(ecodistricts)[polygonsWithData[[i]]],function(j) {
                                    clumpModOutput(paste0(i,"_",j, "_", pmatch("Deciduous", vegLeadingTypes),"_clumps"))
                                  })
                                })
                       ),
                       tabPanel("Immature, Spruce", tabName = "Immature_Spruce2",
                                lapply(pmatch("Immature", ageClasses),function(i) {
                                  lapply(seq_along(ecodistricts)[polygonsWithData[[i]]],function(j) {
                                    clumpModOutput(paste0(i,"_",j, "_", pmatch("Spruce", vegLeadingTypes),"_clumps"))
                                  })
                                })
                       ),
                       tabPanel("Immature, Mixed", tabName = "Immature_Mixed2",
                                lapply(pmatch("Immature", ageClasses),function(i) {
                                  lapply(seq_along(ecodistricts)[polygonsWithData[[i]]],function(j) {
                                    clumpModOutput(paste0(i,"_",j, "_", pmatch("Mixed", vegLeadingTypes),"_clumps"))
                                  })
                                })
                       )
                )
              )
              
      ),
      tabItem("ClumpsMature",
              fluidRow(
                tabBox(width = 16,
                       tabPanel("Mature, Deciduous", tabName = "Mature_Deciduous2",
                                lapply(pmatch("Mature", ageClasses),function(i) {
                                  lapply(seq_along(ecodistricts)[polygonsWithData[[i]]],function(j) {
                                    clumpModOutput(paste0(i,"_",j, "_", pmatch("Deciduous", vegLeadingTypes),"_clumps"))
                                  })
                                })
                       ),
                       tabPanel("Mature, Spruce", tabName = "Mature_Spruce2",
                                lapply(pmatch("Mature", ageClasses),function(i) {
                                  lapply(seq_along(ecodistricts)[polygonsWithData[[i]]],function(j) {
                                    clumpModOutput(paste0(i,"_",j, "_", pmatch("Spruce", vegLeadingTypes),"_clumps"))
                                  })
                                })
                       ),
                       tabPanel("Mature, Mixed", tabName = "Mature_Mixed2",
                                lapply(pmatch("Mature", ageClasses),function(i) {
                                  lapply(seq_along(ecodistricts)[polygonsWithData[[i]]],function(j) {
                                    clumpModOutput(paste0(i,"_",j, "_", pmatch("Mixed", vegLeadingTypes),"_clumps"))
                                  })
                                })
                       )
                )
              )
              
      ),
      tabItem("ClumpsOld",
              fluidRow(
                tabBox(width = 16,
                       tabPanel("Old, Deciduous", tabName = "Old_Deciduous2",
                                lapply(pmatch("Old", ageClasses),function(i) {
                                  lapply(seq_along(ecodistricts)[polygonsWithData[[i]]],function(j) {
                                    clumpModOutput(paste0(i,"_",j, "_", pmatch("Deciduous", vegLeadingTypes),"_clumps"))
                                  })
                                })
                       ),
                       tabPanel("Old, Spruce", tabName = "Old_Spruce2",
                                lapply(pmatch("Old", ageClasses),function(i) {
                                  lapply(seq_along(ecodistricts)[polygonsWithData[[i]]],function(j) {
                                    clumpModOutput(paste0(i,"_",j, "_", pmatch("Spruce", vegLeadingTypes),"_clumps"))
                                  })
                                })
                       ),
                       tabPanel("Old, Mixed", tabName = "Old_Mixed2",
                                lapply(pmatch("Old", ageClasses),function(i) {
                                  lapply(seq_along(ecodistricts)[polygonsWithData[[i]]],function(j) {
                                    clumpModOutput(paste0(i,"_",j, "_", pmatch("Mixed", vegLeadingTypes),"_clumps"))
                                  })
                                })
                       )
                )
              )
              
      ),
      
      # lapply(seq_along(ageClasses)[4], function(xx) {
      #   tabItem(ageClasses[xx],
      #           fluidRow(
      #             lapply(seq_along(ecodistricts)[polygonsWithData[[xx]]],function(i) {
      #               decidOldModUI(i)
      #             })
      #             #   valueBoxOutput("numMapUnits"),
      #             #   valueBoxOutput("numInitialCommunities")
      #           )
      #   )
      #   }
      # ),
      
      tabItem("Young",
               fluidRow(
                 tabBox(width = 16,
                        tabPanel("Young, Deciduous", tabName = "Young_Deciduous",
                           lapply(pmatch("Young", ageClasses),function(i) {
                             lapply(seq_along(ecodistricts)[polygonsWithData[[i]]],function(j) {
                                 vegAgeModUI(paste0(i,"_",j, "_", pmatch("Deciduous", vegLeadingTypes)))
                             })
                           })
                        ),
                        tabPanel("Young, Spruce", tabName = "Young_Spruce",
                                 lapply(pmatch("Young", ageClasses),function(i) {
                                   lapply(seq_along(ecodistricts)[polygonsWithData[[i]]],function(j) {
                                     vegAgeModUI(paste0(i,"_",j, "_", pmatch("Spruce", vegLeadingTypes)))
                                   })
                                 })
                        ),
                        tabPanel("Young, Mixed", tabName = "Young_Mixed",
                                 lapply(pmatch("Young", ageClasses),function(i) {
                                   lapply(seq_along(ecodistricts)[polygonsWithData[[i]]],function(j) {
                                     vegAgeModUI(paste0(i,"_",j, "_", pmatch("Mixed", vegLeadingTypes)))
                                   })
                                 })
                        )
                 )
                )
                 
              #   valueBoxOutput("numMapUnits"),
              #   valueBoxOutput("numInitialCommunities")
              # )
              # fluidRow(
              #   box(
              #     width = 4, status = "info",solidHeader = TRUE,
              #     title = "Map Units",
              #     plotOutput("ecoregionFig"),
              #     collapsible = TRUE
              #   ),
              #   box(
              #     width = 4, status = "info", solidHeader = TRUE,
              #     title = "Region run for this presentation",
              #     plotOutput("CanadaPlot")
              #   )
              #
              # )
      ),
      tabItem("Mature",
              fluidRow(
                tabBox(width = 16,
                       tabPanel("Mature, Deciduous", tabName = "Mature_Deciduous",
                                lapply(pmatch("Mature", ageClasses),function(i) {
                                  lapply(seq_along(ecodistricts)[polygonsWithData[[i]]],function(j) {
                                    vegAgeModUI(paste0(i,"_",j, "_", pmatch("Deciduous", vegLeadingTypes)))
                                  })
                                })
                       ),
                       tabPanel("Mature, Spruce", tabName = "Mature_Spruce",
                                lapply(pmatch("Mature", ageClasses),function(i) {
                                  lapply(seq_along(ecodistricts)[polygonsWithData[[i]]],function(j) {
                                    vegAgeModUI(paste0(i,"_",j, "_", pmatch("Spruce", vegLeadingTypes)))
                                  })
                                })
                       ),
                       tabPanel("Mature, Mixed", tabName = "Mature_Mixed",
                                lapply(pmatch("Mature", ageClasses),function(i) {
                                  lapply(seq_along(ecodistricts)[polygonsWithData[[i]]],function(j) {
                                    vegAgeModUI(paste0(i,"_",j, "_", pmatch("Mixed", vegLeadingTypes)))
                                  })
                                })
                       )
                )
              )
              
              # fluidRow(
              #   lapply(3,function(i) {
              #     lapply(seq_along(ecodistricts)[polygonsWithData[[i]]],function(j) {
              #       lapply(seq_along(vegLeadingTypes) ,function(k) {
              #         vegAgeModUI(paste0(i,"_",j, "_", k))
              #       })
              #     })
              #   })
              #   #   valueBoxOutput("numMapUnits"),
              #   #   valueBoxOutput("numInitialCommunities")
              # )
      ),
      tabItem("Old",
              fluidRow(
                tabBox(width = 16,
                       tabPanel("Old, Deciduous", tabName = "Old_Deciduous",
                                lapply(pmatch("Old", ageClasses),function(i) {
                                  lapply(seq_along(ecodistricts)[polygonsWithData[[i]]],function(j) {
                                    vegAgeModUI(paste0(i,"_",j, "_", pmatch("Deciduous", vegLeadingTypes)))
                                  })
                                })
                       ),
                       tabPanel("Old, Spruce", tabName = "Old_Spruce",
                                lapply(pmatch("Old", ageClasses),function(i) {
                                  lapply(seq_along(ecodistricts)[polygonsWithData[[i]]],function(j) {
                                    vegAgeModUI(paste0(i,"_",j, "_", pmatch("Spruce", vegLeadingTypes)))
                                  })
                                })
                       ),
                       tabPanel("Old, Mixed", tabName = "Old_Mixed",
                                lapply(pmatch("Old", ageClasses),function(i) {
                                  lapply(seq_along(ecodistricts)[polygonsWithData[[i]]],function(j) {
                                    vegAgeModUI(paste0(i,"_",j, "_", pmatch("Mixed", vegLeadingTypes)))
                                  })
                                })
                       )
                )
              )
              
              # fluidRow(
              #   lapply(3,function(i) {
              #     lapply(seq_along(ecodistricts)[polygonsWithData[[i]]],function(j) {
              #       lapply(seq_along(vegLeadingTypes) ,function(k) {
              #         vegAgeModUI(paste0(i,"_",j, "_", k))
              #       })
              #     })
              #   })
              #   #   valueBoxOutput("numMapUnits"),
              #   #   valueBoxOutput("numInitialCommunities")
              # )
      ),
      
      tabItem("Immature",
              fluidRow(
                tabBox(width = 16,
                       tabPanel("Immature, Deciduous", tabName = "Immature_Deciduous",
                                lapply(pmatch("Immature", ageClasses),function(i) {
                                  lapply(seq_along(ecodistricts)[polygonsWithData[[i]]],function(j) {
                                    vegAgeModUI(paste0(i,"_",j, "_", pmatch("Deciduous", vegLeadingTypes)))
                                  })
                                })
                       ),
                       tabPanel("Immature, Spruce", tabName = "Immature_Spruce",
                                lapply(pmatch("Immature", ageClasses),function(i) {
                                  lapply(seq_along(ecodistricts)[polygonsWithData[[i]]],function(j) {
                                    vegAgeModUI(paste0(i,"_",j, "_", pmatch("Spruce", vegLeadingTypes)))
                                  })
                                })
                       ),
                       tabPanel("Immature, Mixed", tabName = "Immature_Mixed",
                                lapply(pmatch("Immature", ageClasses),function(i) {
                                  lapply(seq_along(ecodistricts)[polygonsWithData[[i]]],function(j) {
                                    vegAgeModUI(paste0(i,"_",j, "_", pmatch("Mixed", vegLeadingTypes)))
                                  })
                                })
                       )
                )
              )
              
      ),
      tabItem("inputTables",
              fluidRow(
                box(
                  width = 10,
                  solidHeader = TRUE,
                  status = "info",
                  title = "Species Inputs",
                  dataTableOutput("speciesInputs")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  solidHeader = TRUE,
                  status = "info",
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
                  status = "info",
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
                  status = "info",
                  title = "% Cover by each seral stage",
                  plotOutput("seralStagePctCover")
                ),
                box(
                  width = 4,
                  solidHeader = TRUE,
                  status = "info",
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
                  status = "info",
                  title = "% Cover by each vegatation type",
                  plotOutput("vegTypePctCover")
                ),
                box(
                  width = 4,
                  solidHeader = TRUE,
                  status = "info",
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
                  status = "info",
                  title = "Number of patches, by seral stage",
                  plotOutput("patchSizesFig5")
                ),
                box(
                  width = 5,
                  solidHeader = TRUE,
                  status = "info",
                  title = "Over old patches",
                  plotOutput("patchSizesFig5Raster")
                )
             )
       ),
       tabItem("patchSizesLeadingSp",
               fluidRow(
                box(
                  width = 6,
                  solidHeader = TRUE,
                  status = "info",
                  title = "Number of patches, by leading vegetation type",
                  plotOutput("patchSizesFig6")
                ),
                box(
                  width = 6,
                  height = 900,
                  solidHeader = TRUE,
                  status = "info",
                  title = "Very Large Patches (>5000ha),\nAspen leading (first), Spruce leading patches (second)",
                  plotOutput("patchSizesFig6Raster", width = "100%")
                )
              )
      )
    )
  )
)

