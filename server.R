function(input, output, session) {
  
  react <- reactiveValues()

  #patchSizeReact <- reactive({input$PatchSize33})
    
  callModule(leafletMap, "leafletMap", ecodistrictsFullLFLT=ecodistrictsFullLFLT)
  
  Clumps <- callModule(clumpMod2, "id1", 
                       tsf=tsf, vtm=vtm, ecodistricts=ecodistricts, 
                       #polygonNames = ecodistricts$ECODISTRIC, 
                       #cl=cl, 
                       ageClasses = ageClasses, cacheRepo = paths$cachePath,
                       patchSize = reactive({input$patchSize33}))

  lapply(seq_along(polygonsWithData),function(i) { # i is age
    lapply(polygonsWithData[[i]], function(j) {
      lapply(seq_along(vegLeadingTypes), function(k) {
         callModule(clumpMod,paste0(i,"_",j, "_", k,"_clumps"),
                   Clumps = reactive({Clumps()}),
                   id = paste0(i,"_",j, "_", k,"_clumps")
         )  
      })
    })
  })
  
  lapply(seq_along(polygonsWithData),function(i) {
    lapply(polygonsWithData[[i]], function(j) {
      lapply(seq_along(vegLeadingTypes), function(k) {
        callModule(vegAgeMod,paste0(i,"_",j, "_", k),indivPolygonIndex=j,polygonLayer = ecodistricts, 
                   listOfProportions = leading[[i]], vegLeadingType = vegLeadingTypes[k]
        )  
      })
    })
  })
  
  # lapply(seq_along(ecodistricts)[polygonsWithData[[1]]],function(i) {
  #   #callModule(decidOldMod,i,seed=i)
  #   callModule(decidOldMod,i,indivPolygonIndex=i,polygonLayer = ecodistricts, listOfProportions = leading[[1]])
  # })

  # lapply(seq_along(ecodistricts)[polygonsWithData[[2]]],function(i) {
  #   #callModule(decidOldMod,i,seed=i)
  #   callModule(decidOldMod,i,indivPolygonIndex=i,polygonLayer = ecodistricts, listOfProportions = leading[[2]])
  # })
  # 
  # lapply(seq_along(ecodistricts)[polygonsWithData[[3]]],function(i) {
  #   #callModule(decidOldMod,i,seed=i)
  #   callModule(decidOldMod,i,indivPolygonIndex=i,polygonLayer = ecodistricts, listOfProportions = leading[[3]])
  # })
  # 
  # lapply(seq_along(ecodistricts)[polygonsWithData[[4]]],function(i) {
  #   #callModule(decidOldMod,i,seed=i)
  #   callModule(decidOldMod,i,indivPolygonIndex=i,polygonLayer = ecodistricts, listOfProportions = leading[[4]])
  # })
  
  # lapply(seq_along(ecodistricts),function(i) {
  #   callModule(oldDecidByPolyMod,i,indivPolygonIndex=i,polygonLayer = ecodistricts, listOfProportions = leadingOld)
  # })
  
  
  #output$StudyRegion <- renderPlot(
    bind_shiny(plot_id = "StudyRegion",
    #height = 525, 
    #width = 800,
  #  {
      #ggplotly(ggStudyRegion )
      ggStudyRegion 
      
    #   g[[jj]]  <- ggplot() +
    #     geom_raster(data=packSaturationMapPts[[jj]],
    #                 aes(long, lat, fill=Probability, text=CountryName),
    #                 alpha = 0.98) +
    #     geom_polygon(data=shape.fort, 
    #                  aes(long, lat, group=group), 
    #                  colour='black',fill=NA) +
    #     coord_equal() +
    #     scale_fill_gradientn(colors=c('blue','blue','light blue', 'light blue','orange','red'),
    #                          #scale_fill_gradientn(colors=c('#ffffd4','#fee391','#fec44f','#fe9929','#d95f0e','#993404'),
    #                          name='Probability of a pack',  na.value = NA) + 
    #     theme(
    #       legend.position = 'right',
    #       legend.key.size = unit(1, "cm")
    #     ) +
    #     scale_x_continuous(expand=c(0,0)) + 
    #     scale_y_continuous(expand=c(0,0)) +
    #     labs(x='Easting', y='Northing', title=paste("Probability of pack presence in 2097, based on",
    #                                                 length(numPacks2AByTime[[jj]]), "replicate simulations, \n",
    #                                                 "with", jj, "starting locations"))
    #   
    #   #g1 <- g
     #}
    # gridExtra::grid.arrange(grobs = g)
    #ggplotly(g, tooltip = c("CountryName", "Probability"))
    
    #clearPlot()
    #Plot(shpStudyRegionOrig, title = "Proportion of leading spruce")
    #Plot(shpStudyRegion, addTo = "shpStudyRegionOrig")
  #}
)
  
  output$initialCommunityMap <- renderPlot({
    clearPlot()
    Plot(deciduous, addTo = "deciduous", title = "Proportion deciduous leading")
  })
  
  output$seralStagePctCoverMap <- renderPlot({
    clearPlot()
    Plot(mixed, addTo = "mixed", title = "Proportion mixed leading")
  })
  
  # output$ecoregionFig <- renderPlot({
  #   #zoom <- input$inputZoom
  #   ext <- extent(ecoregionMap)
  #   # halfRangex <- diff(range(ext[1],ext[2]))/2
  #   # halfRangey <- diff(range(ext[3],ext[4]))/2
  #   # extNew <- numeric(4)
  #   # extNew[1] <- ext[1]+halfRangex  - halfRangex/zoom
  #   # extNew[2] <- ext[1]+halfRangex  + halfRangex/zoom
  #   # extNew[3] <- ext[3]+halfRangey  - halfRangey/zoom
  #   # extNew[4] <- ext[3]+halfRangey  + halfRangey/zoom
  #   react$newExtent <- extent(ext)
  #   ecoregionMapNew <- crop(ecoregionMap, react$newExtent)
  #   Plot(ecoregionMapNew, title = FALSE, new=TRUE, speedup = 10)
  # })
  
  # output$initialCommunityMap <- renderPlot({
  #   initialCommunityMapNew <- crop(initialCommunityMap, react$newExtent)
  #   Plot(initialCommunityMapNew, title = FALSE, new=TRUE, speedup = 10)
  # })
  
  output$speciesInputs <- renderDataTable({
    landisInputs})#, digits = 1)
  
  output$speciesEcoregionInputs <- renderDataTable({
    spEcoReg})#, digits = 1)
  
  
  output$seralStagePctCoverMap <- renderPlot({
    browser()
    if(is.null(input$regionSelector)) regionNum <- 139 else regionNum <- as.numeric(input$regionSelector)
    num <- match(regionNum, availableRegions)
    toMap <- get(paste0("seralMapExampleRegion",num))
    Plot(toMap, title = FALSE, new=TRUE, speedup = 10, cols = rev(c("forestgreen", "olivedrab3",
                                                                    "goldenrod3", "grey")))
  })
  
  output$seralStagePctCover <- renderPlot({
    if(is.null(input$regionSelector)) regionNum <- 139 else regionNum <- as.numeric(input$regionSelector)
    ggplot(data = seralStageData[ecoregion == regionNum,],
           aes(fill = as.factor(seralStageData[ecoregion == regionNum,]$type))) + 
      #annotation_custom(sub1Title)+
      #annotation_raster(as.raster(seralMapExampleRegion1), xmin = 0.7, xmax = 3.7,
      #                  ymin = 25, ymax = 55)+
      geom_rect(aes(x = type,
                    xmin = as.numeric(type)-0.2, 
                    xmax = as.numeric(type)+0.2,
                    ymin = minAreaPercentage,
                    ymax = maxAreaPercentage)) +
      scale_fill_manual(name = "Seral Stage", values = c("forestgreen", "olivedrab3",
                                                         "goldenrod3", "grey"),
                        labels = c("Overold (>100)", "Old (80-100)",
                                   "Mature (40-80)", "Young (< 40)"),
                        guide = guide_legend(reverse = TRUE))+
      scale_y_continuous("Percentage in forested landscape (%)", 
                         breaks = c(seq(0, 20, by = 5), seq(80, 100, by = 5)),
                         labels = c(seq(0, 20, by = 5), seq(80, 100, by = 5)))+
      
      theme_bw()+
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect(colour = "black", size = 1),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_blank(),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_blank(),
            axis.ticks.length = unit(5, "points"),
            axis.ticks.y = element_blank(),
            legend.position = c(0.7, 0.6),
            legend.title = element_text(size = 15),
            legend.key = element_rect(fill = "white", colour = "white"),
            legend.margin = unit(50, "points"),
            legend.key.width = unit(20, "points"),
            legend.key.height = unit(20, "points"),
            legend.text = element_text(size = 12))+
      coord_flip()
    
  })
  
  
  
  output$vegTypePctCoverMap <- renderPlot({
    if(is.null(input$regionSelector)) regionNum <- 139 else regionNum <- as.numeric(input$regionSelector)
    num <- match(regionNum, availableRegions)
    toMap <- get(paste0("vegTypeMapExampleRegion",num))
    Plot(toMap, title = FALSE, new=TRUE, speedup = 10, cols = rev(c("white", "yellow3",
                                                                    "grey70", "white")))
  })
  
  output$vegTypePctCover <- renderPlot({
    if(is.null(input$regionSelector)) regionNum <- 139 else regionNum <- as.numeric(input$regionSelector)
    ggplot(data = vegTypeData[ecoregion == regionNum,],
           aes(fill = as.factor(vegTypeData[ecoregion == regionNum,]$type))) + 
      #annotation_custom(sub1Title)+
      geom_rect(aes(x = type,
                    xmin = as.numeric(type)-0.2, 
                    xmax = as.numeric(type)+0.2,
                    ymin = minAreaPercentage,
                    ymax = maxAreaPercentage)) +
      scale_fill_manual(name = "Vegetation Type", values = c("white", "yellow3",
                                                             "grey70", "white"),
                        labels = c(expression(paste("Mixed (", B[none], " > 50%)", sep = "")),
                                   expression(paste("Spruce Leading (",
                                                    B[spruce], " > 50%)", sep = "")),
                                   expression(paste("Aspen Leading (",
                                                    B[aspen], " > 50%)", sep = "")),
                                   expression(paste("Pine Leading (",
                                                    B[pine], " > 50%)", sep = ""))),
                        guide = guide_legend(reverse = TRUE))+
      scale_y_continuous("Percentage in forested landscape (%)", limits = c(0, 60),
                         breaks = c(seq(40, 60, by = 5)),
                         labels = c(seq(40, 60, by = 5)))+
      
      theme_bw()+
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect(colour = "black", size = 1),
            axis.text.x = element_text(size = 12),
            axis.title.x = element_text(size = 15),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.length = unit(5, "points"),
            axis.ticks.y = element_blank(),
            legend.position = c(0.45, 0.6),
            legend.title = element_text(size = 15),
            legend.key = element_rect(fill = "white", colour = "white"),
            legend.margin = unit(50, "points"),
            legend.key.width = unit(20, "points"),
            legend.key.height = unit(20, "points"),
            legend.text = element_text(size = 12),
            legend.text.align = 0)+
      coord_flip()
    
  })
  
  
  output$patchSizesFig5 <- renderPlot({
    if(is.null(input$regionSelector)) regionNum <- 139 else regionNum <- as.numeric(input$regionSelector)
    
    figuredata <- seralStageDataFig5[ecoregion == regionNum & patchClasses == "[0,1e+03)",]
    figuredata[,':='(minNofPatch = minNofPatch/100, maxNofPatch = maxNofPatch/100)]
    figure5_1_1 <- ggplot(data = figuredata,
                          aes(fill = as.factor(landType)))+
      geom_rect(aes(x = landType,
                    xmin = as.numeric(as.factor(landType))-0.2, 
                    xmax = as.numeric(as.factor(landType))+0.2,
                    ymin = minNofPatch,
                    ymax = maxNofPatch))+
      scale_fill_manual(name = "Seral stage", values = figuredata$colour,
                        labels = figuredata$labels)+
      scale_y_log10("Number of patches",
                    breaks = c(9, 10, 13, 16, seq(120, 240, by = 60)),
                    labels = c(9, 10, 13, 16, seq(120, 240, by = 60)))+
      theme_bw()+
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect(colour = "black", size = 1),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 12),
            axis.title = element_blank(),
            axis.ticks.length = unit(5, "points"),
            axis.ticks.y = element_blank(),
            legend.position = "none")+
      coord_flip()
    figure5_1_1gp <- ggplotGrob(figure5_1_1)
    rm(figuredata)
    
    
    # region 1 and class 2
    figuredata <- seralStageDataFig5[ecoregion == regionNum & patchClasses == "[1e+03,3e+03)",]
    figure5_1_2 <- ggplot(data = figuredata,
                          aes(fill = as.factor(figuredata$landType),
                              group = patchClasses))+
      geom_rect(aes(x = landType,
                    xmin = as.numeric(as.factor(landType))-0.2, 
                    xmax = as.numeric(as.factor(landType))+0.2,
                    ymin = minNofPatch,
                    ymax = maxNofPatch))+
      scale_fill_manual(name = "Seral stage", values = figuredata$colour,
                        labels = figuredata$labels)+
      theme_bw()+
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect(colour = "black", size = 1),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 12),
            axis.title = element_blank(),
            axis.ticks.length = unit(5, "points"),
            axis.ticks.y = element_blank(),
            legend.position = "none")+
      coord_flip()
    figure5_1_2gp <- ggplotGrob(figure5_1_2)
    rm(figuredata)
    
    
    # region 1 and class 3
    figuredata <- seralStageDataFig5[ecoregion == regionNum & patchClasses == "[3e+03,5e+03)",]
    figure5_1_3 <- ggplot(data = figuredata,
                          aes(fill = as.factor(figuredata$landType),
                              group = patchClasses))+
      geom_rect(aes(x = landType,
                    xmin = as.numeric(as.factor(landType))-0.2, 
                    xmax = as.numeric(as.factor(landType))+0.2,
                    ymin = minNofPatch,
                    ymax = maxNofPatch))+
      scale_fill_manual(name = "Seral stage", values = figuredata$colour,
                        labels = figuredata$labels)+
      theme_bw()+
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect(colour = "black", size = 1),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 12),
            axis.title = element_blank(),
            axis.ticks.length = unit(5, "points"),
            axis.ticks.y = element_blank(),
            legend.position = "none")+
      coord_flip()
    figure5_1_3gp <- ggplotGrob(figure5_1_3)
    rm(figuredata)
    
    
    # region 1 and class 4
    figuredata <- seralStageDataFig5[ecoregion == regionNum & (patchClasses == "[5e+03,4.88e+06)" |
                                                                 patchClasses == "[5e+03,7.05e+06)"),]
    figure5_1_4 <- ggplot(data = figuredata,
                          aes(fill = as.factor(figuredata$landType),
                              group = patchClasses))+
      geom_rect(aes(x = landType,
                    xmin = as.numeric(as.factor(landType))-0.2, 
                    xmax = as.numeric(as.factor(landType))+0.2,
                    ymin = minNofPatch,
                    ymax = maxNofPatch))+
      scale_fill_manual(name = "Seral stage", values = figuredata$colour,
                        labels = figuredata$labels)+
      theme_bw()+
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect(colour = "black", size = 1),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 12),
            axis.title = element_blank(),
            axis.ticks.length = unit(5, "points"),
            axis.ticks.y = element_blank(),
            legend.position = "none")+
      coord_flip()
    figure5_1_4gp <- ggplotGrob(figure5_1_4)
    
    rm(figuredata)
    
    figure5basedata <- data.table(x = seq(0, 1.99, length = 4), y = seq(0, 4, length = 4))
    
    figure5 <- ggplot(data = figure5basedata, aes(x = x, y = y))+
      geom_abline(, col = "white")+
      scale_x_continuous("", limits = c(-0.35, 1.99), breaks = 0:1,
                         labels = 0:1)+
      scale_y_continuous("", limits = c(-0.4, 4.3), breaks = 1:4,
                         labels = 1:4)+
      theme_bw()+
      theme(panel.grid.major = element_line(colour = "grey"),
            panel.grid.minor = element_blank(),
            panel.border = element_rect(colour = "black"),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank())
    
    figure5 <- figure5+annotation_custom(grob = figure5_1_1gp,
                                         xmin = 0.01, xmax = 1.99,
                                         ymin = 3.05, ymax = 3.99)+
      annotation_custom(grob = figure5_1_2gp,
                        xmin = 0.01, xmax = 1.99,
                        ymin = 2.05, ymax = 2.99)+
      annotation_custom(grob = figure5_1_3gp,
                        xmin = 0.01, xmax = 1.99,
                        ymin = 1.05, ymax = 1.99)+
      annotation_custom(grob = figure5_1_4gp,
                        xmin = 0.01, xmax = 1.99,
                        ymin = 0.05, ymax = 0.99)#+
    #annotation_custom(grob = figure5_2_1gp,
    #                  xmin = 1.01, xmax = 1.99,
    #                  ymin = 3.05, ymax = 3.99)+
    #annotation_custom(grob = figure5_2_2gp,
    #                  xmin = 1.01, xmax = 1.99,
    #                  ymin = 2.05, ymax = 2.99)+
    #annotation_custom(grob = figure5_2_3gp,
    #                  xmin = 1.01, xmax = 1.99,
    #                  ymin = 1.05, ymax = 1.99)+
    #annotation_custom(grob = figure5_2_4gp,
    #                  xmin = 1.01, xmax = 1.99,
    #                  ymin = 0.05, ymax = 0.99)
    a <- 0.35
    figure5 <- figure5+
      #annotate("text", x = 0.5, y = 4.25, label = "Region 1", size = 5)+
      #annotate("text", x = 1.5, y = 4.25, label = "Region 2", size = 5)+
      annotate("text", x = -0.18, y = 4.25, label = "Patch\nsize", size = 5)+
      annotate("text", x = -0.18, y = 3.5, label = "Class 1 \n< 1000ha", size = 5)+
      annotate("text", x = -0.18, y = 2.5, label = "Class 2 \n1000-3000ha", size = 5)+
      annotate("text", x = -0.18, y = 1.5, label = "Class 3 \n3000-5000ha", size = 5)+
      annotate("text", x = -0.18, y = 0.5, label = "Class 4 \n> 5000ha", size = 5)+
      #annotate("text", x = 0.5, y = 3.15, label = "(x100)", size = 5)+
      annotate("text", x = 1, y = 3.15, label = "(x100)", size = 5)+
      geom_rect(aes(xmin = 0.25, xmax = 1.9, ymin = -0.4, ymax = 0.0), fill = "white", col = "black")+
      geom_rect(aes(xmin = 0.55, xmax = 0.65, ymin = -0.3, ymax = -0.1), fill = "red", col = "red")+
      annotate("text", x = 0.65+0.1, y = -0.2, label = "Young", size = 5)+
      geom_rect(aes(xmin = 0.55+a, xmax = 0.65+a, ymin = -0.3, ymax = -0.1), fill = "green", col = "green")+
      annotate("text", x = 0.65+a+0.1, y = -0.2, label = "Mature", size = 5)+
      geom_rect(aes(xmin = 0.55+2*a, xmax = 0.65+2*a, ymin = -0.3, ymax = -0.1), fill = "blue", col = "blue")+
      annotate("text", x = 0.65+2*a+0.1, y = -0.2, label = "Old", size = 5)+
      geom_rect(aes(xmin = 0.55+3*a, xmax = 0.65+3*a, ymin = -0.3, ymax = -0.1), fill = "brown", col = "brown")+
      annotate("text", x = 0.65+3*a+0.1, y = -0.2, label = "Overold", size = 5)+
      annotate("text", x = 0.4, y = -0.2, label = "Seral stage:", size = 5)
    
    figure5
    
  })
  
  
  
  
  
  output$patchSizesFig6 <- renderPlot({
    if(is.null(input$regionSelector)) regionNum <- 139 else regionNum <- as.numeric(input$regionSelector)
    
    
    figuredata <- vegTypeDataFig6[ecoregion == regionNum & patchClasses == "[0,1e+03)",]
    figure6_1_1 <- ggplot(data = figuredata,
                          aes(fill = as.factor(landType)))+
      geom_rect(aes(x = landType,
                    xmin = as.numeric(as.factor(landType))-0.2, 
                    xmax = as.numeric(as.factor(landType))+0.2,
                    ymin = minNofPatch,
                    ymax = maxNofPatch))+
      scale_fill_manual(name = "Seral stage", values = figuredata$colour,
                        labels = figuredata$labels)+
      scale_y_log10("", breaks = c(1, 5, 10, 20, 50, 10000),
                    label = c(1, 5, 10, 20, 50, 10000))+
      # scale_y_log10("Number of patches",
      #               breaks = c(9, 10, 13, 16, seq(120, 240, by = 60)),
      #               labels = c(9, 10, 13, 16, seq(120, 240, by = 60)))+
      theme_bw()+
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect(colour = "black", size = 1),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 12),
            axis.title = element_blank(),
            axis.ticks.length = unit(5, "points"),
            axis.ticks.y = element_blank(),
            legend.position = "none")+
      coord_flip()
    figure6_1_1gp <- ggplotGrob(figure6_1_1)
    rm(figuredata)
    
    
    # region 1 and class 2
    figuredata <- vegTypeDataFig6[ecoregion == regionNum & patchClasses == "[1e+03,3e+03)",]
    figure6_1_2 <- ggplot(data = figuredata,
                          aes(fill = as.factor(figuredata$landType),
                              group = patchClasses))+
      geom_rect(aes(x = landType,
                    xmin = as.numeric(as.factor(landType))-0.2, 
                    xmax = as.numeric(as.factor(landType))+0.2,
                    ymin = minNofPatch,
                    ymax = maxNofPatch))+
      scale_fill_manual(name = "Seral stage", values = figuredata$colour,
                        labels = figuredata$labels)+
      theme_bw()+
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect(colour = "black", size = 1),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 12),
            axis.title = element_blank(),
            axis.ticks.length = unit(5, "points"),
            axis.ticks.y = element_blank(),
            legend.position = "none")+
      coord_flip()
    figure6_1_2gp <- ggplotGrob(figure6_1_2)
    rm(figuredata)
    
    
    # region 1 and class 3
    figuredata <- vegTypeDataFig6[ecoregion == regionNum & patchClasses == "[3e+03,5e+03)",]
    figure6_1_3 <- ggplot(data = figuredata,
                          aes(fill = as.factor(figuredata$landType),
                              group = patchClasses))+
      geom_rect(aes(x = landType,
                    xmin = as.numeric(as.factor(landType))-0.2, 
                    xmax = as.numeric(as.factor(landType))+0.2,
                    ymin = minNofPatch,
                    ymax = maxNofPatch))+
      scale_fill_manual(name = "Seral stage", values = figuredata$colour,
                        labels = figuredata$labels)+
      theme_bw()+
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect(colour = "black", size = 1),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 12),
            axis.title = element_blank(),
            axis.ticks.length = unit(5, "points"),
            axis.ticks.y = element_blank(),
            legend.position = "none")+
      coord_flip()
    figure6_1_3gp <- ggplotGrob(figure6_1_3)
    rm(figuredata)
    
    
    # region 1 and class 4
    figuredata <- vegTypeDataFig6[ecoregion == regionNum & (patchClasses == "[5e+03,4.88e+06)" |
                                                              patchClasses == "[5e+03,7.05e+06)"),]
    figure6_1_4 <- ggplot(data = figuredata,
                          aes(fill = as.factor(figuredata$landType),
                              group = patchClasses))+
      geom_rect(aes(x = landType,
                    xmin = as.numeric(as.factor(landType))-0.2, 
                    xmax = as.numeric(as.factor(landType))+0.2,
                    ymin = minNofPatch,
                    ymax = maxNofPatch))+
      scale_fill_manual(name = "Seral stage", values = figuredata$colour,
                        labels = figuredata$labels)+
      theme_bw()+
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect(colour = "black", size = 1),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 12),
            axis.title = element_blank(),
            axis.ticks.length = unit(5, "points"),
            axis.ticks.y = element_blank(),
            legend.position = "none")+
      coord_flip()
    figure6_1_4gp <- ggplotGrob(figure6_1_4)
    rm(figuredata)
    
    
    
    figure6basedata <- data.table(x = seq(0, 1.99, length = 4), y = seq(0, 4, length = 4))
    figure6 <- ggplot(data = figure6basedata, aes(x = x, y = y))+
      geom_abline(, col = "white")+
      scale_x_continuous("", limits = c(-0.35, 1.99), breaks = 0:1,
                         labels = 0:1)+
      scale_y_continuous("", limits = c(-0.4, 4.3), breaks = 1:4,
                         labels = 1:4)+
      theme_bw()+
      theme(panel.grid.major = element_line(colour = "grey"),
            panel.grid.minor = element_blank(),
            panel.border = element_rect(colour = "black"),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank())
    
    figure6 <- figure6+annotation_custom(grob = figure6_1_1gp,
                                         xmin = 0.01, xmax = 1.99,
                                         ymin = 3.05, ymax = 3.99)+
      annotation_custom(grob = figure6_1_2gp,
                        xmin = 0.01, xmax = 1.99,
                        ymin = 2.05, ymax = 2.99)+
      annotation_custom(grob = figure6_1_3gp,
                        xmin = 0.01, xmax = 1.99,
                        ymin = 1.05, ymax = 1.99)+
      annotation_custom(grob = figure6_1_4gp,
                        xmin = 0.01, xmax = 1.99,
                        ymin = 0.05, ymax = 0.99)
    intervl <- 0.32
    start1 <- 0.34
    
    figure6a <- figure6+
      #annotate("text", x = 0.5, y = 4.25, label = "Region 1", size = 5)+
      #annotate("text", x = 1.5, y = 4.25, label = "Region 2", size = 5)+
      annotate("text", x = -0.18, y = 4.25, label = "Patch\nsize", size = 5)+
      annotate("text", x = -0.18, y = 3.5, label = "Class 1 \n< 1000ha", size = 5)+
      annotate("text", x = -0.18, y = 2.5, label = "Class 2 \n1000-3000ha", size = 5)+
      annotate("text", x = -0.18, y = 1.5, label = "Class 3 \n3000-5000ha", size = 5)+
      annotate("text", x = -0.18, y = 0.5, label = "Class 4 \n> 5000ha", size = 5)+
      geom_rect(aes(xmin = 0.1, xmax = 1.95, ymin = -0.4, ymax = 0.0), fill = "white", col = "black")+
      geom_rect(aes(xmin = start1, xmax = start1+0.1, ymin = -0.3, ymax = -0.1), fill = "red", col = "red")+
      annotate("text", x = start1+0.05+intervl/2, y = -0.2, label = "Pine", size = 5)+
      geom_rect(aes(xmin = start1+intervl, xmax = start1+0.1+intervl, ymin = -0.3, ymax = -0.1), fill = "green", col = "green")+
      annotate("text", x = start1+0.05+intervl*3/2, y = -0.2, label = "Aspen", size = 5)+
      geom_rect(aes(xmin = start1+2*intervl, xmax = start1+0.1+2*intervl, ymin = -0.3, ymax = -0.1), fill = "blue", col = "blue")+
      annotate("text", x = start1+0.05+intervl*5/2, y = -0.2, label = "Spruce", size = 5)+
      geom_rect(aes(xmin = start1+3*intervl, xmax = start1+0.1+3*intervl, ymin = -0.3, ymax = -0.1), fill = "brown", col = "brown")+
      annotate("text", x = start1+0.05+intervl*7/2, y = -0.2, label = "Mixed", size = 5)#+
      #annotate("text", x = 0.28, y = -0.2, label = "Vegetation type:", size = 5)
    
    figure6a
    
  })
  
  
  output$patchSizesFig5Raster <- renderPlot({
    if(is.null(input$regionSelector)) regionNum <- 139 else regionNum <- as.numeric(input$regionSelector)
    
    num <- match(regionNum, availableRegions)
    overold <- get(paste0("overoldpatchmapRegion",num,"_5000"))
    
    Plot(overold, legend = FALSE, title = FALSE, new=TRUE)
    seekViewport("overold")
    grid.text(label = paste("N =", length(unique(getValues(overold)))-1),
              x = 0.1, y = 0.05, gp = gpar(fontsize = 15))
    
  })

  output$patchSizesFig6Raster <- renderPlot(height = 800,  {
    if(is.null(input$regionSelector)) regionNum <- 139 else regionNum <- as.numeric(input$regionSelector)
    
    num <- match(regionNum, availableRegions)
    aspenLeading <- get(paste0("aspenleadingpatchmapRegion",num,"_5000"))
    spruceLeading <- get(paste0("spruceleadingpatchmapRegion",num,"_5000"))
    
    Plot(aspenLeading, spruceLeading, new=TRUE, 
         legend = FALSE, title = FALSE)
    seekViewport("aspenLeading")
    grid.text(label = paste("N =", length(unique(getValues(aspenLeading)))-1),
              x = 0.1, y = 0.05, gp = gpar(fontsize = 15))
    seekViewport("spruceLeading")
    grid.text(label = paste("N =", length(unique(getValues(spruceLeading)))-1),
              x = 0.1, y = 0.05, gp = gpar(fontsize = 15))
    
  })

  output$numMapUnits <- renderValueBox({
    valueBox(
      value = 163,
      subtitle = "Number of unique map units"
    )
  })

  output$numInitialCommunities <- renderValueBox({
    valueBox(
      value = 442,
      subtitle = "Number of unique initial communities"
    )
  })
  
}

