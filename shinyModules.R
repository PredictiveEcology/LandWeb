## Module for plotting histograms of leading vegetation type
vegAgeModUI <- function(id, vegLeadingTypes) {
  ns <- NS(id)
  
  ids <- strsplit(id, split = "_")[[1]]
  ageClassIndex <- as.numeric(ids[1])
  polygonIndex <- as.numeric(ids[2])
  vegTypeIndex <- as.numeric(ids[3])
  tagList(
    box(width = 4, solidHeader = TRUE, collapsible = TRUE, 
        title = paste0(vegLeadingTypes[vegTypeIndex]),
        withSpinner(plotOutput(ns("propCoverHists"), height = 300))
    )
  )
} 

vegAgeMod <- function(input, output, session, listOfProportions, indivPolygonIndex, 
                      vegLeadingType) {
  
  output$propCoverHists <- renderPlot(height = 300, {
      breaksLabels <- 0:11/10
      breaks <- breaksLabels - 0.05
      barplotBreaks <- breaksLabels + 0.05
      
      actualPlot <- hist(listOfProportions, plot = FALSE, breaks = breaks)
      barplot(actualPlot$counts/sum(actualPlot$counts), xlim = range(breaks), xlab="", ylab = "Proportion in NRV",
              col="darkgrey",border="grey", main = "", width = 0.1, space = 0)
      axis(1, at = barplotBreaks, labels = breaksLabels)
    
  })
}



######################################################################################################
######################################################################################################
clumpMod <- function(input, output, session, Clumps, id, ageClasses, vegLeadingTypes, numReps) {
  output$h <- renderPlot({
    
    a <- Clumps()
    ids <- strsplit(id, split = "_")[[1]]
    ageClassIndex <- as.numeric(ids[1])
    polygonIndex <- as.numeric(ids[2])
    vegTypeIndex <- as.numeric(ids[3])
    
    forHistDT <- a[ageClass==ageClasses[ageClassIndex] & vegCover==vegLeadingTypes[vegTypeIndex] & polygonID==polygonIndex]
    maxNumClusters <- a[ageClass==ageClasses[ageClassIndex] & polygonID==polygonIndex, .N, by = c("vegCover","rep")]$N + 1
    maxNumClusters <- if(length(maxNumClusters)==0) 6 else pmax(6, max(maxNumClusters))
    forHist <- rep(0, numReps)
    if(NROW(forHistDT)) {
      numByRep <- forHistDT[,.N,by="rep"]
      forHist[seq_len(NROW(numByRep))] <- numByRep$N
    }
    breaksLabels <- 0:(maxNumClusters)
    breaks <- breaksLabels - 0.5
    barplotBreaks <- breaksLabels + 0.5
    
    actualPlot <- hist(forHist, #plot = FALSE, 
                       breaks = breaks)
    barplot(actualPlot$counts/sum(actualPlot$counts), 
            xlim = range(breaks),#c(0,maxNumClusters), 
            xlab="", ylab = "Proportion in NRV",
            col="darkgrey",border="grey", main = "", width = rep(1, length(forHist)), space = 0)
    axis(1, at = barplotBreaks, labels = breaksLabels)
      
  })
  
}

clumpModOutput <- function(id, vegLeadingTypes) {
  ns <- NS(id)
  
  ids <- strsplit(id, split = "_")[[1]]
  ageClassIndex <- as.numeric(ids[1])
  polygonIndex <- as.numeric(ids[2])
  vegTypeIndex <- as.numeric(ids[3])
  box(width = 4, solidHeader = TRUE, collapsible = TRUE, 
      title = paste0(vegLeadingTypes[vegTypeIndex]),
      withSpinner(plotOutput(ns("h"), height = 300))
  )
  
} 



######################################################################################################
######################################################################################################
clumpMod2Input <- function(id, label = "CSV file") {
  ns <- NS(id)
  
  tagList(
    numericInput(ns("PatchSize33"), value = 500, min = 100, max = NA,
                 label=paste0("Type patch size in hectares that defines 'Large', ",
                              "(numbers below 100 will not work)")
    )
  )
}

clumpMod2 <- function(input, output, session, tsf, vtm, currentPolygon, 
                      cl, 
                      ageClasses = ageClasses,
                      patchSize,
                      cacheRepo = paths$cachePath,
                      id, indivPolygonIndex,
                      largePatchesFn) {
  Clumps <- reactive({
    patchSize <- as.integer(input$PatchSize33)
    
    message(paste("Running largePatchesFn"))
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0, {
                   args <- list(largePatchesFn, timeSinceFireFiles = tsf,
                                vegTypeMapFiles = vtm,
                                cl = if (tryCatch(is(cl, "cluster"), 
                                                  error = function(x) FALSE)) cl,
                                polygonToSummarizeBy = currentPolygon,
                                ageClasses = ageClasses, countNumPatches = countNumPatches,
                                cacheRepo = cacheRepo,
                                debugCache="complete",
                                omitArgs = "cl")
                   args <- args[!unlist(lapply(args, is.null))]
                   largePatches <- do.call(Cache, args)
                   setProgress(1)
                 })
    message(paste("  Finished largePatchesFn"))
    if(FALSE) {
      keepArtifacts3 <- unique(showCache(paths$cachePath, after = startCacheTime)$artifact)
      keepArtifacts <<- setdiff(keepArtifacts3, keepArtifacts)
      archivist::addTagsRepo(keepArtifacts,
                             repoDir = paths$cachePath,
                             tags = paste0("LandWebVersion:", LandWebVersion))
    }
    if(Sys.info()["nodename"]=="W-VIC-A105388") {
      #message("Stopping App using stopApp")
      #stopApp()
    }
    
    return(list(Clumps=largePatches[sizeInHa>patchSize], patchSize = patchSize))
  })
  
  return(Clumps)
}

######################################################################################################
######################################################################################################
leafletMapUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = 12, 
        solidHeader = TRUE, collapsible = TRUE, 
        title = "Area covered by this demo (in red), within the LandWeb study area (blue)",
        withSpinner(leaflet::leafletOutput(ns("leafletMap1"), height = 600)),
        selectInput(ns("leafletMapPolygons"), "Other layers to show summaries with", 
                    choices = names(polygons[1:(length(polygons)/4)+(length(polygons)/4)*3]), 
                    selected = names(polygons[1:(length(polygons)/4)+(length(polygons)/4)*3])[[1]])
    )
  )
}

leafletMap <- function(input, output, session) {
  
  output$leafletMap1 <- renderLeaflet({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0, {
                   polyNum <- polygonInput()
                   polyDemo <- polygons[[polyNum + (length(polygons)/4)*3]]#6]] # leaflet projection, DEMO scale
                   polyFull <- polygons[[polyNum + (length(polygons)/4)*2]] # leaflet projection, Full scale
                   a <- leaflet() %>% addTiles(group = "OSM (default)") %>%
                     addPolygons(data = spTransform(shpStudyRegionFull, crs(polyFull)), color = "blue", group = "Full",
                                 fillOpacity = 0.8, weight = 1,
                                 fillColor = ~colorFactor("Spectral", fireReturnInterval)(fireReturnInterval)) %>% #,
                     #popup = paste(polyFull[[polygonIndivIdsColum[[polyNum]]]])) %>%
                     #spTransform(shpStudyRegionFull, crs(polyFull))
                     addPolygons(data = polyDemo, color = "red", group = "Demo",
                                 fillOpacity = 0.0, weight = 3) %>% #,
                     #popup = paste(polyDemo[[polygonIndivIdsColum[[polyNum]]]]))  %>%
                     #addLegend(position = "bottomright", pal = "Spectral", values = shpStudyRegionFull$fireReturnInterval,
                     #           title = "Fire Return Interval (years)") %>%
                     
                     setView(mean(c(xmin(polyDemo),xmax(polyDemo))), 
                             mean(c(ymin(polyDemo),ymax(polyDemo))), 
                             zoom = 5) 
                   setProgress(1)
                 })
    
    a
  })
  
  observe({#Observer to show Popups on click
    click <- input$leafletMap1_shape_click
    if (!is.null(click)) {
      showpos(x=click$lng, y=click$lat)
    }
  })
  
  showpos <- function(x=NULL, y=NULL) {#Show popup on clicks
    #Translate Lat-Lon to cell number using the unprojected raster
    #This is because the projected raster is not in degrees, we cannot use it!
    #ras1 <- rasterInput()
    polyNum <- polygonInput()
    polyFull <- polygons[[polyNum + (length(polygons)/4)*2]] # leaflet projection, Full scale
    clickedSP <- SpatialPoints(matrix(c(x,y), ncol=2), proj4string=crs(polyFull))
    friVal <- clickedSP %>%
      spTransform(crs(shpStudyRegion)) %>%
      extract(shpStudyRegionFull, .) %>%
      .["fireReturnInterval"]
    
    if (!is.na(friVal)) {#If the click is inside the raster...
      #polyDemo <- polygons[[polyNum + (length(polygons)/4)*3]]#6]] # leaflet projection, DEMO scale
      #Get row and column, to print later
      colNam <- names(polygons)[[(length(polygons)/4)*4]]
      # pol <- polygons[[(length(polygons)/4)*4]]
      # friPoly <- shpStudyRegion
      # rc <- rowColFromCell(ras1, cell)
      # 
      # #Get values from raster and polygon
      polyVal <- clickedSP %>%
        extract(polyFull, .) %>%
        .[polygonIndivIdsColum[[colNam]]]
      
      content <- paste0(polygonIndivIdsColum[[colNam]],": ",polyVal,"<br>",
                        "Fire Return Interval: ", friVal)
      proxy <- leafletProxy("leafletMap1")
      #add Popup
      proxy %>% clearPopups() %>% addPopups(x, y, popup = content)
      
    }
  }
  
  polygonInput <- reactive({
    switch(input$leafletMapPolygons,
           "Ecodistricts Demo" = 1#, 
           #"Alberta FMUs Demo" = 2
    )
  })
  
  return(polygonInput)
}


######################################################################################################
######################################################################################################

timeSinceFireMod <- function(input, output, session, rasts) {
  
  observe({
    rasInp <- rasterInput()
    #ras1 <- rasInp$r
    sliderVal <- rasInp$sliderVal
    pol <- polygons[[(length(polygons)/4)*4]]
    leafZoom <- if(is.null(input$timeSinceFire2_zoom)) leafletZoomInit else input$timeSinceFire2_zoom
    proxy <- leafletProxy("timeSinceFire2")
    if(!exists("ranNum")) ranNum <<- 1 else  ranNum <<- ranNum + 1
    proxy %>%
      addTiles(urlTemplate=file.path(studyArea, paste0("outrstTimeSinceFire_year",
                                                       paddedFloatToChar(sliderVal+summaryPeriod[1], nchar(end(mySim))),
                                                       "LFLT/{z}/{x}/{y}.png")),
               option = tileOptions(tms = TRUE, minZoom = 1, maxZoom = 10,
                                    opacity = 0.8),
               group="Time since fire", layerId = as.character(ranNum)) %>%
      removeTiles(layerId=ranNum - 1) %>%
      #addRasterImage(x = ras1, group = "timeSinceFireRasts", opacity = 0.7,
      #               colors = timeSinceFirePalette, project = FALSE)  %>%
      
      #addPolygons(data = pol, fillOpacity = 0, weight = 1, group="Polygons") %>%
      # addLegend(position = "bottomright", pal = timeSinceFirePalette,
      #           values = na.omit(ras1[]), title = "Time since fire \n(years)") %>%
      addLayersControl(options = layersControlOptions(autoZIndex = TRUE,
                                                      collapsed = FALSE),
                       baseGroups = c("Open Cycle Map", "ESRI World Imagery"), #"Toner Lite"),
                       overlayGroups = c("Time since fire", "Fire return interval"))# %>%
    # addLayersControl(
    #   baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
    #   overlayGroups = c("Quakes", "Outline"),
    #   options = layersControlOptions(collapsed = FALSE)
    # )
    # setView(mean(c(xmin(pol),xmax(pol))),
    #         mean(c(ymin(pol),ymax(pol))),
    #         zoom = leafZoom)
    
    proxy
  })
  
  
  output$timeSinceFire2 <- renderLeaflet({
      leafZoom <- leafletZoomInit #if(is.null(input$timeSinceFire2_zoom)) 7 else input$timeSinceFire2_zoom
      rasInp <- isolate(rasterInput())
      polyNum <- polygonInput()
      #polyDemo <- polygons[[polyNum + (length(polygons)/4)*3]]#6]] # leaflet projection, DEMO scale
      polyFull <- polygons[[polyNum + (length(polygons)/4)*2]] # leaflet projection, Full scale
      
      ras1 <- rasInp$r
      sliderVal <- rasInp$sliderVal
      pol <- polygons[[(length(polygons)/4)*4]]
      shpStudyRegionFullLFLT <- spTransform(shpStudyRegionFull, crs(polyFull))
      leafMap <- leaflet(options = leafletOptions(minZoom = 1, maxZoom = 10)) %>% #addTiles(group = "OSM (default)") %>%
        #addProviderTiles("Esri.WorldTopoMap") %>%
        addProviderTiles("Thunderforest.OpenCycleMap", group="Open Cycle Map",
                         options=providerTileOptions(minZoom = 1, maxZoom = 10)) %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World Imagery", 
                         options=providerTileOptions(minZoom = 1, maxZoom = 10)) %>%
        #addProviderTiles("ESRI.WorldTopoMap", group = "ESRI World Topo Map") %>%
        addPolygons(data = shpStudyRegionFullLFLT, color = "blue", 
                    group = "Fire return interval",
                    fillOpacity = 0.3, weight = 1,
                    fillColor = ~colorFactor("Spectral", fireReturnInterval)(fireReturnInterval)) %>% #,
        #addPolygons(data = polyDemo, color = "red", group = "Demo",
        #            fillOpacity = 0.0, weight = 3) %>% #,
        # addTiles(urlTemplate=file.path(studyArea, paste0("outrstTimeSInceFire_year",
        #                                                  paddedFloatToChar(sliderVal+summaryPeriod[1], 
        #                                                                    nchar(end(mySim))),
        #                                                  "LFLT/{z}/{x}/{y}.png")),
        #          option = tileOptions(tms = FALSE, minZoom = 6, maxZoom = 11)) %>%
        #addRasterImage(x = ras1, group = "timeSinceFireRasts", opacity = 0.7, 
        #               colors = timeSinceFirePalette, project = FALSE)  %>%
        #addPolygons(data = pol, fillOpacity = 0, weight = 1) %>%
        addLegend(position = "bottomright", pal = timeSinceFirePalette,
                  #values = na.omit(ras1[]),
                  values = 1:maxAge,
                  title = paste0("Time since fire",br(),"(years)")) %>%
        addMeasure(
          position = "bottomleft",
          primaryLengthUnit = "kilometers",
          primaryAreaUnit = "hectares",
          activeColor = "#3D535D",
          completedColor = "#7D4479") %>%
        addEasyButton(easyButton(
          icon="fa-map", title="Zoom to Demonstration Area",
          #onClick=JS("function(btn, map){ map.setZoom(5); }"))) %>%
          #onClick=JS(paste0("function(btn, map){ map.setView([",mean(c(ymin(pol),ymax(pol))), 
          #                   ", ",mean(c(xmin(pol),xmax(pol))) ,"], 8)}")))) %>%
          onClick=JS(paste0("function(btn, map){ map.fitBounds([[",ymin(pol),", ",xmin(pol),"], [" 
                          ,ymax(pol),", ",xmax(pol) ,"]])}")))) %>%
        addEasyButton(easyButton(
          icon="fa-globe", title="Zoom out to LandWeb study area",
          #onClick=JS("function(btn, map){ map.setZoom(5); }"))) %>%
          onClick=JS(paste0("function(btn, map){ map.setView([",mean(c(ymin(shpStudyRegionFullLFLT),
                                                                       ymax(shpStudyRegionFullLFLT))), 
                             ", ",mean(c(xmin(shpStudyRegionFullLFLT),
                                         xmax(shpStudyRegionFullLFLT))) ,"], 5)}")))) %>%
          # onClick=JS(paste0("function(btn, map){ map.fitBounds([[",ymin(pol),", ",xmin(pol),"], [" 
          #                   ,ymax(pol),", ",xmax(pol) ,"]])}")))) %>%
        addMiniMap(
          tiles = providers$OpenStreetMap,
          toggleDisplay = TRUE) %>%
        # addLegend(position = "bottomleft", pal = fireReturnIntervalPalette, opacity=0.3,
        #           values = sort(unique(shpStudyRegionFull$fireReturnInterval))[1:6*3],
        #           title = paste0("Fire Return Interval(years)"),
        #           layerId="Fire return interval legend") %>%
        setView(mean(c(xmin(shpStudyRegionFullLFLT),xmax(shpStudyRegionFullLFLT))), 
                mean(c(ymin(shpStudyRegionFullLFLT),ymax(shpStudyRegionFullLFLT))), 
                zoom = leafZoom
        ) 
    leafMap
  })
  
  
  output$timeSinceFire2Hist <- renderPlot({
    ras1 <- rasterInput()$r
    Nbreaks <- ceiling(maxValue(ras1)/10)
    timeSinceFireHist <- hist(ras1[], plot = FALSE, breaks = Nbreaks)
    barplot(timeSinceFireHist$counts*prod(rasterResolution)/1e4, xlab = "Time since fire \n(Years)",
            col = timeSinceFirePalette(1:(maxAge/10)), width = 1, space = 0, ylab = "Area (ha)")
    axis(1, at = timeSinceFireHist$breaks/10, labels = 0:Nbreaks*10)
    
  })
  
  rasterInput <- reactive({
    sliderVal <- if(is.null(input$timeSinceFire1Slider)) 0 else input$timeSinceFire1Slider
    r <- rasts[[sliderVal/10+1]] # slider units are 10, starting at 0; index here is 1 to length (tsf)
    
    if(useGdal2Tiles) {
      message("Running gdal2TilesFn for layer ", sliderVal/10+1, " of ", length(rasts))
      Cache(gdal2TilesFn, r, filename=asPath(filename(r)), #notOlderThan = Sys.time(),
            zoomRange=3:10, color_text_file = asPath(colorTableFile), 
            cacheRepo = paths$cachePath, digestPathContent = TRUE)
    }
    if(TRUE) {
      #if(Sys.info()["nodename"]=="W-VIC-A105388") stopApp()
      if (ncell(r) > 3e5) {
        r <- Cache(sampleRegular, r, size = 4e5, #notOlderThan = Sys.time(),
                   asRaster = TRUE, cacheRepo = paths$cachePath)
        r[r[]>401] <- maxAge
        r[r[]==0] <- NA
      }
      
    }
    list(r=r, sliderVal=sliderVal)
  })
  
  polygonInput <- reactive({
    1
    # switch(input$leafletMapPolygons,
    #        "Ecodistricts Demo" = 1#, 
    #        #"Alberta FMUs Demo" = 2
    # )
  })
  observe({#Observer to show Popups on click
    click <- input$timeSinceFire2_shape_click
    if (!is.null(click)) {
      showpos(x=click$lng, y=click$lat)
    }
  })
  
  showpos <- function(x=NULL, y=NULL) {#Show popup on clicks
    #Translate Lat-Lon to cell number using the unprojected raster
    #This is because the projected raster is not in degrees, we cannot use it!
    colNam <- names(polygons)[[(length(polygons)/4)*4]]
    pol <- polygons[[(length(polygons)/4)*3]]
    friPoly <- shpStudyRegion
    
    sp <- SpatialPoints(cbind(x,y), proj4string = crs(pol))
    ras1 <- rasterInput()$r
    cell <- cellFromXY(ras1, c(x, y))
    #if (!is.na(cell)) {#If the click is inside the raster...
    #Get row and column, to print later
    rc <- rowColFromCell(ras1, cell)
    
    #Get values from raster and polygon
    polyVal <- sp %>%
      extract(pol, .) %>%
      .[polygonIndivIdsColum[[colNam]]]
    friVal <- sp %>%
      spTransform(crs(shpStudyRegionFull)) %>%
      extract(shpStudyRegionFull, .) %>%
      .["fireReturnInterval"]
    
    #if (!is.na(cell)) {#If the click is inside the raster...
    val = ras1[][cell]
    #}
    
    firstPart <- if(!is.na(val)) {
      paste0("Time Since Fire=", round(val, 1), " years <br>")
    } else {
      ""
    }
    content <- paste0(firstPart,
                      polygonIndivIdsColum[[colNam]],": ",polyVal,"<br>",
                      "Fire Return Interval: ", friVal, "<br>",
                      "Lat/Long: ", round(y,4),", ", round(x,4))
    proxy <- leafletProxy("timeSinceFire2")
    #add Popup
    proxy %>% clearPopups() %>% addPopups(x, y, popup = content)
    #}
    #}
  }
  
  
}

timeSinceFireModUI <- function(id, tsf) {
  ns <- NS(id)
  tagList(
    box(width = 8, solidHeader = TRUE, collapsible = TRUE, 
        h4(paste("Below are a sequence of snapshots of the landscape, showing the natural range of",
                 "variation in time since fire. Click on the 'play' button at the bottom right to animate")),
        withSpinner(leaflet::leafletOutput(ns("timeSinceFire2"), height = 600)),
        sliderInput(ns("timeSinceFire1Slider"), 
                    "Individual snapshots of time since fire maps. Use play button (bottom right) to animate.", 
                    min = 0, max = (length(tsf)-1)*10, value = 0, step = 10, 
                    animate = animationOptions(interval = 2500, loop = FALSE))
    ),
    box(width = 4, solidHeader = TRUE, collapsible = TRUE, 
        h4(paste("Current time since distribution distribution")),
        withSpinner(plotOutput(ns("timeSinceFire2Hist"), height = 600))
    )
    
  )
}




######################################################################################################
######################################################################################################

### simulation overview diagrams
simInfoUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    tabBox(width = 12,
           tabPanel("Module Diagram", simModuleDiagramUI(ns("moduleDiagram"))),
           tabPanel("Object Diagram", simObjectDiagramUI(ns("objectDiagram"))),
           tabPanel("Event Diagram", simEventDiagramUI(ns("eventDiagram")))
    )
  )
}

simInfo <- function(input, output, session, sim) {
  callModule(simModuleDiagram, "moduleDiagram", sim)
  callModule(simObjectDiagram, "objectDiagram", sim)
  callModule(simEventDiagram, "eventDiagram", sim)
}

######################################################################################################
######################################################################################################

simModuleDiagramUI <- function(id) {
  ns <- NS(id)
  
  ui_output <- tagList()
  
  ui_output$diagramTitle <- h3("Dependency graph (simplified)")
  
  ui_output$diagramDescription <- p(paste(
    "A network diagram illustrating the simplified module",
    "dependencies of a simulation.",
    "Arrows between modules indicate at least one data object",
    "passed from one module to the other."))
  
  ui_output$diagram <- withSpinner(imageOutput(ns("modDiag"), height = 750))
  
  return(ui_output)
}

simModuleDiagram <- function(input, output, session, sim) {
  output$modDiag <- renderPlot({
    moduleDiagram(sim, vertex.size = 30)
  })
}


######################################################################################################
######################################################################################################

simObjectDiagramUI <- function(id) {
  ns <- NS(id)
  
  ui_output <- tagList()
  
  ui_output$title <- h3("Summary of the data objects shared among modules.")
  
  ui_output$description <- p(paste(
    "A sequence diagram illustrating the data object dependencies",
    "of a simulation.",
    "Arrows between modules indicate at least one data object",
    "passed from one module to the other."
  ))
  
  ui_output$diagram <- withSpinner(DiagrammeR::DiagrammeROutput(ns("objectDiagram"), height = 1500))
  
  return(ui_output)
}

simObjectDiagram <- function(input, output, session, sim) {
  output$objectDiagram <- DiagrammeR::renderDiagrammeR({
    objectDiagram(sim)
  })
}


######################################################################################################
######################################################################################################

simEventDiagramUI <- function(id) {
  ns <- NS(id)
  
  out <- tagList()
  
  out$title <-  h3("Summary of the simulation event sequence.")
  
  out$description <- p(paste(
    "Simulation time is presented on the x-axis.",
    "Each module appears in a color-coded row,",
    "within which each event for that module is displayed",
    "corresponding to the sequence of events for that module.",
    "Note that only the start time of the event is meaningful is",
    "this figure:",
    "the width of the bar associated with a particular module's",
    "event DOES NOT correspond to an event's 'duration'."
  ))
  
  out$diagram <- withSpinner(DiagrammeR::DiagrammeROutput(ns("eventDiagram"), height = 1500))
  
  return(out)
}

simEventDiagram <- function(input, output, session, sim) {
  output$eventDiagram <- DiagrammeR::renderDiagrammeR({
    eventDiagram(sim)
  })
}


######################################################################################################
######################################################################################################

### detailed module info
moduleInfoUI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("allModuleInfo"))
}

moduleInfo <- function(input, output, session, sim) {
  output$allModuleInfo <- renderUI({
    fluidRow(
      tagList(lapply(modules(sim), function(module) {
        m <- slot(depends(sim), "dependencies")[[module]]
        rmdFile <- file.path("m", module, paste0(module, ".Rmd"))
        box(title = module, width = 12, status = "success", collapsible = TRUE,
            div(
              p(paste("Description:", slot(m, "description"))),
              p(paste("Keywords:", paste(slot(m, "keywords"), collapse = ", "))),
              p(paste("Authors:", paste(slot(m, "authors"), collapse = "; "))),
              p(paste("Version:", slot(m, "version"))),
              p("Documentation:", actionLink(paste0(module, "_Rmd"), "Rmd"))
              ## TO DO: add more metadata as required
            ),
            bsModal(module, basename(rmdFile),
                    trigger = paste0(module, "_Rmd"), size = "large",
                    includeMarkdown(rmdFile))
        )
      }))
    )
  })
}
