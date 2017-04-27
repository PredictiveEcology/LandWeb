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
        plotOutput(ns("propCoverHists"), height = 300)
    )
  )
} 

vegAgeMod <- function(input, output, session, listOfProportions, indivPolygonIndex, 
                      vegLeadingType) {
  
  output$propCoverHists <- renderPlot(height = 300, {
    if(useGGplotForHists) {
      #withProgress(message = 'Calculation in progress',
      #             detail = 'This may take a while...', value = 0, {
      actualPlot <- 
                     ggplot(data = data.frame(x = listOfProportions),
                            aes(x = x)) +
                     #stat_bin(bins = 30) +
                     stat_bin(aes(y=..count../sum(..count..)),#bins = max(6, max(a)+2), 
                              fill="grey", colour="darkgrey", size = 1,
                              binwidth = 0.1) + 
                     xlab("") + #xlab("Proportion of polygon") +
                     xlim(-0.1,1.1) +
                     theme_bw() +
                     theme(text = element_text(size = 16)) +
                     ylab("Proportion in NRV")
       #             setProgress(1)
      #            })
     actualPlot
    } else {
      #browser()
      
      breaksLabels <- 0:11/10
      breaks <- breaksLabels - 0.05
      barplotBreaks <- breaksLabels + 0.05
      
      actualPlot <- hist(listOfProportions, plot = FALSE, breaks = breaks)
      barplot(actualPlot$counts/sum(actualPlot$counts), xlim = range(breaks), xlab="", ylab = "Proportion in NRV",
           col="darkgrey",border="grey", main = "", width = 0.1, space = 0)
      axis(1, at = barplotBreaks, labels = breaksLabels)
      
      # actualPlot <-
                   #    try(hist(unlist(lapply(listOfProportions, function(x) x[indivPolygonIndex, "Deciduous leading"])),
                   #             plot = FALSE))
                   # if(!(is(actualPlot, "try-error")))
                   #   actualPlot
                   # Plot(actualPlot, new = TRUE, visualSqueeze = 1, gpText = gpar(fontsize = 16),
                   #      title = "",
                   #      addTo = paste0("actualPlot_dist",polygonLayer$ECODISTRIC[indivPolygonIndex]))
    }
                   
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
    #browser()
    if(NROW(forHistDT)) {
      #browser()
      numByRep <- forHistDT[,.N,by="rep"]
      forHist[seq_len(NROW(numByRep))] <- numByRep$N
    }
    if(useGGplotForHists) {
      
      withProgress(message = 'Calculation in progress',
                   detail = 'This may take a while...', value = 0, {
                     actualPlot <- ggplot(data = data.frame(x = forHist), aes(x = x)) + 
                       stat_bin(aes(y=..density..),#bins = max(6, max(a)+2), 
                                fill="grey", colour="darkgrey", size = 1,
                                binwidth = 1) + 
                       xlab("") + #xlab("Proportion of polygon") + 
                       xlim(-1,maxNumClusters) +
                       #ggthemes::theme_fivethirtyeight() +
                       #ggthemes::scale_color_fivethirtyeight() +
                       theme_bw() + 
                       theme(text = element_text(size = 16)) + 
                       ylab("Proportion in NRV")
                     setProgress(1)
                   })
      
      actualPlot
    } else {
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
      
    }
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
      plotOutput(ns("h"), height = 300)
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
                                cl = if (tryCatch(is(cl, "cluster"), error = function(x) FALSE)) cl,
                                polygonToSummarizeBy = currentPolygon,
                                ageClasses = ageClasses, 
                                cacheRepo = cacheRepo)
                   args <- args[!unlist(lapply(args, is.null))]
                   largePatches <- do.call(Cache, args)
                   setProgress(1)
                 })
    message(paste("  Finished largePatchesFn"))
    
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
        leaflet::leafletOutput(ns("leafletMap1"), height = 600),
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
                     addPolygons(data = polyFull, color = "blue", group = "Full",
                                 fillOpacity = 0.2, weight = 1,
                                 popup = paste(polyFull[[polygonIndivIdsColum[[polyNum]]]])) %>%
                     addPolygons(data = polyDemo, color = "red", group = "Demo",
                                 fillOpacity = 0.6, weight = 3,
                                 popup = paste(polyDemo[[polygonIndivIdsColum[[polyNum]]]]))  %>%
                     setView(mean(c(xmin(polyDemo),xmax(polyDemo))), 
                             mean(c(ymin(polyDemo),ymax(polyDemo))), 
                             zoom = 5) 
                   setProgress(1)
                 })
    
    a
  })
  
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
  output$timeSinceFire1 <- renderLeaflet({
    ras1 <- rasterInput()
    pol <- polygons[[(length(polygons)/4)*4]]
    leafZoom <- if(is.null(input$timeSinceFire1_zoom)) 7 else input$timeSinceFire1_zoom
    leafMap <- leaflet() %>% addTiles(group = "OSM (default)") %>%
      addRasterImage(x = ras1, group = "timeSinceFireRasts", opacity = 0.7, 
                     colors = timeSinceFirePalette, project = FALSE)  %>%
      addPolygons(data = pol, fillOpacity = 0, weight = 1) %>%
      addLegend(position = "bottomright", pal = timeSinceFirePalette, 
                values = na.omit(ras1[]), title = "Time since fire (years)") %>%
      addLayersControl(options = layersControlOptions(autoZIndex = TRUE)) %>%
      setView(mean(c(xmin(pol),xmax(pol))), 
              mean(c(ymin(pol),ymax(pol))), 
              zoom = leafZoom
      ) 
    
    leafMap
  })
  
  rasterInput <- reactive({
    r <- rasts[[input$timeSinceFire1Slider/10+1]] # slider units are 10, starting at 0; index here is 1 to length (tsf)
    if (ncell(r) > 2e5)
      r <- sampleRegular(r, size = 2e5, asRaster = TRUE)
    r
  })
}

timeSinceFireModUI <- function(id, tsf) {
  ns <- NS(id)
  tagList(
    box(width = 12, solidHeader = TRUE, collapsible = TRUE, 
        h4(paste("Below are a sequence of snapshots of the landscape, showing the natural range of",
                 "variation in time since fire. Click on the 'play' button at the bottom right to animate")),
        leaflet::leafletOutput(ns("timeSinceFire1"), height = 600),
        sliderInput(ns("timeSinceFire1Slider"), 
                    "Individual snapshots of time since fire maps. Use play button (bottom right) to animate.", 
                    min = 0, max = (length(tsf)-1)*10, value = 0, step = 10, 
                    animate = animationOptions(interval = 2500, loop = TRUE))
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
  
  ui_output$diagram <- imageOutput(ns("modDiag"), height = 750)
  
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
  
  ui_output$diagram <- DiagrammeR::DiagrammeROutput(ns("objectDiagram"), height = 1500)
  
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
  
  out$diagram <- DiagrammeR::DiagrammeROutput(ns("eventDiagram"), height = 1500)
  
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
