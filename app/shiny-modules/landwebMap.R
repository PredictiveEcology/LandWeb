landwebMapUI <- function(id) {
  ns <- NS(id)

  leafletOutput(ns("map"), width = "100%", height = "600px") %>%
    shinycssloaders::withSpinner()
}

landwebMap <- function(input, output, session, rctStudyArea, polygonList, rctChosenPolyName) {

  userFMA <- reactive({
    fma <- polygonList[[rctChosenPolyName()]] %>% spTransform(., CRS("+init=epsg:4326"))
    Cache(rmapshaper::ms_simplify,
          input = fma,
          keep = 0.05,
          sys = rmapshaper::check_sys_mapshaper(verbose = FALSE))
  })

  output$map <- renderLeaflet({
    lw <- aggregate(rctStudyArea()) %>% spTransform(., CRS("+init=epsg:4326"))
    landweb <- Cache(rmapshaper::ms_simplify,
                     input = lw,
                     keep = 0.05,
                     sys = rmapshaper::check_sys_mapshaper(verbose = FALSE))

    leaflet(landweb, options = leafletOptions(minZoom = 1, maxZoom = 10)) %>%
      enableTileCaching() %>%
      addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "ESRI World Imagery",
                       options = providerTileOptions(minZoom = 1, maxZoom = 10)) %>%
      addPolygons(data = landweb, color = "blue", group = "LandWeb",
                  fillColor = "blue", fillOpacity = 0.2, weight = 3) %>%
      fitBounds(xmin(landweb), ymin(landweb), xmax(landweb), ymax(landweb)) %>%
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "kilometers",
        primaryAreaUnit = "hectares",
        activeColor = "#3D535D",
        completedColor = "#7D4479")
  })

  proxy <- leafletProxy(session$ns("map"))

  observe({
    proxy %>%
      clearGroup("FMA") %>%
      addPolygons(data = userFMA(), color = "orange", group = "FMA",
                  fillColor = "orange", fillOpacity = 0.5)
  })
}
