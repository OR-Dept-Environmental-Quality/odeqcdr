#' Launch a web map in a browser to review monitoring stations
#'
#' @param mloc Data frame of the monitoring location data generated using [odeqcdr::contin_import()].
#' @export
#' @return Launches a leaflet map within a Shiny app

launch_map <- function(mloc){

  df.mloc <-  mloc %>%
    dplyr::mutate(choices=paste(Monitoring.Location.ID, Monitoring.Location.Name, sep = " - "))

  app <- shiny::shinyApp(

    ui = shiny::shinyUI(
      shiny::fluidPage(
        shiny::tags$head(shiny::tags$style('.selectize-dropdown {z-index: 10000}')),
        shiny::selectInput(inputId="selectStation", label="Zoom to Station", choices = unique(df.mloc$choices), multiple=FALSE,
                           width='100%'),
        leaflet::leafletOutput(outputId="map", width = "100%", height = "600px")
      )
    ),

    server = shiny::shinyServer(function(input, output, session) {

      zoom_reactive <- shiny::reactive({

        df.mloc.zoom <- df.mloc %>%
          dplyr::filter(choices==input$selectStation) %>%
          dplyr::mutate(zoom_level=18)

        df.mloc.zoom

      })

      output$map <- leaflet::renderLeaflet({

        zoom_mloc <- zoom_reactive()

        #setView(-122.6006, 45.295541, 12)

        popup_fun <- htmlwidgets::JS("
          function (error, featureCollection) {
            if (error || featureCollection.features.length === 0) {
              return false;
            } else {
              return featureCollection.features[0].properties.CLASS_DESC;
            }
          }
          ")

        map <- leaflet::leaflet() %>%
          leaflet::setView(lng=zoom_mloc$Longitude[1], lat=zoom_mloc$Latitude[1], zoom = zoom_mloc$zoom_level[1]) %>%
          leaflet::addTiles() %>%
          leaflet::addMapPane("Tiles", zIndex = 420) %>%
          leaflet::addMapPane("Lines", zIndex = 430) %>%
          leaflet::addMapPane("Points", zIndex= 440) %>%
          leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "World Imagery") %>%
          leaflet::addWMSTiles("https://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WmsServer",
                               group = "Hydrography",
                               options = leaflet::WMSTileOptions(format = "image/png",
                                                                 transparent = TRUE,
                                                                 pane= "Tiles"),
                               attribution = '<a href="https://basemap.nationalmap.gov/arcgis/rest/services/USGSHydroCached/MapServer">USGS The National Map: National Hydrography Dataset.</a>',
                               layers = "0") %>%
          leaflet.esri::addEsriFeatureLayer(url = "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/NHDH_ORDEQ/MapServer/1",
                                            group = "NHD",
                                            useServiceSymbology = FALSE,
                                            fitBounds = FALSE,
                                            stroke=TRUE,
                                            weight=2,
                                            fill=FALSE,
                                            highlightOptions = leaflet::highlightOptions(color="black",
                                                                                         weight = 4,
                                                                                         fillOpacity = 0.8,
                                                                                         bringToFront = TRUE,
                                                                                         sendToBack = TRUE),
                                            popupProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.mloc_uid+\", \"+props.MonLocType+ \" \"+props.StationDes+\" \"}")
          ) %>%
          leaflet.esri::addEsriFeatureLayer(url = "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/AWQMS_Stations/MapServer/1",
                                            group = "AWQMS Stations",
                                            fill= TRUE,
                                            color= "red",
                                            fillColor="red",
                                            stroke = FALSE,
                                            fillOpacity = 0.5,
                                            useServiceSymbology = FALSE,
                                            fitBounds = FALSE,
                                            markerType="circleMarker",
                                            markerOptions = leaflet::markerOptions(pane = "Points",
                                                                                   riseOnHover = TRUE),
                                            labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.mloc_uid+\", \"+props.MonLocType+ \" \"+props.StationDes+\" \"}"),
                                            popupProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.mloc_uid+\", \"+props.MonLocType+ \" \"+props.StationDes+\" \"}")
          )

        map <- map %>%
          leaflet::addAwesomeMarkers(data = unique(df.mloc),
                                     group ="Review Stations",
                                     popup = ~paste0("<b>Monitoring.Location.ID:</b> ", Monitoring.Location.ID, "<br>",
                                                     "<b>Monitoring.Location.Name:</b> ", Monitoring.Location.Name, "<br>",
                                                     "<b>Monitoring.Location.Type:</b> ", Monitoring.Location.Type, "<br>",
                                                     "<b>Latitude:</b> ", Latitude, "<br>",
                                                     "<b>Longitude:</b> ", Longitude, "<br>",
                                                     "<b>Horizontal.Datum:</b> ", Horizontal.Datum, "<br>",
                                                     "<b>Coordinate.Collection.Method:</b> ", Coordinate.Collection.Method, "<br>",
                                                     "<b>Source.Map.Scale:</b> ", Source.Map.Scale, "<br>",
                                                     "<b>Monitoring.Location.Description:</b> ", Monitoring.Location.Description, "<br>",
                                                     "<b>Tribal.Land:</b> ", Tribal.Land, "<br>",
                                                     "<b>Tribal.Land.Name:</b> ", Tribal.Land.Name, "<br>",
                                                     "<b>Alternate.ID.1:</b> ", Alternate.ID.1, "<br>",
                                                     "<b>Alternate.Context.1:</b> ", Alternate.Context.1, "<br>",
                                                     "<b>Alternate.ID.2:</b> ", Alternate.ID.2, "<br>",
                                                     "<b>Alternate.Context.2:</b> ", Alternate.Context.2, "<br>",
                                                     "<b>Alternate.ID.3:</b> ", Alternate.ID.3, "<br>",
                                                     "<b>Alternate.Context.3:</b> ", Alternate.Context.3, "<br>",
                                                     "<b>Reachcode:</b> ", Reachcode, "<br>",
                                                     "<b>Measure:</b> ", Measure, "<br>",
                                                     "<b>LLID:</b> ", LLID, "<br>",
                                                     "<b>River.Mile:</b> ", River.Mile, "<br>",
                                                     "<b>Permanent.Identifier:</b> ", Permanent.Identifier, "<br>"),
                                     label = ~paste0(Monitoring.Location.ID, ": ", Monitoring.Location.Name),
                                     lat = ~Latitude,
                                     lng = ~Longitude,
                                     icon = leaflet::awesomeIcons(icon = "glyphicon-none",
                                                                  iconColor = 'black',
                                                                  library = 'glyphicon',
                                                                  markerColor = "orange"),
                                     popupOptions = leaflet::popupOptions(maxWidth = 600, maxHeight = 300),
                                     labelOptions = list(offset = c(0,-25), opacity = 0.9, textsize = "14px"),
                                     options = ~leaflet::markerOptions(zIndexOffset = 0,
                                                                       riseOnHover = TRUE,
                                                                       pane = "Points")) %>%
          leaflet::addLayersControl(overlayGroups = c("Review Stations",
                                                      "AWQMS Stations",
                                                      "NHD",
                                                      "Hydrography",
                                                      "World Imagery"),
                                    options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
          leaflet::addMeasure(
            position = "bottomleft",
            primaryLengthUnit = "meters",
            primaryAreaUnit = "sqmeters",
            activeColor = "#3D535D",
            completedColor = "#7D4479") %>%
          leaflet::hideGroup(c("Hydrography","World Imagery"))

        map

      })

      session$onSessionEnded(function() {
        shiny::stopApp()
        })

      })
    )

  shiny::runApp(app, launch.browser = TRUE)

}
