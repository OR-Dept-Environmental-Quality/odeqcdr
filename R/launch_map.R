#' Launch a web map in a browser to review monitoring stations
#'
#' @param mloc Data frame of the monitoring location data generated using [odeqcdr::contin_import()].
#' @export
#' @return Launches a leaflet map within a Shiny app

launch_map <- function(mloc){

  # htmlwidgets::JS("
  #         function (error, featureCollection) {
  #           if (error || featureCollection.features.length === 0) {
  #             return false;
  #           } else {
  #             return featureCollection.features[0].properties.ReachCode;
  #           }
  #         }
  #         ")

  df.mloc <-  mloc %>%
    dplyr::mutate(choices=paste(Monitoring.Location.ID, Monitoring.Location.Name, sep = " - "))

  app <- shiny::shinyApp(

    ui = shiny::shinyUI(
      shiny::fluidPage(
        shiny::tags$head(shiny::tags$style('.selectize-dropdown {z-index: 10000}')),
        shiny::selectInput(inputId="selectStation", label="Zoom to Station", choices = unique(df.mloc$choices), multiple=FALSE,
                           width='100%'),
        leaflet::leafletOutput(outputId="map", width = "100%", height = "500px"),
        shiny::h6("NHD Info"),
        shiny::verbatimTextOutput("NHDprintout"),
        shiny::h6("LLID Info"),
        shiny::verbatimTextOutput("LLIDprintout"),
        shiny::h6("Latitude Longitude of Click"),
        shiny::verbatimTextOutput("xyprintout")
      )
    ),

    server = shiny::shinyServer(function(input, output, session) {

      zoom_reactive <- shiny::reactive({

        df.mloc.zoom <- df.mloc %>%
          dplyr::filter(choices==input$selectStation) %>%
          dplyr::mutate(zoom_level=16)

        df.mloc.zoom

      })

      output$map <- leaflet::renderLeaflet({

        zoom_mloc <- zoom_reactive()

        map <- leaflet::leaflet() %>%
          leaflet::setView(lng=zoom_mloc$Longitude[1], lat=zoom_mloc$Latitude[1], zoom = zoom_mloc$zoom_level[1]) %>%
          leaflet::addTiles() %>%
          leafem::addMouseCoordinates() %>%
          leaflet::addMapPane("Tiles", zIndex = 420) %>%
          leaflet::addMapPane("Select", zIndex = 430) %>%
          leaflet::addMapPane("Lines", zIndex = 440) %>%
          leaflet::addMapPane("Points", zIndex= 450) %>%
          leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "World Imagery") %>%
          leaflet::addWMSTiles("https://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WmsServer",
                               group = "Hydrography",
                               options = leaflet::WMSTileOptions(format = "image/png",
                                                                 transparent = TRUE,
                                                                 pane= "Tiles"),
                               attribution = '<a href="https://basemap.nationalmap.gov/arcgis/rest/services/USGSHydroCached/MapServer">USGS The National Map: National Hydrography Dataset.</a>',
                               layers = "0") %>%
          leaflet.esri::addEsriFeatureLayer(url = "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/NHDH_ORDEQ/MapServer/1",
                                            group = "NHD Streams",
                                            layerId = "NHD",
                                            useServiceSymbology = FALSE,
                                            fitBounds = FALSE,
                                            stroke=TRUE,
                                            weight=2,
                                            fill=FALSE,
                                            options = leaflet::leafletOptions(pane="Lines"),
                                            highlightOptions = leaflet::highlightOptions(color="black",
                                                                                         weight = 4,
                                                                                         fillOpacity = 0.8,
                                                                                         bringToFront = TRUE,
                                                                                         sendToBack = TRUE),
          ) %>%
          leaflet.esri::addEsriFeatureLayer(url = "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/DEQ_Streams/MapServer/0",
                                            group = "LLID Streams",
                                            layerId = "LLID",
                                            useServiceSymbology = FALSE,
                                            fitBounds = FALSE,
                                            stroke=TRUE,
                                            weight=2,
                                            fill=FALSE,
                                            options = leaflet::leafletOptions(pane="Lines"),
                                            highlightOptions = leaflet::highlightOptions(color="black",
                                                                                         weight = 4,
                                                                                         fillOpacity = 0.8,
                                                                                         bringToFront = TRUE,
                                                                                         sendToBack = TRUE),
          ) %>%
          leaflet.esri::addEsriFeatureLayer(url = "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/AWQMS_Stations/MapServer/1",
                                            group = "AWQMS Stations",
                                            layerId = "AWQMS_Stations",
                                            fillOpacity = 0.5,
                                            useServiceSymbology = TRUE,
                                            fitBounds = FALSE,
                                            #markerType="circleMarker",
                                            markerOptions = leaflet::markerOptions(zIndexOffset = 0,
                                                                                   riseOnHover = TRUE,
                                                                                   pane = "Points"),
                                            labelOptions = leaflet::labelOptions(offset = c(0,0),
                                                                                 opacity = 0.9,
                                                                                 textsize = "14px",
                                                                                 sticky = FALSE),
                                            popupOptions = leaflet::popupOptions(maxWidth = 600, maxHeight = 500),
                                            labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.MLocID+\": \"+props.StationDes+\" \"}"),
                                            popupProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return \"<b>Monitoring.Location.ID:</b> \"+props.MLocID+\"<br><b>Monitoring.Location.Name:</b> \"+props.StationDes+\"<br><b>Monitoring.Location.Type:</b> \"+props.MonLocType+\" \"}")
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
                                     popupOptions = leaflet::popupOptions(maxWidth = 600, maxHeight = 500),
                                     labelOptions = list(offset = c(0,-25), opacity = 0.9, textsize = "14px", sticky = FALSE),
                                     options = ~leaflet::markerOptions(zIndexOffset = 0,
                                                                       riseOnHover = TRUE,
                                                                       pane = "Points")) %>%
          leaflet::groupOptions(group="NHD Streams", zoomLevels = 12:20) %>%
          leaflet::groupOptions(group="LLID Streams", zoomLevels = 10:20) %>%
          leaflet::groupOptions(group="AWQMS Stations", zoomLevels = 10:20) %>%
          leaflet::addLayersControl(overlayGroups = c("Review Stations",
                                                      "AWQMS Stations",
                                                      "NHD Streams",
                                                      "LLID Streams",
                                                      "Hydrography",
                                                      "World Imagery"),
                                    options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
          leaflet::addEasyButton(leaflet::easyButton(
            icon = "fa-globe",
            title = "Zoom to all Review Monitoring Statons",
            onClick = htmlwidgets::JS("function(btn, map){
                var groupLayer = map.layerManager.getLayerGroup('Review Stations');
                map.fitBounds(groupLayer.getBounds());
                 }"))) %>%
          leaflet::addMeasure(
            position = "bottomleft",
            primaryLengthUnit = "meters",
            primaryAreaUnit = "sqmeters",
            activeColor = "#3D535D",
            completedColor = "#7D4479") %>%
          leaflet::hideGroup(c("LLID Streams","Hydrography","World Imagery"))

        map

      })

      shiny::observe({
        leaflet::leafletProxy("map") %>%
          leaflet::clearPopups()

        click <- input$map_geojson_click
        if(is.null(click))
          return(NULL)

        if(click$id=="AWQMS_Stations") {
          return(NULL)
        }

        if(click$id=="NHD") {

          NHDpoint <-odeqcdr::get_measure(pid=click$properties$Permanent_Identifier,
                                       x=click$lng,
                                       y=click$lat,
                                       return_sf=TRUE)

          reachcode <- click$properties$ReachCode
          request_NHD <- httr::GET(url = paste0("https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/NHDH_ORDEQ/MapServer/1/query?where=",
                                                "ReachCode='",reachcode,
                                                "'&outFields=*&returnGeometry=true&returnIdsOnly=false&f=GeoJSON"))
          response_NHD <- httr::content(request_NHD, as = "text", encoding = "UTF-8")

          shiny::isolate({
            leaflet::leafletProxy("map") %>%
              leaflet::removeGeoJSON(layerId="reachcodeClick") %>%
              leaflet::removeMarker(layerId="measurePoint") %>%
              leaflet::addGeoJSON(geojson = response_NHD,
                                  layerId = "reachcodeClick",
                                  group = "NHD Streams",
                                  fill=FALSE,
                                  color="orange",
                                  weight = 6,
                                  opacity = 0.7,
                                  options=leaflet::leafletOptions(pane="Select")) %>%
              leaflet::addCircleMarkers(data=NHDpoint,
                                        layerId = "measurePoint",
                                        group = "NHD Streams",
                                        color="red")

            output$NHDprintout <- shiny::renderPrint({
              df <- sf::st_drop_geometry(NHDpoint)
              df
            })

          })
        } else {

          # LLID
          rm_point <-odeqcdr::get_llidrm(llid=click$properties$LLID,
                                      x=click$lng,
                                      y=click$lat,
                                      max_length = 150,
                                      return_sf=TRUE)

          llid <- click$properties$LLID
          pathLLID <- "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/DEQ_Streams/MapServer/0/query?where="

          request_LLID <- httr::GET(url = paste0(pathLLID,
                                                 "LLID='",
                                                 llid,
                                                 "'&outFields=*&returnGeometry=true&returnIdsOnly=false&f=GeoJSON"))
          response_LLID <- httr::content(request_LLID, as = "text", encoding = "UTF-8")

          shiny::isolate({
            leaflet::leafletProxy("map") %>%
              leaflet::removeGeoJSON(layerId="llidClick") %>%
              leaflet::removeMarker(layerId="llidrmPoint") %>%
              leaflet::addGeoJSON(geojson = response_LLID,
                                  layerId = "llidClick",
                                  group = "LLID Streams",
                                  fill=FALSE,
                                  color="green",
                                  weight = 6,
                                  opacity = 0.7,
                                  options=leaflet::leafletOptions(pane="Select")) %>%
              leaflet::addCircleMarkers(data=rm_point,
                                        layerId = "llidrmPoint",
                                        group = "LLID Streams",
                                        color="blue")

            output$LLIDprintout <- shiny::renderPrint({
              df <- sf::st_drop_geometry(rm_point)
              df
            })

          })
        }

      })

      output$xyprintout <- shiny::renderPrint({

        map_click <- input$map_click

        if(is.null(map_click))
          return(NULL)

        df <- data.frame(Latitude=map_click$lat,
                         Longitude=map_click$lng)
        df
      })

      session$onSessionEnded(function() {
        shiny::stopApp()
      })

    })
  )

  shiny::runApp(app, launch.browser = TRUE)

}
