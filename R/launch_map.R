#' Launch a web map in a browser to review and edit monitoring location information
#'
#' @param mloc Data frame of the monitoring location data generated using [odeqcdr::contin_import()].
#' @export
#' @return Launches a leaflet map within a Shiny app. Returns mloc data frame with any saved changes on app close.

launch_map <- function(mloc){

  df.mloc <-  mloc %>%
    dplyr::mutate(choices=paste(Monitoring.Location.ID, Monitoring.Location.Name, sep = " - "))

  app <- shiny::shinyApp(

    ui = shiny::shinyUI(shiny::fluidPage(shiny::tags$head(shiny::tags$style('.selectize-dropdown {z-index: 10000}')),
                                         shiny::fluidRow(shiny::column(width=5, shiny::selectInput(inputId="selectStation",
                                                                                                   label="Zoom to Monitoring Location",
                                                                                                   choices = unique(df.mloc$choices),
                                                                                                   multiple=FALSE, width='100%')),
                                                         shiny::column(width=2, shiny::selectizeInput(inputId="mlocTypeSelect", label="Monitoring Location Type",
                                                                                                      choices = c("",odeqcdr::valid_values(col="Monitoring Location Type")),
                                                                                                      multiple = FALSE, width='100%')),
                                                         shiny::column(width=2, shiny::verbatimTextOutput("STATUSprintout", placeholder=TRUE), style = "margin-top: 25px;"),
                                                         shiny::column(width=1, shiny::actionButton(inputId="STATUSsave", label="Update Status", style = "margin-top: 25px;"), align = "left")),

                                         shiny::fluidRow(shiny::column(width=1, shiny::h6("NHD Info"), align = "right"),
                                                         shiny::column(width=4, shiny::verbatimTextOutput("NHDprintout", placeholder=TRUE)),
                                                         shiny::column(width=1, shiny::actionButton(inputId="NHDsave", label="Save NHD Info", style = "margin-top: 0px;"), align = "left"),
                                                         shiny::column(width=1, shiny::h6("Click Lat/Long"), align = "right"),
                                                         shiny::column(width=3, shiny::verbatimTextOutput("XYprintout", placeholder=TRUE)),
                                                         shiny::column(width=1, shiny::actionButton(inputId="XYsave", label="Save Lat/Long", style = "margin-top: 0px;"), align = "left")),

                                         shiny::fluidRow(shiny::column(width=1, shiny::h6("LLID Info"), align = "right"),
                                                         shiny::column(width=4, shiny::verbatimTextOutput("LLIDprintout", placeholder=TRUE)),
                                                         shiny::column(width=1, shiny::actionButton(inputId="LLIDsave", label="Save LLID Info", style = "margin-top: 0px;"), align = "left"),
                                                         shiny::column(width=1, shiny::h6("AWQMS Alt ID"), align = "right"),
                                                         shiny::column(width=3, shiny::verbatimTextOutput("AWQMSprintout", placeholder=TRUE)),
                                                         shiny::column(width=1, shiny::actionButton(inputId="AWQMSsave", label="Save Alt ID"), align = "left")),

                                         shiny::fluidRow(shiny::column(width=12, leaflet::leafletOutput(outputId="map", width = "100%", height = "470px"))),
                                         shiny::fluidRow(shiny::column(width=1, shiny::actionButton(inputId="return_df", label="Close App, Return Changes", style = "margin-top: 5px;"))))
    ),

    server = shiny::shinyServer(function(input, output, session) {

      zoom_reactive <- shiny::reactive({

        df.mloc.zoom <- df.mloc %>%
          dplyr::filter(choices==input$selectStation)
        df.mloc.zoom

      })

      # Click Reactive Values
      cr <- shiny::reactiveValues(Monitoring.Location.Status.ID=NULL,
                                  Monitoring.Location.Type=NULL,
                                  Permanent.Identifier=NULL,
                                  Reachcode=NULL,
                                  Measure=NULL,
                                  LLID=NULL,
                                  River.Mile=NULL,
                                  Latitude=NULL,
                                  Longitude=NULL,
                                  Alternate.ID.1=NULL,
                                  Alternate.Context.1=NULL,
                                  df=NULL)

      # Populate with intial mloc data frame
      cr$df <- df.mloc

      # Retrieve cr$df from currently selected station
      df_reactive <- shiny::reactive({
        df.selectStation <- cr$df %>%
          dplyr::filter(choices==input$selectStation)
      })

      # Render the map
      output$map <- leaflet::renderLeaflet({

        # Get current station info

        #zoom_mloc <- zoom_reactive()
        zoom_mloc <- df_reactive()

        map <- leaflet::leaflet() %>%
          leaflet::setView(lng=zoom_mloc$Longitude[1], lat=zoom_mloc$Latitude[1], zoom = 16) %>%
          leaflet::addTiles() %>%
          leafem::addMouseCoordinates() %>%
          leaflet::addMapPane("Tiles", zIndex = 400) %>%
          leaflet::addMapPane("Points_AWQMS", zIndex= 405) %>%
          leaflet::addMapPane("Select", zIndex = 410) %>%
          leaflet::addMapPane("Lines", zIndex = 420) %>%
          leaflet::addMapPane("Points_Review", zIndex= 490) %>%
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
                                            fitBounds = FALSE,
                                            stroke=TRUE,
                                            weight=2,
                                            fill=FALSE,
                                            options = leaflet::leafletOptions(pane="Lines", minZoom = 12),
                                            highlightOptions = leaflet::highlightOptions(color="black",
                                                                                         weight = 4,
                                                                                         fillOpacity = 0.8,
                                                                                         bringToFront = TRUE,
                                                                                         sendToBack = TRUE),
                                            labelOptions = leaflet::labelOptions(offset = c(0,0),
                                                                                 opacity = 0.9,
                                                                                 textsize = "14px",
                                                                                 sticky = TRUE),
                                            labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.GNIS_Name+\": \"+props.ReachCode+\" \"}")
          ) %>%
          leaflet.esri::addEsriFeatureLayer(url = "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/DEQ_Streams/MapServer/0",
                                            group = "LLID Streams",
                                            layerId = "LLID",
                                            useServiceSymbology = FALSE,
                                            fitBounds = FALSE,
                                            stroke=TRUE,
                                            weight=2,
                                            fill=FALSE,
                                            options = leaflet::leafletOptions(pane="Lines", minZoom = 10),
                                            highlightOptions = leaflet::highlightOptions(color="black",
                                                                                         weight = 4,
                                                                                         fillOpacity = 0.8,
                                                                                         bringToFront = TRUE,
                                                                                         sendToBack = TRUE),
                                            labelOptions = leaflet::labelOptions(offset = c(0,0),
                                                                                 opacity = 0.9,
                                                                                 textsize = "14px",
                                                                                 sticky = TRUE),
                                            labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.NAME+\": \"+props.LLID+\" \"}")
          ) %>%
          leaflet.esri::addEsriFeatureLayer(url = "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/AWQMS_Stations/MapServer/1",
                                            group = "AWQMS Stations",
                                            layerId = "AWQMS_Stations1",
                                            fillOpacity = 0.5,
                                            fitBounds = FALSE,
                                            options = leaflet::leafletOptions(pane = "Points_AWQMS", minZoom = 13),
                                            markerOptions = leaflet::markerOptions(zIndexOffset = 0,
                                                                                   riseOnHover = TRUE),
                                            labelOptions = leaflet::labelOptions(offset = c(0,0),
                                                                                 opacity = 0.9,
                                                                                 textsize = "14px",
                                                                                 sticky = FALSE),
                                            popupOptions = leaflet::popupOptions(maxWidth = 600, maxHeight = 500),
                                            labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.MLocID+\": \"+props.StationDes+\" \"}"),
                                            popupProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return \"<b>Monitoring.Location.ID:</b> \"+props.MLocID+\"<br><b>Monitoring.Location.Name:</b> \"+props.StationDes+\"<br><b>Alternate.Context/OrgID:</b> \"+props.OrgID+\"<br><b>Monitoring.Location.Type:</b> \"+props.MonLocType+\" \"}")
          ) %>%
          leaflet.esri::addEsriFeatureLayer(url = "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/AWQMS_Stations/MapServer/0",
                                            group = "AWQMS Stations",
                                            layerId = "AWQMS_Stations0",
                                            fillOpacity = 0.5,
                                            fitBounds = FALSE,
                                            options = leaflet::leafletOptions(pane = "Points_AWQMS", minZoom = 13),
                                            markerOptions = leaflet::markerOptions(zIndexOffset = 0,
                                                                                   riseOnHover = TRUE),
                                            labelOptions = leaflet::labelOptions(offset = c(0,0),
                                                                                 opacity = 0.9,
                                                                                 textsize = "14px",
                                                                                 sticky = FALSE),
                                            popupOptions = leaflet::popupOptions(maxWidth = 600, maxHeight = 500),
                                            labelProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return props.station_key+\": \"+props.StationDes+\" \"}"),
                                            popupProperty = htmlwidgets::JS("function(feature){var props = feature.properties; return \"<b>Monitoring.Location.ID:</b> \"+props.station_key+\"<br><b>Monitoring.Location.Name:</b> \"+props.StationDes+\"<br><b>Alternate.Context/OrgID:</b> \"+props.ORGID+\"<br><b>Monitoring.Location.Type:</b> \"+props.MonLocType+\" \"}")
          )

        map <- map %>%
          leaflet::addAwesomeMarkers(data = cr$df,
                                     group ="Review Stations",
                                     layerId = df.mloc$choices,
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
                                     options = leaflet::markerOptions(zIndexOffset = 0,
                                                                      riseOnHover = TRUE,
                                                                      pane = "Points_Review")) %>%
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
          return()

        # Non DEQ Stations only,
        if(click$id == "AWQMS_Stations0") {

          cr$Alternate.ID.1 <- NA
          cr$Alternate.Context.1 <- NA

          output$AWQMSprintout <- shiny::renderPrint({
            df <- data.frame(Alternate.ID.1=NA,
                             Alternate.Context.1=NA)
            df
          })
        }

        # DEQ Stations only
        if(click$id =="AWQMS_Stations1") {

          cr$Alternate.ID.1 <- click$properties$MLocID
          cr$Alternate.Context.1 <- click$properties$OrgID

          output$AWQMSprintout <- shiny::renderPrint({
            df <- data.frame(Alternate.ID.1=click$properties$MLocID,
                             Alternate.Context.1=click$properties$OrgID)
            df
          })
        }

        if(click$id=="NHD") {

          NHDpoint <-odeqcdr::get_measure(pid=click$properties$Permanent_Identifier,
                                          x=click$lng, y=click$lat,
                                          return_sf=TRUE)

          cr$Permanent.Identifier <- click$properties$Permanent_Identifier
          cr$Reachcode <- click$properties$ReachCode
          cr$Measure <- NHDpoint$Measure

          request_NHD <- httr::GET(url = paste0("https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/NHDH_ORDEQ/MapServer/1/query?where=",
                                                "ReachCode='",click$properties$ReachCode,
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
        }

        if(click$id=="LLID") {

          # LLID
          rm_point <-odeqcdr::get_llidrm(llid=click$properties$LLID,
                                         x=click$lng,
                                         y=click$lat,
                                         max_length = 82,
                                         return_sf=TRUE)

          cr$LLID <- click$properties$LLID
          cr$River.Mile <- as.numeric(rm_point$River_Mile)

          pathLLID <- "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/DEQ_Streams/MapServer/0/query?where="

          request_LLID <- httr::GET(url = paste0(pathLLID,
                                                 "LLID='",
                                                 click$properties$LLID,
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

      # Populate printouts with whatever is saved into cr$df
      shiny::observeEvent(input$selectStation, {

        df.selectStation <- df_reactive()

        output$STATUSprintout <- shiny::renderText({
          df.selectStation$Monitoring.Location.Status.ID
        })

        mlt <- dplyr::pull(df.selectStation, Monitoring.Location.Type)

        mlt <- ifelse(is.na(mlt),character(0), mlt)

        shiny::updateSelectizeInput(
          session = session,
          inputId = "mlocTypeSelect",
          selected = mlt
        )

        output$NHDprintout <- shiny::renderPrint({
          df.selectStation %>%
            dplyr::select(Permanent.Identifier, Reachcode, Measure)
        })

        output$LLIDprintout <- shiny::renderPrint({
          df.selectStation %>%
            dplyr::select(LLID, River.Mile)
        })
        output$AWQMSprintout <- shiny::renderPrint({
          df.selectStation %>%
            dplyr::select(Alternate.ID.1, Alternate.Context.1)
        })
      })

      # When the map is clicked, update the Lat/Long
      shiny::observeEvent(input$map_click, {

        output$XYprintout <- shiny::renderPrint({

          map_click <- input$map_click
          if(is.null(map_click))
            return()

          cr$Latitude <- map_click$lat
          cr$Longitude <- map_click$lng

          df <- data.frame(Latitude=map_click$lat,
                           Longitude=map_click$lng)
          df
        })

      })

      # When status button is clicked, update the Status in cr$df
      shiny::observeEvent(input$STATUSsave, {

        cr$df <- cr$df %>%
          dplyr::mutate(Monitoring.Location.Status.ID = dplyr::case_when(choices==input$selectStation & is.na(Monitoring.Location.Status.ID) ~ "Accepted",
                                                                         choices==input$selectStation & Monitoring.Location.Status.ID == "Accepted" ~ "Rejected",
                                                                         choices==input$selectStation & Monitoring.Location.Status.ID == "Rejected" ~ "Accepted",
                                                                         TRUE ~ Monitoring.Location.Status.ID))

        output$STATUSprintout <- shiny::renderText({
          cr$df %>%
            dplyr::filter(choices==input$selectStation) %>%
            dplyr::pull(Monitoring.Location.Status.ID)
        })
      })

      # When mlocType list changed, update the Monitoring.Location.Type in cr$df
      shiny::observeEvent(input$mlocTypeSelect, {

        cr$df <- cr$df %>%
          dplyr::mutate(Monitoring.Location.Type = dplyr::if_else(choices==input$selectStation,
                                                                  input$mlocTypeSelect,
                                                                  Monitoring.Location.Type))
      })

      # When NHD button is clicked, update the NHD info in cr$df
      shiny::observeEvent(input$NHDsave, {

        cr$df <- cr$df %>%
          dplyr::mutate(Reachcode = ifelse(choices==input$selectStation,
                                           cr$Reachcode,
                                           Reachcode),
                        Measure = dplyr::if_else(choices==input$selectStation,
                                                 cr$Measure,
                                                 Measure),
                        Permanent.Identifier = dplyr::if_else(choices==input$selectStation,
                                                              cr$Permanent.Identifier,
                                                              Permanent.Identifier))

        output$NHDprintout <- shiny::renderText({"Success! NHD info saved."})
      })

      # When LLID button is clicked, update the LLID info in cr$df
      shiny::observeEvent(input$LLIDsave, {

        cr$df <- cr$df %>%
          dplyr::mutate(LLID= ifelse(choices==input$selectStation,
                                     cr$LLID,
                                     LLID),
                        River.Mile = dplyr::if_else(choices==input$selectStation,
                                                    cr$River.Mile,
                                                    River.Mile))

        output$LLIDprintout <- shiny::renderText({"Success! LLID info saved."})
      })

      # When AWQMS button is clicked, update the Alternate ID info in cr$df
      shiny::observeEvent(input$AWQMSsave, {

        cr$df <- cr$df %>%
          dplyr::mutate(Alternate.ID.1= ifelse(choices==input$selectStation,
                                               cr$Alternate.ID.1,
                                               Alternate.ID.1),
                        Alternate.Context.1 = dplyr::if_else(choices==input$selectStation,
                                                             cr$Alternate.Context.1,
                                                             Alternate.Context.1))

        output$AWQMSprintout <- shiny::renderText({"Success! Alternate ID saved."})
      })

      # When Lat/Long button is clicked, update the Lat/Long in cr$df
      # update map
      shiny::observeEvent(input$XYsave, {

        shiny::isolate({

          cr$df <- cr$df %>%
            dplyr::mutate(Latitude = ifelse(choices==input$selectStation,
                                            cr$Latitude,
                                            Latitude),
                          Longitude = dplyr::if_else(choices==input$selectStation,
                                                     cr$Longitude,
                                                     Longitude))

          leaflet::leafletProxy("map") %>%
            leaflet::removeMarker(layerId = df.mloc$choices) %>%
            leaflet::addAwesomeMarkers(data = cr$df,
                                       group ="Review Stations",
                                       layerId = df.mloc$choices,
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
                                                                         pane = "Points_Review"))
        })

        output$XYprintout <- shiny::renderText({"Success! Lat/Long saved."})
      })

      # When close app button is pushed, return df
      shiny::observeEvent(input$return_df, {
        return_df <- cr$df
        return_df$choices <- NULL
        shiny::stopApp(returnValue=return_df)
      })

      shiny::onStop(function() {
        shiny::stopApp()
      })

    })
  )

  df.return <- shiny::runApp(app, launch.browser = TRUE, quiet = TRUE)

  return(df.return)

}
