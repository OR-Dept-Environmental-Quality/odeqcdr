map <- leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addMapPane("Tiles", zIndex = 420) %>%
  leaflet::addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  leaflet::addWMSTiles("https://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WmsServer",
                       group = "Hydrography",
                       options = leaflet::WMSTileOptions(format = "image/png",
                                                         transparent = TRUE,
                                                         pane= "Tiles"),
                       attribution = '<a href="https://basemap.nationalmap.gov/arcgis/rest/services/USGSHydroCached/MapServer">USGS The National Map: National Hydrography Dataset.</a>',
                       layers = "0")

map <- map %>%
  leaflet::addLayersControl(overlayGroups = c("Monitoring Stations",
                                              "Hydrography",
                                              "World Imagery"),
                            options = leaflet::layersControlOptions(collapsed = FALSE)
  ) %>%
  leaflet::hideGroup(c("Hydrography", "World Imagery")) %>%

