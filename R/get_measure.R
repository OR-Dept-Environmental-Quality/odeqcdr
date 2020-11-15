#' Get measure value along Oregon DEQ's NHDH_OR_931v220.
#'
#' The function will query Oregon DEQ's NHD feature service to determine the measure value along a target NHD flowline. The Permanent Identifier (pid) is
#' used to select a specific reach from the feature service. The x and y coordinates are snapped to the nearest vertex on the selected flowline and the measure value determined.
#' If return_sf=TRUE the sf is projected into ESPG:4326 (WGS84) which is the expected default for [leaflet].
#'
#' This function is intended to be a helper function for [odeqcdr::launch_map] although it may be used independently as well.
#'
#' The feature service can be accessed at https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/NHDH_ORDEQ/MapServer/1
#'
#' @param pid The NHD Permanent_Identifier value as a string.
#' @param x The longitude in decimal degrees.
#' @param y The latitude in decimal degrees.
#' @param max_length The maximum segment length in meters to split the NHD reach into. Default is 25 meters. Passed to [odeqcdr::split_lines]. The smaller the distance the longer the operation takes.
#' @param return_sf Boolean. A TRUE value will return the sf object with data frame columns for GNIS_Name, Permanent_Identifier, ReachCode, and Measure.
#' FALSE will return the measure value as a character. Default is FALSE.
#' @export
#' @return sf object with data frame columns for Permanent_Identifier, ReachCode, measure, and snap_distance

get_measure <- function(pid, x, y, max_length=25, return_sf=FALSE){

  # Test data
  #pid="165555667"
  #reachcode="18010206003567"
  #y=42.09361
  #x=-122.3822

  # web mercator
  to_crs <- 3857

  # NAD 83
  #to_crs <- 4269

  #   request_NHD <- httr::GET(url = paste0(pathNHD, "ReachCode='",reachcode,"'&outFields=*&returnGeometry=true&returnIdsOnly=false&f=GeoJSON"))
  #   response_NHD <- httr::content(request_NHD, as = "text", encoding = "UTF-8")

  # get the site, make sf object, NAD 83 EPSG:4269
  site <- sf::st_as_sf(data.frame(Longitude=x, Latitude=y), coords = c("Longitude", "Latitude"), crs = 4269) %>%
    sf::st_transform(crs=to_crs)

  pathNHD <- "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/NHDH_ORDEQ/MapServer/1/query?where="

  request_NHD <- httr::GET(url = paste0(pathNHD, "Permanent_Identifier='",pid,"'&outFields=*&returnGeometry=true&returnIdsOnly=false&f=GeoJSON"))
  response_NHD <- httr::content(request_NHD, as = "text", encoding = "UTF-8")

  reach_df <- geojsonsf::geojson_sf(response_NHD) %>%
    sf::st_drop_geometry()

  # get measure value at top and bottom
  reach <- geojsonsf::geojson_sf(response_NHD) %>%
    sf::st_cast( to="POINT") %>%
    dplyr::mutate(Measure=unlist(lapply(geometry, FUN=function(x) {x[4]}), recursive = TRUE))

  meas_max <- max(reach$Measure)
  meas_min <- min(reach$Measure)

  rm(reach)

  # Split line into segments
  reach1 <- geojsonsf::geojson_sf(response_NHD) %>%
    sf::st_transform(crs=to_crs) %>%
    sf::st_cast(to="LINESTRING") %>%
    sf::st_zm() %>%
    odeqcdr::split_lines(max_length = max_length, id ="ReachCode")

  # segment length in meters
  reach1$length_seg <- units::set_units(sf::st_length(reach1), m)

  reach_points <- sf::st_cast(reach1, to="POINT")
  reach_points$row=as.numeric(row.names(reach_points))

  # Get only the first point on each linestring segment, and the last point (most upstream). Use the row number to filter.
  reach_points <- dplyr::filter(reach_points, row %in% c(1:nrow(reach_points),  max(reach_points$row)))

  # Set first point to length zero
  reach_points[1,c("length_seg")] <- 0

  # Snap distance in meters because of crs
  reach_points$Snap_Distance <- sf::st_distance(reach_points, site, by_element = TRUE)

  if(units::set_units(min(reach_points$Snap_Distance), m) > units::set_units(400, m))
    warning("Snap distance is > 400 meters. Is the x and y near the target Reach?")

  # Calculate measure, filter to the closest point and clean up
  meas_snap <- reach_points %>%
    dplyr::mutate(Total_Meters=as.numeric(cumsum(length_seg)),
                  Measure=round(meas_min + ((max(Total_Meters) - Total_Meters) * (meas_max-meas_min) / max(Total_Meters)), 2)) %>%
    dplyr::filter(Snap_Distance==min(Snap_Distance)) %>%
    dplyr::left_join(reach_df) %>%
    sf::st_transform(crs=4326) %>% # for leaflet
    #dplyr::mutate(Snap_Lat=unlist(lapply(geometry, FUN=function(x) {x[2]}), recursive = TRUE),
    #              Snap_Long=unlist(lapply(geometry, FUN=function(x) {x[1]}), recursive = TRUE)) %>%
    sf::st_zm() %>%
    dplyr::select(GNIS_Name, Permanent_Identifier, ReachCode, Measure)

  if(return_sf) {
    return(meas_snap)
  } else {
    measure <- as.character(meas_snap$Measure)
    return(measure)
  }

}
