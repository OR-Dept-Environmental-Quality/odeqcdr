#' Get the river mile along the LLID
#'
#' The function will query Oregon DEQ's LLID streams feature service to determine the river mile value along the target LLID polyline given a nearby latitude and longitude. The LLID value is
#' used to select a specific reach from the feature service. The LLID is split into 150 foot segments with a vertex on either end of the segment.
#' The x and y coordinates are snapped to the nearest vertex. The river mile is calculated based on the distance from downstream end of the LLID.
#' The feature data is projected into ESPG:2994 (Oregon Lambert NAD83 HARN) for all operations. Units are feet. If return_sf=TRUE the sf is projected into ESPG:4326 (WGS84)
#' which is the expected default for \code{\link[leaflet]{leaflet}}.
#'
#' This function is intended to be a helper function for \code{\link{launch_map}} although it may be used independently as well.
#'
#' The LLID feature service can be accessed at \url{https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/DEQ_Streams/MapServer/0/}
#'
#' @param llid The LLID value as a string.
#' @param x The longitude in decimal degrees
#' @param y The latitude in decimal degrees
#' @param max_length The maximum segment length in feet to split the LLID segment into. Default is 82 feet (25 meters). Passed to [odeqcdr::split_lines]. The smaller the distance the longer the operation takes.
#' @param return_sf Boolean. Default is FALSE. A TRUE value will return a sf point object of the
#' snapped location with data frame columns for Stream 'NAME', 'LLID', 'River_Mile', and 'RM_Total' for the total LLID river miles.
#' @export
#' @return river mile info

get_llidrm <- function(llid, x, y, max_length=82, return_sf=FALSE){

  # Test data
  # y=42.09361
  # x=-122.3822
  # llid <- "1223681420918"
  # max_length=150
  # return_sf=FALSE

  warning("get_llidrm is deprecated and will be removed soon. Please
  transistion to the get_llidrm function in the 'odeqmloctools' package instead.")

  # Oregon GIC Lambert (ft) NAD83(HARN)
  to_crs <- 2994

  # get the site, make sf object, NAD 83 EPSG:4269
  site <- sf::st_as_sf(data.frame(Longitude=x, Latitude=y), coords = c("Longitude", "Latitude"), crs = 4269) %>%
    sf::st_transform(crs=to_crs)

  pathLLID <- "https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/DEQ_Streams/MapServer/0/query?where="

  # get the LLID reach
  request_LLID <- httr::GET(url = paste0(pathLLID, "LLID='",llid,"'&outFields=*&returnGeometry=true&returnIdsOnly=false&f=GeoJSON"))
  response_LLID <- httr::content(request_LLID, as = "text", encoding = "UTF-8")

  reach_df<- geojsonsf::geojson_sf(response_LLID) %>%
    sf::st_drop_geometry()

  # get measure value at top and bottom
  reach_points0 <- geojsonsf::geojson_sf(response_LLID) %>%
    sf::st_cast(to="POINT") %>%
    dplyr::mutate(river_feet=units::set_units(unlist(lapply(geometry, FUN=function(x) {x[4]}), recursive = TRUE), ft)) %>%
    dplyr::arrange(river_feet) %>%
    sf::st_transform(crs=to_crs) %>%
    sf::st_zm()

  # Snap distance in feet because of crs
  reach_points0$Snap_Distance <- sf::st_distance(reach_points0, site, by_element = TRUE)

  # Get the the four closest vertices by snap distance
  reach_points0 <- reach_points0 %>%
    dplyr::slice_min(Snap_Distance, n=4) %>%
    dplyr::arrange(river_feet)

  # Get minimum river feet
  rf_min <- min(reach_points0$river_feet)

  # Put the vertices back into a line Split line into smaller segments
  reach1 <- reach_points0 %>%
    dplyr::select(LLID) %>%
    dplyr::group_by(LLID) %>%
    summarise(do_union = FALSE) %>%
    sf::st_cast(to="LINESTRING") %>%
    odeqcdr::split_lines(max_length = max_length, id ="LLID")

  reach1$length_seg <- units::set_units(sf::st_length(reach1), mi)

  reach_points <- sf::st_cast(reach1, to="POINT")
  reach_points$row=as.numeric(row.names(reach_points))

  # Get only the first point on each linestring segment, and the last point (most upstream). Use the row number to filter.
  reach_points <- dplyr::filter(reach_points, row %in% c(1:nrow(reach_points),  max(reach_points$row)))

  # Set first point to length zero so river mile starts at zero. Units need to be established in feet to get conversion correct
  reach_points[1,c("length_seg")] <- units::set_units(rf_min, ft)

  # Snap distance in feet because of crs
  reach_points$Snap_Distance <- sf::st_distance(reach_points, site, by_element = TRUE)

  if(units::set_units(min(reach_points$Snap_Distance), m) > units::set_units(400, m))
    warning("Snap distance is > 400 meters. Is the x and y near the target llid?")

  # filter to the closest point and clean up
  rm_snap <- reach_points %>%
    dplyr::mutate(River_Mile=round(cumsum(length_seg),2)) %>%
    dplyr::filter(Snap_Distance==min(Snap_Distance)) %>%
    dplyr::left_join(reach_df) %>%
    sf::st_transform(crs=4326) %>% # for leaflet
    #dplyr::mutate(Latitude=unlist(lapply(geometry, FUN=function(x) {x[2]}), recursive = TRUE),
    #              Longitude=unlist(lapply(geometry, FUN=function(x) {x[1]}), recursive = TRUE)) %>%
    sf::st_zm() %>%
    dplyr::select(NAME, RM_Total, LLID, River_Mile)

  if(return_sf) {
    return(rm_snap)
  } else {
    return(as.numeric(rm_snap$River_Mile))
  }
}
