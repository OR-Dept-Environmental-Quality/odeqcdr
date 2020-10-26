#' Get the river mile along the LLID
#'
#' The function will query Oregon DEQ's LLID streams feature service to determine the river mile value along the target LLID polyline given a nearby latitude and longitude. The llid value is
#' used to select a specific reach from the feature service. The LLID is split into 150 foot segments with a vertex on either end of the segment.
#' The x and y coordinates are snapped to the nearest vertex. The river mile is calucated based on the distance from downstream end of the LLID.
#' The feature data is projected into ESPG:2994 (Oregon Lmabert NAD83 HARN) for all operations. Units are feet. If return_sf=TRUE the sf is projected into ESPG:4326 (WGS84)
#' which is the expected default for [leaflet].
#'
#' This function is intended to be a helper function for [odeqcdr::launch_map] although it may be used independently as well.
#'
#' The LLID feature service can be accessed at https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/DEQ_Streams/MapServer/0/
#'
#' @param llid The LLID value as a string.
#' @param x The longitude in decimal degrees
#' @param y The latitude in decimal degrees
#' @param max_length The maximum segment length in feet to split the LLID into. Default is 150 feet. Passed to [odeqcdr::split_lines]. The smaller the distance the longer the operation takes.
#' @param return_sf Boolean. Default is FALSE. A TRUE value will return a sf point object of the
#' snapped location with data frame columns for Stream 'NAME', 'LLID', 'River_Mile',
#' 'Latitude', 'Longitude', 'Source', and 'RM_Total' for the total LLID river miles.
#' @export
#' @return river mile info

get_llidrm <- function(llid, x, y, max_length=150, return_sf=FALSE){

  # Test data
  # y=42.09361
  # x=-122.3822
  # llid <- "1223681420918"
  # max_length=150
  # return_sf=FALSE

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

  reach0 <- geojsonsf::geojson_sf(response_LLID) %>%
    sf::st_transform(crs=to_crs) %>%
    sf::st_zm()

  # Split line into segments
  reach1 <- sf::st_cast(reach0, to="LINESTRING") %>%
    odeqcdr::split_lines(max_length = max_length, id ="LLID")

  reach1$length_seg <- units::set_units(sf::st_length(reach1), mi)

    reach_points <- sf::st_cast(reach1, to="POINT")
  reach_points$row=as.numeric(row.names(reach_points))

  # Get only the first point on each linestring segment, and the last point (most upstrem). Use the row number to filter.
  reach_points <- dplyr::filter(reach_points, row %in% c(1:nrow(reach_points),  max(reach_points$row)))

  # Set first point to length zero so rivermile starts at zero
  reach_points[1,c("length_seg")] <- 0

  # Snap distance in feet because of crs
  reach_points$Snap_Distance <- sf::st_distance(reach_points, site, by_element = TRUE)

  if(units::set_units(min(reach_points$Snap_Distance), m) > units::set_units(400, m))
    warning("Snap distance is > 400 meters. Is the x and y near the target llid?")

  # filter to the closest point and clean up
  rm_snap <- reach_points %>%
    dplyr::mutate(River_Mile=cumsum(length_seg)) %>%
    dplyr::filter(Snap_Distance==min(Snap_Distance)) %>%
    dplyr::left_join(reach_df) %>%
    sf::st_transform(crs=4326) %>% # for leaflet
    dplyr::mutate(Latitude=unlist(lapply(geometry, FUN=function(x) {x[2]}), recursive = TRUE),
                  Longitude=unlist(lapply(geometry, FUN=function(x) {x[1]}), recursive = TRUE)) %>%
    sf::st_zm() %>%
    dplyr::select(NAME, Source, RM_Total, Latitude, Longitude, LLID, River_Mile)

  if(return_sf) {
   return(rm_snap)
  } else {
    return(as.numeric(rm_snap$River_Mile))
    }
}
