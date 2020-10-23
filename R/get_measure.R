#' Get measure value along Oregon DEQ's NHDH_OR_931v220.
#'
#' The function will query Oregon DEQ's NHD feature service to determine the measure value along a target NHD flowline. The Permanent Identifier (pid) is
#' used to select a specific reach from the feature service. The x and y coordinates are snapped to the nearest vertex on the selected flowline and the measure value determined. The point is
#' This function is intended to be a helper function for [odeqcdr::launch_map] although it may be used independently also.
#'
#' The feature service can be accessed at https://arcgis.deq.state.or.us/arcgis/rest/services/WQ/NHDH_ORDEQ/MapServer/1
#'
#' @param pid The NHD Permanent_Identifier
#' @param x The longitude in decimal degrees
#' @param y The latitude in decimal degrees
#' @param return_sf Boolean. A TRUE value will return the sf object with data frame columns for Permanent_Identifier, ReachCode, measure, and snap_distance.
#' FALSE will return the measure value as a character. Default is FALSE.
#' @export
#' @return sf object with data frame columns for Permanent_Identifier, ReachCode, measure, and snap_distance

get_measure <- function(pid, x, y, return_sf=FALSE){

  library(dplyr)
  library(httr)
  library(geojsonsf)
  library(sf)

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

  # get the NHD reach and convert each vertex to a point
  reach <- geojsonsf::geojson_sf(response_NHD) %>%
    sf::st_transform(crs=to_crs) %>%
    sf::st_cast( to="POINT") %>%
    dplyr::mutate(measure=unlist(lapply(geometry, FUN=function(x) {x[4]}), recursive = TRUE)) %>%
    sf::st_zm() %>%
    dplyr::select(Permanent_Identifier, ReachCode, measure)

  reach$snap_distance <- sf::st_distance(reach, site, by_element = TRUE)

  reach_snap <- reach %>%
    dplyr::filter(snap_distance==min(snap_distance))

  measure <- as.character(round(reach_snap$measure,2))

  if(return_sf) {
    return(reach_snap)
  } else {
    return(measure)
  }

}

