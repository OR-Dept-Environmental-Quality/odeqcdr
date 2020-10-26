#' @title split lines
#' @description Splits lines longer than a given threshold into the minimum number of pieces to all be under the given threshold. Full credit for this function goes to
#' David Blodgett at the USGS. Code obtained from: https://gist.github.com/dblodgett-usgs/cf87392c02d73f1b7d16153d2b66a8f3
#' @param lines data.frame of class sf with LINESTRING sfc column.
#' @param max_length maximum segment length to return
#' @param id name of ID column in data.frame
#' @return only the split lines.
#' @importFrom dplyr group_by ungroup filter select mutate
#' @export
#'
split_lines <- function(input_lines, max_length, id) {
  if(max_length < 50) warning("short max length detected, do you have your units right?")

  geom_column <- attr(input_lines, "sf_column")

  input_crs <- sf::st_crs(input_lines)

  input_lines[["geom_len"]] <- sf::st_length(input_lines[[geom_column]])

  attr(input_lines[["geom_len"]], "units") <- NULL
  input_lines[["geom_len"]] <- as.numeric(input_lines[["geom_len"]])

  too_long <- filter(select(input_lines, id, geom_column, geom_len), geom_len >= max_length)

  rm(input_lines) # just to control memory usage in case this is big.

  too_long <- mutate(too_long,
                     pieces = ceiling(geom_len / max_length),
                     fID = 1:nrow(too_long)) %>%
    select(-geom_len)

  split_points <- sf::st_set_geometry(too_long, NULL)[rep(seq_len(nrow(too_long)), too_long[["pieces"]]),] %>%
    select(-pieces)

  split_points <- mutate(split_points, split_fID = row.names(split_points)) %>%
    group_by(fID) %>%
    mutate(piece = 1:n()) %>%
    mutate(start = (piece - 1) / n(),
           end = piece / n()) %>%
    ungroup()

  new_line <- function(i, f, t) {
    lwgeom::st_linesubstring(x = too_long[[geom_column]][i], from = f, to = t)[[1]]
  }

  split_lines <- apply(split_points[c("fID", "start", "end")], 1,
                       function(x) new_line(i = x[["fID"]], f = x[["start"]], t = x[["end"]]))

  rm(too_long)

  split_lines <- st_sf(split_points[c(id, "split_fID")], geometry = st_sfc(split_lines, crs = input_crs))

  return(split_lines)
}
