#' Returns a string of index values cooresponding to each vector element that is TRUE
#'
#' This works the same as [which] except each index value is collapsed into a single
#' string separated by a comma and space. +1 is added to the index to equal
#' the row number in excel assuming the header row is the first row.
#' If all values are FALSE, NA is returned. NA values in x are treated the same as a FALSE.
#'
#' @param x Boolean vector. NA values are treated the same as a FALSE.
#' @seealso [odeqcdr::valid_values] [odeqcdr::valid_values_check]
#' @return string

tstr <- function(x) {

  if(any(x, na.rm=TRUE)) {
    # there are some values that are TRUE
    # + 1 is added to account for the header row in the xlsx
    tindex <- paste((which(x)+1), collapse=", ")

  } else {
    tindex <- NA
  }

  return(tindex)

}
