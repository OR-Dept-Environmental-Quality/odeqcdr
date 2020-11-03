#' Returns a string of index values cooresponding to each vector element that is FALSE
#'
#' This works the same as [which] except each index value is collapsed into a single
#' string separated by a comma and space. +1 is added to the index to equal
#' the row number in excel assuming the header row is the first row.
#' If all values are TRUE, NA is returned. NA values in x are treated the same as a TRUE.
#'
#' @param x Boolean vector
#' @seealso [odeqcdr::valid_values] [odeqcdr::valid_values_check]
#' @return string

fstr <- function(x) {

  if(any(!x, na.rm = TRUE)) {
    # there are some values that are FALSE
    # + 1 is added to account for the header row in the xlsx
    findex <- paste((which(!x)+1), collapse=", ")

  } else {
    findex <- NA
  }

  return(findex)

}
