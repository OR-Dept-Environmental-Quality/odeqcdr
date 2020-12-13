#' Returns a string of index values corresponding to each vector element that is FALSE
#'
#' This works the same as [which] except each index value is collapsed into a single
#' string separated by a comma and space. +1 is added to the index to equal the
#' row number in excel assuming the header row is the first row.
#' If all values are TRUE, NA is returned. NA values in x are treated the same as a TRUE.
#'
#' @param x Boolean vector
#' @seealso [odeqcdr::valid_values] [odeqcdr::valid_values_check]
#' @return string

fstr <- function(x) {

  if(any(!x, na.rm = TRUE)) {
    # there are some values that are FALSE
    # + 1 is added to account for the header row in the xlsx
    fvector <- which(!x) + 1

    # split the vector into a list of continuous sequences
    fconseq <- split(fvector, cumsum(c(0, diff(fvector) > 1)))

    # find the range of each sequence
    frange <- lapply(fconseq, FUN=range)

    # collapse the range into start:stop format and unlist to a single string
    findex <- paste(unlist(lapply(frange, FUN=function(x) {paste(unique(x), collapse = ":")})), collapse = ", ")

  } else {
    findex <- NA
  }

  return(findex)

}
