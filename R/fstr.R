#' Returns a string of index values corresponding to each vector element that is FALSE
#'
#' This works the same as \code{\link[base]{which}} except each index value is collapsed into a single
#' string. Continuous indices in a range take the form start:stop and non continuous
#' indices are separated by a comma and space. If all values are TRUE, NA is returned.
#' NA values in x are treated the same as a TRUE.
#'
#' @param x Boolean vector
#' @param n Integer value to add to the index. Default is 0. Use n=1 to equal the
#' row number in excel assuming the header row is the first row.
#' @seealso \code{\link{valid_values}} \code{\link{valid_values_check}}
#' @return string

fstr <- function(x, n=0) {

  if(any(!x, na.rm = TRUE)) {
    # there are some values that are FALSE
    # + 1 is added to account for the header row in the xlsx
    fvector <- which(!x) + n

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
