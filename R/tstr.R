#' Returns a string of index values corresponding to each vector element that is TRUE
#'
#' This works the same as [which] except each index value is collapsed into a single
#' string. Continuous indices in a range take the form start:stop and non continuous
#' indices are separated by a comma and space. If all values are FALSE, NA is returned.
#' NA values in x are treated the same as a FALSE.
#'
#' @param x Boolean vector. NA values are treated the same as a FALSE.
#' @param n Integer value to add to the index. Default is 0. Use n=1 to equal the
#' row number in excel assuming the header row is the first row.
#' @seealso [odeqcdr::valid_values] [odeqcdr::valid_values_check]
#' @return string

tstr <- function(x, n=0) {

  if(any(x, na.rm=TRUE)) {
    # there are some values that are TRUE
    # + 1 is added to account for the header row in the xlsx
    tvector <- which(x) + n

    # split the vector into a list of continuous sequences
    tconseq <- split(tvector, cumsum(c(0, diff(tvector) > 1)))

    # find the range of each sequence
    trange <- lapply(tconseq, FUN=range)

    # collapse the range into start:stop format and unlist to a single string
    tindex <- paste(unlist(lapply(trange, FUN=function(x) {paste(unique(x), collapse = ":")})), collapse = ", ")

  } else {
    tindex <- NA
  }

  return(tindex)

}
