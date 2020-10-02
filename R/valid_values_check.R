#' Return a vector of boolean values indicating if the value is a valid value
#'
#' This function will return valid values for certain columns in the continuous
#' data submission template xlsx file v2.03. NA is included as a valid value for columns that are optional or conditional.
#'
#' @param col Name of the column in the continuous data submission template xlsx file v2.03 to return valid values.
#' The name can be formatted as shown the xlsx template or in R format derived using [make.names()].
#' @param val vector of values to evaluate.
#' @export
#' @return vector of valid values or list of all valid values.

valid_values_check <- function(col, vals) {

  vals_valid <- odeqcdr::valid_values(col)

  result <- vals %in% vals_valid

  return(result)

}
