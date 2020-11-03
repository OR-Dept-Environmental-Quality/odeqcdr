#' Return a vector of boolean values indicating if the value is a valid value
#'
#' This function will evaluate if a value for certain columns in the continuous
#' data submission template xlsx file v2.03 are valid values. Returns TRUE if valid and FALSE if not.
#' NA is included as a valid value for columns that are optional or conditional.
#'
#' @param col Name of the column in the continuous data submission template xlsx file v2.03 to return valid values.
#' col is passed to [odeqcdr::valid_values] to get the valid values.
#' The col name can be formatted as shown in the xlsx template or in R format derived using [make.names].
#' If you need to determine the correct value for col see [odeqcdr::cols_audit], [odeqcdr::cols_deploy], [odeqcdr::cols_mloc], [odeqcdr::cols_prepost], or [odeqcdr::cols_results].
#' @param vals Vector of values to evaluate.
#' @seealso [odeqcdr::valid_values]
#' @export
#' @return Vector of boolean values indicating if the value is a valid value

valid_values_check <- function(col, vals) {

  vals_valid <- odeqcdr::valid_values(col)

  result <- vals %in% vals_valid

  return(result)

}
