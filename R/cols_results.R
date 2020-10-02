#' Results Worksheet column names
#'
#' Retrieve column names for the Results worksheet in Oregon DEQ's continuous data submission template xlsx file v2.03.
#' Includes internal staff columns: "accDQL", "precDQL", and "rDQL.
#'
#' @export
#' @return Vector of column names.

cols_results <-function() {

  results_col_names <- c("Monitoring Location ID", "Activity Start Date", "Activity Start Time", "Activity Start End Time Zone",
                        "Equipment ID", "Characteristic Name", "Result Value", "Result Unit",
                        "Result Status ID", "Result Comment", "accDQL", "precDQL", "rDQL" )

  return(results_col_names)

}
