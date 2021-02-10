#' Equipment Worksheet column names
#'
#' Retrieve column names for the Equipment worksheet in Oregon DEQ's continuous data submission template xlsx file v3.0.
#'
#' @export
#' @return Vector of column names.

cols_equipment <-function() {

  equipment_col_names <- c("Equipment Type",	"Equipment ID",
                           "Equipment Name",	"Model Number",	"Serial Number",	"Comments",
                           "Quality Assurance Plan", "Continuous Monitoring")

  return(equipment_col_names)

}
