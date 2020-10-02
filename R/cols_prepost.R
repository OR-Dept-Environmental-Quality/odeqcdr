#' PrePost Worksheet column names
#'
#' Retrieve column names for the PrePost worksheet in Oregon DEQ's continuous data submission template xlsx file v2.03. 
#'
#' @export
#' @return Vector of column names.

cols_prepost <-function() {
  
  prepost_col_names <-c("Equipment ID", "Characteristic Name", "Equipment Result Value", "Equipment Result Unit", "Reference Result Value",
                        "Reference Result Unit",  "Reference ID")
  
  return(prepost_col_names)
}