#' Deployment Worksheet column names
#'
#' Retrieve column names for the Deployment worksheet in Oregon DEQ's continuous data submission template xlsx file v2.03. 
#'
#' @export
#' @return Vector of column names.

cols_deploy <-function() {
  
  deploy_col_names <- c("Monitoring Location ID", "Equipment ID", "Characteristic Name", "Deployment Start Date",  "Deployment End Date",   
                        "Sample Depth", "Sample Depth Unit", "Sample Media", "Sample Sub Media")
  
  return(deploy_col_names)
  
}