#' Deployment Worksheet column names
#'
#' Retrieve column names for the Deployment worksheet in Oregon DEQ's continuous data submission template xlsx file v2.03.
#'
#' @param ver numeric version of the template. Valid values are ver=2 (the default) or ver=3. The column names are different between the two versions.
#' @export
#' @return Vector of column names.

cols_deploy <-function(ver=2) {


  if(ver==2) {
    deploy_col_names <- c("Monitoring Location ID", "Equipment ID", "Characteristic Name", "Deployment Start Date",  "Deployment End Date",
                          "Sample Depth", "Sample Depth Unit", "Sample Media", "Sample Sub Media") }
  if(ver==3) {
    deploy_col_names <- c("Equipment ID", "Monitoring Location ID", "Deployment Start Date Time",  "Deployment End Date Time",
                          "Deployment Start End Time Zone", "Media", "Media Subdivision", "Project ID", "Alternate Project ID 1", "Alternate Project ID 2",
                          "Frequency in min", "Depth in m")
  }

  if(!ver %in% c(2:3)) {
    stop("ver must equal 2 or 3")
  }

  return(deploy_col_names)

}
