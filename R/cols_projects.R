#' Projects Worksheet column names
#'
#' Retrieve column names for the Projects worksheet in Oregon DEQ's continuous data submission template xlsx file v2.03. 
#'
#' @export
#' @return Vector of column names.

cols_projects <-function() {
  
  projects_col_names <- c("Project ID", "Project Name", "Project Description", "Approved QAPP Indicator",
                          "QAPP Approval Agency Name", "Project Attachment File Name", "Project Attachment Type")
  
  return(projects_col_names)
  
}