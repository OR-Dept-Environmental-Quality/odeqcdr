#' Audit Data Worksheet column names
#'
#' Retrieve column names for the Audit Data worksheet in Oregon DEQ's continuous data submission template xlsx file v2.03. 
#'
#' @export
#' @return Vector of column names.
#' 

cols_audit <-function() {
  
  audit_col_names <-c("Project ID", "Alternate Project ID 1", "Alternate Project ID 2", "Monitoring Location ID",        
                      "Activity Start Date", "Activity Start Time", "Activity End Date", "Activity End Time",             
                      "Activity Start End Time Zone", "Activity Type", "Activity ID", "Equipment ID",               
                      "Sample Collection Method", "Characteristic Name", "Result Value", "Result Unit",                  
                      "Result Analytical Method ID", "Result Analytical Method Context", "Result Value Type", "Result Status ID",                
                      "Result Measure Qualifier", "Result Comment")
  
  return(audit_col_names)
  
}

