#' Monitoring Location worksheet column names
#'
#' Retrieve column names for the Monitoring Locations worksheet in Oregon DEQ's continuous data submission template xlsx file v2.03.
#' Includes internal staff columns: "Reachcode", "Measure", "LLID", "River Mile", "Permanent Identifier", "Monitoring Location Status ID", and
#' "Monitoring Location Status Comment".
#' @export
#' @return vector of column names.

cols_mloc <-function() {

  mloc_col_names <- c("Monitoring Location ID", "Monitoring Location Name", "Monitoring Location Type", "Latitude",
                      "Longitude","Horizontal Datum", "Coordinate Collection Method", "Source Map Scale",
                      "Monitoring Location Description", "Tribal Land", "Tribal Land Name", "County Name",
                      "State Code","HUC 8 Code", "Date Established", "Monitoring Location Comments",
                      "Alternate ID 1", "Alternate Context 1", "Alternate ID 2", "Alternate Context 2",
                      "Alternate ID 3", "Alternate Context 3", "Reachcode", "Measure",
                      "LLID", "River Mile", "Permanent Identifier", "Monitoring Location Status ID", "Monitoring Location Status Comment")

  return(mloc_col_names)
}
