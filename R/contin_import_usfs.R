#' Import continuous information from an USFS access database.
#'
#' Retrieve continuous monitoring results from a USFS access database (.mdb or .accdb).
#' This function will read the access database and return a list with each list element holding a
#' dataframe of the information formatted in the same way as DEQ's continuous data submission template v2.03.
#'
#' The function returns a named list holding each table from the access table. The name of each list element is the same as the template:
#'
#'   Organization_Details
#'   Projects
#'   Monitoring_Locations
#'   Deployment
#'   Results
#'   PrePost
#'   Audit_Data
#'
#'   Column names are made into syntactically valid names acceptable by R.
#'
#'   This function relies heavily upon the [DBI] package.
#'
#' @param db The path and file name to template .mdb or .accdb database file.
#' @param sheets Optional named list identifying tables to import. Default is a list of all tables to be imported.
#' i.e. sheets=list("Organization Details"="Organization Details", "Projects"="Projects", "Monitoring_Locations"="Monitoring_Locations", "Deployment"="Deployment", "Results"="Results", "PrePost"="PrePost", "Audit_Data"="Audit_Data"))
#' If the access database table has a different name specify the table name like this: "Monitoring_Locations"="table_name_from_database".
#' @export
#' @return list of each continuous template data


contin_import_usfs <- function(db,
                               sheets=list("Organization Details"="Organization Details",
                                           "Projects"="Projects",
                                           "Monitoring_Locations"="Monitoring_Locations",
                                           "Deployment"="Deployment",
                                           "Results"="Results",
                                           "PrePost"="PrePost",
                                           "Audit_Data"="Audit_Data")) {



  options(scipen=999)

  # Make sure that the file exists before attempting to connect
  if (!file.exists(file)) {
    stop("Access database file does not exist at ", db)
  }

  # Make database connection
  con <- DBI::dbConnect(odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=", db))

  # get table names from database
  db_tbl_names <- DBI::dbListTables(con)

  # remove NULL values from sheets list
  sheets <- sheets[!unlist(lapply(sheets, function(x) {is.null(x[[1]])}), use.names = FALSE)]

  # remove NA elements from sheets list
  sheets <- sheets[!unlist(lapply(sheets, function(x) {is.na(x[[1]])}), use.names = FALSE)]

  # get table names to import
  tbl_sheets = unlist(sheets, use.names = FALSE)

  tbl_check <- tbl_sheets %in% db_tbl_names

  if(any(!tbl_check)) {
    stop(paste0("The following table name from 'sheets' is not in 'db': ", tbl_sheets[!tbl_check]))

  }

}
