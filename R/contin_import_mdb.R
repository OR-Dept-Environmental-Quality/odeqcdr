#' Import continuous information from an access database.
#'
#' Retrieve monitoring results from Oregon DEQ's continuous data submission template file v2.03 formatted as an access database (.mdb or .accdb).
#' This function will read the template and return a list with each list element holding a
#' dataframe of the information for each template table. Any rows with all NAs are removed.
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

contin_import <- function(db,
                          sheets=list("Organization Details"="Organization Details",
                                      "Projects"="Projects",
                                      "Monitoring_Locations"="Monitoring_Locations",
                                      "Deployment"="Deployment",
                                      "Results"="Results",
                                      "PrePost"="PrePost",
                                      "Audit_Data"="Audit_Data")) {
  library(DBI)
  library(odbc)
  library(odeqcdr)


  # test
  db <- "C:\\workspace\\Data_Solicitation\\BLM\\BLM_NW_Oregon\\BLM_Data_NWOregon.mdb"
  #file <- choose.files()

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

    results_query <- DBI::dbSendQuery(con, "SELECT MONITORING_LOCATION_ID, ACTIVITY_ST_DATE, ACTIVITY_ST_TIME, ACTIVITY_TIME_ZONE,
                                      EQUIPMENT_ID_NUM, CHARACTERISTIC_NAME, RESULT_VALUE, RESULT_UNIT, RESULT_STATUS_ID FROM DEQ_RESULTS;")
    results_import <- DBI::dbFetch(results_query)

    projects_import <- DBI::dbReadTable(con, "DEQ_PROJECT")
    mloc_import <- DBI::dbReadTable(con, "DEQ_MONLOC")
    deploy_import <- DBI::dbReadTable(con, "DEQ_DEPLOY")
    audit_import <- DBI::dbReadTable(con, "DEQ_AUDIT")

    DBI::dbClearResult(results_query)


  DBI::dbDisconnect(con)

  }

  # set NA defaults for imports
  org_import <- NA
  projects_import <- NA
  locations_import <- NA
  deployment_import <- NA
  results_import <- NA
  prepost_import <- NA
  audit_import <- NA

  # Organizational Details -----------------------------------------------------

  if("Organization Details" %in% names(sheets)) {
    org_import <- readxl::read_excel(file, sheet = "Organization Details",
                                     range = "B6:C19", col_types = c('text', 'text'), col_names = FALSE)
    colnames(org_import) <- c('key', "value")
  }

  # Import Project Info -------------------------------------------------------------------

  # Column
  # 1	Project ID
  # 2	Project Name
  # 3	Project Description
  # 4	QAPP Approved Indicator
  # 5	QAPP Approval Agency Name
  # 6	Project Attachment File Name
  # 7	Project Attachment Type
  if("Projects" %in% db_tbl_names) {

    projects_col_types <- c('text', 'text', 'text', 'text', 'text', 'text', 'text')

    projects_col_names <- c("Project.ID", "Project.Name", "Project.Description", "Approved.QAPP.Indicator",
                            "QAPP.Approval.Agency.Name", "Project.Attachment.File.Name", "Project.Attachment.Type")

    projects_import <- readxl::read_excel(file, sheet = "Projects", col_types = projects_col_types)

    colnames(projects_import) <- projects_col_names

    # remove rows with all NAs
    projects_import <- projects_import[rowSums(is.na(projects_import)) != ncol(projects_import), ]
  }
  # Import Monitoring_Locations Info -------------------------------------------------------------------

  # Column
  # 1	Monitoring Location ID
  # 2	Monitoring Location Name
  # 3	Monitoring Location Type
  # 4	Monitoring Location Latitude
  # 5	Monitoring Location Longitude
  # 6	Horizontal Datum
  # 7	Coordinate Collection Method
  # 8	Monitoring Location Source Map Scale
  # 9	Monitoring Location Description
  # 10	Tribal Land Indicator
  # 11	Tribal Land Name
  # 12	County Name
  # 13	State Code
  # 14	HUC 8 Code
  # 15	Date Established
  # 16	Monitoring Location Comments
  # 17	Alternate Monitoring Location ID 1
  # 18	Alternate Context 1
  # 19	Alternate Monitoring Location ID 2
  # 20	Alternate Context 2
  # 21	Alternate Monitoring Location ID 3
  # 22	Alternate Context 3
  # 23	Reachcode (DEQ Staff Only)
  # 24	Measure (DEQ Staff Only)
  # 25	LLID (DEQ Staff Only)
  # 26	River Mile (DEQ Staff Only)
  # 27	Permanent Identifier  (ADDED)
  # 28  Monitoring Location Status ID (ADDED)
  # 29  Monitoring Location Status Comment (ADDED)

  if("Monitoring_Locations" %in% names(sheets)) {

    locations_col_types <- c('text', 'text', 'text', 'numeric', 'numeric', 'text', 'text', 'text', 'text', 'text',
                             'text', 'text', 'text', 'text', 'date', 'text', 'text', 'text', 'text', 'text',
                             'text', 'text', 'text','numeric', 'text', 'numeric', 'text', 'text', 'text')

    locations_col_names <- c("Monitoring.Location.ID", "Monitoring.Location.Name", "Monitoring.Location.Type", "Latitude",
                             "Longitude", "Horizontal.Datum", "Coordinate.Collection.Method", "Source.Map.Scale",
                             "Monitoring.Location.Description", "Tribal.Land", "Tribal.Land.Name", "County.Name",
                             "State.Code", "HUC.8.Code", "Date.Established", "Monitoring.Location.Comments",
                             "Alternate.ID.1", "Alternate.Context.1", "Alternate.ID.2", "Alternate.Context.2",
                             "Alternate.ID.3", "Alternate.Context.3", "Reachcode", "Measure",
                             "LLID", "River.Mile", "Permanent.Identifier", "Monitoring.Location.Status.ID", "Monitoring.Location.Status.Comment")


    # Note the "Permanent.Identifier" column may not be present so it is hard coded in.

    locations_import <- readxl::read_excel(file, sheet = "Monitoring_Locations", range = cellranger::cell_cols(1:29), col_types = locations_col_types)
    colnames(locations_import) <- locations_col_names

    # remove rows with all NAs
    locations_import <- locations_import[rowSums(is.na(locations_import)) != ncol(locations_import), ]

  }
  # Import Deployment Info -------------------------------------------------------------------

  # Column
  # 1	Monitoring Location ID
  # 2	Equipment ID #
  # 3	Characteristic Name
  # 4	Deployment Start Date
  # 5	Deployment End Date
  # 6	Sample Depth
  # 7	Sample Depth Unit
  # 8	Sample Media
  # 9	Sample Media Subdivision

  if("Deployment" %in% names(sheets)) {

    deployment_col_types <- c('text', 'text', 'text', 'date', 'date', 'numeric', 'text', 'text', 'text')

    deployment_col_names <- c("Monitoring.Location.ID", "Equipment.ID", "Characteristic.Name", "Deployment.Start.Date",
                              "Deployment.End.Date","Sample.Depth", "Sample.Depth.Unit","Sample.Media", "Sample.Sub.Media")

    deployment_import <- readxl::read_excel(file, sheet = "Deployment", col_types = deployment_col_types)
    colnames(deployment_import) <- deployment_col_names

    # remove rows with all NAs
    deployment_import <- deployment_import[rowSums(is.na(deployment_import)) != ncol(deployment_import), ]
  }

  # Import Results -------------------------------------------------------------------

  # Column
  # 1	Monitoring Location ID
  # 2	Activity Start Date
  # 3	Activity Start Time
  # 4	Activity Time Zone
  # 5	Characteristic Name
  # 6	Equipment ID #
  # 7	Result Value
  # 8	Result Unit
  # 9	Result Status ID
  # 10 Result Comment (ADDED)

  if("Results" %in% names(sheets)) {

    results_col_types <- c('text', 'date', 'date', 'text', 'text', 'text', 'numeric', 'text', 'text', 'text')

    results_col_names <- c("Monitoring.Location.ID", "Activity.Start.Date", "Activity.Start.Time", "Activity.Start.End.Time.Zone",
                           "Equipment.ID", "Characteristic.Name", "Result.Value", "Result.Unit", "Result.Status.ID", "Result.Comment")

    # read results tab of submitted file
    results_import <- readxl::read_excel(file, sheet = "Results", range = cellranger::cell_cols(1:10), col_types = results_col_types)
    colnames(results_import) <- results_col_names

    # remove rows with all NAs
    results_import <- results_import[rowSums(is.na(results_import)) != ncol(results_import), ]
  }

  # Import PrePost Info --------------------------------------------------------------

  # Column
  # 1	Equipment ID #
  # 2	Characteristic Name
  # 3	Equipment Result Value
  # 4	Equipment Result Unit
  # 5	Reference Result Value
  # 6	Reference Result Unit
  # 7	Reference ID #

  if("PrePost" %in% names(sheets)) {

    prepost_col_types <- c('text', 'text',  'numeric', 'text',  'numeric', 'text', 'text')

    prepost_col_names <- c("Equipment.ID", "Characteristic.Name", "Equipment.Result.Value", "Equipment.Result.Unit",
                           "Reference.Result.Value", "Reference.Result.Unit", "Reference.ID")

    # read results tab of submitted file
    prepost_import <- readxl::read_excel(file, sheet = "PrePost", col_types = prepost_col_types)
    colnames(prepost_import) <- prepost_col_names

    # remove rows with all NAs
    prepost_import <- prepost_import[rowSums(is.na(prepost_import)) != ncol(prepost_import), ]
  }
  # Read Audit Data --------------------------------------------------------

  # Column
  # 1	Project ID
  # 2	Alternate Project ID #1
  # 3	Alternate Project ID #2
  # 4	Monitoring Location ID
  # 5	Activity Start Date
  # 6	Activity Start Time
  # 7	Activity End Date
  # 8	Activity End Time
  # 9	Activity Start/End Time Zone
  # 10	Activity Type
  # 11	Activity ID (Locked)
  # 12	Equipment ID #
  # 13	Sample Collection Method
  # 14	Characteristic Name
  # 15	Result Value
  # 16	Result Unit
  # 17	Result Analytical Method ID
  # 18	Result Analytical Method Context
  # 19	Result Value Type
  # 20	Result Status ID
  # 21	Result Measure Qualifier
  # 22	Result Comment

  if("Audit_Data" %in% names(sheets)) {
    audit_col_types <- c('text', 'text', 'text', 'text', 'date', 'date', 'date', 'date', 'text', 'text',
                         'text', 'text', 'text', 'text', 'numeric', 'text', 'text', 'text', 'text', 'text',
                         'text', 'text')

    audit_col_names <- c("Project.ID", "Alternate.Project.ID.1", "Alternate.Project.ID.2", "Monitoring.Location.ID",
                         "Activity.Start.Date", "Activity.Start.Time", "Activity.End.Date", "Activity.End.Time",
                         "Activity.Start.End.Time.Zone", "Activity.Type", "Activity.ID", "Equipment.ID",
                         "Sample.Collection.Method", "Characteristic.Name", "Result.Value", "Result.Unit",
                         "Result.Analytical.Method.ID", "Result.Analytical.Method.Context", "Result.Value.Type", "Result.Status.ID",
                         "Result.Measure.Qualifier", "Result.Comment")

    audit_import <- readxl::read_excel(file, sheet = "Audit_Data", col_types = audit_col_types)
    colnames(audit_import) <- audit_col_names

    # remove rows with all NAs
    audit_import <- audit_import[rowSums(is.na(audit_import)) != ncol(audit_import), ]
  }
  # Add Sheets to list  --------------------------------------------------------

  template_sheets <-list(Organization_Details=as.data.frame(org_import),
                         Projects=as.data.frame(projects_import),
                         Monitoring_Locations=as.data.frame(locations_import),
                         Deployment=as.data.frame(deployment_import),
                         Results=as.data.frame(results_import),
                         PrePost=as.data.frame(prepost_import),
                         Audit_Data=as.data.frame(audit_import))

  return(template_sheets)

}
