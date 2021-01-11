#' Import continuous information from a template xlsx file.
#'
#' Retrieve monitoring results from Oregon DEQ's continuous data submission template xlsx file v2.03.
#' This function will read the template and return a list with each list element holding a
#' dataframe of the information for each spreadsheet. Any rows with all NAs are removed.
#'
#' The function returns a named list holding each worksheet from the xlsx. The name of each list element is the same as the xlsx:
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
#'   This function relies heavily upon the [readxl] package.
#'
#' @param file The path and file name to template xlsx file.
#' @param sheets Optional vector identifying sheets to import. Default is a vector of all sheets to be imported.
#' Acceptable values include "Organization Details", "Projects", "Monitoring_Locations", "Deployment", "Results", "PrePost", and "Audit_Data".
#' @seealso [readxl::read_excel()]
#' @export
#' @return list of each continuous template data

contin_volmon_import <- function(file, project = 'ODEQVolMonWQProgram',
                                 timezone = "PDT") {

  #library(readxl)
  file <- "C:/Users/tpritch/Documents/odeqcdr/test templates/OriginalCopy_PBWC_ODEQ_ContinuousTempDataSubmittal_2019.xlsx"

  options(scipen=999)

  #sheet_check <- sheets %in% c("Organization Details", "Projects", "Monitoring_Locations", "Deployment", "Results", "PrePost", "Audit_Data")

  # if(any(!sheet_check)) {
  #   stop(paste0("The following are not acceptable input values for variable 'sheets': ", sheets[!sheet_check]))
  # }
  #
  # # set NA defaults for imports
  # org_import <- NA
  # projects_import <- NA
  # locations_import <- NA
  # deployment_import <- NA
  # results_import <- NA
  # prepost_import <- NA
  # audit_import <- NA

  # Organizational Details -----------------------------------------------------


    org_import <- readxl::read_excel(file, sheet = "SiteMasterInfo",
                                     range = "C1:D1", col_types = c('text'), col_names = FALSE)
    colnames(org_import) <- c('key', "value")

    other_info <- data.frame(
             stringsAsFactors = FALSE,
                    key = c("DESCRIPTION OF ORGANIZATION", "ADDRESS (INCLUDE MAILING IF DIFFERENT FROM PHYSICAL)",
                                                    "PHONE","FAX","EMAIL",
                                                    "WEB ADDRESS",
                                                    "CONTACT PERSON (INCLUDE ADDRESS/PHONE/EMAIL IF DIFFERENT FROM ORGANIZATION)",
                                                    "TYPE(S) OF DATA (e.g. water quality, macroinvertebrate, pebble, etc.)",
                                                    "319 Grant? If so, Years funded",
                                                    "Have you submitted data to DEQ before (Yes/No)? If you are unsure say No.",
                                                    "EPA/USGS ORGANIZATION ID, USERNAME &  PASSWORD",
                                                    "DATE ORG ID/U/P ISSUED:",
                                                    "OTHER PEOPLE WORKING WITH THIS PROJECT THAT HAVE THE USERNAME & PASSWORD:"),
                                              value = c(NA, NA,NA,NA,NA,NA,NA,NA,
                                                    NA,NA,
                                                    "[ADMINISTRATIVE USE ONLY]",
                                                    "[ADMINISTRATIVE USE ONLY]",NA)
                  )

    org_import <- dplyr::bind_rows(org_import, other_info)
    org_import[1,1] <- "ORGANIZATION NAME"



  # Import Project Info -------------------------------------------------------------------




    projects_col_names <- c("Project.ID", "Project.Name", "Project.Description", "Approved.QAPP.Indicator",
                            "QAPP.Approval.Agency.Name", "Project.Attachment.File.Name", "Project.Attachment.Type")

    projects_import <-setNames(data.frame(matrix(ncol = 7, nrow = 0)), projects_col_names)

    projects_import[1,1] <- project


    # remove rows with all NAs
    projects_import <- projects_import[rowSums(is.na(projects_import)) != ncol(projects_import), ]

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



    locations_col_names_orig <- c( "Monitoring.Location.ID", "Site_ID", "Monitoring.Location.Name", "Latitude",
                                  "Longitude", "Coordinate.Collection.Method")



    locations_import <- readxl::read_excel(file, sheet = "SiteMasterInfo",
                                     range = "B7:G39", col_types = c('text'), col_names = FALSE)

    #Use this list to get consistent naming for coumn names
    colnames(locations_import) <- locations_col_names_orig



    #Remove Site_ID column
    locations_import <-  dplyr::select(locations_import, -Site_ID)





    #These columns are in the TMDL template, but are not in volmon due to different ways of entering stations
    #keep them to maintain code reliability for next steps
    locations_col_names <- c("Monitoring.Location.Type", "Horizontal.Datum", "Coordinate.Collection.Method", "Source.Map.Scale",
                             "Monitoring.Location.Description", "Tribal.Land", "Tribal.Land.Name", "County.Name",
                             "State.Code", "HUC.8.Code", "Date.Established", "Monitoring.Location.Comments",
                             "Alternate.ID.1", "Alternate.Context.1", "Alternate.ID.2", "Alternate.Context.2",
                             "Alternate.ID.3", "Alternate.Context.3", "Reachcode", "Measure",
                             "LLID", "River.Mile", "Permanent.Identifier", "Monitoring.Location.Status.ID", "Monitoring.Location.Status.Comment")


    # Add missing columns to dataframe
    locations_import[, locations_col_names] <- NA


    # Reorder columns
    locations_import <- dplyr::select(locations_import, "Monitoring.Location.ID", "Monitoring.Location.Name", "Monitoring.Location.Type", "Latitude",
                                      "Longitude", "Horizontal.Datum", "Coordinate.Collection.Method", "Source.Map.Scale",
                                      "Monitoring.Location.Description", "Tribal.Land", "Tribal.Land.Name", "County.Name",
                                      "State.Code", "HUC.8.Code", "Date.Established", "Monitoring.Location.Comments",
                                      "Alternate.ID.1", "Alternate.Context.1", "Alternate.ID.2", "Alternate.Context.2",
                                      "Alternate.ID.3", "Alternate.Context.3", "Reachcode", "Measure",
                                      "LLID", "River.Mile", "Permanent.Identifier", "Monitoring.Location.Status.ID", "Monitoring.Location.Status.Comment")

    # remove rows with all NAs
    locations_import <- locations_import[rowSums(is.na(locations_import)) != ncol(locations_import), ]


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

 #Get logger parameters
template_sheets <- readxl::excel_sheets(file)

sheet_exclude <- c("SiteMasterInfo","FieldAuditResults", "PrePostResults", "LoggerID", "Sheet1" )

template_sheets <-setdiff(template_sheets, sheet_exclude)

param_list <- list()


deply_col_types <- c('date', 'date', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                         'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

for(i in 1:length(template_sheets)){
param_read <- readxl::read_excel(file, sheet = template_sheets[i],
                                 range = "A5:O10000", col_types = deply_col_types)
param_read <- dplyr::mutate(param_read, Equipment.ID = template_sheets[i],
                            Deployment.Start.Date = min(DATE, na.rm = TRUE),
                            Deployment.End.Date = max(DATE, na.rm = TRUE))

param_read <- head(param_read, 1)

# param_read <- dplyr::filter(param_read, DATE == min(DATE, na.rm = TRUE) |
#                               DATE == max(DATE, na.rm = TRUE)  )
param_read <- dplyr::select(param_read, -DATE, -TIME, -TEMP_DQL, -DO_DQL, -PH_DQL, -TURB_DQL, -COND_DQL, -Q_DQL )
param_read <- dplyr::rename(param_read, "Temperature, water" = 'TEMP_r',
                            "Dissolved oxygen (DO)" = 'DO_r',
                            "Dissolved oxygen saturation" = "DOs_r",
                            "pH" = "PH_r",
                            "Turbidity" = "TURB_r",
                            "Conductivity" = "COND_r",
                            "Flow" = "Q_r")
param_read <- tidyr::pivot_longer(param_read, cols = c("Temperature, water", "Dissolved oxygen (DO)",
                                                       "Dissolved oxygen saturation", "pH",
                                                       "Turbidity", "Conductivity", "Flow"), names_to = "Characteristic.Name",
                                  values_drop_na = TRUE)
param_read <- dplyr::select(param_read, -value)

param_list[[i]] <- param_read
}

params <- dplyr::bind_rows(param_list)

    deployment_col_types <- c('text', 'text', 'text', 'date', 'date', 'numeric', 'text', 'text', 'text')

    deployment_col_names <- c("Equipment.ID", "Monitoring.Location.ID", "Site_ID" , "Station_Description", "Decimal_Latitude",
                              "Decimal_Longitude", "LAT_LONG_SOURCE", "Sample.Depth")

    deployment_import <- readxl::read_excel(file, sheet = "SiteMasterInfo",
                                           range = "A7:H10000", col_types = c('text'), col_names = FALSE)

    colnames(deployment_import) <- deployment_col_names

    # remove rows with all NAs
    deployment_import <- deployment_import[rowSums(is.na(deployment_import)) != ncol(deployment_import), ]
    deployment_import <- dplyr::left_join(deployment_import, params, by = "Equipment.ID")
    deployment_import <- dplyr::mutate(deployment_import,
                                       Sample.Depth.Unit = "m",
                                       Sample.Media = 'Water',
                                       Sample.Sub.Media = "Surface Water")
    deployment_import <- dplyr::select(deployment_import,
                                       "Monitoring.Location.ID", "Equipment.ID", "Characteristic.Name", "Deployment.Start.Date",
                                       "Deployment.End.Date","Sample.Depth", "Sample.Depth.Unit","Sample.Media", "Sample.Sub.Media")






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

    results_col_types <- c('date', 'date', 'numeric',
                           'text','numeric','text','numeric','text','numeric',
                           'text','numeric','text','numeric','tex', 'numeric', 'text')

    results_col_names <- c("Activity.Start.Date", "Activity.Start.Time","Temperature, water",
                           "TEMP_DQL",
                           "Dissolved oxygen (DO)",
                           "Dissolved oxygen saturation",
                           "DO_DQL",
                           "pH",
                           "PH_DQL",
                           "Turbidity",
                           "TURB_DQL",
                           "Conductivity",
                           "COND_DQL",
                            "Flow",
                           "Q_DQL")

    results_list <- list()

    mloc_lookup <- distinct(select(deployment_import,
                           Equipment.ID, Monitoring.Location.ID))

    for(i in 1:length(template_sheets)){





    # read results tab of submitted file
    #results_import <- readxl::read_excel(file, sheet = template_sheets[i], range = "A5:N10000", col_types = results_col_types)
    results_import <-openxlsx::read.xlsx(file, sheet = template_sheets[i],
                                         startRow = 5,
                                         colNames = FALSE,
                                         detectDates = TRUE)
    results_import <- results_import[-1,]

    colnames(results_import) <- results_col_names
    results_import$Activity.Start.Date <- openxlsx::convertToDateTime(results_import$Activity.Start.Date)
    results_import$Activity.Start.Time <- openxlsx::convertToDateTime(results_import$Activity.Start.Time)

  #Error check for time formatting. Stop processing and direct the user to correct time issue beofre continuing

    #   if(anyNA(results_import$Activity.Start.Time)){
    #   stop(paste0("There is an issue parsing the times for logger", template_sheets[i],
    #              ". There is a likely excel formatting issue or an invalid time due to time changes. ",
    #              "Please fix time formatting in template sheet before continuing"))
    #
    # }
    #

    results_import <- dplyr::select(results_import,
                                    -TEMP_DQL,
                                    -DO_DQL,
                                    -PH_DQL,
                                    -TURB_DQL,
                                    -COND_DQL,
                                    -Q_DQL)

    results_import <- tidyr::pivot_longer(results_import,
                                           cols = c("Temperature, water","Dissolved oxygen (DO)",
                                                    "Dissolved oxygen saturation", "pH",
                                                    "Turbidity","Conductivity",
                                                    "Flow"),
                                           names_to = "Characteristic.Name",
                                           values_to = "Result.Value",
                                           values_drop_na = TRUE
                                           )
    # remove rows with all NAs
    results_import <- dplyr::mutate(results_import,
                                    Activity.Start.End.Time.Zone = timezone,
                                    Equipment.ID = template_sheets[i],
                                    Result.Unit = dplyr::case_when(Characteristic.Name == "Temperature, water" ~ "deg C",
                                                                   Characteristic.Name == "Dissolved oxygen (DO)" ~ "mg/l",
                                                                   Characteristic.Name == "Dissolved oxygen saturation" ~ "%",
                                                                   Characteristic.Name == "pH" ~ "pH Units",
                                                                   Characteristic.Name == "Turbidity" ~ "NTU",
                                                                   Characteristic.Name == "Conductivity" ~'umho/cm',
                                                                   Characteristic.Name == 'Flow' ~ 'cfs',
                                                                   TRUE ~ "ERROR"),
                                    Result.Comment = NA,
                                    Result.Status.ID = "Final")


    results_import <- dplyr::left_join(results_import, mloc_lookup, by = "Equipment.ID")

    results_import <- select(results_import,
                             "Monitoring.Location.ID", "Activity.Start.Date", "Activity.Start.Time", "Activity.Start.End.Time.Zone",
                             "Equipment.ID", "Characteristic.Name", "Result.Value", "Result.Unit", "Result.Status.ID", "Result.Comment")


    results_list[[i]] <- results_import
  }

    results_import <- dplyr::bind_rows(results_list)



  # Import PrePost Info --------------------------------------------------------------

  # Column
  # 1	Equipment ID #
  # 2	Characteristic Name
  # 3	Equipment Result Value
  # 4	Equipment Result Unit
  # 5	Reference Result Value
  # 6	Reference Result Unit
  # 7	Reference ID #


    prepost_col_types <- c('text', 'text','text', 'date', 'numeric', 'numeric',  'text', 'numeric', 'text', 'text')

    prepost_col_names <- c("Equipment.ID", "Characteristic.Name", "Equipment.Result.Unit",
                           "Datetime",
                           "Reference.Result.Value", "Equipment.Result.Value", "Reference.ID", "DIFF",
                           "DQL", "COMMENTS")

    # read results tab of submitted file
    prepost_import <- readxl::read_excel(file, sheet = "PrePostResults", col_types = prepost_col_types)
    colnames(prepost_import) <- prepost_col_names

    prepost_import$Reference.Result.Unit <- prepost_import$Equipment.Result.Unit
    prepost_import <- dplyr::select(prepost_import,
                                    "Equipment.ID", "Characteristic.Name", "Equipment.Result.Value", "Equipment.Result.Unit",
                                    "Reference.Result.Value", "Reference.Result.Unit", "Reference.ID")


    #Error check for correct parameters. Stop processing and direct the user to correct parameter name issue before continuing

      if(!all(prepost_import$Characteristic.Name %in% c("Temperature, water", "Dissolved oxygen (DO)",
                                                        "Dissolved oxygen saturation",
                                                        "pH","Turbidity","Conductivity"))){
      stop(paste0("Invalid Parameter name in PrePostResults tab. Please fix before continuing"))

    }





    # remove rows with all NAs
    prepost_import <- prepost_import[rowSums(is.na(prepost_import)) != ncol(prepost_import), ]

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

    audit_col_types <- c('text', 'text', 'text', 'text', 'text', 'text', 'date', 'date', 'numeric', 'numeric','numeric',
                         'text', "text")

    audit_col_names <- c("Equipment.ID", 'Monitoring.Location.ID',  'Characteristic.Name', 'Result.Unit', 'Reference.ID',
                         'AuditType', 'Activity.Start.Date', 'Activity.Start.Time', 'Result.Value', 'logger.value', 'DIFF', 'DQL', 'Result.Comment')



    # audit_col_names <- c("Project.ID", "Alternate.Project.ID.1", "Alternate.Project.ID.2", "Monitoring.Location.ID",
    #                      "Activity.Start.Date", "Activity.Start.Time", "Activity.End.Date", "Activity.End.Time",
    #                      "Activity.Start.End.Time.Zone", "Activity.Type", "Activity.ID", "Equipment.ID",
    #                      "Sample.Collection.Method", "Characteristic.Name", "Result.Value", "Result.Unit",
    #                      "Result.Analytical.Method.ID", "Result.Analytical.Method.Context", "Result.Value.Type", "Result.Status.ID",
    #                      "Result.Measure.Qualifier", "Result.Comment")

    audit_import <- readxl::read_excel(file, sheet = "FieldAuditResults", col_types = audit_col_types)
    colnames(audit_import) <- audit_col_names


    audit_import$Project.ID <- project
    audit_import$Alternate.Project.ID.1 <- NA
    audit_import$Alternate.Project.ID.2 <- NA
    audit_import$Activity.End.Date <- audit_import$Activity.Start.Date
    audit_import$Activity.End.Time <- audit_import$Activity.Start.Time
    audit_import$Activity.Start.End.Time.Zone <- project
    audit_import$Activity.Type <- "Quality Control Field Replicate Portable Data Logger"
    audit_import <- dplyr::mutate(audit_import, Activity.ID  = paste0(Monitoring.Location.ID, ":",
                                                                      format.Date(Activity.Start.Date, "%Y"),
                                                                      format.Date(Activity.Start.Date, "%m"),
                                                                      format.Date(Activity.Start.Date, "%d"),
                                                                      format.Date(Activity.Start.Time, "%H"),
                                                                      format.Date(Activity.Start.Time, "%M"),
                                                                      ":QPDL"
                                                                      ))
    audit_import$Sample.Collection.Method <-"Grab"
    audit_import$Result.Analytical.Method.ID <- '170.1'
    audit_import$Result.Analytical.Method.Context <- "USEPA"
    audit_import$Result.Value.Type <- NA
    audit_import$Result.Status.ID <- "Accepted"
    audit_import$Result.Measure.Qualifier <- NA

    audit_import <- dplyr::select(audit_import,
                                  "Project.ID", "Alternate.Project.ID.1", "Alternate.Project.ID.2", "Monitoring.Location.ID",
                                  "Activity.Start.Date", "Activity.Start.Time", "Activity.End.Date", "Activity.End.Time",
                                  "Activity.Start.End.Time.Zone", "Activity.Type", "Activity.ID", "Equipment.ID",
                                  "Sample.Collection.Method", "Characteristic.Name", "Result.Value", "Result.Unit",
                                  "Result.Analytical.Method.ID", "Result.Analytical.Method.Context", "Result.Value.Type",
                                  "Result.Status.ID", "Result.Measure.Qualifier", "Result.Comment")




    # remove rows with all NAs
    audit_import <- audit_import[rowSums(is.na(audit_import)) != ncol(audit_import), ]

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
