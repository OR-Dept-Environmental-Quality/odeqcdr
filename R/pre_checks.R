#' Basic pre-processing completeness review.
#'
#' This function will execute a basic completeness review for Oregon DEQ's continuous
#' data submission template xlsx file v2.03. The function checks for missing data,
#' missing values in required columns, invalid domain values, and flags potential errors.
#' If a check result is TRUE, it means the check failed and something is missing or there is an invalid value.
#'
#' @param template_list A continuous data list object with each list holding
#' a different worksheet from the xlsx data template. Use [odeqcdr::contin_import()] to make the list.
#' @export
#' @return data frame of the check and check result.

pre_checks <- function(template_list) {

  # Test
  #template_list=df0

  # The following are invalid characters in Monitoring Location IDs
  # ` ~ ! @ # $ % ^ & * ( ) [ { ] } \ | ; : < > / ? [space]
  invalid_chars <- "\\~+|\\`+|\\!+|\\@+|\\#+|\\$+|\\%+|\\^+|\\&+|\\*+|\\(+|\\)+|\\[+|\\{+|\\]+|\\}+|\\;+|\\:+|\"+|\'+|\\,+|\\|+|\\\\+|[<>]|\\/+|\\?+|\\s+"

  org_import <- template_list[["Organization_Details"]]
  projects_import <- template_list[["Projects"]]
  locations_import <- template_list[["Monitoring_Locations"]]
  deployment_import <- template_list[["Deployment"]]
  results_import <- template_list[["Results"]]
  prepost_import <- template_list[["PrePost"]]
  audit_import <- template_list[["Audit_Data"]]

  mlocid_locations <- unique(locations_import$Monitoring.Location.ID)
  mlocid_results <- unique(results_import$Monitoring.Location.ID)

  eqiupid_char_results <- unique(paste(results_import$Equipment.ID, results_import$Characteristic.Name))
  eqiupid_char_prepost <- unique(paste(prepost_import$Equipment.ID, prepost_import$Characteristic.Name))

  audits_deploy <- unique(paste0("[",audit_import$Monitoring.Location.ID," - ",audit_import$Equipment.ID," - ",audit_import$Characteristic.Name,"]"))
  results_deploy <- unique(paste0("[",results_import$Monitoring.Location.ID," - ",results_import$Equipment.ID," - ", results_import$Characteristic.Name,"]"))
  deploy_deploy <- unique(paste0("[",deployment_import$Monitoring.Location.ID," - ",deployment_import$Equipment.ID," - ", deployment_import$Characteristic.Name,"]"))

  #- Organization Details ------------------------------------------------------

  org_msg <- c("Missing Organization Name",
               "Missing Address",
               "Missing Phone Number",
               "Missing Email",
               "Missing response to Type(s) of Data Submitted",
               "Missing response to Prior DEQ Data Submission")

  org_checks <- c(
    is.na(org_import$value[1]),
    is.na(org_import$value[3]),
    is.na(org_import$value[4]),
    is.na(org_import$value[6]),
    is.na(org_import$value[9]),
    is.na(org_import$value[11])
  )

  org_t_row <- ifelse(org_checks, c(1,3,4,6,9,11) + 5, NA)

  org_df <- data.frame(worksheet=rep("Organization Details",length(org_msg)),
                          check=org_msg,
                          check_result=org_checks,
                          TRUE_row=org_t_row)

  #- Projects ------------------------------------------------------------------

  projects_msg <- c("Worksheet is empty",
                    "Missing value in column Project.IDs",
                    "Missing value in column Project.Name",
                    "Missing value in column Project.Description",
                    "Missing value in column Approved.QAPP.Indicator",
                    "Value in column Approval.Agency.Name is NA but value in column Approved.QAPP.Indicator = 'Yes'")

  projects_checks <- list((nrow(projects_import) <= 0),
                          is.na(projects_import$Project.ID),
                          is.na(projects_import$Project.Name),
                          is.na(projects_import$Project.Description),
                          is.na(projects_import$Approved.QAPP.Indicator),
                          !projects_import$Approved.QAPP.Indicator=="Yes" & is.na(projects_import$QAPP.Approval.Agency.Name)
  )

  projects_result <- unlist(lapply(projects_checks, FUN=any, na.rm = TRUE))
  projects_t_row <- unlist(lapply(projects_checks, FUN=odeqcdr:::tstr))

  projects_df <- data.frame(worksheet=rep("Projects",length(projects_msg)),
                            check=projects_msg,
                            check_result=projects_result,
                            TRUE_row=projects_t_row)

  #- Monitoring_Locations-------------------------------------------------------

  mloc_ml_check <- any(!mlocid_results %in% locations_import$Monitoring.Location.ID)

  if(mloc_ml_check) {

    mloc_ml_missing <- mlocid_results[!mlocid_results %in% locations_import$Monitoring.Location.ID]
    mloc_ml_msg <- "Monitoring Location ID in Results and not in Monitoring_Locations. Missing IDs listed in TRUE_row."
    mloc_ml_t <- paste0(mloc_ml_missing, collapse = ", ")

  } else {

    mloc_ml_msg <- paste0("Monitoring Location ID in results and not in Monitoring_Locations")
    mloc_ml_t <- NA
  }

  mloc_msg <- c("Worksheet is empty",
                mloc_ml_msg,
                "Value in column Monitoring.Location.ID is > 16 characters",
                'Value in column in Monitoring.Location.ID has an invalid character: ` ~ ! @ # $ % ^ & * ( ) [ { ] } \ | ; : \' \" < > / ? [space]',
                "Missing value in column Monitoring.Location.Name",
                "Invalid value in column Monitoring.Location.Type",
                "Missing value in column Latitude",
                "Missing value in column Longitude",
                "Invalid value in column Horizontal.Datum",
                "Invalid value in column Coordinate.Collection.Method",
                "Invalid value in column Tribal.Land",
                "Invalid value in column County.Name",
                "Invalid value in column State.Code",
                "Invalid value in column HUC.8.Code",
                "Value in column Latitude is outside of Oregon",
                "Value in column Longitude is outside of Oregon",
                "Value in column Source.Map.Scale is NA but column Coordinate.Collection.Method ='Interpolation-Map'",
                "Value in column County.Name is NA but value in column State.Code=='OR'",
                "Missing value in column Alternatve.Context.1 because column Alternate.ID.1 has information",
                "Missing value in column Alternatve.Context.2 because column Alternate.ID.2 has information",
                "Missing value in column Reachcode",
                "Missing value in column Measure",
                "Missing value in column LLID",
                "Missing value in column River.Mile"
  )

  mloc_checks <- list((nrow(locations_import) <= 0),
                      mloc_ml_check,
                      nchar(locations_import$Monitoring.Location.ID) > 16,
                      grepl(pattern=invalid_chars, x=locations_import$Monitoring.Location.ID),
                      is.na(locations_import$Monitoring.Location.Name),
                      !valid_values_check(col="Monitoring.Location.Type", vals=locations_import$Monitoring.Location.Type),
                      is.na(locations_import$Latitude),
                      is.na(locations_import$Longitude),
                      !valid_values_check(col="Horizontal.Datum", vals=locations_import$Horizontal.Datum),
                      !valid_values_check(col="Coordinate.Collection.Method", vals=locations_import$Coordinate.Collection.Method),
                      !valid_values_check(col="Tribal.Land", vals=locations_import$Tribal.Land),
                      !valid_values_check(col="County.Name", vals=locations_import$County.Name),
                      !valid_values_check(col="State.Code", vals=locations_import$State.Code),
                      !valid_values_check(col="HUC.8.Code", vals=locations_import$HUC.8.Code),
                      (locations_import$Latitude < 41.8075 | locations_import$Latitude > 46.3586),
                      (locations_import$Longitude < -124.6155 | locations_import$Longitude > -116.3519),
                      (locations_import$Coordinate.Collection.Method=="Interpolation-Map" & is.na(locations_import$Source.Map.Scale)),
                      !is.na(locations_import$County.Name) & !locations_import$State.Code=="OR",
                      (!is.na(locations_import$Alternate.ID.1) & is.na(locations_import$Alternate.Context.1)),
                      (!is.na(locations_import$Alternate.ID.2) & is.na(locations_import$Alternate.Context.2)),
                      is.na(locations_import$Reachcode),
                      is.na(locations_import$Measure),
                      is.na(locations_import$LLID),
                      is.na(locations_import$River.Mile)
  )

  mloc_result <- unlist(lapply(mloc_checks, FUN=any, na.rm = TRUE))
  mloc_t_row <- unlist(lapply(mloc_checks, FUN=odeqcdr:::tstr))
  mloc_t_row[2] <- mloc_ml_t

  mloc_df <- data.frame(worksheet=rep("Monitoring_Locations", length(mloc_msg)),
                        check=mloc_msg,
                        check_result=mloc_result,
                        TRUE_row=mloc_t_row)

  #- Deployment-----------------------------------------------------------------

  deploy_d_check <- any(!deploy_deploy %in% results_deploy)

  if(deploy_d_check) {
    deploy_d_missing <- deploy_deploy[!deploy_deploy %in% results_deploy]

    deploy_d_msg <- "Deployments [Monitoring.Location.ID - Equipment.ID - Characteristic.Name] in Results and not in Deployment. Missing deployments listed in TRUE_row."
    deploy_d_t <-paste0(deploy_d_missing, collapse = ", ")
  } else {
    deploy_d_msg <- "Deployments in Results and not in Deployment."
    deploy_d_t <- NA
  }

  deploy_msg <- c("Worksheet is empty",
                  deploy_d_msg,
                  "Missing value in column Equipment.ID",
                  "Invalid value in column Characteristic.Name",
                  "Missing value in column Deployment.Start.Date",
                  "Missing value in column Deployment.End.Date",
                  "Missing value in column Sample.Depth",
                  "Invalid value in column Sample.Depth.Unit",
                  "Invalid value in column Sample.Media",
                  "Invalid value in column Sample.Sub.Media"
  )

  deploy_checks <- list((nrow(deployment_import) <= 0),
                        deploy_d_check,
                        is.na(deployment_import$Equipment.ID),
                        !valid_values_check(col="Characteristic.Name", vals=deployment_import$Characteristic.Name),
                        is.na(deployment_import$Deployment.Start.Date),
                        is.na(deployment_import$Deployment.End.Date),
                        is.na(deployment_import$Sample.Depth),
                        !valid_values_check(col="Sample.Depth.Unit", vals=deployment_import$Sample.Depth.Unit),
                        !valid_values_check(col="Sample.Media", vals=deployment_import$Sample.Media),
                        !valid_values_check(col="Sample.Sub.Media", vals=deployment_import$Sample.Sub.Media)
  )

  deploy_result <- unlist(lapply(deploy_checks, FUN=any, na.rm = TRUE))
  deploy_t_row <- unlist(lapply(deploy_checks, FUN=odeqcdr:::tstr))
  deploy_t_row[2] <- deploy_d_t

  deploy_df <- data.frame(worksheet=rep("Deployment",length(deploy_msg)),
                          check=deploy_msg,
                          check_result=deploy_result,
                          TRUE_row=deploy_t_row)

  #- Results--------------------------------------------------------------------

  result_ml_check <- any(!mlocid_results %in% locations_import$Monitoring.Location.ID)

  if(result_ml_check) {
    result_ml_missing <- mlocid_results[!mlocid_results %in% locations_import$Monitoring.Location.ID]

    result_ml_msg <- paste0("Monitoring Location ID in Monitoring_Locations and not in Results. Missing IDs listed in TRUE_row.")
    result_ml_t <- paste0(result_ml_missing, collapse = ", ")
  } else {
    result_ml_msg <- paste0("Monitoring Location ID in Monitoring_Locations and not in Results")
    result_ml_t <- NA
  }

  results_msg <- c(
    "Worksheet is empty",
    result_ml_msg,
    "Missing value in column Activity.Start.Date",
    "Missing value in column Activity.Start.Time",
    "Invalid value in column Activity.Start.End.Time.Zone",
    "Missing value in column Equipment.ID",
    "Invalid value in column Characteristic.Name",
    "Missing value in column Result.Value",
    "Invalid value in column Result.Unit",
    "Invalid value in column Result.Status.ID"
  )

  results_checks <- list((nrow(results_import) <= 0),
                         result_ml_check,
                         is.na(results_import$Activity.Start.Date),
                         is.na(results_import$Activity.Start.Time),
                         !valid_values_check(col="Activity.Start.End.Time.Zone", vals=results_import$Activity.Start.End.Time.Zone),
                         is.na(results_import$Equipment.ID),
                         !valid_values_check(col="Characteristic.Name", vals=results_import$Characteristic.Name),
                         is.na(results_import$Result.Value),
                         !valid_values_check(col="Result.Unit", vals=results_import$Result.Unit),
                         !valid_values_check(col="Result.Status.ID", vals=results_import$Result.Status.ID)
  )

  results_result <- unlist(lapply(results_checks, FUN=any, na.rm = TRUE))
  results_t_row <- unlist(lapply(results_checks, FUN=odeqcdr:::tstr))
  results_t_row[2] <- result_ml_t

  result_df <- data.frame(worksheet=rep("Results",length(results_msg)),
                          check=results_msg,
                          check_result=results_result,
                          TRUE_row=results_t_row)

  #- PrePost--------------------------------------------------------------------

  prepost_eqid_check <- any(!eqiupid_char_prepost %in% eqiupid_char_results)

  if(prepost_eqid_check) {
    prepost_eqid_missing <- eqiupid_char_prepost[!eqiupid_char_prepost %in% eqiupid_char_results]

    prepost_eqid_msg <- "Missing PrePost Results for Equipment IDs that are in Results worksheet. Missing IDs listed in TRUE_row"
    prepost_eqid_t <- paste0(prepost_eqid_missing, collapse = ", ")
  } else {
    prepost_eqid_msg <- paste0("Missing PrePost Results for Equipment IDs that are in Results worksheet.")
    prepost_eqid_t <- NA
  }


  prepost_msg <- c("Worksheet is empty",
                   prepost_eqid_msg,
                   "Invalid value in column Characteristic.Name",
                   "Missing value in column Equipment.Result.Value",
                   "Invalid value in column Equipment.Result.Unit",
                   "Missing value in column Reference.Result.Value",
                   "Invalid value in column Reference.Result.Unit",
                   "Missing value in column Reference.ID"
  )

  prepost_checks <- list((nrow(prepost_import) <= 0),
                         prepost_eqid_check,
                         !valid_values_check(col="Characteristic.Name", vals=prepost_import$Characteristic.Name),
                         is.na(prepost_import$Equipment.Result.Value),
                         !valid_values_check(col="Equipment.Result.Unit", vals=prepost_import$Equipment.Result.Unit),
                         is.na(prepost_import$Reference.Result.Value),
                         !valid_values_check(col="Reference.Result.Unit", vals=prepost_import$Reference.Result.Unit),
                         is.na(prepost_import$Reference.ID)
  )

  prepost_result <- unlist(lapply(prepost_checks, FUN=any, na.rm = TRUE))
  prepost_t_row <- unlist(lapply(prepost_checks, FUN=odeqcdr:::tstr))
  prepost_t_row[2] <- prepost_eqid_t

  prepost_df <- data.frame(worksheet=rep("PrePost",length(prepost_msg)),
                           check=prepost_msg,
                           check_result=prepost_result,
                           TRUE_row=prepost_t_row)

  #- Audit Data-----------------------------------------------------------------

  audit_d_check <- any(!audits_deploy %in% results_deploy)

  if(audit_d_check) {
    audit_d_missing <- audits_deploy[!audits_deploy %in% results_deploy]

    audit_d_msg <- "Audits missing for some deployments [Monitoring.Location.ID - Equipment.ID - Characteristic.Name] in Results worksheet. Missing audits for deployments listed in TRUE_row."
    audit_d_t <- paste0(audit_d_missing, collapse = ", ")

  } else {
    audit_d_msg <- paste0("Audits missing for some for some deployments [Monitoring.Location.ID - Equipment.ID - Characteristic.Name] in Results worksheet")
    audit_d_t <- NA
  }

  audit_msg <- c("Worksheet is empty",
                 audit_d_msg,
                 "Missing value in column Project.ID",
                 "Missing value in column Activity.Start.Date",
                 "Missing value in column Activity.Start.Time",
                 "Missing value in column Activity.End.Date",
                 "Missing value in column Activity.End.Time",
                 "Invalid value in column Activity.Start.End.Time.Zone",
                 "Invalid value in column Activity.Type",
                 "Missing value in column Activity.ID",
                 "Missing value in column Equipment.ID",
                 "Invalid value in column Sample.Collection.Method",
                 "Invalid value in column Characteristic.Name",
                 "Missing value in column Result.Value",
                 "Invalid value in column Result.Unit",
                 "Invalid value in column Result.Analytical.Method.ID",
                 "Invalid value in column Result.Analytical.Method.Context",
                 "Invalid value in column Result.Value.Type",
                 "Invalid value in column Result.Status.ID",
                 "Invalid value in column Result.Measure.Qualifier"
  )

  audit_checks <- list((nrow(audit_import) <= 0),
                       audit_d_check,
                       is.na(audit_import$Project.ID),
                       is.na(audit_import$Activity.Start.Date),
                       is.na(audit_import$Activity.Start.Time),
                       is.na(audit_import$Activity.End.Date),
                       is.na(audit_import$Activity.End.Time),
                       !valid_values_check(col="Activity.Start.End.Time.Zone", vals=audit_import$Activity.Start.End.Time.Zone),
                       !valid_values_check(col="Activity.Type", vals=audit_import$Activity.Type),
                       is.na(audit_import$Activity.ID),
                       is.na(audit_import$Equipment.ID),
                       !valid_values_check(col="Sample.Collection.Method", vals=audit_import$Sample.Collection.Method),
                       !valid_values_check(col="Characteristic.Name", vals=audit_import$Characteristic.Name),
                       is.na(audit_import$Result.Value),
                       !valid_values_check(col="Result.Unit", vals=audit_import$Result.Unit),
                       !valid_values_check(col="Result.Analytical.Method.ID", vals=audit_import$Result.Analytical.Method.ID),
                       !valid_values_check(col="Result.Analytical.Method.Context", vals=audit_import$Result.Analytical.Method.Context),
                       !valid_values_check(col="Result.Value.Type", vals=audit_import$Result.Value.Type),
                       !valid_values_check(col="Result.Status.ID", vals=audit_import$Result.Status.ID),
                       !valid_values_check(col="Result.Measure.Qualifier", vals=audit_import$Result.Measure.Qualifier)
  )

  audit_result <- unlist(lapply(audit_checks, FUN=any, na.rm = TRUE))
  audit_t_row <- unlist(lapply(audit_checks, FUN=odeqcdr:::tstr))
  audit_t_row[2] <- audit_d_t

  audit_df <- data.frame(worksheet=rep("Audit_Data",length(audit_msg)),
                         check=audit_msg,
                         check_result=audit_result,
                         TRUE_row=audit_t_row)

  checks_df <- rbind(org_df, projects_df, mloc_df, deploy_df, result_df, prepost_df, audit_df)

  return(checks_df)

}
