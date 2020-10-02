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

  #template_list=df0

  projects_import <- template_list[["Projects"]]
  locations_import <- template_list[["Monitoring_Locations"]]
  deployment_import <- template_list[["Deployment"]]
  results_import <- template_list[["Results"]]
  prepost_import <- template_list[["PrePost"]]
  audit_import <- template_list[["Audit_Data"]]


  mlocid_locations <- unique(locations_import$Monitoring.Location.ID)
  mlocid_results <- unique(results_import$Monitoring.Location.ID)

  mlocid_char_deploy <- unique(paste(deployment_import$Monitoring.Location.ID, deployment_import$Characteristic.Name))
  mlocid_char_results <- unique(paste(results_import$Monitoring.Location.ID, results_import$Characteristic.Name))
  mlocid_char_audits <- unique(paste(audit_import$Monitoring.Location.ID, audit_import$Characteristic.Name))

  eqiupid_char_results <- unique(paste(results_import$Equipment.ID, results_import$Characteristic.Name))
  eqiupid_char_prepost <- unique(paste(prepost_import$Equipment.ID, prepost_import$Characteristic.Name))

  #- Valid Value checks --------------------------------------------------------

  mloc_val_msg <- c(
    "Invalid value in column Monitoring.Location.Type",
    "Invalid value in column Horizontal.Datum",
    "Invalid value in column Coordinate.Collection.Method",
    "Invalid value in column Tribal.Land",
    "Invalid value in column County.Name",
    "Invalid value in column State.Code",
    "Invalid value in column HUC.8.Code"
  )

  mloc_val_check <- c(
    any(!valid_values_check(col="Monitoring.Location.Type", vals=locations_import$Monitoring.Location.Type)),
    any(!valid_values_check(col="Horizontal.Datum", vals=locations_import$Horizontal.Datum)),
    any(!valid_values_check(col="Coordinate.Collection.Method", vals=locations_import$Coordinate.Collection.Method)),
    any(!valid_values_check(col="Tribal.Land", vals=locations_import$Tribal.Land)),
    any(!valid_values_check(col="County.Name", vals=locations_import$County.Name)),
    any(!valid_values_check(col="State.Code", vals=locations_import$State.Code)),
    any(!valid_values_check(col="HUC.8.Code", vals=locations_import$HUC.8.Code))
  )


  deploy_val_msg <- c(
    "Invalid value in column Characteristic.Name",
    "Invalid value in column Sample.Depth.Unit",
    "Invalid value in column Sample.Media",
    "Invalid value in column Sample.Sub.Media"
  )

  deploy_val_check <- c(
    any(!valid_values_check(col="Characteristic.Name", vals=deployment_import$Characteristic.Name)),
    any(!valid_values_check(col="Sample.Depth.Unit", vals=deployment_import$Sample.Depth.Unit)),
    any(!valid_values_check(col="Sample.Media", vals=deployment_import$Sample.Media)),
    any(!valid_values_check(col="Sample.Sub.Media", vals=deployment_import$Sample.Sub.Media))
  )

  results_val_msg <- c(
    "Invalid value in column Activity.Start.End.Time.Zone",
    "Invalid value in column Characteristic.Name",
    "Invalid value in column Result.Unit",
    "Invalid value in column Result.Status.ID"
  )

  results_val_check <- c(
    any(!valid_values_check(col="Activity.Start.End.Time.Zone", vals=results_import$Activity.Start.End.Time.Zone)),
    any(!valid_values_check(col="Characteristic.Name", vals=results_import$Characteristic.Name)),
    any(!valid_values_check(col="Result.Unit", vals=results_import$Result.Unit)),
    any(!valid_values_check(col="Result.Status.ID", vals=results_import$Result.Status.ID))
  )

  prepost_val_msg <- c(
    "Invalid value in column Characteristic.Name",
    "Invalid value in column Equipment.Result.Unit",
    "Invalid value in column Reference.Result.Unit"
  )

  prepost_val_check <- c(
    any(!valid_values_check(col="Characteristic.Name", vals=prepost_import$Characteristic.Name)),
    any(!valid_values_check(col="Equipment.Result.Unit", vals=prepost_import$Equipment.Result.Unit)),
    any(!valid_values_check(col="Reference.Result.Unit", vals=prepost_import$Reference.Result.Unit))
  )

  audit_val_msg <- c(
    "Invalid value in column ctivity.Start.End.Time.Zone",
    "Invalid value in column Activity.Type",
    "Invalid value in column Sample.Collection.Method",
    "Invalid value in column Characteristic.Name",
    "Invalid value in column Result.Unit",
    "Invalid value in column Result.Analytical.Method.ID",
    "Invalid value in column Result.Analytical.Method.Context",
    "Invalid value in column Result.Value.Type",
    "Invalid value in column Result.Status.ID",
    "Invalid value in column Result.Measure.Qualifier"
  )

  audit_val_check <- c(
    any(!valid_values_check(col="Activity.Start.End.Time.Zone", vals=audit_import$Activity.Start.End.Time.Zone)),
    any(!valid_values_check(col="Activity.Type", vals=audit_import$Activity.Type)),
    any(!valid_values_check(col="Sample.Collection.Method", vals=audit_import$Sample.Collection.Method)),
    any(!valid_values_check(col="Characteristic.Name", vals=audit_import$Characteristic.Name)),
    any(!valid_values_check(col="Result.Unit", vals=audit_import$Result.Unit)),
    any(!valid_values_check(col="Result.Analytical.Method.ID", vals=audit_import$Result.Analytical.Method.ID)),
    any(!valid_values_check(col="Result.Analytical.Method.Context", vals=audit_import$Result.Analytical.Method.Context)),
    any(!valid_values_check(col="Result.Value.Type", vals=audit_import$Result.Value.Type)),
    any(!valid_values_check(col="Result.Status.ID", vals=audit_import$Result.Status.ID)),
    any(!valid_values_check(col="Result.Measure.Qualifier", vals=audit_import$Result.Measure.Qualifier))
  )

  #- Projects --------------------------------------------------------------------

  projects_msg <- c("No data",
                    "Missing Project IDs",
                    "Missing Project Name",
                    "Missing Project Description",
                    "Missing Approved QAPP Indicator",
                    "Approval Agency Name is NA but QAPP Approved QAPP Indicator = 'Yes'")

  projects_checks <- c(
    (nrow(projects_import) <= 0),
    any(is.na(projects_import$Project.ID)),
    any(is.na(projects_import$Project.Name)),
    any(is.na(projects_import$Project.Description)),
    any(is.na(projects_import$Approved.QAPP.Indicator)),
    any(!projects_import$Approved.QAPP.Indicator=="Yes" & is.na(projects_import$QAPP.Approval.Agency.Name))
  )

  projects_df <- data.frame(worksheet=rep("Projects",length(projects_msg)),
                            check=projects_msg,
                            check_result=projects_checks)

  #- Monitoring_Locations-------------------------------------------------------

  mloc_ml_check <- any(!mlocid_results %in% locations_import$Monitoring.Location.ID)

  if(mloc_ml_check) {

    mloc_ml_missing <- mlocid_results[!mlocid_results %in% locations_import$Monitoring.Location.ID]
    mloc_ml_msg <- paste0("Monitoring Location ID in results and not in Monitoring_Locations. Missing IDs include: ",
                          paste0(mloc_ml_missing, collapse = ", "))

  } else {

    mloc_ml_msg <- paste0("Monitoring Location ID in results and not in Monitoring_Locations")
  }

  mloc_msg <- c("No data",
                mloc_ml_msg,
                "Missing Monitoring Location Name",
                "Missing Monitoring Location Type",
                "Missing Latitude",
                "Missing Longitude",
                "One or more Latitude values outside of Oregon",
                "One or more Longitude values outside of Oregon",
                "Missing Horizontal Datum",
                "Missing Coordinate Collection Method",
                "Source Map Scale is NA but Coordinate Collection Method ='Interpolation-Map'",
                "Missing Tribal Land",
                "County Name is NA but State Code=='OR'",
                "Missing Alternatve Context 1 because Alternate ID 1 has information",
                "Missing Alternatve Context 2 because Alternate ID 2 has information",
                "Missing Reach code",
                "Missing Measure",
                "Missing LLID",
                "Missing River Mile",
                mloc_val_msg
  )

  mloc_checks <- c((nrow(locations_import) <= 0),
                   mloc_ml_check,
                   any(is.na(locations_import$Monitoring.Location.Name)),
                   any(is.na(locations_import$Monitoring.Location.Type)),
                   any(is.na(locations_import$Latitude)),
                   any(is.na(locations_import$Longitude)),
                   any(!(locations_import$Latitude >= 41.8075 & locations_import$Latitude <= 46.3586)),
                   any(!(locations_import$Longitude >= -124.7777 & locations_import$Longitude <= -116.3519)),
                   any(is.na(locations_import$Horizontal.Datum)),
                   any(is.na(locations_import$Coordinate.Collection.Method)),
                   any((locations_import$Coordinate.Collection.Method=="Interpolation-Map" & is.na(locations_import$Source.Map.Scale))),
                   any(is.na(locations_import$Tribal.Land)),
                   any(!is.na(locations_import$County.Name) & !locations_import$State.Code=="OR"),
                   any((!is.na(locations_import$Alternate.ID.1) & is.na(locations_import$Alternate.Context.1))),
                   any((!is.na(locations_import$Alternate.ID.2) & is.na(locations_import$Alternate.Context.2))),
                   any(is.na(locations_import$Reachcode)),
                   any(is.na(locations_import$Measure)),
                   any(is.na(locations_import$LLID)),
                   any(is.na(locations_import$River.Mile)),
                   mloc_val_check
  )

  mloc_df <- data.frame(worksheet=rep("Monitoring_Locations",length(mloc_msg)),
                                      check=mloc_msg,
                        check_result=mloc_checks)

  #- Deployment-----------------------------------------------------------------

  deploy_ml_check <- any(!mlocid_char_deploy %in% mlocid_char_results)

  if(deploy_ml_check) {
    deploy_ml_missing <- mlocid_char_deploy[!mlocid_char_deploy %in% mlocid_char_results]

    deploy_ml_msg <- paste0("Monitoring Location ID in Results and not in Deployment. Missing IDs include: ",
                            paste0(deploy_ml_missing, collapse = ", "))
  } else {
    deploy_ml_msg <- paste0("Monitoring Location ID in Results and not in Deployment")
  }

  deploy_msg <- c("No data",
                  deploy_ml_msg,
                  "Missing Equipment ID",
                  "Missing Characteristic Name",
                  "Missing Deployment Start Date",
                  "Missing Deployment End Date",
                  "Missing Sample Depth",
                  "Missing Sample Depth Unit",
                  "Missing Sample Media",
                  deploy_val_msg
  )

  deploy_checks <- c((nrow(deployment_import) <= 0),
                     deploy_ml_check,
                     any(is.na(deployment_import$Equipment.ID)),
                     any(is.na(deployment_import$Characteristic.Name)),
                     any(is.na(deployment_import$Deployment.Start.Date)),
                     any(is.na(deployment_import$Deployment.End.Date)),
                     any(is.na(deployment_import$Sample.Depth)),
                     any(is.na(deployment_import$Sample.Depth.Unit)),
                     any(is.na(deployment_import$Sample.Media)),
                     deploy_val_check
  )

  deploy_df <- data.frame(worksheet=rep("Deployment",length(deploy_msg)),
                          check=deploy_msg,
                          check_result=deploy_checks)

  #- Results--------------------------------------------------------------------

  result_ml_check <- any(!mlocid_results %in% locations_import$Monitoring.Location.ID)

  if(result_ml_check) {
    result_ml_missing <- mlocid_results[!mlocid_results %in% locations_import$Monitoring.Location.ID]

    result_ml_msg <- paste0("Monitoring Location ID in Monitoring_Locations and not in Results. Missing IDs include: ",
                            paste0(result_ml_missing, collapse = ", "))
  } else {
    result_ml_msg <- paste0("Monitoring Location ID in Monitoring_Locations and not in Results")
  }

  result_msg <- c("No data",
                  result_ml_msg,
                  "Missing Activity Start Date",
                  "Missing Activity Start Time",
                  "Missing Activity Start/End Time Zone",
                  "Missing Equipment ID #",
                  "Missing Characteristic Name",
                  "Missing Result Value",
                  "Missing Result Unit",
                  "Missing Result Status ID",
                  results_val_msg
  )


  result_checks <- c((nrow(results_import) <= 0),
                     result_ml_check,
                     any(is.na(results_import$Activity.Start.Date)),
                     any(is.na(results_import$Activity.Start.Time)),
                     any(is.na(results_import$Activity.Start.End.Time.Zone)),
                     any(is.na(results_import$Equipment.ID)),
                     any(is.na(results_import$Characteristic.Name)),
                     any(is.na(results_import$Result.Value)),
                     any(is.na(results_import$Result.Unit)),
                     any(is.na(results_import$Result.Status.ID)),
                     results_val_check
  )

  result_df <- data.frame(worksheet=rep("Results",length(result_msg)),
                          check=result_msg,
                          check_result=result_checks)

  #- PrePost--------------------------------------------------------------------

  prepost_eqid_check <- any(!eqiupid_char_prepost %in% eqiupid_char_results)

  if(prepost_eqid_check) {
    prepost_eqid_missing <- eqiupid_char_prepost[!eqiupid_char_prepost %in% eqiupid_char_results]

    prepost_eqid_msg <- paste0("Missing PrePost Results for Equipment IDs that are in Results worksheet. Missing IDs include: ",
                               paste0(prepost_eqid_missing, collapse = ", "))
  } else {
    prepost_eqid_msg <- paste0("Missing PrePost Results for Equipment IDs that are in Results worksheet.")
  }

  prepost_msg <- c("No data",
                   prepost_eqid_msg,
                   "Missing Characteristic Name",
                   "Missing Equipment Result Value",
                   "Missing Equipment Result Unit",
                   "Missing Reference Result Value",
                   "Missing Reference Result Unit",
                   "Missing Reference ID",
                   prepost_val_msg
  )

  prepost_checks <- c((nrow(prepost_import) <= 0),
                      prepost_eqid_check,
                      any(is.na(prepost_import$Characteristic.Name)),
                      any(is.na(prepost_import$Equipment.Result.Value)),
                      any(is.na(prepost_import$Equipment.Result.Unit)),
                      any(is.na(prepost_import$Reference.Result.Value)),
                      any(is.na(prepost_import$Reference.Result.Unit)),
                      any(is.na(prepost_import$Reference.ID)),
                      prepost_val_check
  )

  prepost_df <- data.frame(worksheet=rep("PrePost",length(prepost_msg)),
                           check=prepost_msg,
                           check_result=prepost_checks)

  #- Audit Data-----------------------------------------------------------------

  audit_ml_check <- any(!mlocid_char_audits %in% mlocid_char_results)

  if(audit_ml_check) {
    audit_ml_missing <- mlocid_char_audits[!mlocid_char_audits %in% mlocid_char_results]

    audit_ml_msg <- paste0("Audits missing for some some Monitoring Location ID/Characteristic Names in Results worksheet. Missing audits for these IDs: ",
                           paste0(audit_ml_missing, collapse = ", "))
  } else {
    audit_ml_msg <- paste0("Audits missing for some Monitoring Location ID/Characteristic Names in Results worksheet")
  }


  audit_msg <- c("No data",
                 audit_ml_msg,
                 "Missing Project ID",
                 "Missing Activity Start Date",
                 "Missing Activity Start Time",
                 "Missing Activity End Date",
                 "Missing Activity End Time",
                 "Missing Activity Start End Time Zone",
                 "Missing Activity Type",
                 "Missing Activity ID",
                 "Missing Equipment ID",
                 "Missing Sample Collection Method",
                 "Missing Characteristic Name",
                 "Missing Result Value",
                 "Missing Result Unit",
                 "Missing Result Analytical Method ID",
                 "Missing Result Status ID",
                 audit_val_msg
  )

  audit_checks <- c((nrow(audit_import) <= 0),
                    audit_ml_check,
                    any(is.na(audit_import$Project.ID)),
                    any(is.na(audit_import$Activity.Start.Date)),
                    any(is.na(audit_import$Activity.Start.Time)),
                    any(is.na(audit_import$Activity.End.Date)),
                    any(is.na(audit_import$Activity.End.Time)),
                    any(is.na(audit_import$Activity.Start.End.Time.Zone)),
                    any(is.na(audit_import$Activity.Type)),
                    any(is.na(audit_import$Activity.ID)),
                    any(is.na(audit_import$Equipment.ID)),
                    any(is.na(audit_import$Sample.Collection.Method)),
                    any(is.na(audit_import$Characteristic.Name)),
                    any(is.na(audit_import$Result.Value)),
                    any(is.na(audit_import$Result.Unit)),
                    any(is.na(audit_import$Result.Analytical.Method.ID)),
                    any(is.na(audit_import$Result.Status.ID)),
                    audit_val_check
  )

  audit_df <- data.frame(worksheet=rep("Audit_Data",length(audit_msg)),
                         check=audit_msg,
                         check_result=audit_checks)

  checks_df <- rbind(projects_df, mloc_df, deploy_df, result_df, prepost_df, audit_df)

  return(checks_df)

}
