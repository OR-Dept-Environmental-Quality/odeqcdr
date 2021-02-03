#' Assign precision Data Quality Levels (DQLs) to continuous data
#'
#' This function assigns a precision data quality level to parameters measured in the field using replicate measurements
#' such as field duplicates, field audits, or split samples. The data quality levels are assigned following DEQ’s Data Quality Matrix (DEQ, 2013).
#' The data quality matrix defines the accuracy and precision criteria for equipment calibration and field audits respectively respectively.
#'
#' use [odeqcdr::dql_accuracy()] to assign the data quality level using calibration verifications and/or pre- and post-deployment checks.
#'
#' Inputs into this function must retrieved from Oregon DEQ's continuous data submission template xlsx file v2.03.
#' Use [odeqcdr::contin_import()] to get the list that hold the data frames used for this function's inputs.
#'
#' Oregon Department of Environmental Quality (DEQ). 2013. "Data validation criteria for water quality parameters measured in the field.
#' DEQ04-LAB-0003-QAG Version5.0." http://www.oregon.gov/deq/FilterDocs/DataQualMatrix.pdf
#'
#' @param audits Data frame of the audit data generated using [odeqcdr::contin_import()].
#' @param results Data frame of the results data generated using [odeqcdr::contin_import()].
#' @param deployment Data frame of the deployment data generated using [odeqcdr::contin_import()].
#' @param audits_only Boolean to indicate if the audit data frame should be returned with new columns for the audit sample DQL,
#' corresponding result value, result units, absolute difference between the result and audit. Default is FALSE.
#' @export
#' @return Vector of the precision DQL indexed in the same order as the result input. Or if audit_only=TRUE the audit data frame with the precision DQL for each audit.

dql_precision <- function(audits, results, deployment, audits_only=FALSE) {

  #results=df3.results
  #audits=df3.audits
  #deployment=df1.deployment

  if(nrow(audits)==0 & !audits_only) {
    print("There are no audit data, precDQL = E")
    results$precDQL <- "E"

    return(results$precDQL)
  }

  if(nrow(audits)==0 & audits_only) {
    warning("There are no audit data")
  }

  audit.cols <- names(audits)

  conqc <- odeqcdr::conqc

  # join results and deployments
  df.results <- results %>%
    dplyr::left_join(deployment, by=c("Monitoring.Location.ID", "Equipment.ID",
                                         "Characteristic.Name")) %>%
    dplyr::mutate(deployed=dplyr::if_else(datetime >= Deployment.Start.Date &
                                            datetime <= Deployment.End.Date, TRUE, FALSE))

  df.audits.grab <- audits %>%
    dplyr::select(audit.datetime.start, Result.Status.ID, Project.ID, Monitoring.Location.ID,
                  Characteristic.Name, Equipment.ID, Audit.Result.Value=Result.Value,
                  Audit.Result.Status.ID=Result.Status.ID, row.audits)

  # Grade the grab sample Audits
  df.audits.grab.dql <- results %>%
    dplyr::select(-Activity.Start.Date, -Activity.Start.Time, -Activity.Start.End.Time.Zone) %>%
    dplyr::full_join(df.audits.grab, by=c("Monitoring.Location.ID", "Characteristic.Name", "Equipment.ID")) %>%
    dplyr::group_by(Monitoring.Location.ID, Characteristic.Name, Equipment.ID, row.audits) %>%
    dplyr::slice(which.min(abs(datetime - audit.datetime.start))) %>%
    dplyr::mutate(diff.minutes=abs(as.numeric(difftime(datetime, audit.datetime.start, units="mins")))) %>%
    dplyr::left_join(conqc, by=c("Characteristic.Name", "Result.Unit")) %>%
    dplyr::mutate(diff.Result=abs(Result.Value - Audit.Result.Value),
                  DQL_prec=dplyr::case_when(diff.Result < prec_A & diff.minutes <=30 ~ "A",
                                            diff.Result < prec_B & diff.minutes <=30  ~ "B",
                                            diff.Result >= prec_B & diff.minutes <=30 ~ "C",
                                            diff.minutes >30 ~ "E",
                                            TRUE ~ "E")
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(rDQL=DQL_prec) %>%
    dplyr::select(datetime, dplyr::any_of(audit.cols), diff.minutes, Audit.Result.Value, diff.Result,
                  DQL_prec, rDQL, Audit.Result.Status.ID, row.audits) %>%
    dplyr::arrange(row.audits)
    as.data.frame()

  if(audits_only) {

    df.audits.grab.dql <- dplyr::rename(df.audits.grab.dql, precDQL=DQL_prec)

    return(df.audits.grab.dql)
  }

  audits_deploy <- df.audits.grab.dql %>%
    dplyr::filter(!Audit.Result.Status.ID=="Rejected") %>%
    dplyr::mutate(audits_deploy = paste0("[",Monitoring.Location.ID," - ",Equipment.ID," - ",Characteristic.Name,"]")) %>%
    dplyr::pull(audits_deploy) %>%
    unique()

  df.results.grade <- df.audits.grab.dql %>%
    dplyr::filter(!Audit.Result.Status.ID=="Rejected") %>%
    dplyr::select(Monitoring.Location.ID, Equipment.ID, Characteristic.Name,
                  Result.Value, Result.Unit, datetime, DQL_prec) %>%
    dplyr::arrange(Monitoring.Location.ID, Equipment.ID, Characteristic.Name, datetime) %>%
    dplyr::group_by(Monitoring.Location.ID, Equipment.ID, Characteristic.Name) %>%
    # choose lower grade in order of index
    dplyr::mutate(DQL_prec2=pmax(DQL_prec, dplyr::lead(DQL_prec, n=1), na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::right_join(df.results, by = c("Monitoring.Location.ID", "Equipment.ID",
                                         "Characteristic.Name", "Result.Value",
                                         "Result.Unit", "datetime")) %>%
    # Keep original calculated audit grade for datetime of audit. The rest will be filled in from lower grade in DQL_prec2
    dplyr::mutate(results_deploy=paste0("[",Monitoring.Location.ID," - ",Equipment.ID," - ", Characteristic.Name,"]"),
                  precDQL=DQL_prec) %>%
    dplyr::group_by(Monitoring.Location.ID, Equipment.ID, Characteristic.Name) %>%
    dplyr::arrange(Monitoring.Location.ID, Equipment.ID, Characteristic.Name, datetime) %>%
    tidyr::fill(DQL_prec2, .direction = "downup") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(precDQL=dplyr::if_else(is.na(precDQL), DQL_prec2, precDQL)) %>%
    # set precDQL=E outside of deployment period and for any deployments without audits
    dplyr::mutate(precDQL=dplyr::if_else(deployed, precDQL, "E"),
                  precDQL=dplyr::case_when(results_deploy %in% audits_deploy ~ precDQL,
                                   TRUE ~ "E")) %>%
    dplyr::group_by(row.results) %>%
    # if there are two audits for the same sample take the lowest precDQL
    dplyr::mutate(precDQL=max(precDQL, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(Monitoring.Location.ID, Equipment.ID, Characteristic.Name, row.results, precDQL) %>%
    dplyr::arrange(row.results) %>%
    as.data.frame()

  return(df.results.grade$precDQL)

}
