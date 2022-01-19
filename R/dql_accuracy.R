#' Assign accuracy Data Quality Levels (DQLs) to continuous data
#'
#' This function assigns an accuracy data quality level to parameters measured in the field using pre- and post-deployment checks.
#' The data quality levels are assigned following DEQ’s Data Quality Matrix (DEQ, 2013).
#' The data quality matrix defines the accuracy and precision criteria for equipment calibration and field audits respectively respectively.
#'
#' use \code{\link{dql_precision}} to assign the data quality level using field duplicates, field audits, or split samples.
#'
#' Inputs into this function must retrieved from Oregon DEQ's continuous data submission template xlsx file v2.03.
#' Use \code{\link{contin_import}} to get the list that hold the data frames used for this function's inputs.
#'
#' Oregon Department of Environmental Quality (DEQ). 2013. "Data validation criteria for water quality parameters measured in the field.
#' DEQ04-LAB-0003-QAG Version5.0." \url{http://www.oregon.gov/deq/FilterDocs/DataQualMatrix.pdf}
#'
#' @param prepost Data frame of the prepost data generated using \code{\link{contin_import}}.
#' @param results Data frame of the results data generated using \code{\link{contin_import}}.
#' @param prepost_only Boolean to indicate if the prepost data frame should be returned with a new columns for the accuracy DQL,
#' and absolute difference between the result and references values. Default is FALSE.
#' @export
#' @return Vector of the precision DQL indexed in the same order as the result input. Or if audit_only=TRUE a data frame.

dql_accuracy <- function(prepost, results, prepost_only=FALSE) {

  #results=df3.results
  #prepost=df3.prepost

  conqc <- odeqcdr::conqc

  if(nrow(prepost)==0 & !prepost_only) {
    warning("There are no prepost data, DQL_acc = E")
    results$DQL_acc <- "E"

    return(results$DQL_acc)
  }

  if(nrow(prepost)==0 & prepost_only) {
    warning("There are no prepost data")
  }


  prepost.cols <- names(prepost)

  df.prepost.dql <- prepost %>%
    dplyr::left_join(conqc, by="Characteristic.Name") %>%
    dplyr::mutate(diff.d=abs(Equipment.Result.Value - Reference.Result.Value),
                  DQL_acc=dplyr::case_when(diff.d < acc_A ~ "A",
                                           diff.d < acc_B ~ "B",
                                           diff.d >= acc_B ~ "C",
                                           TRUE ~ "E")
    ) %>%
    dplyr::select(dplyr::any_of(prepost.cols), diff.d, DQL_acc) %>%
    as.data.frame()

  if(prepost_only) {
    return(df.prepost.dql)
  }


  df.results.grade <- df.prepost.dql %>%
    dplyr::select(Equipment.ID, Characteristic.Name, DQL_acc) %>%
    dplyr::group_by(Equipment.ID, Characteristic.Name) %>%
    dplyr::add_tally() %>%
    # if there is only one ccv measure it get B at best, or if DQL_acc < B then DQL_acc
    dplyr::mutate(DQL_acc=dplyr::case_when(n==1 & DQL_acc == 'A' ~ 'B',
                                           TRUE ~ DQL_acc)) %>%
    # Take lowest DQL for multiple checks
    dplyr::summarize(DQL_acc=max(DQL_acc, na.rm = TRUE), .groups="keep") %>%
    dplyr::ungroup()

  df.results.grade.results <- results %>%
    dplyr::left_join(df.results.grade, by = c("Equipment.ID", "Characteristic.Name")) %>%
    dplyr::mutate(DQL_acc=dplyr::case_when(is.na(DQL_acc) ~ 'E',
                                           TRUE ~ DQL_acc)) %>%
    as.data.frame()

  return(df.results.grade.results$DQL_acc)

}
