#' Check for datetime shifts (Temperature only)
#'
#' A datetine shift is evaluated by checking that the daily maximum water temperature
#' occurs between 13:00 and 19:00. If the daily maximum falls outside this hour range the function
#' flags all temperature results on that day as an anomaly where dt_shift=TRUE. A datetime shift may
#' indicate an issue with the time not being adjusted to the correct time zone
#' (i.e. still in UTC/GMT), a copy/paste/transcription error, or invalid results. results dataframe must
#' have a column called 'datetime' formatted as POSIXct.
#'
#' @param results Data frame of the results data generated using [odeqcdr::import_contin()].
#' @param return_df TRUE means the results data frame will be returned with the dt_shift column. FALSE only returns the dt_shift vector. Default is FALSE.
#' @export
#' @return data frame.

dt_shift <- function(results, return_df=FALSE) {

  # test
  # results <- df1.results

  result.cols <- names(results)

  results.date <- results %>%
    dplyr::mutate(date=as.Date(datetime))

  df.dt_shift <- results.date %>%
    dplyr::filter(Characteristic.Name == "Temperature, water") %>%
    dplyr::group_by(Monitoring.Location.ID, Equipment.ID, Characteristic.Name, date) %>%
    dplyr::slice(which.max(Result.Value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dt_shift=dplyr::if_else(lubridate::hour(datetime) %in% c(13:19), FALSE, TRUE)) %>%
    dplyr::select(Monitoring.Location.ID, Equipment.ID, Characteristic.Name, date, dt_shift) %>%
    dplyr::right_join(results.date) %>%
    dplyr::select(dplyr::any_of(result.cols), dt_shift) %>%
    as.data.frame()

  if(return_df) {
    return(df.dt_shift)
  } else {
    return(df.dt_shift$dt_shift)
  }

}
