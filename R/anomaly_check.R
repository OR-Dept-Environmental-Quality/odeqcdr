#' Flag anomalies based on daily summary statistics that deviate from the typical range.
#'
#' A result is flagged as an anomaly (Anomaly = TRUE) when when there is a datetime shift or
#' the daily summary statistics derived from the results fall outside the monthly 10th or 90th percentile
#' of the daily mean, 10th percentile of the daily minimum, or the 90 percentile of the
#' daily maximum for streams of the same strahler stream order; or any stream size
#' if the strahler stream order is not known. Currently these checks are only relevant
#' for river temperature. If a result is flagged as an anomaly it should be closely reviewed.
#'
#' The 10th and 90th percentiles were calculated from all
#' available continuous temperature summary statistics in DEQ’s AWQMS database collected
#' between January 1, 1990 and December 31, 2019 with a "Final" or "Accepted" result status.
#'
#' The typical range for different characteristics are contained in odeqcdr::anomaly_stats
#' data by month and stream order.
#'
#' A datetine shift is evaluated by checking that the daily maximum water temperature
#' occurs between 13:00 and 19:00. If the daily maximum falls outside this hour range the function
#' flags all temperature results on that day as an anomaly where dt_shift=TRUE.
#' See [odeqcdr::dt_shift()] to run this check without all the other anomaly checks.
#' A datetime shift may indicate an issue with the time not being adjusted to the correct time zone
#' (i.e. still in UTC/GMT), a copy/paste/transcription error, or invalid results.
#'
#' Also calculated for each deployment is the absolute change from one result value to the next along the timseries. The change
#' is normalized per hour. The output value is saved to the column 'delta_per_hour'.
#'
#' @param results Data frame of the results data generated using [odeqcdr::contin_import()].
#' @param deployment Data frame of the deployment data generated using [odeqcdr::contin_import()].
#' @param return_df Boolean to indicate if the results data frame should be returned with each of the anomaly stats and final anomaly results. Default is FALSE.
#' @export
#' @return Vector of the anomaly result as TRUE or FALSE indexed in the same order as the result input. Or if return_df=TRUE a data frame.

anomaly_check <- function(results, deployment, return_df=FALSE) {

  #results=df5.results
  #deployment=df1.deployment

  anomaly_stats <- odeqcdr::anomaly_stats

  if(!"StreamOrder" %in% colnames(results)) {
    print("'StreamOrder' is not a column in results. Defaulting to anomaly detection based on all stream order sizes in Oregon. StreamOrder <-NA.")
    results$StreamOrder <- NA
  }

  # keep original col names
  result.cols <- names(results)

  results <- results %>%
    dplyr::mutate(date_char1=format(datetime, format="%Y-%m-%d"),
                  date_char2=format(datetime, format="%Y-%m-%d %Z"),
                  row.anom=dplyr::row_number())

  # Calculate the change in result value per hour. This is normalized to every 60 min regardless of sample frequency
  df.d_hour <- results %>%
    dplyr::group_by(Monitoring.Location.ID, Equipment.ID, Characteristic.Name) %>%
    dplyr::arrange(datetime) %>%
    dplyr::mutate(delta_per_hour=abs(Result.Value - dplyr::lag(Result.Value)) / abs(as.numeric(datetime - dplyr::lag(datetime), units="hours"))) %>%
    dplyr::ungroup() %>%
    dplyr::select(Monitoring.Location.ID, Equipment.ID, Characteristic.Name, delta_per_hour, row.anom) %>%
    dplyr::arrange(row.anom) %>%
    as.data.frame()

  df.dt_shift <- results %>%
    dplyr::filter(Characteristic.Name == "Temperature, water") %>%
    dplyr::group_by(Monitoring.Location.ID, Equipment.ID, Characteristic.Name, date_char1) %>%
    dplyr::slice(which.max(Result.Value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dt_shift=dplyr::if_else(lubridate::hour(datetime) %in% c(13:19), FALSE, TRUE)) %>%
    dplyr::select(Monitoring.Location.ID, Equipment.ID, Characteristic.Name, date_char1, dt_shift) %>%
    as.data.frame()

  df1.stats <- results %>%
    dplyr::group_by(Monitoring.Location.ID, Equipment.ID, Characteristic.Name, StreamOrder, date_char1) %>%
    dplyr::mutate(d_min=min(Result.Value, na.rm = TRUE),
                  d_max=max(Result.Value, na.rm = TRUE),
                  d_mean=mean(Result.Value, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(deployment) %>%
    as.data.frame()

  df.results.anom <- df1.stats %>%
    dplyr::mutate(deployed=dplyr::if_else(datetime >= Deployment.Start.Date &
                                            datetime <= Deployment.End.Date, TRUE, FALSE),
                  month=lubridate::month(datetime)
    ) %>%
    dplyr::left_join(anomaly_stats) %>%
    dplyr::left_join(df.dt_shift) %>%
    dplyr::left_join(df.d_hour) %>%
    dplyr::mutate(daily_max_q90=dplyr::if_else(d_max > q90_daily_max, TRUE, FALSE),
                  daily_min_q10=dplyr::if_else(d_min < q10_daily_min, TRUE, FALSE),
                  daily_mean_q10=dplyr::if_else(d_mean < q10_daily_mean, TRUE, FALSE),
                  daily_mean_q90=dplyr::if_else(d_mean > q90_daily_mean , TRUE, FALSE),
                  Anomaly=dplyr::case_when(dt_shift |
                                             d_max > q90_daily_max |
                                             d_min < q10_daily_min |
                                             d_mean < q10_daily_mean |
                                             d_mean > q90_daily_mean ~ TRUE,
                                           TRUE ~ FALSE),
                  # No Anomaly outside deployment period
                  Anomaly=dplyr::if_else(deployed, Anomaly, FALSE)
    ) %>%
    dplyr::arrange(row.anom) %>%
    dplyr::select(dplyr::any_of(result.cols), deployed, Anomaly, dt_shift, delta_per_hour, daily_min_q10, daily_max_q90, daily_mean_q10, daily_mean_q90) %>%
    as.data.frame()

  if(return_df) {
    return(df.results.anom)
  }

  return(df.results.anom$Anomaly)

}
