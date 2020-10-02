#' Oregon Water Quality Anomaly Stats
#'
#'Summary statistics for various water quality parameters that help identify
#'potential anomalous results. Any result flagged as an anomaly should be closely
#'reviewed. For stream temperature the status represents the 10th or 90th percentile
#'of the daily mean, 10th percentile of the daily minimum,
#'or the 90 percentile of the daily maximum stratified by month and strahler stream order;
#'and all stream sizes if StreamOrder=NA. The 10th and 90th 'percentiles were calculated from all
#'available continuous temperature summary 'statistics in DEQ’s AWQMS database.
#'The data period is from January 1, 1990 to  December 31, 2019. Data used had
#'a "Final" or "Accepted" result status.
#'
#' \itemize{
#'   \item Characteristic.Name:	Parameter name.
#'   \item month: Month applicable to summary stat. NA is all months.
#'   \item StreamOrder: Strahler Stream order as defined in NHD.
#'   \item q10_daily_diel: 10th percentile of the daily diel.
#'   \item q10_daily_max: 10th percentile of the daily maximums.
#'   \item q10_daily_mean: 10th percentile of the daily means.
#'   \item q10_daily_min: 10th percentile of the daily minimums.
#'   \item q90_daily_diel: 90th percentile of the daily diel.
#'   \item q90_daily_max: 90th percentile of the daily maximums.
#'   \item q90_daily_mean: 90th percentile of the daily means.
#'   \item q90_daily_min: 90th percentile of the daily minimums.
#'   \item daily_diel:
#'   \item daily_max:
#'   \item daily_mean_low:
#'   \item daily_mean_high:
#'   \item Result.Unit: Result unit
#'   \item cnCharID:
#' }
#'
#' @docType data
#' @usage data(anomaly_stats)
#' @keywords datasets
#' @examples
#' anomaly_stats <- odeqcdr::anomaly_stats
#'

"anomaly_stats"
