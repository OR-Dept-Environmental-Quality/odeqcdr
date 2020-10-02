#' Combine PoSIXct date, time, and timezone columns into a single POSIXct datetime.
#'
#' This function will combine POSIXct date and time values pulled from an excel using odeqcdr::import_contin() to make a POSIXct datetime.
#' There are various options on how to handle the time and timezone column. If the datetime falls in a daylight savings break NA is returned.
#'
#' @param df Data frame to be evaluated.
#' @param date_col Column name in df holding the POSIXct date values. Default is "Activity.Start.Date"
#' @param time_col Column name in df holding the POSIXct time values. Default is "Activity.Start.Time". If NULL uses date_col or time_val if provided.
#' @param time_val string in format 'hh:mm:ss' representing the time to use for all rows. Default is NULL. If NULL uses date_col or time_col if provided.
#' @param tz_col Column name in df holding the timezone string values. Acceptable values
#' include "PST", "PDT", "MST", "MDT" or Olson Names such as 'America/Los_Angeles' for Pacific Time
#' zone or 'America/Boise' for Mountain time zone.
#' See OlsonNames() for more info. Default is tz_col="Activity.Start.End.Time.Zone".
#' @export
#' @return vector of POSIXct datetime

dt_combine <- function(df, date_col="Activity.Start.Date", time_col="Activity.Start.Time",
                       time_val=NULL, tz_col="Activity.Start.End.Time.Zone") {


  #- Test---
  # df_test <- data.frame(Activity.Start.Date=lubridate::ymd(rep("2020/03/08", 5),  tz = "UTC"),
  #                       Activity.Start.Time=as.POSIXct(c("1:00:00", "1:30:00", "3:00:00", "3:30:00", "4:00:00"),
  #                                                       format = "%H:%M:%S", origin = "1970-01-01", tz ="UTC"),
  #                       Activity.Start.End.Time.Zone=c("PST", "PST", "PDT", "PDT", "PDT"),
  #                       tz_name=rep('America/Los_Angeles', 5))
  #
  # # Test for when there is break in DST
  # df_test <- data.frame(Activity.Start.Date=lubridate::ymd(rep("2020/03/08", 5),  tz = "UTC"),
  #                       Activity.Start.Time=as.POSIXct(c("1:00:00", "1:30:00", "2:00:00", "2:30:00", "3:00:00"),
  #                                                      format = "%H:%M:%S", origin = "1970-01-01", tz ="UTC"),
  #                       Activity.Start.End.Time.Zone=c("PST", "PST", "PDT", "PDT", "PDT"),
  #                       tz_name=rep('America/Los_Angeles', 5))
  #
  # df <- df_test
  # date_col="Activity.Start.Date"
  # time_col="Activity.Start.Time"
  # time_val=NULL
  # tz_col="Activity.Start.End.Time.Zone"

  #----

  if(nrow(df)==0) {
    warning("df has no rows")
    return(as.POSIXct(numeric(0)))
  }

  # quick col checks
  if(!date_col %in% names(df)) {
    stop(print(paste("date_col",date_col,"is not a column in df.")))
  }

  if(!tz_col %in% names(df)) {
    stop(print(paste("tz_col",tz_col,"is not a column in df.")))
  }

  df$date_char <- strftime(df[,c(date_col)], format = "%Y-%m-%d", origin = "1970-01-01", tz = "UTC")

  if(is.null(time_col) & is.null(time_val)) {
    # use date_col

    print("time_col and time_val are NULL, defaulting to date_col for time.")
    df$time_char <- strftime(df[, c(date_col)], format = "%H:%M:%S", tz ="UTC")

  } else {

    if(!is.null(time_val)) {
      # use string time
      df$time_char <- time_val
    } else {
      # use time col
      df$time_char <- strftime(df[, c(time_col)], format = "%H:%M:%S", tz ="UTC")
    }
  }

  df_datetime <- df %>%
    dplyr::select(date_char, time_char, tz_vector=!!tz_col) %>%
    dplyr::mutate(tz_vector=dplyr::case_when(tz_vector %in% c("PST", "PDT") ~ "America/Los_Angeles",
                                             tz_vector %in% c("MST", "MDT") ~ "America/Boise",
                                      TRUE ~ tz_vector),
                  datetime_char=paste(date_char, time_char),
                  datetime_utc=lubridate::ymd_hms(datetime_char, tz="UTC")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(datetime_tz=lubridate::force_tz(datetime_utc, tzone=tz_vector))

  return(df_datetime$datetime_tz)

}
