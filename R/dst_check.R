#' Check that date and time conform to changes between Daylight Time and Standard Time.
#'
#' This function will evaluate if an hour was added or subtracted correctly to account
#' for the switch from daylight time to standard time and vice versa.
#' For example in 2020, the switch from standard time to daylight time occurred
#' on March 8, 2020 at 2:00 AM. In a time series where samples are collected
#' every 30 minutes there should be no results recorded from 2:00 AM until 2:59 AM
#' on March 8. If there are results recorded during this period it’s likely an adjustment
#' for daylight savings was not made. This function attempts to find these occurrences and
#' adds or subtracts an hour to all datetime values during the entire daylight savings
#' or standard time period. A manual review of the results is recommend.
#'
#' @param df Data frame to be evaluated.
#' @param mloc_col Column name in df holding the Monitoring Location ID. Default is "Monitoring.Location.ID".
#' @param char_col Column name in df holding the Characteristic Name. Default is "Characteristic.Name".
#' @param equip_col Column name in df holding the Equipment ID. Default is "Equipment.ID"
#' @param date_col Column name in df holding the POSIXct date values. Default is "Activity.Start.Date"
#' @param time_col Column name in df holding the POSIXct time values. Default is "Activity.Start.Time".
#' @param tz_col Column name in df holding the timezone string values. Default is tz_col ="Activity.Start.End.Time.Zone".
#' @export
#' @return Vector of corrected datetime in POSISXct.

dst_check <- function(df, mloc_col="Monitoring.Location.ID", char_col="Characteristic.Name", equip_col="Equipment.ID", date_col="Activity.Start.Date", time_col="Activity.Start.Time", tz_col="Activity.Start.End.Time.Zone") {

  #- Test---
  # df_test <- data.frame(Monitoring.Location.ID= rep("ORDEQ-12345",5),
  #                       Activity.Start.Date=lubridate::ymd(rep("2020/03/08", 5),  tz = "UTC"),
  #                       Activity.Start.Time=as.POSIXct(c("1:00:00", "1:30:00", "2:00:00", "2:30:00", "3:00:00"),
  #                                                      format = "%H:%M:%S", origin = "1970-01-01", tz ="UTC"),
  #                       Activity.Start.End.Time.Zone=c("PST", "PST", "PDT", "PDT", "PDT"),
  #                       Characteristic.Name=rep("Temperature, water", 5),
  #                       Equipment.ID=rep("abc", 5),
  #                       tz_name=rep('America/Los_Angeles', 5))
  #
  # df_test$datetime <- datetime_make(date_col = df_test$Activity.Start.Date,
  #                                   time_col = df_test$Activity.Start.Time,
  #                                   tz_col=df_test$tz_name)
  #
  # df_test$datetime_utc <- datetime_make(date_col = df_test$Activity.Start.Date,
  #                                   time_col = df_test$Activity.Start.Time,
  #                                   tz_col="UTC")
  # df <- df_test
  #
  # date_col <- "Activity.Start.Date"
  # time_col <- "Activity.Start.Time"
  # tz_col <- "tz_name"
  # mloc_col <- "Monitoring.Location.ID"
  # char_col <- "Characteristic.Name"
  # equip_col <- "Equipment.ID"

  #----

  if(nrow(df)==0) {
    warning("df has no rows")
    return(as.POSIXct(numeric(0)))
  }

  # quick col checks
  if(!mloc_col %in% names(df)) {
    stop(print(paste("mloc_col",mloc_col,"is not a column in df.")))

  }

  if(!char_col %in% names(df)) {
    stop(print(paste("char_col",char_col,"is not a column in df.")))
  }

   if(!equip_col %in% names(df)) {
     stop(print(paste("equip_col",equip_col,"is not a column in df.")))
   }

  if(!date_col %in% names(df)) {
    stop(print(paste("date_col",date_col,"is not a column in df.")))
  }

  if(!time_col %in% names(df)) {
    stop(print(paste("time_col",time_col,"is not a column in df.")))
  }

  if(!tz_col %in% names(df)) {
    stop(print(paste("tz_col",tz_col,"is not a column in df.")))
  }

  df <- df[,c(mloc_col, char_col, equip_col, date_col, time_col, tz_col)]

  df$tz_utc <- "UTC"

  df$datetime_tz <- odeqcdr::dt_combine(df=df,
                                        date_col = date_col,
                                        time_col = time_col,
                                        time_val = NULL,
                                        tz_col = tz_col)

  df$datetime_utc <- odeqcdr::dt_combine(df=df,
                                         date_col = date_col,
                                         time_col = time_col,
                                         time_val = NULL,
                                         tz_col ="tz_utc")

  if(length(df$datetime_tz)==0) {
    stop("datetime has length of zero. No Change")
  }

  if(length(unique(lubridate::dst(df$datetime_tz))) == 1) {

    print("No Time Change")

    return(df$datetime_tz)

    } else {

      df2 <- select(df, -!!tz_col) %>%
        cbind(lutz::tz_offset(df$datetime_tz)) %>%
        dplyr::group_by_at(dplyr::vars(mloc_col, char_col, equip_col)) %>%
        tidyr::fill(utc_offset_h, .direction="up") %>%
        tidyr::fill(is_dst, .direction="up") %>%
        dplyr::ungroup()

      df3 <- df2 %>%
        dplyr::filter(is.na(datetime_tz)) %>%
        dplyr::select(!!mloc_col, !!char_col, !!equip_col, is_dst) %>%
        dplyr::distinct() %>%
        dplyr::mutate(hours_added=dplyr::if_else(is_dst,1,-1))

      df4 <- df2 %>%
        dplyr::left_join(df3) %>%
        dplyr::mutate(hours_added=tidyr::replace_na(hours_added, 0),
                      datetime_utc_fix=datetime_utc+lubridate::dhours(hours_added))

      if(any(is.na(df2$datetime_tz))) {
        print("DST break. Time change correction made for the following stations and periods:")

        df5 <- df4 %>%
          dplyr::filter(is.na(datetime_tz)) %>%
          dplyr::select(!!mloc_col, !!char_col, !!equip_col, datetime_utc, is_dst) %>%
          dplyr::mutate(hours_added=dplyr::if_else(is_dst,1,-1)) %>%
          dplyr::group_by_at(dplyr::vars(mloc_col, char_col, equip_col, hours_added, is_dst)) %>%
          dplyr::summarize(start_datetime_fix=min(datetime_utc),
                           end_datetime_fix=max(datetime_utc),
                           n_fixed=n()) %>%
          as.data.frame()
        print(df5)
      }

      return(df4$datetime_utc_fix)

  }


}

