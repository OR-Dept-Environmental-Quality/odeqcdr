#' Check that date and time conform to changes between Daylight Time and Standard Time.
#'
#' This function will evaluate if an hour was added or subtracted correctly to account
#' for the switch from daylight time to standard time and vice versa. The check can only
#' be used on timeseries occurring on or after the start of standard time on October 27, 1985 at 2:01 AM.
#'
#' The dst check fails and time is adjusted when either of the two conditions occur:
#'
#' 1. When there are results recorded between 2:00 AM and 2:59 AM when daylight savings begins.
#'    As an example, in 2020 the switch from standard time to daylight time (+1 hour) occurred
#'    on March 8, 2020 at 2:00 AM. In a time series where samples are collected
#'    every 30 minutes there should be no results recorded from 2:00 AM until 2:59 AM.
#'    If there are results recorded during this period it's likely an adjustment
#'    to daylight savings time was not made.
#' 2. When there is only one timestamp recorded for any results between 1:00 AM and 1:59 AM
#'    after the transition back to standard time (-1 hour). As an example, in 2020 the switch from daylight savings
#'    back to standard time occurred on November 1, 2020 at 2:00 AM resulting in a repeat of time
#'    between 1:00 AM and 1:59 AM. If there are not duplicate time stamps recorded it is likely
#'    the adjustment to standard time was not made.
#'
#' When one of the conditions are detected, the time at the beginning
#' of the deployment period is considered the correct time and the appropriate UTC offset is applied
#' for daylight savings from that point forward. Use 'base_offset' if a specific
#' offset from UTC is desired instead. For example if the time stamp were all recorded using Pacific Standard Time the base_offset=-8. For
#' Pacific Daylight Time the base_offset=-7. The default is to use the offset at the time of deployment, base_offset=NULL.
#'
#'The dst check evaluates
#'each deployment \[Monitoring.Location.ID, Characteristic.Name, Equipment.ID\]
#'and any adjustments are made only to an individual deployment. A manual review of the results
#'is recommend.
#'
#' Daylight Savings in Oregon since 1986
#'
#' 1986 - 2006:
#'  Daylight savings time began by adding on hour (+1) on the first Sunday in April at 2:00 AM.
#'  Daylight savings time ended by subtracting an hour (-1) on the last Sunday in October at 2:00 AM.
#'
#' 2007 - Current:
#'  Daylight savings time begins by adding an hour (+1) on the second Sunday in March at 2:00 AM.
#'  Daylight savings time ends by subtracting an hour (-1) on the first Sunday in November at 2:00 AM.
#'
#' @param df Data frame to be evaluated.
#' @param mloc_col Column name in df holding the Monitoring Location ID. Default is "Monitoring.Location.ID".
#' @param char_col Column name in df holding the Characteristic Name. Default is "Characteristic.Name".
#' @param equip_col Column name in df holding the Equipment ID. Default is "Equipment.ID"
#' @param date_col Column name in df holding the POSIXct date values. Default is "Activity.Start.Date"
#' @param time_col Column name in df holding the POSIXct time values. Default is "Activity.Start.Time".
#' @param tz_col Column name in df holding the timezone string values. Default is tz_col ="Activity.Start.End.Time.Zone".
#' @param base_offset The numeric offset from UTC representing the "correct time". Using Pacific Standard Time the base_offset=-8. For
#' Pacific Daylight Time the base_offset=-7. The Default is NULL and the time at the offset at the beginning
#' of the deployment period is considered the correct time.
#' @param awqms_bug Due to a bug in AWQMS, results with the exact same Monitoring.Location.ID, Equipment.ID,
#' Characteristic.Name, Start.Date, Start.Time, and Result.Unit but different Time Zone values are not allowed, even if
#' they occur during the transition from daylight time to standard time. By setting awqms_bug=TRUE (the default) one second
#' is added to the repeat POSIXct time values that fall between 1:00 AM and 1:59 AM right after the switch to standard time.
#' This is here as a work around until the AWQMS bug is fixed.
#' @export
#' @return Vector of corrected datetime in POSISXct.

dst_check <- function(df, mloc_col="Monitoring.Location.ID", char_col="Characteristic.Name",
                      equip_col="Equipment.ID", date_col="Activity.Start.Date",
                      time_col="Activity.Start.Time", tz_col="Activity.Start.End.Time.Zone",
                      base_offset=NULL, awqms_bug=TRUE) {

  #- Test---

  # # From data
  # df_test <- df1.results

  # # correct
  # dt <- as.POSIXct(c("2020/03/08 0:30:00", "2020/03/08 1:00:00", "2020/03/08 1:30:00",
  #                    "2020/03/08 3:00:00", "2020/03/08 3:30:00", "2020/03/08 4:00:00",
  #                    "2020/11/01 0:30:00", "2020/11/01 1:00:00", "2020/11/01 1:30:00",
  #                    "2020/11/01 1:00:00", "2020/11/01 1:30:00", "2020/11/01 2:00:00",
  #                    "2020/11/01 2:30:00", "2020/11/01 3:00:00", "2020/11/01 3:30:00"),
  #                  format = "%Y/%m/%d %H:%M:%S", origin = "1970-01-01", tz ="UTC")
  #
  # # Always standard time =-8 or,
  # # Deploy starts in March (PST), Needs to add hour in March PST -> PDT
  # # Detected by findings NA
  # dt <- as.POSIXct(c("2020/03/08 0:30:00", "2020/03/08 1:00:00", "2020/03/08 1:30:00",
  #                    "2020/03/08 2:00:00", "2020/03/08 2:30:00", "2020/03/08 3:00:00",
  #                    "2020/10/31 23:30:00", "2020/11/01 0:00:00", "2020/11/01 0:30:00",
  #                    "2020/11/01 1:00:00", "2020/11/01 1:30:00", "2020/11/01 2:00:00",
  #                    "2020/11/01 2:30:00", "2020/11/01 3:00:00", "2020/11/01 3:30:00"),
  #                  format = "%Y/%m/%d %H:%M:%S", origin = "1970-01-01", tz ="UTC")
  # base_offset <- -8
  #
  #
  # # Deploy starts in March (PDT) Needs to subtract hour in November PDT -> PST
  # # Detected by lack of duplicates
  # dt <- as.POSIXct(c("2020/03/08 1:30:00", "2020/03/08 2:00:00", "2020/03/08 2:30:00",
  #                    "2020/03/08 3:00:00", "2020/03/08 3:30:00", "2020/03/08 4:00:00",
  #                    "2020/10/31 23:30:00", "2020/11/01 0:00:00", "2020/11/01 0:30:00",
  #                    "2020/11/01 1:00:00", "2020/11/01 1:30:00", "2020/11/01 2:00:00",
  #                    "2020/11/01 2:30:00", "2020/11/01 3:00:00", "2020/11/01 3:30:00"),
  #                   format = "%Y/%m/%d %H:%M:%S", origin = "1970-01-01", tz ="UTC")
  # base_offset <- NULL
  #
  # # Deploy starts in March (PDT) Hour was correctly subtracted in November PDT -> PST but timezone is wrong
  # # becuase of issue with R and UTC
  # df_test <- data.frame(Monitoring.Location.ID= rep("ORDEQ-12345",15),
  #                       Activity.Start.Date=dt,
  #                       Activity.Start.Time=dt,
  #                       Activity.Start.End.Time.Zone=c("PDT", "PDT", "PDT",
  #                                                      "PDT", "PDT", "PDT",
  #                                                      "PDT", "PDT", "PDT",
  #                                                      "PDT", "PDT", "PDT",
  #                                                      "PDT", "PDT", "PDT"),
  #                       Characteristic.Name=rep("Temperature, water", 15),
  #                       Equipment.ID=rep("abc", 15),
  #                       tz_name=rep('America/Los_Angeles', 15),
  #                       tz_utc=rep('UTC', 15), stringsAsFactors = FALSE)
  #
  #
  #
  # df_test <- data.frame(Monitoring.Location.ID= rep("ORDEQ-12345",15),
  #                       Activity.Start.Date=dt,
  #                       Activity.Start.Time=dt,
  #                       Activity.Start.End.Time.Zone=c("PST", "PST", "PST",
  #                                                      "PDT", "PDT", "PDT",
  #                                                      "PDT", "PDT", "PDT",
  #                                                      "PST", "PST", "PST",
  #                                                      "PST", "PST", "PST"),
  #                       Characteristic.Name=rep("Temperature, water", 15),
  #                       Equipment.ID=rep("abc", 15),
  #                       tz_name=rep('America/Los_Angeles', 15),
  #                       tz_utc=rep('UTC', 15), stringsAsFactors = FALSE)
  #
  # df_test$datetime <- odeqcdr::dt_combine(df=df_test,
  #                                         tz_col="tz_name")
  #
  # df_test$datetime_utc <- odeqcdr::dt_combine(df = df_test,
  #                                             tz_col="tz_utc")
  # df <- df_test
  #
  # date_col <- "Activity.Start.Date"
  # time_col <- "Activity.Start.Time"
  # tz_col <- "tz_name"
  # mloc_col <- "Monitoring.Location.ID"
  # char_col <- "Characteristic.Name"
  # equip_col <- "Equipment.ID"
  # base_offset <- NULL
  # awqms_bug <- TRUE

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

  df <- dplyr::mutate(df, row=dplyr::row_number())

  df <- df[,c(mloc_col, char_col, equip_col, date_col, time_col, tz_col, "row")]

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

  if(min(df$datetime_utc, na.rm = TRUE) < as.POSIXct("1985/10/27 02:01:00", tz="UTC")) {
    warning("df has datetimes before 2:01 AM on October 27, 1985. Only deployments that start on or after this date will be evaluated.")


  }

  if(length(df$datetime_tz)==0) {
    stop("datetime has length of zero. No Change")
  }

  if(length(unique(lubridate::dst(df$datetime_tz))) == 1) {

    print("No time change. Timeseries in df does not switch between standard time and daylight savings time.")

    return(df$datetime_tz)

  } else {

    df2 <- dplyr::select(df, -!!tz_col) %>%
      cbind(lutz::tz_offset(df$datetime_tz)) %>%
      dplyr::group_by_at(dplyr::all_of(c(mloc_col, char_col, equip_col))) %>%
      tidyr::fill(utc_offset_h, .direction="up") %>%
      tidyr::fill(is_dst, .direction="up") %>%
      dplyr::mutate(dst_eval=dplyr::if_else(min(datetime_utc, na.rm = TRUE) >= as.POSIXct("1985/10/27 02:01:00", tz="UTC"), TRUE, FALSE)) %>%
      dplyr::ungroup()

    # Data frame of dates when DST time shift occurred during the period of data. Add two days for buffer.
    df.dst <- data.frame(date=seq(lubridate::ymd(format(min(df2$datetime_utc - lubridate::ddays(2), na.rm = TRUE), "%Y-%m-%d")),
                                  lubridate::ymd(format(max(df2$datetime_utc + lubridate::ddays(2), na.rm = TRUE), "%Y-%m-%d")),
                                  by = '1 day')) %>%
      dplyr::mutate(year=lubridate::year(date),
                    month=lubridate::month(date),
                    wday=lubridate::wday(date, label=TRUE)) %>%
      dplyr::group_by(year, wday, month) %>%
      dplyr::mutate(dnum=dplyr::row_number()) %>%
      dplyr::mutate(shift=dplyr::case_when(year > 1985 & year < 2007 & month==4 & wday=="Sun" & dnum==1 ~ 1,
                                           year > 1985 & year < 2007 & month==10 & wday=="Sun" & dnum==max(dnum) ~ -1,
                                           year > 2006 & month==3 & wday=="Sun" & dnum==2 ~ 1,
                                           year > 2006 & month==11 & wday=="Sun" & dnum==1 ~ -1,
                                           TRUE ~ 0)) %>%
      dplyr::filter(shift %in% c(-1, 1)) %>%
      dplyr::mutate(start=if_else(shift==1,date+lubridate::hms("2:00:00"),date+lubridate::hms("1:00:00")),
                    stop=if_else(shift==1,date+lubridate::hms("2:59:59"),date+lubridate::hms("1:59:59"))) %>%
      dplyr::mutate(date=format(date, "%Y-%m-%d"))

    # Get the timezone at the start of the deployment
    df.start.tz <- df2 %>%
      dplyr::group_by_at(dplyr::all_of(c(mloc_col, char_col, equip_col))) %>%
      dplyr::filter(min(datetime_utc)==datetime_utc) %>%
      dplyr::ungroup() %>%
      dplyr::select(dplyr::all_of(c(mloc_col, char_col, equip_col)), deploy.start.tz=zone)

    # Find deployments where condition #1 is TRUE
    df.start.dst <- df2 %>%
      dplyr::mutate(date=format(datetime_utc, "%Y-%m-%d")) %>%
      dplyr::inner_join(df.dst) %>%
      dplyr::filter(datetime_utc >= start & datetime_utc <= stop & shift == 1 & dst_eval) %>%
      dplyr::group_by_at(dplyr::all_of(c(mloc_col, char_col, equip_col, "datetime_utc"))) %>%
      dplyr::summarise(n=dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(n > 0) %>%
      dplyr::select(-n, -datetime_utc) %>%
      dplyr::distinct() %>%
      dplyr::mutate(dst_fail=TRUE)

    # Find deployments where condition #2 is TRUE
    df.end.dst <- df2 %>%
      dplyr::mutate(date=format(datetime_utc, "%Y-%m-%d")) %>%
      dplyr::inner_join(df.dst) %>%
      dplyr::filter(datetime_utc >= start &  datetime_utc <= stop & shift == -1 & dst_eval) %>%
      dplyr::group_by_at(dplyr::all_of(c(mloc_col, char_col, equip_col, "datetime_utc"))) %>%
      dplyr::summarise(n=dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(n < 2) %>%
      dplyr::select(-n, -datetime_utc) %>%
      dplyr::distinct() %>%
      dplyr::mutate(dst_fail=TRUE)

    # During the transition from daylight time to standard there is always a repeat of time
    # between 1:00 AM and 1:59 AM. The first pass of time during this period should always be daylight
    # time and the second pass is standard time. Unfortunately when time is forced to UTC (as it is here) R
    # can't tell the first pass from the second and always assumes standard time.
    # This means the first set of timestamps have the wrong UTC offset and needs +1 hour added.
    # This bit of code finds these situations and corrects the UTC offset and the "is_dst" boolean.
    # The assumption with this code is that the timestamp that comes first in
    # the template is the first one chronologically and hence in daylight time.

    df3 <- df2 %>%
      dplyr::mutate(date_utc_str=format(datetime_utc, format = "%Y-%m-%d"),
                    time_utc_str=format(datetime_utc, format = "%H:%M:%S")) %>%
      dplyr::group_by(Monitoring.Location.ID, Equipment.ID, Characteristic.Name, date_utc_str, time_utc_str) %>%
      dplyr::arrange(Monitoring.Location.ID, Equipment.ID, Characteristic.Name, date_utc_str, time_utc_str, row) %>%
      dplyr::mutate(dst.pass=dplyr::row_number()) %>% # orders the duplicate results
      dplyr::ungroup()%>%
      dplyr::mutate(date=format(datetime_utc, "%Y-%m-%d")) %>%
      dplyr::left_join(df.dst) %>%
      dplyr::mutate(dst_pass1=datetime_utc >= start & datetime_utc <= stop & shift == -1 & !is_dst & dst.pass==1 & dplyr::lead(dst.pass)==2,
                    utc_offset_h=dplyr::case_when(dst_pass1 ~ utc_offset_h + 1,
                                                  TRUE ~ utc_offset_h),
                    is_dst=dplyr::if_else(dst_pass1, TRUE, is_dst)) %>%
      dplyr::select(dplyr::all_of(names(df2)), dst_pass1) %>%
      dplyr::arrange(row)

    df.fail <- rbind(df.start.dst, df.end.dst) %>%
      dplyr::distinct() %>%
      dplyr::left_join(df.start.tz) %>%
      as.data.frame()

    if(is.null(base_offset)) {

      # get offset at deployment
      df.deployoffset <- df3 %>%
        dplyr::group_by_at(dplyr::all_of(c(mloc_col, char_col, equip_col))) %>%
        dplyr::filter(min(datetime_utc)==datetime_utc) %>%
        dplyr::select(!!mloc_col, !!char_col, !!equip_col, deploy_offset=utc_offset_h)

      # convert to true UTC and re-adjust to local timezone
      df.fix <- df3 %>%
        dplyr::left_join(df.deployoffset) %>%
        dplyr::left_join(df.fail) %>%
        dplyr::mutate(dst_fail=dplyr::if_else(is.na(dst_fail), FALSE, dst_fail),
                      datetime_utc_fix=dplyr::if_else(dst_fail, datetime_utc+(-1*lubridate::dhours(deploy_offset)), datetime_utc+(-1*lubridate::dhours(utc_offset_h)))) %>%
        dplyr::group_by(tz_name) %>%
        dplyr::mutate(datetime_tz_fix=lubridate::with_tz(time=datetime_utc_fix, tzone=unique(tz_name))) %>%
        dplyr::arrange(row)

    } else {

      # Use the set UTC offset, convert to true UTC and re-adjust to local timezone
      df.fix <- df3 %>%
        dplyr::mutate(deploy_offset=base_offset) %>%
        dplyr::left_join(df.fail) %>%
        dplyr::mutate(datetime_utc_fix=dplyr::if_else(dst_fail, datetime_utc+(-1*lubridate::dhours(deploy_offset)), datetime_utc)) %>%
        dplyr::group_by(tz_name) %>%
        dplyr::mutate(datetime_tz_fix=lubridate::with_tz(time=datetime_utc_fix, tzone=unique(tz_name))) %>%
        dplyr::arrange(row)

    }

    if(any(df.fix$dst_fail)) {
      print("Time change correction made for the following deployments:")
      print(df.fail)

    }

    if(awqms_bug) {

      # Deal with AWQMS bug
      df.fix <- df.fix %>%
        dplyr::mutate(date_str=format(datetime_tz_fix, format = "%Y-%m-%d"),
                      time_str=format(datetime_tz_fix, format = "%H:%M:%S")) %>%
        dplyr::group_by(Monitoring.Location.ID, Equipment.ID, Characteristic.Name, date_str, time_str) %>%
        dplyr::arrange(Monitoring.Location.ID, Equipment.ID, Characteristic.Name, date_str, time_str, row) %>%
        dplyr::mutate(dst.pass=dplyr::n()) %>% # of duplicates
        dplyr::ungroup()%>%
        dplyr::mutate(date=format(datetime_tz_fix, "%Y-%m-%d")) %>%
        dplyr::left_join(df.dst) %>%
        dplyr::mutate(is_dst=lubridate::dst(datetime_tz_fix),
                      dst_pass2=lubridate::hour(datetime_tz_fix) == 1 & shift == -1 & !is_dst & dst.pass==2,
                      datetime_tz_fix=dplyr::if_else(dst_pass2, datetime_tz_fix + lubridate::dseconds(1), datetime_tz_fix)) %>%
        dplyr::arrange(row)

      if(any(df.fix$dst_pass2)) {

        df.awqms_bug <- df.fix %>%
          dplyr::filter(dst_pass2) %>%
          dplyr::mutate(datetime=format(datetime_tz_fix, format = "%Y-%m-%d %H:%M:%S %Z")) %>%
          dplyr::select(Monitoring.Location.ID, Equipment.ID, Characteristic.Name, datetime, row) %>%
          as.data.frame()

        warning(message("AWQMS bug bites again! One second added to the following results:\n\n",
                        paste0(capture.output(df.awqms_bug), collapse = "\n")))
      }

    }

    return(df.fix$datetime_tz_fix)

  }

}
