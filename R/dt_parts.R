#' Pull POSIXct datetime back into date, time, and time zone columns. Add comments if there are any differences.
#'
#' This function will take a datetime POSIXct vector and save the date, time, and
#' timezone elements into their respective destination columns. This is intended to apply any
#' corrections from datetime back to the original date, time, and timezone columns.
#'
#' The destination date_col and time_col are formatted as POSIXct values and are equal datetime_col.
#' tz_col is formatted as a string. e.g. format(df$datetime_col, format="%Z").
#'
#' A comparison is made between the value derived from datetime_col to the
#' value in the destination column. If the values are different a comment is
#' recorded in comment_col. Any existing comments are retained and new comments appended.
#'
#' There are three different comments for three different situations:
#' when only the timezone changes, when only  the date/time changes, and when both
#' the timezone and the date/time changes. A date/time change is usually due to adjustment for DST.
#'
#' @param df Data frame to be evaluated.
#' @param datetime_col Column name in df holding the POSIXct datetime values. Default is "datetime"
#' @param date_col Destination column name in df holding the POSIXct date values. Default is "Activity.Start.Date"
#' @param time_col Destination column name in df holding the POSIXct time values. Default is "Activity.Start.Time".
#' @param tz_col Destination column name in df holding the timezone string values. Default is "Activity.Start.End.Time.Zone".
#' @param comment_col Column name in df holding the comment column. Default is "Datetime.Comment".
#' @param tz_comment Comment in string format to add to comment_col if the timezone changed. Default is "corrected timezone".
#' @param dst_comment Comment in string format to add to comment_col if their was a time change. Default is "corrected for DST".
#' @param tzdst_comment Comment in string format to add to comment_col if their was a timezone and time change. Default is "corrected timezone and for DST".
#' @export
#' @return returned data frame.

dt_parts <- function(df, datetime_col="datetime", date_col="Activity.Start.Date",
                     time_col="Activity.Start.Time",  tz_col="Activity.Start.End.Time.Zone",
                     comment_col="Datetime.Comment",
                     tz_comment="corrected timezone",
                     dst_comment="corrected for DST",
                     tzdst_comment="corrected timezone and for DST") {

  #- Test---
  # df_fix <- data.frame(Activity.Start.Date=lubridate::ymd(rep("2020/03/08", 5),  tz = "UTC"),
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
  #                       tz_name=rep('America/Los_Angeles', 5),
  #                       Result.Comment=rep(NA, 5))
  # df_test$datetime <- odeqcdr::dt_combine(df=df_fix, tz_col="tz_name")
  #
  # df <- df_test
  # datetime_col="datetime"
  # date_col="Activity.Start.Date"
  # time_col="Activity.Start.Time"
  # tz_col="Activity.Start.End.Time.Zone"
  # comment_col="Result.Comment"
  # tz_comment="corrected timezone"
  # dst_comment="corrected for DST"
  # tzdst_comment="corrected timezone and for DST"

  #----

  if(nrow(df)==0) {
    warning("df has no rows")
    return(df)
  }

  # quick col checks
  if(!date_col %in% names(df)) {
    stop(print(paste("date_col",date_col,"is not a column in df.")))
  }

  if(!time_col %in% names(df)) {
    stop(print(paste("time_col",time_col,"is not a column in df.")))
  }

  if(!tz_col %in% names(df)) {
    stop(print(paste("tz_col",tz_col,"is not a column in df.")))
  }

  if(!comment_col %in% names(df)) {
    df[,c(comment_col)] <- NA
  }

  # Add temporary fields
  df$datetime_orig <- odeqcdr::dt_combine(df=df, date_col=date_col,
                                           time_col=time_col, tz_col=tz_col)
  df$tz_new <- format(df[,c(datetime_col)], format="%Z")
  df$tz_old <- df[, c(tz_col)]
  df$diff_h <- as.numeric(difftime(time1=df$datetime_orig,
                              time2=df[,c(datetime_col)], units = "hours"))
  df$comment_orig <- df[,c(comment_col)]
  df$comment_orig[is.na(df$comment_orig)] <- ""

  # pull datetime into date, time, and tz destination columns
  df[,c(date_col)] <- df[,c(datetime_col)]
  df[,c(time_col)] <- df[,c(datetime_col)]
  df[,c(tz_col)] <- df$tz_new

  # Add comments
  df <- df %>%
    dplyr::mutate(!!comment_col:=dplyr::case_when(tz_new!=tz_old & (diff_h!=0 | is.na(diff_h)) ~paste(comment_orig, tzdst_comment),
                                                  tz_new!=tz_old & diff_h==0 ~paste(comment_orig, tz_comment),
                                                  tz_new==tz_old & (diff_h!=0 | is.na(diff_h)) ~paste(comment_orig, dst_comment),
                                                  TRUE ~ comment_orig))


  # put "" comments back to NA
  df[,c(comment_col)][df[,c(comment_col)]==""] <- NA

  # remove temporary columns
  df <- dplyr::select(df, -datetime_orig, -tz_new, -tz_old, -diff_h, -comment_orig)

  return(df)
}
