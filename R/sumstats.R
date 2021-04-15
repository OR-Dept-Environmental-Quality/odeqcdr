#' Generate AWQMS Summary Statistics
#'
#' Calculates summary statistics and DQLs. Returns a data frame formatted for AWQMS upload.
#'
#' Summary statistics calculated include:
#'
#' * Temperature, water: Daily Maximum, Daily Minimum, Daily Mean, and 7-day mean of the daily maximums (7DMADMax);
#' * Dissolved oxygen (DO): Daily Maximum, Daily Minimum, Daily Mean, 7-day mean of the daily minimums (7DMADMin), 7-day mean of the daily mean (7DMADMean), and 30-day mean of the daily mean (30DMADMean);
#' * All others: Daily Maximum, Daily Minimum, and Daily Mean
#'
#' Rolling means are trailing means reported on the last day of the 7-day or 30-day period.
#'
#' @param results Data frame of the results data generated using [odeqcdr::contin_import()] or [odeqcdr::contin_volmon_import()].
#' @param deployment Data frame of the deployment data generated using [odeqcdr::contin_import()] or [odeqcdr::contin_volmon_import()].
#' @param project_id Unique Project ID from the projects data frame generated using [odeqcdr::contin_import()] or [odeqcdr::contin_volmon_import()]. Only accepts one project ID.
#' @export
#' @return returned data frame.

sumstats <-function(results, deployment, project_id) {

  # Testing parameters
  # results=df.results.final
  # deployment=df1.deployment
  # project_id=df0.projects$Project.ID


  # convert F to C, filter out rejected data, and create datetime column
  results_data <- results  %>%
    dplyr::mutate(Result.Value=dplyr::case_when(Result.Unit=="deg F" ~ (Result.Value - 32) * (5 / 9),
                                                Result.Unit=="ug/l" ~ Result.Value * 0.001,
                                                TRUE ~ Result.Value),
                  Result.Unit=dplyr::case_when(Result.Unit=="deg F" ~ "deg C",
                                               Result.Unit=="ug/l" ~ "mg/l",
                                               TRUE ~ Result.Unit)) %>%
    dplyr::filter(Result.Status.ID != "Rejected" | rDQL != 'C') %>%
    dplyr::mutate(time_char = strftime(Activity.Start.Time, format = "%H:%M:%S", tz = lubridate::tz(Activity.Start.Time)),
                  datetime =lubridate::ymd_hms(paste(as.Date(Activity.Start.Date, tz = lubridate::tz(Activity.Start.Time)), time_char)),
                  Activity.Start.Date = as.Date(Activity.Start.Date, tz = lubridate::tz(Activity.Start.Time))
    ) %>%
    dplyr::left_join(deployment[,c("Monitoring.Location.ID", "Equipment.ID",
                                   "Characteristic.Name", "Sample.Depth", "Sample.Depth.Unit")],
                     by=c("Monitoring.Location.ID", "Equipment.ID", "Characteristic.Name")) %>%
    dplyr::arrange(Monitoring.Location.ID, Equipment.ID, Characteristic.Name, datetime)


  # get unique list of characteristics to run for loop through
  unique_characteritics <- unique(results_data$Characteristic.Name)

  #create list for getting data out of loop
  monloc_do_list <- list()
  monloc_temp_list <- list()
  sumstatlist <- list()

  # For loop for summary statistics -----------------------------------------
  start_time <- Sys.time()
  # Loop goes through each characteristic and generates summary stats
  # After loop, data gets pushed in to single table
  for (i in 1:length(unique_characteritics)){

    print(paste("Begin",  unique_characteritics[i], "- characteristic", i, "of", length(unique_characteritics)))

    # Characteristic for this loop iteration
    char <- unique_characteritics[i]

    # Filter so table only contains single characteristic
    results_data_char <- results_data %>%
      dplyr::filter(Characteristic.Name == char) %>%
      # generate unique hour field for hourly values and stats
      dplyr::mutate(hr =  format(datetime, "%Y-%j-%H"))

    # Simplify to hourly values and Stats
    hrsum <- results_data_char %>%
      dplyr::group_by(Monitoring.Location.ID, Equipment.ID, Sample.Depth, Sample.Depth.Unit, hr, Result.Unit, Activity.Start.End.Time.Zone) %>%
      dplyr::summarise(date = mean(Activity.Start.Date),
                       hrDTmin = min(datetime),
                       hrDTmax = max(datetime),
                       hrN = sum(!is.na(Result.Value)),
                       hrMean = mean(Result.Value, na.rm=TRUE),
                       hrMin = min(Result.Value, na.rm=TRUE),
                       hrMax = max(Result.Value, na.rm=TRUE),
                       hrDQL  = max(rDQL, na.rm = TRUE))


    # For each date, how many hours have hrN > 0
    # remove rows with zero records in an hour.
    hrdat<- hrsum[which(hrsum$hrN >0),]

    # Summarise to daily statistics
    daydat <- hrdat %>%
      dplyr::group_by(Monitoring.Location.ID, Equipment.ID, Sample.Depth, Sample.Depth.Unit, date, Result.Unit, Activity.Start.End.Time.Zone) %>%
      dplyr::summarise(dDTmin = min(hrDTmin),
                       dDTmax = max(hrDTmax),
                       hrNday = length(hrN),
                       dyN = sum(hrN),
                       dyMean = dplyr::case_when(length(hrNday[hrDQL %in% c('A', 'B')]) >= 22 ~ mean(hrMean[hrDQL %in% c('A', 'B')], na.rm=TRUE),
                                                 length(hrNday[hrDQL %in% c('A', 'B', 'E')]) >= 22 ~  mean(hrMean[hrDQL %in% c('A', 'B', 'E')], na.rm=TRUE),
                                                 TRUE ~ mean(hrMean, na.rm=TRUE)
                       ),
                       dyMin = dplyr::case_when(length(hrNday[hrDQL %in% c('A', 'B')]) >= 22 ~ min(hrMin[hrDQL %in% c('A', 'B')], na.rm=TRUE),
                                                length(hrNday[hrDQL %in% c('A', 'B', 'E')]) >= 22 ~  min(hrMin[hrDQL %in% c('A', 'B', 'E')], na.rm=TRUE),
                                                TRUE ~ min(hrMin, na.rm=TRUE)
                       ),
                       dyMax = dplyr::case_when(length(hrNday[hrDQL %in% c('A', 'B')]) >= 22 ~ max(hrMax[hrDQL %in% c('A', 'B')], na.rm=TRUE),
                                                length(hrNday[hrDQL %in% c('A', 'B', 'E')]) >= 22 ~  max(hrMax[hrDQL %in% c('A', 'B', 'E')], na.rm=TRUE),
                                                TRUE ~ max(hrMax, na.rm=TRUE)
                       ),
                       dyDQL = dplyr::case_when(length(hrNday[hrDQL == 'A']) >= 24 ~ 'A',
                                                length(hrNday[hrDQL %in% c('A', 'B')]) >= 22 ~ 'B',
                                                length(hrNday[hrDQL %in% c('A', 'B', 'E')]) >= 22 ~ 'E'
                       )) %>%
      dplyr::mutate(ResultStatusID = dplyr::if_else(hrNday >= 22, 'Final', "Rejected"),
                    cmnt = dplyr::case_when(hrNday >= 22 ~ "Generated by ORDEQ",
                                            hrNday <= 22 & hrNday >= 20 ~ paste0("Generated by ORDEQ; Estimated - ", as.character(hrNday), ' hrs with valid data in day'),
                                            TRUE ~ paste0("Generated by ORDEQ; Rejected - ", as.character(hrNday), ' hrs with valid data in day')),
                    ma.mean7 = as.numeric(""),
                    ma.mean7_DQL = as.character(""),
                    ma.min7 = as.numeric(""),
                    ma.min7_DQL = as.character(""),
                    ma.mean30 = as.numeric(""),
                    ma.mean30_DQL = as.character(""),
                    ma.max7 = as.numeric(""),
                    ma.max7_DQL = as.character(""))


    #Deal with DO Results
    if (results_data_char$Characteristic.Name[1] == "Dissolved oxygen (DO)") {

      #monitoring location loop
      for(j in 1:length(unique(daydat$Monitoring.Location.ID))){

        equipment <- unique(daydat$Equipment.ID)[j]

        print(paste("Equipment ID:",equipment, "-", j, "of", length(unique(daydat$Equipment.ID))))

        #Filter dataset to only look at 1 monitoring location at a time
        daydat_station <- daydat %>%
          dplyr::filter(Equipment.ID == equipment)%>%
          dplyr::filter(hrNday >= 22)%>%
          dplyr::mutate(startdate7 = as.Date(date) - 6,
                        startdate30 = as.Date(date) - 29)

        # 7 day loop
        # Loops through each row in the monitoring location dataset
        # And pulls out records that are within the preceding 7 day window
        # If there are at least 6 values, then calculate 7 day min and mean
        # Assigns data back to daydat_station
        print("Begin 7 day moving averages")
        pb <- txtProgressBar(min = 0, max = nrow(daydat_station), style = 3)
        for(k in 1:nrow(daydat_station)){

          start7 <- daydat_station$startdate7[k]
          end7 <- daydat_station$date[k]

          station_7day <- daydat_station %>%
            dplyr::filter(date <= end7 & date >= start7) %>%
            dplyr::filter(hrNday >= 22)

          ma.mean7 <- dplyr::case_when(nrow(subset(station_7day, dyDQL %in% c("A"))) == 7 ~ mean(station_7day$dyMean),
                                       nrow(subset(station_7day, dyDQL %in% c("A", 'B'))) >= 6 ~ mean(station_7day$dyMean),
                                       max(station_7day$dyDQL == 'E') & nrow(subset(station_7day, dyDQL %in% c("A", "B"))) >= 6  ~ mean(station_7day$dyMean[station_7day$dyDQL %in% c("A", "B")]),
                                       nrow(subset(station_7day, dyDQL %in% c("A", "B", "E"))) >= 6 ~  mean(station_7day$dyMean),
                                       TRUE ~ NA_real_
          )
          ma.mean7_DQL <- dplyr::case_when(nrow(subset(station_7day, dyDQL %in% c("A"))) == 7 ~ "A",
                                           nrow(subset(station_7day, dyDQL %in% c("A", 'B'))) >= 6 ~ "B",
                                           max(station_7day$dyDQL == 'E') & nrow(subset(station_7day, dyDQL %in% c("A", "B"))) >= 6  ~ "B",
                                           nrow(subset(station_7day, dyDQL %in% c("A", "B", "E"))) >= 6 ~ "E",
                                           TRUE ~ NA_character_
          )


          ma.min7 <-  dplyr::case_when(nrow(subset(station_7day, dyDQL %in% c("A"))) == 7 ~ mean(station_7day$dyMin),
                                       nrow(subset(station_7day, dyDQL %in% c("A", 'B'))) >= 6 ~ mean(station_7day$dyMin),
                                       max(station_7day$dyDQL == 'E') & nrow(subset(station_7day, dyDQL %in% c("A", "B"))) >= 6  ~ mean(station_7day$dyMin[station_7day$dyDQL %in% c("A", "B")]),
                                       nrow(subset(station_7day, dyDQL %in% c("A", "B", "E"))) >= 6 ~  mean(station_7day$dyMin),
                                       TRUE ~ NA_real_
          )



          daydat_station[k,"ma.mean7"] <- ifelse(k >=7, ma.mean7, NA)
          daydat_station[k,"ma.mean7_DQL"] <- ifelse(k >=7, ma.mean7_DQL, NA)
          daydat_station[k, "ma.min7"] <- ifelse(k >=7, ma.min7, NA)
          daydat_station[k, "ma.min7_DQL"] <- ifelse(k >=7, ma.mean7_DQL, NA)
          daydat_station[k, "ana_startdate7"] <-  min(station_7day$dDTmin)
          daydat_station[k, "ana_enddate7"] <-  max(station_7day$dDTmax)
          daydat_station[k, "act_enddate7"] <-  max(station_7day$dDTmax)



          setTxtProgressBar(pb, k)

        } #end of 7day loop
        close(pb)
        # 30 day loop
        # Loops through each row in the monitoring location dataset
        # And pulls out records that are within the preceding 30 day window
        # If there are at least 29 values, then calculate 30 day mean
        # Assigns data back to daydat_station
        print("Begin 30 day moving averages" )
        pb <- txtProgressBar(min = 0, max = nrow(daydat_station), style = 3)
        for(l in 1:nrow(daydat_station)){


          start30 <- daydat_station$startdate30[l]
          end30 <- daydat_station$date[l]

          station_30day <- daydat_station %>%
            dplyr::filter(date <= end30 & date >= start30) %>%
            dplyr::filter(hrNday >= 22)

          ma.mean30 <-  dplyr::case_when(nrow(subset(station_30day, dyDQL %in% c("A"))) == 30 ~ mean(station_30day$dyMean),
                                         nrow(subset(station_30day, dyDQL %in% c("A", 'B'))) >= 29 ~ mean(station_30day$dyMean),
                                         max(station_30day$dyDQL == 'E') & nrow(subset(station_30day, dyDQL %in% c("A", "B"))) >= 29  ~ mean(station_30day$dyMean[station_30day$dyDQL %in% c("A", "B")]),
                                         nrow(subset(station_30day, dyDQL %in% c("A", "B", "E"))) >= 29 ~  mean(station_30day$dyMean),
                                         TRUE ~ NA_real_
          )

          ma.mean30_DQL <- dplyr::case_when(nrow(subset(station_30day, dyDQL %in% c("A"))) == 30 ~"A",
                                            nrow(subset(station_30day, dyDQL %in% c("A", 'B'))) >= 29 ~ "B",
                                            max(station_30day$dyDQL == 'E') & nrow(subset(station_30day, dyDQL %in% c("A", "B"))) >= 29  ~ "B",
                                            nrow(subset(station_30day, dyDQL %in% c("A", "B", "E"))) >= 29 ~ "E",
                                            TRUE ~ NA_character_
          )




          daydat_station[l,"ma.mean30"] <- ifelse(l >= 30, ma.mean30, NA)
          daydat_station[l,"ma.mean30_DQL"] <- ifelse(l >= 30, ma.mean30_DQL, NA)
          daydat_station[l,"ana_startdate30"] <-  min(station_30day$dDTmin)
          daydat_station[l,"ana_enddate30"] <-  max(station_30day$dDTmax)
          daydat_station[l,"act_enddate30"] <-  max(station_30day$dDTmax)


          setTxtProgressBar(pb, l)
        } #end of 30day loop

        close(pb)
        # Assign dataset filtered to 1 monitoring location to a list for combining outside of for loop
        monloc_do_list[[j]] <- daydat_station



      } # end of monitoring location for loop

      # Combine list to single dataframe
      sum_stats <- dplyr::bind_rows(monloc_do_list)

    } # end of DO if statement


    ##  TEMPERATURE

    if (results_data_char$Characteristic.Name[1] == 'Temperature, water' ) {



      #monitoring location loop
      for(j in 1:length(unique(daydat$Monitoring.Location.ID))){
        equipment <- unique(daydat$Equipment.ID)[j]

        print(paste("Equipment ID:",equipment, "-", j, "of", length(unique(daydat$Equipment.ID))))

        #Filter dataset to only look at 1 monitoring location at a time
        daydat_station <- daydat %>%
          dplyr::filter(Equipment.ID == equipment)%>%
          dplyr::filter(hrNday >= 22)


        # 7 day loop
        # Loops through each row in the monitoring location dataset
        # And pulls out records that are within the preceding 7 day window
        # If there are at least 6 values, then calculate 7 day min and mean
        # Assigns data back to daydat_station
        print("Begin 7 day moving averages")



        pb <- txtProgressBar(min = 0, max = nrow(daydat_station), style = 3)
        for (l in seq_len(nrow(daydat_station))){


          station_7day <- filter(daydat_station,
                                 dplyr::between(date, daydat_station[[l,'date']] - lubridate::days(6), daydat_station[l,'date']))


          daydat_station[l,"ma.max7"] <- dplyr::case_when(nrow(subset(station_7day, dyDQL %in% c("A")))== 7 & l >=7  ~ mean(station_7day$dyMax),
                                                          nrow(subset(station_7day, dyDQL %in% c("A", 'B'))) >= 6 & l >=7~ mean(station_7day$dyMax),
                                                          max(station_7day$dyDQL == 'E') & nrow(subset(station_7day, dyDQL %in% c("A", "B"))) >= 6  & l >=7 ~ mean(station_7day$dyMax[station_7day$dyDQL %in% c("A", "B")]),
                                                          nrow(subset(station_7day, dyDQL %in% c("A", "B", "E"))) >= 6 & l >=7~  mean(station_7day$dyMax),
                                                          TRUE ~ NA_real_)


          daydat_station[l, "ma.max7_DQL"] <-  dplyr::case_when(nrow(subset(station_7day, dyDQL %in% c("A")))== 7 & l >=7  ~ "A",
                                                                nrow(subset(station_7day, dyDQL %in% c("A", 'B'))) >= 6 & l >=7~ "B",
                                                                max(station_7day$dyDQL == 'E') & nrow(subset(station_7day, dyDQL %in% c("A", "B"))) >= 6  & l >=7 ~ "B",
                                                                nrow(subset(station_7day, dyDQL %in% c("A", "B", "E"))) >= 6 & l >=7~ "E",
                                                                TRUE ~ NA_character_)
          daydat_station[l, "ana_startdate7"] <-  min(station_7day$dDTmin)
          daydat_station[l, "ana_enddate7"] <-  max(station_7day$dDTmax)
          daydat_station[l, "act_enddate7"] <-  max(station_7day$dDTmax)
          setTxtProgressBar(pb, l)

        } #end of 7day loop
        close(pb)

        monloc_temp_list[[j]] <- daydat_station
      } # end of monitoring location for loop

      # Combine list to single dataframe
      sum_stats <- dplyr::bind_rows(monloc_temp_list) %>%
        dplyr::arrange(Monitoring.Location.ID, Equipment.ID, date)




    } #end of temp if statement


    ## Other - just set sum_stats to daydat, since no moving averages need to be generated.
    if (results_data_char$Characteristic.Name[1] != 'Temperature, water' & results_data_char$Characteristic.Name[1] != "Dissolved oxygen (DO)"  ) {

      sum_stats <- daydat

    } #end of not DO or temp statement

    #Assign the char ID to the dataset
    sum_stats <- sum_stats %>%
      dplyr::mutate(charID = char)

    #Set to list for getting out of for loop
    sumstatlist[[i]] <-  sum_stats


  } # end of characteristics for loop

  Sys.time() - start_time

  # Bind list to dataframe
  sumstat <- dplyr::bind_rows(sumstatlist)

  #Pivot summary statistics from wide format into long format
  #rename summary statistics to match AWQMS Import COnfiguration
  sumstat_long <- sumstat %>%
    dplyr::rename("Daily Maximum" = dyMax,
                  "Daily Minimum" = dyMin,
                  "Daily Mean"    = dyMean,
                  "7DMADMin"      = ma.min7,
                  "7DMADMean"     = ma.mean7,
                  "7DMADMax"      = ma.max7,
                  "30DMADMean"    = ma.mean30) %>%
    tidyr::pivot_longer(
      cols=c("Daily Maximum",
             "Daily Minimum",
             "Daily Mean",
             "7DMADMin",
             "7DMADMean",
             "7DMADMax",
             "30DMADMean"),
      names_to = "StatisticalBasis",
      values_to = "Result",
      values_drop_na = TRUE
    ) %>%
    dplyr::arrange(Monitoring.Location.ID, Equipment.ID, charID, date)

  # AWQMS summary stats -----------------------------------------------------


  # Join method to sumstat table
  # sumstat_long <- sumstat_long %>%
  #   dplyr::mutate(Equipment = as.character(Equipment)) %>%
  #   left_join(Audits_unique, by = c("Monitoring.Location.ID", "charID" = "Characteristic.Name") )
  AQWMS_sum_stat <- sumstat_long %>%
    ungroup() %>%
    dplyr::mutate(RsltTimeBasis = dplyr::case_when(StatisticalBasis %in% c("7DMADMin", "7DMADMean", "7DMADMax") ~ "7 Day",
                                                   StatisticalBasis %in% c("30DMADMean") ~ "30 Day",
                                                   TRUE ~"1 Day"),
                  ActivityType = "FMC",
                  Result.Analytical.Method.ID = dplyr::case_when(charID == "Temperature, water" ~ "170.1",
                                                                 charID == "Conductivity" ~ "120.1",
                                                                 charID == "Dissolved oxygen (DO)" ~ "NFM 6.2.1-LUM",
                                                                 charID == "Dissolved oxygen saturation" ~ "NFM 6.2.1-LUM",
                                                                 charID == "pH" ~ "150.1",
                                                                 charID == "Turbidity" ~ "180.1",
                                                                 TRUE ~ "error"),
                  SmplColMthd = "ContinuousPrb",
                  SmplColEquip = "Probe/Sensor",
                  SmplDepth = Sample.Depth,
                  SmplDepthUnit = Sample.Depth.Unit,
                  SmplColEquipComment = "",
                  Samplers = "",
                  Equipment = Equipment.ID,
                  r_units = Result.Unit,
                  Project = project_id,
                  AnaStartDate = case_when(RsltTimeBasis == "1 Day" ~ format(dDTmin, format="%Y-%m-%d"),
                                           RsltTimeBasis == "7 Day" ~ format(ana_startdate7, format="%Y-%m-%d"),
                                           RsltTimeBasis == "30 Day" ~ format(ana_startdate30, format="%Y-%m-%d")),
                  AnaStartTime = case_when(RsltTimeBasis == "1 Day" ~ format(dDTmin, format="%H:%M:%S"),
                                           RsltTimeBasis == "7 Day" ~ format(ana_startdate7, format="%H:%M:%S"),
                                           RsltTimeBasis == "30 Day" ~ format(ana_startdate30, format="%H:%M:%S")),
                  AnaEndDate = case_when(RsltTimeBasis == "1 Day" ~ format(dDTmax, format="%Y-%m-%d"),
                                         RsltTimeBasis == "7 Day" ~ format(ana_enddate7, format="%Y-%m-%d"),
                                         RsltTimeBasis == "30 Day" ~ format(ana_enddate30, format="%Y-%m-%d")),
                  AnaEndTime = case_when(RsltTimeBasis == "1 Day" ~ format(dDTmax, format="%H:%M:%S"),
                                         RsltTimeBasis == "7 Day" ~ format(ana_enddate7, format="%H:%M:%S"),
                                         RsltTimeBasis == "30 Day" ~ format(ana_enddate30, format="%H:%M:%S")),
                  ActStartDate = date,
                  ActStartTime = "0:00",
                  ActEndDate = AnaEndDate,
                  ActEndTime = AnaEndTime,
                  RsltType = "Calculated",
                  ActStartTimeZone = Activity.Start.End.Time.Zone,
                  ActEndTimeZone = Activity.Start.End.Time.Zone,
                  AnaStartTimeZone = Activity.Start.End.Time.Zone,
                  AnaEndTimeZone = Activity.Start.End.Time.Zone,
                  Result = round(Result, digits = 2),
                  DQL = dplyr::case_when(StatisticalBasis %in% c("Daily Maximum", "Daily Minimum", "Daily Mean") ~ dyDQL,
                                         StatisticalBasis %in% c("7DMADMax") ~ ma.max7_DQL,
                                         StatisticalBasis %in% c("7DMADMin") ~ ma.min7_DQL,
                                         StatisticalBasis %in% c("7DMADMean") ~ ma.mean7_DQL,
                                         StatisticalBasis %in% c("30DMADMean") ~ ma.mean30_DQL,
                                         TRUE ~ NA_character_)
    ) %>%
    dplyr::select(charID,
                  Result,
                  r_units,
                  Result.Analytical.Method.ID,
                  RsltType,
                  ResultStatusID,
                  StatisticalBasis,
                  RsltTimeBasis,
                  cmnt,
                  ActivityType,
                  Monitoring.Location.ID,
                  DQL,
                  SmplColMthd,
                  SmplColEquip,
                  SmplDepth,
                  SmplDepthUnit,
                  SmplColEquipComment,
                  Samplers,
                  Equipment,
                  Project,
                  ActStartDate,
                  ActStartTime,
                  ActStartTimeZone,
                  ActEndDate,
                  ActEndTime,
                  ActEndTimeZone,
                  AnaStartDate,
                  AnaStartTime,
                  AnaStartTimeZone,
                  AnaEndDate,
                  AnaEndTime,
                  AnaEndTimeZone)

  return(AQWMS_sum_stat)

}
