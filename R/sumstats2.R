#' Generate AWQMS Summary Statistics
#'
#' Calculates summary statistics but does not determine DQLs. Returns a data frame formatted for AWQMS upload. Version 0.12, commit 8de029f2565fe4645a06e6eaee36f545557888db was the last instance where this function was the original sumstats.
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

sumstats2 <-function(results, deployment, project_id) {

  # Tests
  #results=df.results.final
  #deployment=df1.deployment
  #project_id=df0.projects$Project.ID

  # convert F to C, filter out rejected data, and create datetime column
  results_data <- results  %>%
    dplyr::mutate(Result.Value=dplyr::case_when(Result.Unit=="deg F" ~ (Result.Value - 32) * (5 / 9),
                                                Result.Unit=="ug/l" ~ Result.Value * 0.001,
                                                TRUE ~ Result.Value),
                  Result.Unit=dplyr::case_when(Result.Unit=="deg F" ~ "deg C",
                                               Result.Unit=="ug/l" ~ "mg/l",
                                               TRUE ~ Result.Unit)) %>%
    dplyr::filter(Result.Status.ID != "Rejected") %>%
    dplyr::mutate(time_char = strftime(Activity.Start.Time, format = "%H:%M:%S", tz = 'UTC'),
                  datetime = lubridate::ymd_hms(paste(as.Date(Activity.Start.Date), time_char)),
                  Activity.Start.Date = as.Date(Activity.Start.Date)) %>%
    dplyr::left_join(deployment[,c("Monitoring.Location.ID", "Equipment.ID",
                                   "Characteristic.Name", "Sample.Depth", "Sample.Depth.Unit")],
                     by=c("Monitoring.Location.ID", "Equipment.ID", "Characteristic.Name")) %>%
    dplyr::arrange(Monitoring.Location.ID, Equipment.ID, Characteristic.Name, datetime)


  # get unique list of characteristics to run for loop through
  unique_characteritics <- unique(results_data$Characteristic.Name)

  #create list for getting data out of loop
  monloc_do_list <- list()
  sumstatlist <- list()

  # For loop for summary statistics -----------------------------------------

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
                       hrMax = max(Result.Value, na.rm=TRUE))


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
                       dyMean = mean(hrMean, na.rm=TRUE),
                       dyMin = min(hrMin, na.rm=TRUE),
                       dyMax = max(hrMax, na.rm=TRUE)) %>%
      dplyr::mutate(ResultStatusID = dplyr::if_else(hrNday >= 22, 'Final', "Rejected"),
                    cmnt = dplyr::case_when(hrNday >= 22 ~ "Generated by ORDEQ",
                                            hrNday <= 22 & hrNday >= 20 ~ paste0("Generated by ORDEQ; Estimated - ", as.character(hrNday), ' hrs with valid data in day'),
                                            TRUE ~ paste0("Generated by ORDEQ; Rejected - ", as.character(hrNday), ' hrs with valid data in day')),
                    ma.mean7 = as.numeric(""),
                    ma.min7 = as.numeric(""),
                    ma.mean30 = as.numeric(""),
                    ma.max7 = as.numeric(""))

    #Deal with DO Results
    if (results_data_char$Characteristic.Name[1] == "Dissolved oxygen (DO)") {

      #monitoring location loop
      for(j in 1:length(unique(daydat$Monitoring.Location.ID))){
        print(paste("Station", j, "of", length(unique(daydat$Monitoring.Location.ID))))

        station <- unique(daydat$Monitoring.Location.ID)[j]

        #Filter dataset to only look at 1 monitoring location at a time
        daydat_station <- daydat %>%
          dplyr::filter(Monitoring.Location.ID == station) %>%
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

          ma.mean7 <- ifelse(length(unique(station_7day$date)) >= 6, mean(station_7day$dyMean), NA )
          ma.min7 <- ifelse(length(unique(station_7day$date)) >= 6, mean(station_7day$dyMin), NA )

          daydat_station[k,"ma.mean7"] <- ifelse(k >=7, ma.mean7, NA)
          daydat_station[k, "ma.min7"] <- ifelse(k >=7, ma.min7, NA)

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

          ma.mean30 <- ifelse(length(unique(station_30day$date)) >= 29, mean(station_30day$dyMean), NA )


          daydat_station[l,"ma.mean30"] <- ifelse(l >= 30, ma.mean30, NA)
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

      # Temperature is much easier to calculate, since it needs a complete 7 day record to calculate the 7day moving average
      # This can happen with a simple grouping
      sum_stats <- daydat %>%
        dplyr::arrange(Monitoring.Location.ID, date) %>%
        dplyr::group_by(Monitoring.Location.ID) %>%
        dplyr::mutate(startdate7 = dplyr::lag(date, 6, order_by = date),
                      macmt = paste(dplyr::lag(ResultStatusID, 6),
                                    dplyr::lag(ResultStatusID, 5),
                                    dplyr::lag(ResultStatusID, 4),
                                    dplyr::lag(ResultStatusID, 3),
                                    dplyr::lag(ResultStatusID, 2),
                                    dplyr::lag(ResultStatusID, 1)),
                      # flag out which result gets a moving average calculated
                      calc7ma = ifelse(startdate7 == (as.Date(date) - 6) & (!grepl("Rejected",macmt )), 1, 0 ))%>%
        dplyr::mutate(ma.max7 = ifelse(calc7ma == 1 , round(zoo::rollmean(x = dyMax, 7, align = "right", fill = NA),2) , NA )) %>%
        dplyr::select(-startdate7, -calc7ma, -macmt )

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

  AQWMS_sum_stat <- sumstat_long %>%
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
                  SmplColEquipComment = NA_character_,
                  Samplers = NA_character_,
                  Equipment = Equipment.ID,
                  r_units = Result.Unit,
                  Project = project_id,
                  AnaStartDate = "",
                  AnaStartTime = "",
                  AnaEndDate = "",
                  AnaEndTime = "",
                  ActStartDate = format(dDTmax, "%Y-%m-%d"),
                  ActStartTime = format(dDTmax, "%H:%M"),
                  ActEndDate = format(dDTmax, "%Y-%m-%d"),
                  ActEndTime = format(dDTmax, "%H:%M"),
                  RsltType = "Calculated",
                  ActStartTimeZone = Activity.Start.End.Time.Zone,
                  ActEndTimeZone = Activity.Start.End.Time.Zone,
                  AnaStartTimeZone = Activity.Start.End.Time.Zone,
                  AnaEndTimeZone = Activity.Start.End.Time.Zone,
                  Result = round(Result, digits = 2)
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
