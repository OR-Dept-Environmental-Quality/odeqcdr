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
    dplyr::arrange(Monitoring.Location.ID, Equipment.ID, Characteristic.Name, datetime) %>%
    dplyr::mutate(mloc_equip = paste0(Monitoring.Location.ID, ":", Equipment.ID))


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
      dplyr::group_by(Monitoring.Location.ID, Equipment.ID, Sample.Depth, Sample.Depth.Unit, hr, Result.Unit, Activity.Start.End.Time.Zone, mloc_equip) %>%
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
      dplyr::group_by(Monitoring.Location.ID, Equipment.ID, Sample.Depth, Sample.Depth.Unit, date, Result.Unit, Activity.Start.End.Time.Zone, mloc_equip) %>%
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
                                            TRUE ~ paste0("Generated by ORDEQ; Rejected - ", as.character(hrNday), ' hrs with valid data in day')))
                    # ma.mean7 = as.numeric(""),
                    # ma.mean7_DQL = as.character(""),
                    # ma.min7 = as.numeric(""),
                    # ma.min7_DQL = as.character(""),
                    # ma.mean30 = as.numeric(""),
                    # ma.mean30_DQL = as.character(""),
                    # ma.max7 = as.numeric(""),
                    # ma.max7_DQL = as.character(""))


    #Deal with DO Results
    if (results_data_char$Characteristic.Name[1] == "Dissolved oxygen (DO)") {


        #Filter dataset to only look at 1 monitoring location at a time
        daydat_station <- daydat %>%
          dplyr::filter(hrNday >= 22)%>%
          dplyr::mutate(startdate7 = as.Date(date) - 6,
                        startdate30 = as.Date(date) - 29)

        # 7 day loop
        # Loops through each row in the monitoring location dataset
        # And pulls out records that are within the preceding 7 day window
        # If there are at least 6 values, then calculate 7 day min and mean
        # Assigns data back to daydat_station
        daydat_station2 <- daydat_station %>%
          ungroup() %>%
          group_by(mloc_equip) %>%
          mutate(row = row_number(),
                 dyDQL = factor(dyDQL, levels = c("A", "B", "E"), ordered = T),
                 d = runner(x = data.frame(dyMean_run = dyMean, dyMin_run = dyMin, dyDQL_run = dyDQL, dDTmin_run = dDTmin,
                                           dDTmax_run = dDTmax),
                            k = "7 days",
                            lag = 0,
                            idx = date,
                            f = function(x) list(x)),
                 d30 = runner(x = data.frame(dyMean_run = dyMean, dyDQL_run = dyDQL, dDTmin_run = dDTmin,
                                             dDTmax_run = dDTmax),
                              k = "30 days",
                              lag = 0,
                              idx = date,
                              f = function(x) list(x))) %>%
          mutate(d = purrr::map(d, ~ .x %>%
                                  dplyr::summarise(ma.mean7 = case_when(length(dyMean_run[dyDQL_run %in% c('A')]) == 7 ~ mean(dyMean_run),
                                                                        length(dyMean_run[dyDQL_run %in% c('A', 'B')]) >= 6 & max(dyDQL_run) != 'E' ~ mean(dyMean_run),
                                                                        max(dyDQL_run) == 'E' & length(dyMean_run[dyDQL_run %in% c('A', 'B')]) >= 6 ~
                                                                          mean(dyMean_run[dyDQL_run  %in% c('A', 'B')]),
                                                                        length(dyMean_run[dyDQL_run %in% c('A', 'B', 'E')]) >= 6 ~  mean(dyMean_run),
                                                                        TRUE ~ NA_real_),
                                                   ma.mean7_DQL = case_when(length(dyMean_run[dyDQL_run %in% c('A')]) == 7 ~ "A",
                                                                            length(dyMean_run[dyDQL_run %in% c('A', 'B')]) >= 6 & max(dyDQL_run) != 'E' ~ 'B',
                                                                            max(dyDQL_run) == 'E' & length(dyMean_run[dyDQL_run %in% c('A', 'B')]) >= 6 ~ 'B',
                                                                            length(dyMean_run[dyDQL_run %in% c('A', 'B', 'E')]) >= 6 ~  'E',
                                                                            TRUE ~ NA_character_),
                                                   ma.min7 = case_when(length(dyMin_run[dyDQL_run %in% c('A')]) == 7 ~ mean(dyMin_run),
                                                                       length(dyMin_run[dyDQL_run %in% c('A', 'B')]) >= 6 & max(dyDQL_run) != 'E' ~ mean(dyMin_run),
                                                                       max(dyDQL_run) == 'E' & length(dyMin_run[dyDQL_run %in% c('A', 'B')]) >= 6 ~
                                                                         mean(dyMin_run[dyDQL_run  %in% c('A', 'B')]),
                                                                       length(dyMin_run[dyDQL_run %in% c('A', 'B', 'E')]) >= 6 ~  mean(dyMin_run),
                                                                       TRUE ~ NA_real_),
                                                   ma.min7_DQL = case_when(length(dyMin_run[dyDQL_run %in% c('A')]) == 7 ~ 'A',
                                                                           length(dyMin_run[dyDQL_run %in% c('A', 'B')]) >= 6 & max(dyDQL_run) != 'E' ~ 'B',
                                                                           max(dyDQL_run) == 'E' & length(dyMin_run[dyDQL_run %in% c('A', 'B')]) >= 6 ~ 'B',
                                                                           length(dyMin_run[dyDQL_run %in% c('A', 'B', 'E')]) >= 6 ~ 'E',
                                                                           TRUE ~ NA_character_),
                                                   ana_startdate7 = min(dDTmin_run),
                                                   ana_enddate7   = max(dDTmax_run),
                                                   act_enddate7   = max(dDTmax_run))

          ))%>%
          mutate(d30 = purrr::map(d30, ~ .x %>%
                                    dplyr::summarise(ma.mean30 = case_when(length(dyMean_run[dyDQL_run %in% c('A')]) == 30 ~ mean(dyMean_run),
                                                                          length(dyMean_run[dyDQL_run %in% c('A', 'B')]) >= 29 & max(dyDQL_run) != 'E' ~ mean(dyMean_run),
                                                                          max(dyDQL_run) == 'E' & length(dyMean_run[dyDQL_run %in% c('A', 'B')]) >= 29 ~
                                                                            mean(dyMean_run[dyDQL_run  %in% c('A', 'B')]),
                                                                          length(dyMean_run[dyDQL_run %in% c('A', 'B', 'E')]) >= 29 ~  mean(dyMean_run),
                                                                          TRUE ~ NA_real_),
                                                     ma.mean30_DQL = case_when(length(dyMean_run[dyDQL_run %in% c('A')]) == 30 ~ "A",
                                                                              length(dyMean_run[dyDQL_run %in% c('A', 'B')]) >= 29 & max(dyDQL_run) != 'E' ~ 'B',
                                                                              max(dyDQL_run) == 'E' & length(dyMean_run[dyDQL_run %in% c('A', 'B')]) >= 29 ~ 'B',
                                                                              length(dyMean_run[dyDQL_run %in% c('A', 'B', 'E')]) >= 29 ~  'E',
                                                                              TRUE ~ NA_character_),
                                                     ana_startdate30 = min(dDTmin_run),
                                                     ana_enddate30 =  max(dDTmax_run),
                                                     act_enddate30 = max(dDTmax_run))

          )) %>%
          tidyr::unnest_wider(d) %>%
          tidyr::unnest_wider(d30) %>%
          mutate(ma.mean7 = ifelse(row < 7, NA, ma.mean7),
                 ma.mean7_DQL = ifelse(row < 7, NA, as.character(ma.mean7_DQL)),
                 ma.min7 = ifelse(row < 7, NA, ma.min7),
                 ma.min7_DQL = ifelse(row < 7, NA, as.character(ma.min7_DQL)),
                 ma.mean30 = ifelse(row < 30, NA, ma.mean30),
                 ma.mean30_DQL = ifelse(row < 30, NA, as.character(ma.mean30_DQL))) %>%
          select(-row)





      # Combine list to single dataframe
      sum_stats <-daydat_station2

    } # end of DO if statement




# Temperature -----------------------------------------------------------------------------------------------------

    a = Sys.time()
    if (results_data_char$Characteristic.Name[1] == 'Temperature, water' ) {



      #monitoring location loop

        #Filter dataset to only look at 1 monitoring location at a time
        daydat_station <- daydat %>%
          ungroup() %>%
          group_by(mloc_equip) %>%
          dplyr::filter(hrNday >= 22)


        # 7 day loop
        # Loops through each row in the monitoring location dataset
        # And pulls out records that are within the preceding 7 day window
        # If there are at least 6 values, then calculate 7 day min and mean
        # Assigns data back to daydat_station
        print("Begin 7 day moving averages")



        daydat_station2 <- daydat_station %>%
          ungroup() %>%
          group_by(mloc_equip) %>%
          mutate(row = row_number(),
                 dyDQL = factor(dyDQL, levels = c("A", "B", "E"), ordered = T),
                 d = runner(x = data.frame(dyMax_run = dyMax, dyDQL_run = dyDQL, dDTmin_run = dDTmin,
                                           dDTmax_run = dDTmax),
                            k = "7 days",
                            lag = 0,
                            idx = date,
                            f = function(x) list(x))) %>%
          mutate(d = purrr::map(d, ~ .x %>%
                                  dplyr::summarise(ma.max7 = case_when(length(dyMax_run[dyDQL_run %in% c('A')]) == 7 ~ mean(dyMax_run),
                                                                       length(dyMax_run[dyDQL_run %in% c('A', 'B')]) >= 6 & max(dyDQL_run) != 'E' ~ mean(dyMax_run),
                                                                       max(dyDQL_run) == 'E' & length(dyMax_run[dyDQL_run %in% c('A', 'B')]) >= 6 ~
                                                                         mean(dyMax_run[dyDQL_run  %in% c('A', 'B')]),
                                                                       length(dyMax_run[dyDQL_run %in% c('A', 'B', 'E')]) >= 6 ~  mean(dyMax_run),
                                                                       TRUE ~ NA_real_
                                  ),
                                  ma.max7_DQL = case_when(length(dyMax_run[dyDQL_run %in% c('A')]) == 7 ~ 'A',
                                                          length(dyMax_run[dyDQL_run %in% c('A', 'B')]) >= 6 & max(dyDQL_run) != 'E' ~ 'B',
                                                          max(dyDQL_run) == 'E' & length(dyMax_run[dyDQL_run %in% c('A', 'B')]) >= 6 ~ 'B',
                                                          length(dyMax_run[dyDQL_run %in% c('A', 'B', 'E')]) >= 6 ~  'E'
                                  ),
                                  ana_startdate7 = min(dDTmin_run),
                                  ana_enddate7 =  max(dDTmax_run),
                                  act_enddate7 = max(dDTmax_run))

                                ))%>%
          tidyr::unnest_wider(d) %>%
          mutate(ma.max7 = ifelse(row < 7, NA, ma.max7),
                 ma.max7_DQL = ifelse(row < 7, NA, as.character(ma.max7_DQL))) %>%
          select(-row)




      # Combine list to single dataframe
      sum_stats <- daydat_station2 %>%
        dplyr::arrange(Monitoring.Location.ID, Equipment.ID, date)




    } #end of temp if statement

    Sys.time() - a
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


  #add any missing columns
  sumstat_long_cols <- c("ana_startdate7", "ana_startdate30", "ana_enddate7", "ana_enddate30", "ma.max7_DQL",
                         "ma.min7_DQL", "ma.mean7_DQL", "ma.mean30_DQL")

  missing_cols <- setdiff(sumstat_long_cols, names(sumstat_long))

  sumstat_long[missing_cols] <- NA

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
                  DQL = dplyr::case_when(StatisticalBasis %in% c("Daily Maximum", "Daily Minimum", "Daily Mean") ~ as.character(dyDQL),
                                         StatisticalBasis %in% c("7DMADMax") ~ as.character(ma.max7_DQL),
                                         StatisticalBasis %in% c("7DMADMin") ~   as.character(ma.min7_DQL),
                                         StatisticalBasis %in% c("7DMADMean") ~  as.character(ma.mean7_DQL),
                                         StatisticalBasis %in% c("30DMADMean") ~ as.character(ma.mean30_DQL),
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
