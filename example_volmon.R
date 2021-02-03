library(dplyr)
library(lutz)
library(odeqcdr)
library(writexl)
library(magrittr)



# Setup -----------------------------------------------------------------------------------------------------------

#Analyst Name
analyst <- "Travis Pritchard"

#Set directory where files come from
input_dir <- ("//deqlab1/Vol_Data/Siuslaw/2018/SWC_2018_Cont_Data_Sub")

#Directory where files are saved to- SHould be a voldata folder
output_dir <-"//deqlab1/Vol_Data/Siuslaw/2018/SWC_2018_Cont_Data_Sub/R"

#Volmon template file
xlsx_input <- "WorkingCopy_Siuslaw_WC_2018_Continuous_Temp.xlsx"

#precheck file name
xlsx_pre_check_output <- "Siuslaw_WC_2018_Continuous_Temp_PRECHECK.xlsx"

# Filename for Data to load into shiny
shiny_output <- "Siuslaw_WC_2018_Continuous_Temp_SHINY_CDR.Rdata"

#Script output
xlsx_output <- "Siuslaw_WC_2018_Continuous_Temp_export.xlsx"

#changelog output
changelog <-  'Siuslaw_WC_2018_Continuous_Temp_changelog'



#- Import the Data -------------------------------------------------------------

df0 <-contin_volmon_import(file=paste0(input_dir,"/",xlsx_input))

df0.projects <- df0[["Projects"]]

df0.org <- df0[["Organization_Details"]]

df0.mloc <- df0[["Monitoring_Locations"]]

df0.results <- df0[["Results"]]

df0.audits <- df0[["Audit_Data"]]

df0.deployment <- df0[["Deployment"]]

df0.prepost <- df0[["PrePost"]]

#- Sort Results and Audits -----------------------------------------------------

df0.results <- df0.results %>%
  dplyr::arrange(Monitoring.Location.ID, Equipment.ID, Characteristic.Name,
                 Activity.Start.Date, Activity.Start.Time)

df0.audits <- df0.audits %>%
  dplyr::arrange(Monitoring.Location.ID, Equipment.ID, Characteristic.Name,
                 Activity.Start.Date, Activity.Start.Time)

df0[["Results"]] <- df0.results
df0[["Audit_Data"]] <- df0.audits

#- Completeness Pre checks -----------------------------------------------------
# A TRUE result means something is missing
checks_df <- odeqcdr::pre_checks(template_list = df0)

# Save pre check results to xlsx
writexl::write_xlsx(checks_df, path=paste0(output_dir, "/", xlsx_pre_check_output),
                    format_headers=TRUE)

#- Row numbers for indexing ----------------------------------------------------
df1.results <- dplyr::mutate(df0.results, row.results=dplyr::row_number())
df1.audits <- dplyr::mutate(df0.audits, row.audits=dplyr::row_number())
df1.deployment <- dplyr::mutate(df0.deployment, row.deployment=dplyr::row_number())
df1.prepost <- dplyr::mutate(df0.prepost, row.prepost=dplyr::row_number())

# Keep a record of the original units
# This is to convert the units back to the original after grading.
# Only needed for Results worksheet
df1.results.units <- dplyr::select(df1.results, row.results, Result.Unit.orig=Result.Unit)

#- Set Project ID --------------------------------------------------------------

 df1.projects <- df0.projects
#- Review Monitoring Location Info----------------------------------------------

 odeqcdr::launch_map(mloc=df0.mloc)

# Make manual changes to the xlsx spreadsheet and re import if needed:
# df1.mloc <- odeqcdr::contin_import(file=xlsx_input, sheets=c("Monitoring_Locations"))[["Monitoring_Locations"]]

# Make sure there are no duplicate entries.
df1.mloc <- dplyr::distinct(df0.mloc)

# Save R global environment just in case.
save.image(paste0(output_dir, "/Renv.RData"))

#- Update Monitoring Location ID Name-------------------------------------------

# Fix monitoring location IDs w/ invalid characters
# The following are invalid characters in Monitoring Location IDs
# ` ~ ! # $ % ^ & * ( ) [ { ] } \ | ; ' " < > / ? [space]
# @ is replaced with 'at'
# The rest are replaced with '_'
df1.mloc$Monitoring.Location.ID <- odeqcdr::inchars(x=df1.mloc$Monitoring.Location.ID)
df1.deployment$Monitoring.Location.ID <- odeqcdr::inchars(x=df1.deployment$Monitoring.Location.ID)
df1.results$Monitoring.Location.ID <- odeqcdr::inchars(x=df1.results$Monitoring.Location.ID)
df1.audits$Monitoring.Location.ID <- odeqcdr::inchars(x=df1.audits$Monitoring.Location.ID)

#- Check if the correct timezone is used ---------------------------------------
# Check that monitoring stations located in the Pacific time zone have pacific time
# zones (e.g. PST/PDT) and stations in the Mountain time zone have mountain time
# zones (e.g. MST/MDT). This is checked by adding the Olson name
# timezone (see OlsonNames()) based on the monitoring location latitude and longitude.
# Make sure the latitude and longitude are correct before running this code.
# The Olson name timezone is used in dt_combine() and dst_check()

df.tz <- df1.mloc %>%
  dplyr::select(Monitoring.Location.ID, Latitude, Longitude) %>%
  dplyr::distinct() %>%
  dplyr::mutate(tz_name=lutz::tz_lookup_coords(lat=Latitude,lon=Longitude, method="accurate", warn=FALSE)) %>%
  dplyr::select(-Latitude, -Longitude)

df1.deployment <- merge(df1.deployment, df.tz, by="Monitoring.Location.ID")
df1.results <- merge(df1.results, df.tz, by="Monitoring.Location.ID")
df1.audits <- merge(df1.audits, df.tz, by="Monitoring.Location.ID")

# Add a timezone if one is missing, The code will correct in dst_check if it's wrong.
# Flag timezones that are wrong.
df1.results <- df1.results %>%
  dplyr::mutate(Activity.Start.End.Time.Zone=dplyr::case_when(tz_name=="America/Los_Angeles" &
                                                                is.na(Activity.Start.End.Time.Zone) ~ "PDT",
                                                              tz_name=="America/Boise" &
                                                                is.na(Activity.Start.End.Time.Zone) ~  "MDT",
                                                              TRUE ~ Activity.Start.End.Time.Zone),
                tz_wrong=dplyr::case_when(tz_name=="America/Los_Angeles" &
                                            Activity.Start.End.Time.Zone %in% c("PDT", "PST") ~ FALSE,
                                          tz_name=="America/Boise" &
                                            Activity.Start.End.Time.Zone %in% c("MDT", "MST") ~ FALSE,
                                          TRUE ~ TRUE))

df1.audits <- df1.audits %>%
  dplyr::mutate(Activity.Start.End.Time.Zone=dplyr::case_when(tz_name=="America/Los_Angeles" &
                                                                is.na(Activity.Start.End.Time.Zone) ~ "PDT",
                                                              tz_name=="America/Boise" &
                                                                is.na(Activity.Start.End.Time.Zone) ~  "MDT",
                                                              TRUE ~ Activity.Start.End.Time.Zone),
                tz_wrong=dplyr::case_when(tz_name=="America/Los_Angeles" &
                                            Activity.Start.End.Time.Zone %in% c("PDT", "PST") ~ FALSE,
                                          tz_name=="America/Boise" &
                                            Activity.Start.End.Time.Zone %in% c("MDT", "MST") ~ FALSE,
                                          TRUE ~ TRUE))

# Show which rows failed the tz check (tz_wrong=TRUE)
df1.results[df1.results$tz_wrong, c("row.results")]
df1.audits[df1.audits$tz_wrong, c("row.audits")]

# check and correct for DST ----------------------------------------------------
# dst_check() checks that date and time conform to changes between
# Daylight Time and Standard Time. The output is an updated PoSIXct datetime.
# This also runs dt_combine(). Time change corrections will be identified by stations and periods.
# Any changes should be manually reviewed.

df1.results$datetime <- odeqcdr::dst_check(df=df1.results,
                                           tz_col="tz_name")

df1.audits$audit.datetime.start <- odeqcdr::dst_check(df=df1.audits,
                                                      date_col="Activity.Start.Date",
                                                      time_col="Activity.Start.Time",
                                                      tz_col="tz_name")

df1.audits$audit.datetime.end  <- odeqcdr::dst_check(df=df1.audits,
                                                     date_col="Activity.End.Date",
                                                     time_col="Activity.End.Time",
                                                     tz_col="tz_name")

#- Combine Deployment date and time --------------------------------------------
# No need to check for dst.

df1.deployment$Deployment.Start.Date <- odeqcdr::dt_combine(df=df1.deployment,
                                                            date_col = "Deployment.Start.Date",
                                                            time_val = "00:00:00",
                                                            tz_col="tz_name")

df1.deployment$Deployment.End.Date <- odeqcdr::dt_combine(df=df1.deployment,
                                                          date_col = "Deployment.End.Date",
                                                          time_val = "23:59:00",
                                                          tz_col="tz_name")

#- Apply any corrections back to date and time columns Adds Comments -----------

df2.results <- odeqcdr::dt_parts(df=df1.results)

df2.audits <- odeqcdr::dt_parts(df=df1.audits,
                                datetime_col="audit.datetime.start",
                                date_col="Activity.Start.Date",
                                time_col="Activity.Start.Time")

df2.audits <- odeqcdr::dt_parts(df=df1.audits,
                                datetime_col="audit.datetime.end",
                                date_col="Activity.End.Date",
                                time_col="Activity.End.Time")


#- Convert Units ---------------------------------------------------------------
# This converts the result value and changes the Unit column.
# This is needed for grading and anomaly checking
# This converts any
# deg F -> deg C
# ug/l -> mg/l
# Add others as needed.

df3.audits <- df2.audits %>%
  dplyr::mutate(Result.Value=dplyr::case_when(Result.Unit=="deg F" ~ (Result.Value - 32) * (5 / 9),
                                              Result.Unit=="ug/l" ~ Result.Value * 0.001,
                                              TRUE ~ Result.Value),
                Result.Unit=dplyr::case_when(Result.Unit=="deg F" ~ "deg C",
                                             Result.Unit=="ug/l" ~ "mg/l",
                                             TRUE ~ Result.Unit))

df3.prepost <- df1.prepost  %>%
  dplyr::mutate(Equipment.Result.Value=dplyr::case_when(Equipment.Result.Unit=="deg F" ~ (Equipment.Result.Value - 32) * (5 / 9),
                                                        Equipment.Result.Unit=="ug/l" ~ Equipment.Result.Value * 0.001,
                                                        TRUE ~ Equipment.Result.Value),
                Equipment.Result.Unit=dplyr::case_when(Equipment.Result.Unit=="deg F" ~ "deg C",
                                                       Equipment.Result.Unit=="ug/l" ~ "mg/l",
                                                       TRUE ~ Equipment.Result.Unit),
                Reference.Result.Value=dplyr::case_when(Reference.Result.Unit=="deg F" ~ (Reference.Result.Value - 32) * (5 / 9),
                                                        Reference.Result.Unit=="ug/l" ~ Reference.Result.Value * 0.001,
                                                        TRUE ~Reference.Result.Value),
                Reference.Result.Unit=dplyr::case_when(Reference.Result.Unit=="deg F" ~ "deg C",
                                                       Reference.Result.Unit=="ug/l" ~ "mg/l",
                                                       TRUE ~  Reference.Result.Unit))

df3.results <- df2.results %>%
  dplyr::mutate(Result.Value=dplyr::case_when(Result.Unit=="deg F" ~ (Result.Value - 32) * (5 / 9),
                                              Result.Unit=="ug/l" ~ Result.Value * 0.001,
                                              TRUE ~ Result.Value),
                Result.Unit=dplyr::case_when(Result.Unit=="deg F" ~ "deg C",
                                             Result.Unit=="ug/l" ~ "mg/l",
                                             TRUE ~ Result.Unit))

#- Grade PrePost ---------------------------------------------------------------

df3.results$accDQL <- odeqcdr::dql_accuracy(prepost=df3.prepost, results=df3.results)

#- Grade Audits ----------------------------------------------------------------
df3.results$precDQL <- odeqcdr::dql_precision(audits=df3.audits, results=df3.results, deployment=df1.deployment)
df3.audits.dql <- odeqcdr::dql_precision(audits=df3.audits, results=df3.results, deployment=df1.deployment,
                                         audits_only = TRUE)


#- Final DQL -------------------------------------------------------------------

# Set up final grade column to be verified using shiny app and further review
# Update the rDQL when the submitted result status == "Rejected"
# Automatically set Result.Status.ID = "Rejected" when results are outside of deployment period
df4.results <- df3.results %>%
  dplyr::left_join(df1.deployment[,c("Monitoring.Location.ID", "Equipment.ID",
                                     "Characteristic.Name", "Deployment.Start.Date",
                                     "Deployment.End.Date")],
                   by=c("Monitoring.Location.ID", "Equipment.ID", "Characteristic.Name")) %>%
  dplyr::mutate(deployed=dplyr::if_else(datetime >= Deployment.Start.Date &
                                          datetime <= Deployment.End.Date, TRUE, FALSE),
                Result.Status.ID=dplyr::case_when(!deployed ~ "Rejected",
                                                  TRUE ~ Result.Status.ID),
                rDQL=dplyr::case_when(precDQL == 'C' | accDQL== 'C' ~ 'C',
                                      precDQL == 'B' | accDQL== 'B' ~ 'B',
                                      precDQL == 'A' & accDQL== 'A' ~ 'A',
                                      precDQL == 'E' & accDQL== 'E' ~ 'E',
                                      TRUE ~ 'B'),
                rDQL=dplyr::if_else(Result.Status.ID == "Rejected","C",rDQL)) %>%
  dplyr::select(-Deployment.Start.Date, -Deployment.End.Date) %>%
  dplyr::arrange(row.results) %>%
  as.data.frame()

#- Anomalies -------------------------------------------------------------------
# Flag potential anomalies
# Anomaly = TRUE if one of the daily summary statistics deviate from the typical range.

# First add Stream Order
df5.results <- df4.results %>%
  dplyr::left_join(df1.mloc[,c("Monitoring.Location.ID", "Reachcode", "Permanent.Identifier")], by="Monitoring.Location.ID") %>%
  dplyr::left_join(odeqcdr::ornhd[,c("StreamOrder", "Permanent_Identifier")], by=c("Permanent.Identifier"="Permanent_Identifier"))

# Get a dataframe of just the anomaly stats
df5.results.anom.stats <- df5.results %>%
  dplyr::mutate(month=lubridate::month(datetime)) %>%
  dplyr::left_join(odeqcdr::anomaly_stats) %>%
  dplyr::select(Monitoring.Location.ID, Equipment.ID, Characteristic.Name, dplyr::contains("daily"))


#Note- Anomaly check does not work for DO, so this manually sets anomaly to FALSE.
df5.results.anom <- odeqcdr::anomaly_check(results=df5.results, deployment=df1.deployment, return_df=TRUE) %>%
  mutate(Anomaly = ifelse(is.na(Anomaly), FALSE, Anomaly))


#- Output for further review using Shiny Tool ----------------------------------

# list to export to Shiny
shiny_list <-list(Deployment=df1.deployment,
                  Audit_Stats=df3.audits.dql,
                  Results_Anom=df5.results.anom)

save(shiny_list, file=paste0(output_dir, "/", shiny_output))

# Launch Shiny app for further review.
odeqcdr::launch_shiny()

#- Make DQL and Status edits based on Shiny Review------------------------------
# Updates Result Status ID also
# This section should be structured like this:

#######################################################################################################################
#######################################################################################################################

#df5.results <- df4.results %>%
  # dql_update(rows = c(1:6), "C", "test Comment 1") %>%
  # dql_update(rows = c(3:6), "D", "test Comment 2") %>%
  # dql_update(c(60, 65,67), "E")


#df4.audits.dql <- df3.audits.dql %>%
  # dql_update(rows = c(1:6), "C", "test Comment 1")

#######################################################################################################################

# If no edits are required:
#df5.results <- df4.results
#df4.audits.dql <- df3.audits.dql

#######################################################################################################################
#######################################################################################################################

#Once that is done update status IDs
# Set status IDs
df.results.final <- status_update(df5.results)
df.audits.final <- status_update(df4.audits.dql)

#Output changelog

#Calualte difference in the dataframes
differences <- compareDF::compare_df(df.results.final, df4.results, group_col = 'row.results')

#output this file into excel
compareDF::create_output_table(differences, output_type = "xlsx", file_name = paste0(output_dir,"/", changelog,"_", analyst, ".xlsx"))


#Set DOsat grades to grades for DO concentration
df.results.final <- odeqcdr::DOsat_DQLs(df.results.final)
# Generate Summary Stats -------------------------------------------------------

df.sumstats <- odeqcdr::sumstats(results=df.results.final, deployment=df1.deployment, project_id=df1.projects$Project.ID)


#- Output updated data back to xlsx template -----------------------------------

# First set the result units back to the original
# This only converts deg C -> deg F and mg/l -> ug/l
# Add others as needed. Only needed for Results worksheet.
df.results.final <- df.results.final %>%
  dplyr::left_join(df1.results.units, by="row.results") %>%
  dplyr::mutate(Result.Value=dplyr::case_when(Result.Unit.orig=="deg F" ~ (Result.Value * (9 / 5)) + 32,
                                              Result.Unit.orig=="ug/l" ~ Result.Value * 1000,
                                              TRUE ~ Result.Value),
                Result.Unit=dplyr::case_when(Result.Unit.orig=="deg F" ~ "deg F",
                                             Result.Unit.orig=="ug/l" ~ "ug/l",
                                             TRUE ~ Result.Unit)) %>%
  dplyr::select(-Result.Unit.orig) %>%
  dplyr::arrange(row.results) %>%
  as.data.frame()

df2.deployment <- odeqcdr::update_deploy(deploy = df1.deployment)

# Fill in the project ID
df2.deployment$Project.ID <- df0.projects$Project.ID


# Save R global environment just in case.
save.image(paste0(output_dir, "/Renv.RData"))

# Export
odeqcdr::contin_export(file=paste0(output_dir, "/", xlsx_output),
                       org=df0.org,
                       projects=df0.projects,
                       mloc=df1.mloc,
                       deployment=df2.deployment,
                       results=df.results.final,
                       prepost=df0.prepost,
                       audits=df.audits.final,
                       sumstats=df.sumstats,
                       ver=3)

