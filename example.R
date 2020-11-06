library(dplyr)
library(lutz)
library(odeqcdr)
library(writexl)

setwd("E://GitHub/odeqcdr/test_templates")
#setwd("/Users/rmichie/GitHub/odeqcdr/test_templates")

xlsx_input <- "ContinuousDataTemplate_example.xlsx"
#xlsx_input <- "ContinuousDataTemplate_example_no_audits_prepost.xlsx"

output_dir <-"E:/GitHub/odeqcdr/test_templates"
#output_dir <-"/Users/rmichie/GitHub/odeqcdr/test_templates"

xlsx_output <- "ContinuousDataTemplate_example_output.xlsx"

#- Import the Data -------------------------------------------------------------

df0 <- odeqcdr::contin_import(file=xlsx_input)

df0.projects <- df0[["Projects"]]

df0.org <- df0[["Organization_Details"]]

df0.mloc <- df0[["Monitoring_Locations"]]

df0.results <- df0[["Results"]]

df0.audits <- df0[["Audit_Data"]]

df0.deployment <- df0[["Deployment"]]

df0.prepost <- df0[["PrePost"]]


#- Completeness Pre checks -----------------------------------------------------
# A TRUE result means something is missing
checks_df <- odeqcdr::pre_checks(template_list = df0)

# Save to xlsx
writexl::write_xlsx(checks_df, path=paste0(output_dir, "/", gsub(".xlsx","",xlsx_output),"_PRE_CHECKS.xlsx"),
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

df1.projects <- df1.projects %>%
  dplyr::mutate(Project.ID="TMDL Data Submission",
                Project.Name="TMDL Data Submission",
                Project.Description="Data submitted to DEQ to support TMDL development or TMDL implementation")

df1.audits <- df1.audits %>%
  dplyr::mutate(Alternate.Project.ID.2=Alternate.Project.ID.1,
                Alternate.Project.ID.1=Project.ID)

#- Review Monitoring Location Info----------------------------------------------
odeqcdr::launch_map(mloc=df0.mloc)

# Make manual changes to the xlsx spreadsheet and re import if needed:
df0.mloc <- odeqcdr::contin_import(file=xlsx_input, sheets=c("Monitoring_Locations"))[["Monitoring_Locations"]]

# Record Final Status
reject.mlocs <- c(NA)
final.mlocs <- c("BXDW", "BXON", "BXOS", "GRZL", "JNSX", "JNYI", "JNYU", "LWRX", "SDAL")

df1.mloc <- df0.mloc %>%
  dplyr::mutate(Monitoring.Location.Status.ID=dplyr::case_when(Monitoring.Location.ID %in% reject.mlocs  ~ "Rejected",
                                                  Monitoring.Location.ID %in% final.mlocs  ~ "Final",
                                                  TRUE ~ Monitoring.Location.Status.ID))

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
  dplyr::mutate(tz_name=lutz::tz_lookup_coords(lat=Latitude,lon=Longitude, method="accurate", warn=FALSE)) %>%
  dplyr::select(-Latitude, -Longitude)

df1.deployment <- merge(df1.deployment, df.tz, by="Monitoring.Location.ID")
df1.results <- merge(df1.results, df.tz, by="Monitoring.Location.ID")
df1.audits <- merge(df1.audits, df.tz, by="Monitoring.Location.ID")

df1.results <- df1.results %>%
  dplyr::mutate(tz_wrong=dplyr::case_when(tz_name=="America/Los_Angeles" &
                                            Activity.Start.End.Time.Zone %in% c("PDT", "PST") ~ FALSE,
                                          tz_name=="America/Boise" &
                                            Activity.Start.End.Time.Zone %in% c("MDT", "MST") ~ FALSE,
                                          TRUE ~ TRUE))

df1.audits <- df1.audits %>%
  dplyr::mutate(tz_wrong=dplyr::case_when(tz_name=="America/Los_Angeles" &
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
df4.results <- df3.results %>%
  dplyr::left_join(df1.deployment[,c("Monitoring.Location.ID", "Equipment.ID",
                                     "Characteristic.Name", "Deployment.Start.Date",
                                     "Deployment.End.Date")],
                   by=c("Monitoring.Location.ID", "Equipment.ID", "Characteristic.Name")) %>%
  dplyr::mutate(date=as.Date(datetime),
                deployed=dplyr::if_else(date >= as.Date(Deployment.Start.Date) &
                                          date <= as.Date(Deployment.End.Date), TRUE, FALSE),
                rDQL=dplyr::case_when(precDQL == 'C' | accDQL== 'C' ~ 'C',
                                      precDQL == 'B' | accDQL== 'B' ~ 'B',
                                      precDQL == 'A' & accDQL== 'A' ~ 'A',
                                      precDQL == 'E' & accDQL== 'E' ~ 'E',
                                      TRUE ~ 'B'),
                rDQL=dplyr::if_else(Result.Status.ID == "Rejected","C",rDQL)) %>%
  dplyr::select(-Deployment.Start.Date, -Deployment.End.Date, -date) %>%
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

df5.results.anom <- odeqcdr::anomaly_check(results=df5.results, deployment=df1.deployment, return_df=TRUE)


#- Output for further review using Shiny Tool ----------------------------------

# list to export to Shiny
shiny_list <-list(Deployment=df1.deployment,
                  Audit_Stats=df3.audits.dql,
                  Results_Anom=df5.results.anom)

save(shiny_list, file=paste0(gsub(".xlsx","",xlsx_output),"_CDR.Rdata"))

# Launch Shiny app for further review.
odeqcdr::launch_shiny()

#- Make DQL and Status edits based on Shiny Review------------------------------
# Edits can also be made in the xlsx. Just skip this step.

# Results Worksheet edits
reject.rows <- c(NA)
final.rows <- c(NA)
A.rows <- c(NA)
B.rows <- c(NA)
C.rows <- c(NA)
D.rows <- c(NA)
E.rows <- c(NA)
F.rows <- c(NA)

df.results.final <- df4.results %>%
  dplyr::mutate(Result.Status.ID=dplyr::case_when(row.results %in% reject.rows ~ "Rejected",
                                                row.results %in% final.rows ~ "Final",
                                                TRUE ~ Result.Status.ID),
                rDQL=dplyr::case_when(row.results %in% A.rows ~ "A",
                                      row.results %in% B.rows ~ "B",
                                      row.results %in% C.rows ~ "C",
                                      row.results %in% D.rows ~ "D",
                                      row.results %in% E.rows ~ "E",
                                      row.results %in% F.rows ~ "F",
                                      TRUE ~ rDQL))

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

# Export
odeqcdr::contin_export(file=paste0(output_dir, "/", xlsx_output),
                       org=df0.org,
                       projects=df1.projects,
                       mloc=df1.mloc,
                       deployment=df1.deployment,
                       results=df.results.final,
                       prepost=df0.prepost,
                       audits=df2.audits,
                       sumstats=df.sumstats)
