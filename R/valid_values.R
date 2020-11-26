#' Return a vector of valid values for a column.
#'
#' This function will return valid values for certain columns in the continuous
#' data submission template xlsx file v2.03. NA is included as a valid value for
#' columns that are optional or conditional.
#'
#' @param col Name of the column in the continuous data submission template xlsx file v2.03 to return valid values.
#' The name can be formatted as shown in the xlsx template or in R format derived using [make.names()]. if col=NULL All valid values are
#' returned as a list with the column name as the list lookup. Default is NULL
#' @export
#' @return vector of valid values or list of all valid values.

valid_values <- function(col=NULL) {

  #- General -------------------------------------------------------------------

  Characteristic.Name <- c("Algae, blue-green (phylum cyanophyta) density",
                           "Chlorophyll",
                           "Chlorophyll a",
                           "Conductivity",
                           "Depth",
                           "Depth, data-logger (non-ported)",
                           "Depth, data-logger (ported)",
                           "Dissolved oxygen (DO)",
                           "Dissolved oxygen saturation",
                           "Flow",
                           "pH",
                           "Salinity",
                           "Temperature, air",
                           "Temperature, hyporheic",
                           "Temperature, water",
                           "Temperature, wet bulb",
                           "Turbidity")

  Activity.Start.End.Time.Zone <- c("PST",
                                    "PDT",
                                    "MST",
                                    "MDT",
                                    "America/Los_Angeles",
                                    "America/Boise", NA)

  Result.Unit <- c("cfm", "cfs", "MGD", "deg C", "deg F", "umho/cm", "ft", "in", "m",
                   "ug/l", "mg/l", "ppth", "%", "FTU", "NTU", "SU", "pH Units", "%saturatn",
                   "cm", "cm/sec", "count", "counts/sec", "cm3", "cm3/hr",
                   "cm3/l", "cm3/min", "cm3/sec", "m3/hr", "m3/min", "m3/sec", "um3/l",
                   "um3/ml", "um3/cm2", "mm3/l", "yd3", "dm", "umho", "mho/cm",
                   "mmhos/cm", "ft/day", "ft/min", "ft/sec", "gal/day", "gal/hr",
                   "gal/min", "gal/sec", "kg/m3", "Kg/m3-1000", "kg/m2", "kg/t CaCO3",
                   "km/hr", "km/sec", "l/day", "l/hr", "L/mg-cm", "l/min", "l/sec",
                   "m/sec", "metric ton", "MT/km2/yr", "umol/S/m2", "umol/m2/s", "ug",
                   "ug/cm3", "ug/m3", "ug/kg", "ug/cm2/day", "ug/cm2", "ug/m2",
                   "ug/cm2-day", "ug/m2-hr", "umol/g", "umol/kg", "umol/L", "micron",
                   "uS/cm", "uW/cm2", "mg/cm3", "mg/m3", "mg/m3/day", "mg/m3/hr",
                   "mg/day", "mg/g", "mg/hr", "mg/kg", "mg/l CaCO3", "mg/ml", "mg/min",
                   "mg/sec", "mg/cm2", "mg/m2", "mg/m2/day", "mg/m2/hr", "mg/m2-day",
                   "mg/m2-hr", "ml/l", "mS/cm", "ng/g", "ng/kg", "ng/l", "None",
                   "Normal", "nu", "ppb", "ppm", "ppt", "per m", "% CaCO3", "PSS", "PSU",
                   "mmHg", "S/m", "AU", "BU", "FBU", "FNMU", "FNRU", "FNU", "FAU", "JTU",
                   "NTMU", "NTRU")

  Result.Status.ID <- c("Accepted",
                        "Final",
                        "Preliminary",
                        "Rejected",
                        "Validated")

  #- Organizational Details ----------------------------------------------------

  org_key <- c("ORGANIZATION NAME",
               "DESCRIPTION OF ORGANIZATION",
               "ADDRESS (INCLUDE MAILING IF DIFFERENT FROM PHYSICAL)",
               "PHONE",
               "FAX",
               "EMAIL",
               "WEB ADDRESS",
               "CONTACT PERSON (INCLUDE ADDRESS/PHONE/EMAIL IF DIFFERENT FROM ORGANIZATION)",
               "TYPE(S) OF DATA (e.g. water quality, macroinvertebrate, pebble, etc.)",
               "319 Grant? If so, Years funded",
               "Have you submitted data to DEQ before (Yes/No)? If you are unsure say No.",
               "EPA/USGS ORGANIZATION ID, USERNAME &  PASSWORD",
               "DATE ORG ID/U/P ISSUED:",
               "OTHER PEOPLE WORKING WITH THIS PROJECT THAT HAVE THE USERNAME & PASSWORD:")

  #- Projects ------------------------------------------------------------------

  Approved.QAPP.Indicator <- c("Yes", "No", NA)

  Project.Attachment.Type <- c(".pdf",".zip",".doc", NA)

  #- Monitoring_Locations ------------------------------------------------------


  Monitoring.Location.Type <- c("Canal Transport", "Estuary", "Facility Industrial",
                                "Facility Municipal Sewage (POTW)", "Facility Other", "Lake", "Ocean",
                                "Other-Surface Water", "Pipe, Unspecified Source", "Reservoir",
                                "River/Stream", "Seep", "Spring", "Storm Sewer")

  Horizontal.Datum <- c("NAD27",
                        "NAD83",
                        "WGS84")

  Coordinate.Collection.Method <- c("GPS-Unspecified",
                                    "Interpolation-Map",
                                    "Unknown")

  Tribal.Land. <- c("Yes", "No")

  Tribal.Land <- c("Yes", "No")

  County.Name <- c("BAKER", "BENTON", "CLACKAMAS", "CLATSOP", "COLUMBIA", "COOS",
                   "CROOK", "CURRY", "DESCHUTES", "DOUGLAS", "GILLIAM", "GRANT",
                   "HARNEY", "HOOD RIVER", "JACKSON", "JEFFERSON", "JOSEPHINE",
                   "KLAMATH", "LAKE", "LANE", "LINCOLN", "LINN", "MALHEUR", "MARION",
                   "MORROW", "MULTNOMAH", "POLK", "SHERMAN", "TILLAMOOK", "UMATILLA",
                   "UNION", "WALLOWA", "WASCO", "WASHINGTON", "WHEELER", "YAMHILL", NA)

  State.Code <- c("OR",
                  "WA",
                  "CA",
                  "ID",
                  "NV", NA)

  HUC.8.Code <- c("17100205", "17120009", "17100309", "17070303", "17050201",
                  "17050118", "17050202", "18010205", "17100312", "17090011",
                  "17090002", "17100304", "17100305", "17050109", "17120003",
                  "17050106", "18020001", "17120008", "17120001", "17060101",
                  "17100311", "17060102", "17050108", "17120006", "17070302",
                  "18010204", "17080006", "17080003", "17080001", "17070305",
                  "17070306", "17060106", "17070204", "18010209", "17050117",
                  "17050110", "17100310", "17060103", "17090012", "17090004",
                  "17070105", "17070101", "17070203", "17090001", "17050107",
                  "17100308", "17050115", "17050103", "17090007", "17090009",
                  "17100201", "17100202", "17070202", "17090005", "17100301",
                  "17050203", "17100204", "17100207", "17120004", "17120002",
                  "17100206", "17100306", "18010101", "17050105", "17090006",
                  "17100302", "18010202", "17120005", "16040205", "17070307",
                  "17090010", "17070103", "17100303", "17070304", "17070301",
                  "17060104", "17070201", "18010206", "18010203", "17050116",
                  "16040201", "17100307", "17090003", "17070102", "17060105",
                  "17120007", "18010201", "17050119", "17070104", "17100203",
                  "17090008", NA)

  #- Deployment ----------------------------------------------------------------

  Sample.Depth.Unit <- Result.Unit

  Sample.Media <- c("Air",
                    "Water",
                    "Other")

  Sample.Sub.Media <- c("Ambient Air", "Borrow Soil, Waste Rock, and Protore material",
                        "Const. Material", "Drinking Water", "Dry Fall Material", "Elutriate",
                        "Filter Residue", "Finished Water", "Groundwater", "Indoor Air",
                        "Industrial Effluent", "Industrial Waste", "Interstitial Water",
                        "Lake Sediment", "Leachate", "Mine Tailings Pond",
                        "Municipal Sewage Effluent", "Municipal Waste", "Ocean Water",
                        "Rainwater", "Rock/Cobbles/Gravel", "Septic Effluent", "Sieved Sediment",
                        "Sludge", "Snowmelt", "Soil Gas", "Stack Gases",
                        "Stormwater", "Subsurface Soil/Sediment", "Surface Soil/Sediment",
                        "Surface Water", NA)

  #- PrePost -------------------------------------------------------------------

  Equipment.Result.Unit <- Result.Unit

  Reference.Result.Unit <- Result.Unit

  #- Audit Data ----------------------------------------------------------------

  Activity.Type <- c("Field Measurement/Observation",
                     "Field Measurement/Observation-Portable Data Logger",
                     "Sample-Routine",
                     "Quality Control Field Replicate Portable Data Logger",
                     "Sample-Field Split",
                     "Sample-Field Subsample",
                     "Sample-Integrated Cross-Sectional Profile",
                     "Sample-Integrated Flow Proportioned",
                     "Sample-Integrated Horizontal and Vertical Composite Sample",
                     "Sample-Integrated Horizontal Profile",
                     "Sample-Integrated Time Series",
                     "Sample-Integrated Vertical Profile",
                     "Sample-Negative Control",
                     "Sample-Other",
                     "Sample-Positive Control",
                     "Sample-Routine Resample")

  Sample.Collection.Method <- c("Composite",
                                "Continuous Summar",
                                "Field Meter",
                                "Grab",
                                "Staff Gage")

  Result.Analytical.Method.ID <- c("446",
                                   "120.1",
                                   "120.1",
                                   "360.2",
                                   "8229",
                                   "360.1",
                                   "NFM 6.2.1-LUM",
                                   "150.1",
                                   "150.2",
                                   "170.1",
                                   "180.1",
                                   "D5413(A)")

  Result.Value.Type <- c("Actual",
                         "Blank Corrected Calc",
                         "Calculated",
                         "Control Adjusted",
                         "Estimated", NA)

  Result.Analytical.Method.Context <- c("USEPA",
                                        "YSI",
                                        "HACH",
                                        "USDOI/USGS",
                                        "ASTM", NA)

  Result.Measure.Qualifier <- c("FDC",
                                "PBH",
                                "PBL",
                                "R",
                                "SUS", NA)

  # Add everything to a list
  val_list <-list(Characteristic.Name=Characteristic.Name,
                  Activity.Start.End.Time.Zone=Activity.Start.End.Time.Zone,
                  Result.Unit=Result.Unit,
                  Result.Status.ID=Result.Status.ID,
                  Monitoring.Location.Type= Monitoring.Location.Type,
                  org_key=org_key,
                  Approved.QAPP.Indicator=Approved.QAPP.Indicator,
                  Project.Attachment.Type=Project.Attachment.Type,
                  Horizontal.Datum=Horizontal.Datum,
                  Coordinate.Collection.Method=Coordinate.Collection.Method,
                  Tribal.Land.=Tribal.Land.,
                  Tribal.Land=Tribal.Land,
                  County.Name=County.Name,
                  State.Code=State.Code,
                  HUC.8.Code=HUC.8.Code,
                  Sample.Depth.Unit=Sample.Depth.Unit,
                  Sample.Media=Sample.Media,
                  Sample.Sub.Media=Sample.Sub.Media,
                  Equipment.Result.Unit=Equipment.Result.Unit,
                  Reference.Result.Unit=Reference.Result.Unit,
                  Activity.Type=Activity.Type,
                  Sample.Collection.Method=Sample.Collection.Method,
                  Result.Analytical.Method.ID=Result.Analytical.Method.ID,
                  Result.Value.Type=Result.Value.Type,
                  Result.Analytical.Method.Context=Result.Analytical.Method.Context,
                  Result.Measure.Qualifier=Result.Measure.Qualifier)

  if(is.null(col)) {
    # Return entire list

    return(val_list)

  } else {

    col <- make.names(col, unique=TRUE)

    if(!col %in% names(val_list)) {
      print(paste(col, "is not a column name with valid domain values."))
      stop()
    } else {

      vals <- val_list[[col]]

      return(vals)
    }
  }
}
