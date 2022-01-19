#' Import continuous information from a template xlsx file.
#'
#' Retrieve monitoring results from Oregon DEQ's continuous data submission template version 2, 3, or volmon.
#' This function will read the template and return a list with each list element holding a
#' data frame of the information for each spreadsheet. Any rows with all NAs are removed.
#'
#' The function returns a named list holding each worksheet from the xlsx. The
#' name of each list element is the same as the xlsx:
#' \itemize{
#'   \item Organization_Details
#'   \item Projects
#'   \item Monitoring_Locations
#'   \item Deployment
#'   \item New_Equipment (version 3 only)
#'   \item Results
#'   \item PrePost
#'   \item Audit_Data
#' }
#'
#' Column names are made into syntactically valid names acceptable by R.
#'
#' This function relies heavily upon the \code{readxl} package.
#'
#' @param file The path and file name to template xlsx file.
#' @param sheets Optional vector identifying sheets to import. Default is a vector of all sheets to be imported.
#' Acceptable values include "Organization Details", "Projects", "Monitoring_Locations", "Deployment", "Results", "PrePost", "Audit_Data" and "New_Equipment" if ver=3.
#' @param ver version of the template. Valid values are ver=2 (the default), ver=3, or "volmon". Default is ver=2.
#' @param project Optional variable identifying project. Only used when ver="volmon". Default is project="ODEQVolMonWQProgram".
#' @param timezone Optional variable setting time zone. Only used when ver="volmon". Default is timezone="PDT".
#' @param append_ordeq Optional boolean to append "-ORDEQ" to monitoring locations. Only used when ver="volmon". Default is TRUE.
#' @seealso \code{\link[readxl]{read_excel}}
#' @export
#' @return list of each continuous template data

contin_import <- function(file,
                          sheets=c("Organization_Details", "Projects", "Monitoring_Locations", "Deployment", "New_Equipment", "Results", "PrePost", "Audit_Data"), ver=2,
                          project = 'ODEQVolMonWQProgram', timezone = "PDT", append_ordeq = TRUE) {

  #library(readxl)
  #file <- "E:/GitHub/ContinuousDataReviewR/ContinuousTemplate_example.xlsx"

  ver <- as.character(ver)

  if(ver=="2") {

    # Remove "New_Equipment" for version 2
    sheets <- sheets[!("New_Equipment" == sheets)]

    template_sheets <- odeqcdr::contin_import_v2(file=file, sheets=sheets)

  }

  if(ver=="3") {

    template_sheets <- odeqcdr::contin_import_v3(file=file, sheets=sheets)

  }

  if(ver=="volmon") {
    template_sheets <- odeqcdr::contin_volmon_import(file=file, project = project,
                                                     timezone = timezone, append_ordeq = append_ordeq)
  }

  return(template_sheets)
}
