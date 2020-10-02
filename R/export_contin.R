#' Export continuous result information from to template xlsx file.
#'
#' Export monitoring results into Oregon DEQ's continuous data submission template xlsx file format v2.03.
#' This function will export a list with each element holding a data frame of the information for each spreadsheet in the template.
#' The cell colors and quality control elements from the actual template are not included in the output
#'
#' @param file The path and file name to the output xlsx file.
#' @param org data frame holding Organization Details
#' @param projects data frame with Projects
#' @param mloc data frame holding Monitoring_Locations
#' @param deployment data frame holding Deployment
#' @param results data frame holding Results
#' @param prepost data frame holding PrePost
#' @param audits data frame holding Audit_Data
#' @export

export_contin <- function(file, org, projects, mloc, deployment, results, prepost, audits) {

  # remove period from column names
  names(projects) <- gsub("\\."," ", names(projects))
  names(mloc) <- gsub("\\."," ", names(mloc))
  names(deployment) <- gsub("\\."," ", names(deployment))
  names(results) <- gsub("\\."," ", names(results))
  names(prepost) <- gsub("\\."," ", names(prepost))
  names(audits) <- gsub("\\."," ", names(audits))

  #filter and arrange by column names in template
  projects <- projects[, odeqcdr::cols_projects()]
  mloc <- mloc[, odeqcdr::cols_mloc()]
  deployment <- deployment[, odeqcdr::cols_deploy()]
  results <- results[, odeqcdr::cols_results()]
  prepost <- prepost[, odeqcdr::cols_prepost()]
  audits <- audits[,odeqcdr::cols_audit()]

  xlsx_list <- list(org, projects, mloc, deployment, results, prepost, audits)
  names(xlsx_list) <- c("Organization Details",
                        "Projects",
                        "Monitoring_Locations",
                        "Deployment",
                        "Results",
                        "PrePost",
                        "Audit_Data")

  openxlsx::write.xlsx(xlsx_list,
                       file=file,
                       colWidths="auto",
                       firstActiveRow=c(5,2,2,2,2,2,2),
                       firstRow=c(FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE),
                       rowNames=c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE), borders="rows",
                       startCol=c(2,1,1,1,1,1,1), startRow=c(5,1,1,1,1,1,1),
                       headerStyle=openxlsx::createStyle(fgFill = "#000000", halign = "LEFT", textDecoration = "Bold",
                                                         wrapText = TRUE, border = "Bottom", fontColour = "white",
                                                         fontName = "Arial", fontSize = 10))

  wb <- openxlsx::loadWorkbook(file=file, isUnzipped = FALSE)

  openxlsx::modifyBaseFont(wb, fontSize = 10, fontName = "Arial")

  for (sheet_name in names(xlsx_list[2:length(xlsx_list)])) {

    openxlsx::setColWidths(wb, sheet=sheet_name, cols=c(1:ncol(xlsx_list[[sheet_name]])), widths = "auto")

    openxlsx::addStyle(wb, sheet=sheet_name,
                       rows=c(1:nrow(xlsx_list[[sheet_name]])),
                       cols=c(1:ncol(xlsx_list[[sheet_name]])),
                       stack=TRUE, gridExpand = TRUE,
                       style=openxlsx::createStyle(valign = "top", fontName = "Arial", fontSize = 10))

  }

  # Format datesand times
  date_format <- openxlsx::createStyle(numFmt = "yyyy/mm/dd")
  time_format <- openxlsx::createStyle(numFmt = "hh:mm")
  openxlsx::addStyle(wb,  sheet="Deployment", style = date_format, rows = 2:(nrow(deployment)+1), cols = c(4:5), stack=TRUE, gridExpand = TRUE)
  openxlsx::addStyle(wb,  sheet="Results", style = date_format, rows = 2:(nrow(results)+1), cols = 2, stack=TRUE, gridExpand = TRUE)
  openxlsx::addStyle(wb,  sheet="Results", style = time_format, rows = 2:(nrow(results)+1), cols = 3, stack=TRUE, gridExpand = TRUE)
  openxlsx::addStyle(wb,  sheet="Audit_Data", style = date_format, rows = 2:(nrow(audits)+1), cols = c(5, 7), stack=TRUE, gridExpand = TRUE)
  openxlsx::addStyle(wb,  sheet="Audit_Data", style = time_format, rows = 2:(nrow(audits)+1), cols = c(6, 8), stack=TRUE, gridExpand = TRUE)

  openxlsx::setColWidths(wb, sheet="Organization Details", cols=c(2:3), widths = c(20,100))

  openxlsx::addStyle(wb, sheet="Organization Details", rows=c(6:19), cols=c(2:3), stack=TRUE, gridExpand = TRUE,
                     style=openxlsx::createStyle(wrapText = TRUE, valign = "top", fontName = "Arial", fontSize = 9))

  openxlsx::addStyle(wb, sheet="Organization Details", rows=c(6:19), cols=2, stack=TRUE,
                     style=openxlsx::createStyle(textDecoration="bold", fontName = "Arial", fontSize = 9))

  openxlsx::saveWorkbook(wb, file=file, overwrite = TRUE)

}
