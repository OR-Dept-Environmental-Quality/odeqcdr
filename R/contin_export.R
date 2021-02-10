#' Export continuous result information from to template xlsx file.
#'
#' Export monitoring results into the most recent version of Oregon DEQ's continuous data submission template xlsx file format.
#' This function will export a list with each element holding a data frame of the information for each spreadsheet in the template.
#' The cell colors and quality control elements from the actual template are not included in the output xlsx.
#'
#' This function relies upon the [openxlsx] package.
#'
#' @param file The path and file name to the output xlsx file.
#' @param org data frame holding Organization Details
#' @param projects data frame with Projects
#' @param mloc data frame holding Monitoring_Locations
#' @param deployment data frame holding Deployment
#' @param results data frame holding Results
#' @param prepost data frame holding PrePost
#' @param audits data frame holding Audit_Data
#' @param sumstats Optional data frame holding the AWQMS summary statistics generated using [odeqcdr::sumstats]. Default is NULL.
#' @param equipment Optional data frame holding equipment Info. Only used for version 3 template. Default is NULL.
#' @param ver numeric version of the template. Valid values are ver=2 (the default) or ver=3.
#' @seealso [openxlsx], [contin_export_v2], [contin_export_v3]
#' @export

contin_export <- function(file, org, projects, mloc, deployment, results, prepost, audits, sumstats=NULL, equipment=NULL, ver=2) {

  if(ver==2) {
    export <- odeqcdr::contin_export_v2(file=file, org=org, projects=projects,
                                        mloc=mloc, deployment=deployment,
                                        results=results, prepost=prepost,
                                        audits=audits, sumstats=sumstats)
  }

  if(ver==3) {
    export <- odeqcdr::contin_export_v3(file=file, org=org, projects=projects,
                                        mloc=mloc, deployment=deployment,
                                        results=results, prepost=prepost,
                                        audits=audits, sumstats=sumstats,
                                        equipment=equipment)

  }

}
