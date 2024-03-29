% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/contin_import_v2.R
\name{contin_import_v2}
\alias{contin_import_v2}
\title{Import continuous information from a template xlsx file.}
\usage{
contin_import_v2(
  file,
  sheets = c("Organization_Details", "Projects", "Monitoring_Locations", "Deployment",
    "Results", "PrePost", "Audit_Data")
)
}
\arguments{
\item{file}{The path and file name to template xlsx file.}

\item{sheets}{Optional vector identifying sheets to import. Default is a vector of all sheets to be imported.
Acceptable values include "Organization_Details", "Projects", "Monitoring_Locations", "Deployment", "Results", "PrePost", and "Audit_Data".}
}
\value{
list of each continuous template data
}
\description{
Retrieve monitoring results from Oregon DEQ's continuous data submission template xlsx file v2.03.
This function will read the template and return a list with each list element holding a
dataframe of the information for each spreadsheet. Any rows with all NAs are removed.
}
\details{
The function returns a named list holding each worksheet from the xlsx. The
name of each list element is the same as the xlsx:
\itemize{
\item Organization_Details
\item Projects
\item Monitoring_Locations
\item Deployment
\item Results
\item PrePost
\item Audit_Data
}

Column names are made into syntactically valid names acceptable by R.

This function relies heavily upon the \code{readxl} package.
}
\seealso{
\code{\link[readxl]{read_excel}}
}
