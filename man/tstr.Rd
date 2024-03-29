% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tstr.R
\name{tstr}
\alias{tstr}
\title{Returns a string of index values corresponding to each vector element that is TRUE}
\usage{
tstr(x, n = 0)
}
\arguments{
\item{x}{Boolean vector. NA values are treated the same as a FALSE.}

\item{n}{Integer value to add to the index. Default is 0. Use n=1 to equal the
row number in excel assuming the header row is the first row.}
}
\value{
string
}
\description{
This works the same as \code{\link[base]{which}} except each index value is collapsed into a single
string. Continuous indices in a range take the form start:stop and non continuous
indices are separated by a comma and space. If all values are FALSE, NA is returned.
NA values in x are treated the same as a FALSE.
}
\seealso{
\code{\link{valid_values}} \code{\link{valid_values_check}}
}
