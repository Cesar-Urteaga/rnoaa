% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-functions.R
\name{download_earthquake_data}
\alias{download_earthquake_data}
\title{Downloads the latest NOAA's Significant Earthquake dataset (requires
internet access).}
\usage{
download_earthquake_data()
}
\value{
Returns a \href{https://blog.rstudio.org/2016/03/24/tibble-1-0-0/}{tibble}
object.
}
\description{
The function \code{download_earthquake_data} gets the most recent earthquake
data from \href{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}{the NOAA's Webpage}.
}
\details{
For more information about this dataset, please read the documentation of
\code{\link{get_earthquake_data}}.
}
\examples{
raw_data <- download_earthquake_data()
}
\seealso{
\code{\link{eq_clean_data}} and \code{\link{eq_location_clean}} for
data tidying.
}
