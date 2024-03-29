% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vimpact.R
\name{get_coverage_data}
\alias{get_coverage_data}
\title{Get coverage data for a particular touchstone and vaccine delivery method}
\usage{
get_coverage_data(
  con,
  touchstone,
  delivery_methods,
  focal_scenario_type,
  countries = NULL,
  vaccination_years = NULL
)
}
\arguments{
\item{con}{DB connection}

\item{touchstone}{Touchstone to get data for}

\item{delivery_methods}{List of delivery methods to get data for}

\item{focal_scenario_type}{A character determining from which scenario_type
coverage estimates shall be extracted}

\item{countries}{Optional vector of countries to filter data}

\item{vaccination_years}{Option vector of years to filter data}

\item{baseline_vaccine_delivery}{A list of lists containing vaccine and
activity_type (routine or campaign) describing delivery}

\item{focal_vaccine_delivery}{A list of lists containing vaccine and
activity_type (routine or campaign) describing delivery}
}
\value{
Tibble of coverage data
}
\description{
Get coverage data for a particular touchstone and vaccine delivery method
}
\keyword{internal}
