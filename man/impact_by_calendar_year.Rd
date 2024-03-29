% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impact_central.R
\name{impact_by_calendar_year}
\alias{impact_by_calendar_year}
\title{Calculate impact by calendar year}
\usage{
impact_by_calendar_year(baseline_burden, focal_burden)
}
\arguments{
\item{baseline_burden}{Data frame of baseline burden data this needs to have
columns country, burden_outcome, year, age, value}

\item{focal_burden}{Data frame of focal burden data this needs to have
columns country, burden_outcome, year, age, value}
}
\value{
Vaccine impact by country and year for burden outcomes as a data
frame with columns country, year, burden_outcome and impact
}
\description{
Calculate impact accrued over all ages for a specific year. This calculates
the difference in disease burden between baseline and focal scenarios for a
given year. The baseline scenario can have no vaccination or different
coverage to the focal scenario. This aggregates the impact over all ages
modelled. This does not account for the future disease burden averted through
current vaccine activities.
}
