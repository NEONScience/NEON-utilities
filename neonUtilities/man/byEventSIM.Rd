% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/byEventSIM.R
\name{byEventSIM}
\alias{byEventSIM}
\title{Get site management data by event type.}
\usage{
byEventSIM(
  eventType,
  site = "all",
  startdate = NA,
  enddate = NA,
  metadata = TRUE,
  release = "current",
  include.provisional = FALSE,
  token = NA_character_
)
}
\arguments{
\item{eventType}{The value of eventType to search for. Can be multiple values. See categoricalCodes file for DP1.10111.001 for possible values.}

\item{site}{Either the string 'all', meaning all available sites, or a character vector of 4-letter NEON site codes, e.g. c('ONAQ','RMNP'). Defaults to all.}

\item{startdate}{Either NA, meaning all available dates, or a character vector in the form YYYY-MM, e.g. 2017-01. Defaults to NA.}

\item{enddate}{Either NA, meaning all available dates, or a character vector in the form YYYY-MM, e.g. 2017-01. Defaults to NA.}

\item{metadata}{T or F, should metadata files be included in the download? Defaults to TRUE.}

\item{release}{The data release to be downloaded; either 'current' or the name of a release, e.g. 'RELEASE-2021'. 'current' returns the most recent release, as well as provisional data if include.provisional is set to TRUE. To download only provisional data, use release='PROVISIONAL'. Defaults to 'current'.}

\item{include.provisional}{T or F, should provisional data be included in downloaded files? Defaults to FALSE. See https://www.neonscience.org/data-samples/data-management/data-revisions-releases for details on the difference between provisional and released data.}

\item{token}{User specific API token (generated within data.neonscience.org user accounts)}
}
\value{
A named list containing a data frame of sim_eventData data, matching the query criteria, and, if metadata=TRUE, associated metadata tables such as issue log and citation information. Because this function can retrieve data from any sites and months, the metadata files are retrieved from the most recent data accessed, and the citation file is returned only if a release is specified in the function call.
}
\description{
Query site management data to return records matching a specific eventType.
}
\examples{
\dontrun{
# Search for fires across all NEON event data
sim.fires <- byEventSIM(eventType="fire")

# Search for grazing events at several sites
sim.graz <- byEventSIM(eventType="grazing", site=c("CPER","KONA","MOAB","STER","LAJA"))
}

}
\references{
License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
}
\author{
Claire Lunch \email{clunch@battelleecology.org}
}
