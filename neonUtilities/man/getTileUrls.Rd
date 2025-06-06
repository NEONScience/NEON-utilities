% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getTileUrls.R
\name{getTileUrls}
\alias{getTileUrls}
\title{Get and store the file names, S3 URLs, file size, and download status (default = 0) in a data frame}
\usage{
getTileUrls(
  m.urls,
  tileEasting,
  tileNorthing,
  include.provisional,
  token = NA_character_
)
}
\arguments{
\item{m.urls}{The monthly API URL for the AOP tile.}

\item{tileEasting}{A vector containing the easting UTM coordinates of the locations to download.}

\item{tileNorthing}{A vector containing the northing UTM coordinates of the locations to download.}

\item{include.provisional}{T or F, should provisional data be included in downloaded files?}

\item{token}{User specific API token (generated within data.neonscience.org user accounts). Optional.}
}
\value{
A dataframe comprised of file names, S3 URLs, file size, and download status (default = 0)
}
\description{
Produces a data frame that is populated by available tiles for the AOP product.
}
\references{
License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
}
\author{
Claire Lunch \email{clunch@battelleecology.org}
Christine Laney \email{claney@battelleecology.org}
}
\keyword{internal}
