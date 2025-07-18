% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/schemaAllStringsFromSet.R
\name{schemaAllStringsFromSet}
\alias{schemaAllStringsFromSet}
\title{Create an arrow schema with every variable coded as a string field, and fields read from file headers.}
\usage{
schemaAllStringsFromSet(fileset)
}
\arguments{
\item{fileset}{A vector of file paths}
}
\value{
An arrow schema for the relevant files with all variables set to string.
}
\description{
Take a set of files, read their header rows to get field names, and create a schema with all fields set to string.
}
\references{
License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
}
\author{
Claire Lunch \email{clunch@battelleecology.org}
}
\keyword{internal}
