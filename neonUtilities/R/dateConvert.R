##############################################################################################
#' @title Convert date stamps from character

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Attempt to convert date stamps from character, iterating through known NEON date formats
#'
#' @keywords internal
#' @param dates A vector of date values in character format
#' @return A POSIXct vector, if possible; if conversion was unsuccessful, the original vector is returned

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2019-11-08)
##############################################################################################

dateConvert <- function(dates){
  d <- try(as.POSIXct(dates, format='%Y-%m-%dT%H:%M:%S', tz='GMT'), silent=T)
  if(any(c(class(d)=='try-error', all(is.na(d))))) {
    d <- try(as.POSIXct(dates, format='%Y-%m-%dT%H:%M', tz='GMT'), silent=T)
  }
  if(any(c(class(d)=='try-error', all(is.na(d))))) {
    d <- try(as.POSIXct(dates, format='%Y-%m-%dT%H', tz='GMT'), silent=T)
  }
  if(any(c(class(d)=='try-error', all(is.na(d))))) {
    d <- try(as.POSIXct(dates, format='%Y-%m-%d', tz='GMT'), silent=T)
  }
  if(any(c(class(d)=='try-error', all(is.na(d))))) {
    d <- dates
  }
  return(d)
}

