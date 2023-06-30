##############################################################################################
#' @title Convert date stamps from character and check for only one record in a day

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Convert SAE time stamps to POSIX and check for missing data
#'
#' @keywords internal
#' @param tab A table of SAE data
#' @param useFasttime Should the fasttime package be used to convert time stamps?
#' @return The same table of SAE data, with time stamps converted and empty records representing a single day (filler records inserted during processing) removed.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2019-11-08)
##############################################################################################

eddyStampCheck <- function(tab, 
                           useFasttime=FALSE){

  # convert time stamps
  tBgnErr <- FALSE
  tEndErr <- FALSE
  
  if(useFasttime) {
    tabBP <- try(fasttime::fastPOSIXct(tab$timeBgn, tz='GMT'), silent=T)
    tabEP <- try(fasttime::fastPOSIXct(tab$timeEnd, tz='GMT'), silent=T)
  } else {
    tabBP <- try(as.POSIXct(tab$timeBgn, format='%Y-%m-%dT%H:%M:%OS', tz='GMT'), silent=T)
    tabEP <- try(as.POSIXct(tab$timeEnd, format='%Y-%m-%dT%H:%M:%OS', tz='GMT'), silent=T)
  }
  
  if(any(c(inherits(tabBP,'try-error'), all(is.na(tabBP))))) {
    tBgnErr <- TRUE
  }
  
  if(any(c(inherits(tabBP,'try-error'), all(is.na(tabEP))))) {
    tEndErr <- TRUE
  }
  
  # if conversion failed, keep character time stamps and pass along message
  tabN <- tab
  err <- FALSE
  if(tBgnErr) {
    err <- TRUE
  } else {
    tabN$timeBgn <- tabBP
  }
 
  if(tEndErr) {
    err <- TRUE
  } else {
    tabN$timeEnd <- tabEP
  }
  
  # if conversion was successful, check for single-day empty records and remove
  if(err) {
    tabN <- tabN
  } else {
    dayDiff <- base::as.difftime(tabEP - tabBP)
    secDiff <- base::abs(base::as.numeric(dayDiff, units="secs"))
    dayDup <- which(secDiff >= 86399)
    if(length(dayDup)==0) {
      tabN <- tabN
    } else {
      tabN <- tabN[-dayDup,]
    }
  }
  
  return(list(tabN, err))
}

