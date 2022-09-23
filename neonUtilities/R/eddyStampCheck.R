##############################################################################################
#' @title Convert date stamps from character and check for only one record in a day

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Convert SAE time stamps to POSIX and check for missing data
#'
#' @keywords internal
#' @param tab A table of SAE data
#' @return The same table of SAE data, with time stamps converted and empty records representing a single day (filler records inserted during processing) removed.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2019-11-08)
##############################################################################################

eddyStampCheck <- function(tab){

  # convert time stamps
  tBgnErr <- FALSE
  tEndErr <- FALSE
  
  tabBP <- try(as.POSIXct(tab$timeBgn, format='%Y-%m-%dT%H:%M:%OS', tz='GMT'), silent=T)
  if(any(c(class(tabBP)=='try-error', all(is.na(tabBP))))) {
    tBgnErr <- TRUE
  }
  tabEP <- try(as.POSIXct(tab$timeEnd, format='%Y-%m-%dT%H:%M:%OS', tz='GMT'), silent=T)
  if(any(c(class(tabEP)=='try-error', all(is.na(tabEP))))) {
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
  
  # if conversion was successful, check for single-day empty records
  if(err) {
    tabN <- tabN
  } else {
    days <- as.Date(tabN$timeBgn)
    dayDup <- intersect(which(!base::duplicated(days)), 
                        which(!base::duplicated(days, fromLast=T)))
    if(length(dayDup)==0) {
      tabN <- tabN
    } else {
      emptyDays <- numeric()
      for(i in 1:length(dayDup)) {
        if(all(tabN[dayDup[i], base::setdiff(names(tabN), c('timeBgn', 'timeEnd'))]=='NaN', na.rm=T)) {
          emptyDays <- c(emptyDays, dayDup[i])
        }
      }
      if(length(emptyDays)==0) {
        tabN <- tabN
      } else {
        tabN <- tabN[-emptyDays,]
      }
    }
  }
  
  return(list(tabN, err))
}

