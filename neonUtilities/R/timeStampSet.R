##############################################################################################
#' @title Generate a consensus set of time stamps from a set of input tables.

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Generate consensus SAE time stamps from a set of tables. Used in stackEddy(), not intended for independent use.
#'
#' @keywords internal
#' @param tabList A list of SAE data tables
#' @return A table of time stamps (start and end times) aggregated from the input tables

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2023-05-30)
##############################################################################################

timeStampSet <- function(tabList){

  nameSet <- c("timeBgn","timeEnd")
  
  # get a set of time stamps to initiate the table. leave out qfqm to exclude 
  # filler records created as placeholders for days with no data
  timeSet <- tabList[grep("qfqm", names(tabList), invert=T)]
  # turbulent flux and footprint end time stamps don't quite match the others
  timeSet <- timeSet[grep("turb", names(timeSet), invert=T)]
  timeSet <- timeSet[grep("foot", names(timeSet), invert=T)]
  
  # initiate the table with consensus set of time stamps
  timeSetInit <- timeSet[[1]][,nameSet]
  if(length(timeSet)==1) {
    timeSetInit <- timeSetInit
  } else {
    for(q in 2:length(timeSet)) {
      # check for additional start time stamps
      timeSetTemp <- timeSet[[q]][,nameSet]
      timeSetTempMerg <- data.table::as.data.table(timeSetTemp[,"timeBgn"])
      timeSetInitMerg <- data.table::as.data.table(timeSetInit[,"timeBgn"])
      misTime <- data.table::fsetdiff(timeSetTempMerg, timeSetInitMerg)
      if(nrow(misTime)==0) {
        timeSetInit <- timeSetInit
      } else {
        # combine all, then de-dup
        allTime <- data.table::rbindlist(list(timeSetInit, timeSetTemp), fill=TRUE)
        timeSetInit <- as.data.frame(unique(allTime, by="timeBgn"))
      }
    }
  }
  
  return(data.table::as.data.table(timeSetInit))
  
}

