##############################################################################################
#' @title Rename column names in old sensor position files

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Rename columns in old sensor positions files to match current names
#'
#' @keywords internal
#' @param sptable A data frame of sensor positions
#' @return A data frame with updated naming

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch 2026-09-09
##############################################################################################

alignSpCols <- function(sptable){
  
  oldcols <- cbind(c("name","description","start","end",
                     "referenceName","referenceDescription",
                     "referenceStart","referenceEnd",
                     "referenceLatitude","referenceLongitude",
                     "referenceElevation"),
                   c("sensorLocationID","sensorLocationDescription",
                     "positionStartDateTime",
                     "positionEndDateTime","referenceLocationID",
                     "referenceLocationIDDescription",
                     "referenceLocationIDStartDateTime",
                     "referenceLocationIDEndDateTime",
                     "locationReferenceLatitude",
                     "locationReferenceLongitude",
                     "locationReferenceElevation"))
  oldcols <- data.frame(oldcols)
  names(oldcols) <- c("old","new")
  
  if(inherits(sptable, "data.frame")) {
    for(i in 1:length(names(sptable))) {
      if(names(sptable)[i] %in% oldcols$old) {
        names(sptable)[i] <- oldcols$new[which(oldcols$old==names(sptable)[i])]
      }
    }
  } else {
    sptable <- sptable
  }
  
  return(sptable)

}

