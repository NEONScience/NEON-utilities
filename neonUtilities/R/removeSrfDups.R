##############################################################################################
#' @title Aggregate science review flag files to unique records

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Aggregate repeated science review flags to a unique set matching the download
#'
#' @keywords internal
#' @param srftable A data frame of science review flags
#' @return A data frame of science review flags with duplicates removed

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch 2025-05-30
##############################################################################################

removeSrfDups <- function(srftable){
  
  # remove fully identical duplicates
  outputScienceReview <- unique(srftable)
  
  # check for non-identical duplicates with the same ID and keep the most recent one
  if(length(unique(outputScienceReview$srfID))!=nrow(outputScienceReview)) {
    dupRm <- numeric()
    rowids <- 1:nrow(outputScienceReview)
    origNames <- colnames(outputScienceReview)
    outputScienceReview <- cbind(rowids, outputScienceReview)
    for(k in unique(outputScienceReview$srfID)) {
      scirvwDup <- outputScienceReview[which(outputScienceReview$srfID==k),]
      if(nrow(scirvwDup)>1) {
        maxk <- scirvwDup$rowids[which(scirvwDup$lastUpdateDateTime==max(scirvwDup$lastUpdateDateTime))]
        if(length(maxk)>1) {
          maxk <- maxk[1]
        }
        dupRmk <- scirvwDup$rowids[which(scirvwDup$rowids!=maxk)]
        dupRm <- c(dupRm, dupRmk)
      }
    }
    if(length(dupRm)>0) {
      outputScienceReview <- outputScienceReview[-dupRm,origNames]
    } else {
      outputScienceReview <- outputScienceReview[,origNames]
    }
  }
  
  return(outputScienceReview)

}

