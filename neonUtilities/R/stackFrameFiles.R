##############################################################################################
#' @title Stack data frame (per sample) files

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Stacking and re-naming workflow for data frame files, published as a data table for each sample
#'
#' @keywords internal
#' @param framefiles A vector of file paths to data frame files
#' @param dpID Data product ID of the product to be stacked
#' @param seqType For microbe community files, 16S or ITS
#' @param cloud.mode Are data stacked in a cloud-to-cloud transfer?
#' @return A data frame of the stacked version of the input tables

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch 2025-06-03
##############################################################################################

stackFrameFiles <- function(framefiles, dpID, 
                            seqType=NA_character_, 
                            cloud.mode=FALSE) {
  
  # get custom variables file for relevant dpID
  # if dpID isn't on the list, use a string schema
  if(!dpID %in% frame_file_codes$dpID) {
    strsch <- TRUE
  } else {
    module <- frame_file_codes$code[which(frame_file_codes$dpID==dpID)]
    vartab <- frame_file_variables[which(frame_file_variables$table==module),]
    strsch <- FALSE
  }
  
  if(isFALSE(strsch)) {
    
    
    
  }
  
}

