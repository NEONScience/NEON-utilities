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
  # if dpID isn't on the list, infer schema
  if(!dpID %in% frame_file_codes$dpID) {
    noschema <- TRUE
  } else {
    module <- frame_file_codes$code[which(frame_file_codes$dpID==dpID)]
    vartab <- frame_file_variables[which(frame_file_variables$table==module),]
    noschema <- FALSE
  }
  
  # assign table name
  tabnm <- "per_sample"
  if(dpID=="DP1.20190.001") {
    tabnm <- "rea_conductivityRawData"
  } else {
    if(dpID=="DP1.20193.001") {
      tabnm <- "sbd_conductivityRawData"
    } else {
      if(dpID=="DP4.00132.001") {
        tabnm <- "bat_processedSonarFile"
      } else {
        tabnm <- 
      }
    }
  }
}
  
  if(isFALSE(noschema)) {
    
    # get schema from custom variables file and read dataset
    frameschema <- schemaFromVar(vartab, tab=module, package="expanded")
    fdat <- arrow::open_csv_dataset(sources=framefiles, 
                                    schema=frameschema, skip=1)
    fdattab <- try(data.frame(dplyr::collect(fdat)), silent=TRUE)
    
    # if stacking fails, redirect to infer the schema
    if(inherits(fdattab, "try-error")) {
      noschema <- TRUE
    }
    
  }
  
  if(isTRUE(noschema)) {
    
    message(paste("Variables file was not found or was inconsistent for table ", tabnm, ". Schema will be inferred; performance may be reduced.", sep=""))
    fsdat <- arrow::open_csv_dataset(sources=framefiles, col_names=TRUE, 
                                     unify_schemas=TRUE, skip=0)
    fdattab <- try(data.frame(dplyr::collect(fsdat)), silent=TRUE)
    if(inherits(fdattab, "try-error")) {
      message(paste("Stacking table ", tabnm, " failed. Check inputs for inconsistencies.", sep=""))
    }
    
  }
  
  return(list(fdattab, tabnm))
  
}

