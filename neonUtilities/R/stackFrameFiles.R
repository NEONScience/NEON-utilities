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
  }
  if(dpID=="DP1.20193.001") {
    tabnm <- "sbd_conductivityRawData"
  }
  if(dpID=="DP4.00132.001") {
    tabnm <- "bat_processedSonarFile"
  }
  if(dpID=="DP1.30012.001") {
    tabnm <- "fsp_rawSpectra"
  }
  if(dpID=="DP1.10081.001") {
    tabnm <- paste("mcc_soilPerSampleTaxonomy_", seqType, sep="")
  }
  if(dpID=="DP1.20086.001") {
    tabnm <- paste("mcc_benthicPerSampleTaxonomy_", seqType, sep="")
  }
  if(dpID=="DP1.20141.001") {
    tabnm <- paste("mcc_surfaceWaterPerSampleTaxonomy_", seqType, sep="")
  }
  if(dpID=="DP1.10081.002") {
    tabnm <- paste("mct_soilPerSampleTaxonomy_", seqType, sep="")
  }
  if(dpID=="DP1.20086.002") {
    tabnm <- paste("mct_benthicPerSampleTaxonomy_", seqType, sep="")
  }
  if(dpID=="DP1.20141.002") {
    tabnm <- paste("mct_surfaceWaterPerSampleTaxonomy_", seqType, sep="")
  }
  
  
  if(isFALSE(noschema)) {
    
    # get schema from custom variables file and read dataset
    frameschema <- schemaFromVar(vartab, tab=module, package="expanded")
    fdat <- arrow::open_csv_dataset(sources=framefiles, 
                                    schema=frameschema, skip=1)
    fdattab <- try(data.frame(dplyr::collect(fdat)), silent=TRUE)
    
    # if stacking fails, check for alternate variables file, then redirect to infer the schema
    if(inherits(fdattab, "try-error")) {
      if(paste(module, "2", sep="") %in% frame_file_variables$table) {
        vartab2 <- frame_file_variables[which(frame_file_variables$table==paste(module, "2", sep="")),]
        frameschema <- schemaFromVar(vartab2, tab=paste(module, "2", sep=""), package="expanded")
        fdat <- arrow::open_csv_dataset(sources=framefiles, 
                                        schema=frameschema, skip=1)
        fdattab <- try(data.frame(dplyr::collect(fdat)), silent=TRUE)
      }
      if(inherits(fdattab, "try-error")) {
        noschema <- TRUE
      }
    }
  }
  
  if(isTRUE(noschema)) {
    
    message(paste("Variables file was not found or was inconsistent for table ", tabnm, ". Schema will be inferred; performance may be reduced.", sep=""))
    fsdat <- try(arrow::open_csv_dataset(sources=framefiles, col_names=TRUE, 
                                     unify_schemas=TRUE, skip=0), silent=TRUE)
    if(inherits(fsdat, "try-error")) {
      # if unifying schemas fails, make a string schema from the superset of field names
      message("Inferring schema failed. All fields will be read as strings. This can be slow, and can usually be avoided by excluding provisional data.")
      stringschema <- schemaAllStringsFromSet(framefiles)
      fsdat <- try(arrow::open_csv_dataset(sources=framefiles, 
                                           schema=stringschema,
                                           skip=1), silent=TRUE)
    }
    fdattab <- try(data.frame(dplyr::collect(fsdat)), silent=TRUE)
    if(inherits(fdattab, "try-error")) {
      message(paste("Stacking table ", tabnm, " failed. Try excluding provisional data, and contact NEON if unable to resolve.", sep=""))
      return(invisible())
    }
    
  }
  
  return(list(fdattab, tabnm))
  
}

