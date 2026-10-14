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
  
  frame_file_variables <- try(utils::read.csv("https://raw.githubusercontent.com/NEONScience/NEON-utilities/refs/heads/main/helper_files/frame_file_variables.csv"),
                              silent=TRUE)
  frame_defs <- try(utils::read.csv("https://raw.githubusercontent.com/NEONScience/NEON-utilities/refs/heads/main/helper_files/frame_file_codes.csv"),
                    silent=TRUE)
  frame_names <- try(utils::read.csv("https://raw.githubusercontent.com/NEONScience/NEON-utilities/refs/heads/main/helper_files/frame_names.csv"),
                     silent=TRUE)
  if(inherits(frame_defs, "try-error")) {
    message("Internet connection error. Some metadata may rely on cached versions.")
    frame_defs <- frame_file_codes
    frame_file_variables <- frame_file_variables_internal
    frame_names <- frame_names_internal
  }
  
  # get custom variables file for relevant dpID
  # if dpID isn't on the list, infer schema
  if(!dpID %in% frame_defs$dpID) {
    noschema <- TRUE
  } else {
    module <- frame_defs$code[which(frame_defs$dpID==dpID)]
    vartab <- frame_file_variables[which(frame_file_variables$table==module),]
    noschema <- FALSE
  }
  if(length(vartab)==0) {
    noschema <- TRUE
  }
  
  # assign table name
  if(dpID %in% frame_names$dpID) {
    tabnm <- frame_names$tableName[which(frame_names$dpID==dpID)]
  } else {
    tabnm <- "per_sample"
  }
  
  # add sequence type suffix if needed
  if(grepl(pattern="[_]$", tabnm)) {
    tabnm <- paste(tabnm, seqType, sep="")
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
      # try string schema for each file and unify
      stringtablist <- list()
      stringpiecewise <- TRUE
      for(p in 1:length(framefiles)) {
        
        stringschema <- schemaAllStringsFromSet(framefiles[p])
        
        ds <- try(arrow::open_csv_dataset(sources=framefiles[p], 
                                          schema=stringschema,
                                          skip=1), silent=TRUE)
        if(inherits(ds, "try-error")) {
          stringpiecewise <- FALSE
          next
        } else {
          stringtablist[[p]] <- ds
        }
        
      }
      
      if(isFALSE(stringpiecewise)) {
        message(paste("Reading data as strings failed for table ", tabnm, ". Try excluding provisional data, and contact NEON if unable to resolve.", sep=""))
        return(invisible())
      } else {
        # if all chunks succeeded, merge them
        ds <- try(arrow::open_csv_dataset(sources=stringtablist, 
                                          unify_schemas=TRUE,
                                          skip=0), silent=TRUE)
        
        fdattab <- try(data.frame(dplyr::collect(ds)), silent=TRUE)
        
        if(inherits(fdattab, "try-error")) {
          message(paste("Reading data as strings failed for table ", tabnm, ". Try excluding provisional data, and contact NEON if unable to resolve.", sep=""))
          return(invisible())
        }
      }
    }
  }
  
  return(list(fdattab, tabnm))
  
}

