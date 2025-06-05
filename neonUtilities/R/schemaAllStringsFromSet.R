##############################################################################################
#' @title Create an arrow schema with every variable coded as a string field, and fields read from file headers.

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Take a set of files, read their header rows to get field names, and create a schema with all fields set to string.
#'
#' @param fileset A vector of file paths
#' @keywords internal
#' 
#' @return An arrow schema for the relevant files with all variables set to string.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2025-06-05)
##############################################################################################

schemaAllStringsFromSet <- function(fileset) {

  # iterate over files to read names
  varnms <- character()
  for(i in fileset) {
    varnms <- c(varnms, arrow::open_csv_dataset(i, skip=0)$schema$names)
  }
  varnms <- unique(varnms)
  
  # make schema
  stringschema <- arrow::schema(unlist(sapply(varnms, FUN=function(x) {
    arrow::field(name=x, type=arrow::string())
  })))
  
  return(stringschema)
  
}

