##############################################################################################
#' @title Create an arrow schema with every variable coded as a string field.

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Use the field names in a NEON variables file to create an arrow schema of all strings, or, if no variables file is available, read the header and assign everything as string.
#'
#' @param variables A data frame containing a NEON variables file for a single table, or a set of field names.
#' @keywords internal
#' 
#' @return An arrow schema for the relevant data table with all variables set to string.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2025-03-12)
##############################################################################################

schemaAllStrings <- function(variables) {

  # if input is just a set of names
  if(inherits(variables, "character")) {
    stringschema <- arrow::schema(unlist(sapply(variables, FUN=function(x) {
      arrow::field(name=x, type=arrow::string())
    })))
  } else {
    # if input is a variables table
    if(inherits(variables, "data.frame")) {
      stringschema <- arrow::schema(unlist(apply(variables, MARGIN=1, FUN=function(x) {
        arrow::field(name=x["fieldName"], type=arrow::string())
      })))
    } else {
      # if input isn't one of the above, don't know what to do with it, return null
      message("Variable names and data types could not be identified.")
      return(invisible())
    }
  }
  
  return(stringschema)
  
}

