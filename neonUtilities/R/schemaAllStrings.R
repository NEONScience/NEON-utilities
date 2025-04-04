##############################################################################################
#' @title Create an arrow schema with every variable coded as a string field.

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Use the field names in a NEON variables file to create an arrow schema of all strings, or, if no variables file is available, read the header and assign everything as string.
#'
#' @param variables A data frame containing a NEON variables file for a single table, or a set of field names.
#' 
#' @return An arrow schema for the relevant data table with all variables set to string.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2025-03-12)
##############################################################################################

schemaAllStrings <- function(variables) {

  # still need to build in option for set of names, without full variables file
  # schemaFromVar() is now set to only call this function if the variables file is available
  # so only need the names option if this function is called from elsewhere in the future
  stringschema <- arrow::schema(unlist(apply(variables, MARGIN=1, FUN=function(x) {
    arrow::field(name=x["fieldName"], type=arrow::string())
  })))
  
  return(stringschema)
  
}

