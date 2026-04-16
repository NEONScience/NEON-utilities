##############################################################################################
#' @title Create a duckdb schema with every variable coded as a string field.

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Use the field names in a NEON variables file to create a duckdb schema of all strings, or, if no variables file is available, read the header and assign everything as string.
#'
#' @param variables A data frame containing a NEON variables file for a single table, or a set of field names.
#' @param listOrSchema Should the variable set be returned as a list of names and types, or a pasted schema?
#' @keywords internal
#' 
#' @return A duckdb schema for the relevant data table with all variables set to string.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2026-04-16)
##############################################################################################

schemaAllStringsDuck <- function(variables,
                                 listOrSchema="schema") {

  # if input is just a set of names
  if(inherits(variables, "character")) {
    stringset <- unlist(sapply(variables, FUN=function(x) {
      paste("'", x, "'", ": 'VARCHAR'", sep="")
    }))
    stringschema <- paste("{", paste(stringset, collapse=", "), "}", sep="")
  } else {
    # if input is a variables table
    if(inherits(variables, "data.frame")) {
      stringset <- unlist(sapply(variables$fieldName, FUN=function(x) {
        paste("'", x, "'", ": 'VARCHAR'", sep="")
      }))
      stringschema <- paste("{", paste(stringset, collapse=", "), "}", sep="")
    } else {
      # if input isn't one of the above, don't know what to do with it, return null
      message("Variable names and data types could not be identified.")
      return(invisible())
    }
  }
  
  if(listOrSchema=="schema") {
    return(stringschema)
  } else {
    return(stringset) 
  }
  
}

