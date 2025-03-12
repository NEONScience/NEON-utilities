##############################################################################################
#' @title Create an arrow schema from a NEON variables file.

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Use the field names and data types in a NEON variables file to create an arrow schema.
#'
#' @param variables A data frame containing a NEON variables file for a single table.
#' 
#' @return An arrow schema for the relevant data table.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2025-03-11)
##############################################################################################

schemaFromVar <- function(variables) {

  # start by making a schema with everything as a string
  vschema <- schemaAllStrings(variables)
  
  # translate data types to arrow types for non-character fields
  ind <- which(!variables$dataType %in% c("string", "uri"))
  for(i in ind) {
    if(variables$dataType[i]=="real") {
      vschema[[i]] <- arrow::field(name=variables$fieldName[i], type=arrow::float64())
    }
    if(variables$dataType[i] %in% c("integer", "unsigned integer",
                                    "signed integer")) {
      vschema[[i]] <- arrow::field(name=variables$fieldName[i], type=arrow::int64())
    }
    if(variables$dataType[i]=="dateTime" & 
       variables$pubFormat[i] %in% c("yyyy-MM-dd'T'HH:mm:ss'Z'(floor)",
                                  "yyyy-MM-dd'T'HH:mm:ss'Z'",
                                  "yyyy-MM-dd'T'HH:mm:ss'Z'(round)")) {
      vschema[[i]] <- arrow::field(name=variables$fieldName[i], 
                                        type=arrow::timestamp("s", tz="UTC"))
    }
    if(variables$dataType[i]=="dateTime" & 
       variables$pubFormat[i] %in% c("yyyy-MM-dd(floor)", "yyyy-MM-dd")) {
      vschema[[i]] <- arrow::field(name=variables$fieldName[i], type=arrow::date32())
    }
    if(variables$dataType[i]=="dateTime" & 
       variables$pubFormat[i] %in% c("yyyy(floor)", "yyyy(round)")) {
      vschema[[i]] <- arrow::field(name=variables$fieldName[i], type=arrow::int64())
    }
    
  }
  
  return(vschema)
  
}

