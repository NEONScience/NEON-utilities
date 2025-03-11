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

  # translate data types to arrow types
  # this doesn't work at all - can't write arrow data types to data frame cells.
  variables$dtype <- arrow::string()
  variables$dtype[which(variables$dataType=="real")] <- arrow::float64()
  variables$dtype[which(variables$dataType %in% c("integer", "unsigned integer",
                                                  "signed integer"))] <- arrow::int64()
  variables$dtype[which(variables$dataType=="dateTime" & 
                          variables$pubFormat %in% c("yyyy-MM-dd'T'HH:mm:ss'Z'(floor)",
                                                     "yyyy-MM-dd'T'HH:mm:ss'Z'",
                                                     "yyyy-MM-dd'T'HH:mm:ss'Z'(round)"))] <- arrow::timestamp("s", tz="UTC")
  variables$dtype[which(variables$dataType=="dateTime" & 
                          variables$pubFormat %in% c("yyyy-MM-dd(floor)", "yyyy-MM-dd")] <- arrow::date64()
  variables$dtype[which(variables$dataType=="dateTime" & 
                          variables$pubFormat %in% c("yyyy(floor)", "yyyy(round)")] <- arrow::int64()
  
  dum <- unlist(lapply(variables, FUN=function(x) {
    arrow::field(name=x$table, type=x$dtype)
  }))
  
}

