##############################################################################################
#' @title Create an arrow schema from a NEON variables file.

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Use the field names and data types in a NEON variables file to create an arrow schema.
#'
#' @param variables A data frame containing a NEON variables file, or a url pointing to a NEON variables file.
#' @param tab The name of the table to generate a schema from.
#' @param package Should the schema be created for the basic or expanded package?
#' 
#' @return An arrow schema for the relevant data table.
#' 
#' @export

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2025-03-11)
##############################################################################################

schemaFromVar <- function(variables, tab, package) {

  unify <- FALSE
  
  # is the input a file or a url?
  if(inherits(variables, "character")) {
    # read in variables file
    vartab <- try(data.frame(arrow::read_csv_arrow(variables)), silent=TRUE)
    vartab <- try(vartab[which(vartab$table==tab),], silent=TRUE)
    if(inherits(vartab, "try-error")) {
      message("There was a problem reading the variables file. Data types will be inferred.")
      unify <- TRUE
    } else {
      if(nrow(vartab)==0) {
        message("There was a problem reading the variables file. Data types will be inferred.")
        unify <- TRUE
      }
    }
  } else {
    vartab <- try(variables[which(variables$table==tab),], silent=TRUE)
    if(inherits(vartab, "try-error")) {
      message("There was a problem reading the variables file. Data types will be inferred.")
      unify <- TRUE
    }
  }
  
  # if reading the file failed, function returns NULL
  if(isTRUE(unify)) {
    vschema <- NULL
  } else {
    
    # if working with the basic package, subset the table
    if(package=="basic") {
      vartab <- vartab[which(vartab$downloadPkg=="basic"),]
    }
    
    # start by making a schema with everything as a string
    vschema <- schemaAllStrings(vartab)
    
    # translate data types to arrow types for non-character fields
    ind <- which(!vartab$dataType %in% c("string", "uri"))
    for(i in ind) {
      if(vartab$dataType[i]=="real") {
        vschema[[i]] <- arrow::field(name=vartab$fieldName[i], type=arrow::float64())
      }
      if(vartab$dataType[i] %in% c("integer", "unsigned integer",
                                      "signed integer")) {
        vschema[[i]] <- arrow::field(name=vartab$fieldName[i], type=arrow::int64())
      }
      if(vartab$dataType[i]=="dateTime" & 
         vartab$pubFormat[i] %in% c("yyyy-MM-dd'T'HH:mm:ss'Z'(floor)",
                                    "yyyy-MM-dd'T'HH:mm:ss'Z'",
                                    "yyyy-MM-dd'T'HH:mm:ss'Z'(round)",
                                    "yyyy-MM-dd'T'HH:mm'Z'(floor)",
                                    "yyyy-MM-dd'T'HH:mm'Z'",
                                    "yyyy-MM-dd'T'HH:mm'Z'(round)",
                                    "yyyy-MM-dd'T'HH'Z'(floor)",
                                    "yyyy-MM-dd'T'HH'Z'",
                                    "yyyy-MM-dd'T'HH'Z'(round)")) {
        vschema[[i]] <- arrow::field(name=vartab$fieldName[i], 
                                     type=arrow::timestamp("s", timezone="UTC"))
      }
      if(vartab$dataType[i]=="dateTime" & 
         vartab$pubFormat[i] %in% c("yyyy-MM-dd(floor)", "yyyy-MM-dd")) {
        vschema[[i]] <- arrow::field(name=vartab$fieldName[i], type=arrow::date32())
      }
      if(vartab$dataType[i]=="dateTime" & 
         vartab$pubFormat[i] %in% c("yyyy(floor)", "yyyy(round)")) {
        vschema[[i]] <- arrow::field(name=vartab$fieldName[i], type=arrow::int64())
      }
      
    }
    
  }
  
  return(vschema)
  
}

