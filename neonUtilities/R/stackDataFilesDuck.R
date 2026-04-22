##############################################################################################
#' @title Align data files into a duckdb dataset

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description Takes a set of paths pointing to data files and a variables file and attempts to unify them into a duckdb dataset.
#'
#' @param urls The set of urls to be combined into a dataset.
#' @param varset A list of urls pointing to the set of variables files relevant to the url set.
#' @param tabl The table name of the table the url set represents.
#' @param package Basic or expanded data package?
#' 
#' @return A duckdb dataset for the input data paths.

#' @export

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2026-04-21 Claire Lunch: Adapted from workflow in datasetQuery().
#   
##############################################################################################

stackDataFilesDuck <- function(urls,
                               varset,
                               tabl,
                               package) {
  
  # start with variables file returned by queryFiles
  trystring <- FALSE
  onevar <- FALSE
  
  # check for inconsistencies in variables files
  if(length(varset)>1) {
    
    # check for differences in fieldNames and dataTypes for the relevant table
    varFieldDiff <- checkVarFields(variableSet=varset, tableName=tabl)
    if(isTRUE(varFieldDiff)) {
      
      # if there are inconsistencies, infer schema
      # add table name to messages
      message("Differences in variables files detected. Schema will be inferred. If this causes errors, try querying released and provisional data separately.")
      ds <- try(duckdbfs::open_dataset(sources=urls, 
                                       unify_schemas=TRUE,
                                       format="csv"), silent=TRUE)
      if(inherits(ds, "try-error")) {
        trystring <- TRUE
      }
        
    } else {
      # if fieldNames and dataTypes match across files, use first variables file
      varend <- arrow::read_csv_arrow(varset[[1]], col_names=TRUE, skip=0)
      onevar <- TRUE
    }
  }
  
  if(length(varset)==1) {
    varend <- arrow::read_csv_arrow(varset[[1]], col_names=TRUE, skip=0)
  }
  
  if(length(varset)==1 | isTRUE(onevar)) {
    tableschema <- schemaFromVarDuck(varend,
                                 tab=tabl,
                                 package=package)
    ds <- try(duckdbfs::open_dataset(sources=urls, 
                                     parser_options = c(
                                       columns=tableschema[[1]],
                                       header=TRUE,
                                       filename=TRUE,
                                       timestampformat=tableschema[[2]]
                                     ),
                                      format="csv"), silent=TRUE)
    if(inherits(ds, "try-error")) {
      trystring <- TRUE
    }
  }
  
  # if making dataset via the paths above failed, try a string schema
  # update message: this now shows up in loadByProduct() when cloud.mode=T
  if(isTRUE(trystring)) {
    message("Data retrieval using variables file to generate schema failed. All fields will be read as strings. This can be slow, and will reduce the possible types of queries you can make. This can usually be avoided by excluding provisional data, and if that does not resolve the problem, consider downloading data using loadByProduct().")
    ds <- try(duckdbfs::open_dataset(sources=urls, 
                                     parser_options = c(
                                       all_varchar=TRUE,
                                       header=TRUE,
                                       filename=TRUE
                                     ),
                                     format="csv"), silent=TRUE)
    if(inherits(ds, "try-error")) {
      message("Reading data as strings failed. Try excluding provisional data, and contact NEON if unable to resolve.")
      return(invisible())
    }
  }
  
  return(ds)
  
}
