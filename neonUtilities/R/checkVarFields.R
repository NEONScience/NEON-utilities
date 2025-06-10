##############################################################################################
#' @title Check for differences in field names among variables files

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' For a set of variables files, check whether there are any differences in the set of field names and data types for a particular table
#'
#' @keywords internal
#' @param variableSet A list of file paths or urls to variables files
#' @param tableName Name of table to check for differences
#' @return TRUE or FALSE: were there any mismatches in field names and data types among the files?

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2025-06-10 (Claire Lunch): Created original function
##############################################################################################

checkVarFields <- function(variableSet, tableName) {
  
  var1 <- data.frame(arrow::read_csv_arrow(variableSet[[1]], col_names=TRUE, skip=0))
  var1 <- var1[which(var1$table==tableName),]
  varany <- unlist(lapply(variableSet, FUN=function(x) {
    varx <- data.frame(arrow::read_csv_arrow(x, col_names=TRUE, skip=0))
    varx <- varx[which(varx$table==tableName),]
    if(nrow(var1)!=nrow(varx)) {
      tst <- FALSE
    } else {
      tst <- any(varx$fieldName!=var1$fieldName | varx$dataType!=var1$dataType)
    }
    return(tst)
  }))
  
  return(any(!varany))
}
