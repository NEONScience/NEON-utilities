##############################################################################################
#' @title Add column to data containing name of file

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' In arrow data retrieval, add file name column to tables. Replicated from https://github.com/apache/arrow/blob/main/r/R/dplyr-funcs-augmented.R because add_filename() is unexported in arrow.
#'
#' @keywords internal
#' @return A `FieldRef` Expression that refers to the filename augmented column.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2025-06-17)
##############################################################################################
addFilename <- function() {
  
  arrow::Expression$field_ref("__filename")
  
}
