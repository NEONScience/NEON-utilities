##############################################################################################
#' @title Find unique data tables in dataset

#' @author
#' Christine Laney \email{claney@battelleecology.org}

#' @description
#' Find the unique data tables that are present in the dataset (e.g., 2 minute vs 30 minute, or pinning vs identification data)
#'
#' @keywords internal
#' @param datatables A list of data files
#' @return An array of unique table names

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2017-09-28 (Christine Laney): created original function
#   2018-04-13 (Christine Laney): updated to check for extra "_pub" appending table names, and remove
##############################################################################################

findTablesUnique <- function(datatables, tabletypes){
  dt <- datatables
  tt <- tabletypes
  splitNames <- strsplit(x = dt, split = "\\.")
  t <- character()
  for (i in 1:length(splitNames)){
    for(j in 1:length(splitNames[[i]])){
      s <- splitNames[[i]][j]
      s <- gsub(x = s, pattern = "_pub", replacement = "")
      if(s %in% tt$tableName){
        t <- c(t, s)
      }
    }
    if(length(splitNames[[i]]) == 4){
      n <- splitNames[[i]][3]
      n <- gsub(x = n, pattern = "_pub", replacement = "")
      t <- c(t, n)
    }
    if(length(splitNames[[i]]) %in% c(10,11)){
      n <- splitNames[[i]][7]
      n <- gsub(x = n, pattern = "_pub", replacement = "")
      t <- c(t, n)
    }
    if(length(splitNames[[i]]) == 12){
      n <- splitNames[[i]][11]
      n <- gsub(x = n, pattern = "_pub", replacement = "")
      t <- c(t, n)
    }
    if(length(splitNames[[i]]) == 14){
      n <- splitNames[[i]][10]
      n <- gsub(x = n, pattern = "_pub", replacement = "")
      t <- c(t, n)
    }
  }
  return(unique(t))
}
