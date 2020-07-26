##############################################################################################
#' @title Find unique data tables in dataset

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Find the unique data tables that are present in the dataset (e.g., 2 minute vs 30 minute, or pinning vs identification data) and their types, based on the file name formatting. Adapted from findTablesUnique().
#'
#' @keywords internal
#' @param datatables A list of data files
#' @return An array of unique table names and their types

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2017-09-28 (Claire Lunch): created function, based on findTablesUnique()
##############################################################################################

findTablesByFormat <- function(datatables){
  dt <- basename(datatables)
  splitNames <- strsplit(x = dt, split = "\\.")
  t <- character()
  for (i in 1:length(splitNames)){
    for(j in 3:length(splitNames[[i]])){
      s <- splitNames[[i]][j]
      s <- gsub(x = s, pattern = "_pub", replacement = "")
      if(s != "sensor_positions" & length(grep('_', s, fixed=T))>0) {
        t <- c(t, s)
      }
    }
  }
  tn <- unique(t)
  tt <- character(length(tn))
  
  for(k in 1:length(tn)) {
    names.k <- splitNames[union(grep(paste(".", tn[k], ".", sep=""), dt, fixed=T),
                                grep(paste(".", tn[k], "_pub.", sep=""), dt, fixed=T))][[1]]
    if(length(which(names.k==""))>0) {
      names.k <- names.k[-which(names.k=="")]
    }
    if(length(names.k)==4) {
      tt[k] <- "lab"
    } else {
      if(length(grep("[0-9]{4}-[0-9]{2}", names.k))>0) {
        tt[k] <- "site-date"
      }
      else {
        tt[k] <- "site-all"
      }
    }
  }
  
  tf <- cbind(tn,tt)
  tf <- data.frame(tf)
  names(tf) <- c("tableName","tableType")
  
  return(tf)
}
