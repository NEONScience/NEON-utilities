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
      if(s != "sensor_positions" & s != "science_review_flags" & length(grep('_', s, fixed=T))>0) {
        t <- c(t, s)
      }
    }
  }
  
  if(length(t)==0) {
    stop("No data tables found, only metadata. Try downloading expanded package, and check availability on the NEON data portal.")
  }
  
  tn <- unique(t)
  tt <- character(length(tn))
  
  for(k in 1:length(tn)) {
    names.k.all <- splitNames[union(grep(paste(".", tn[k], ".", sep=""), dt, fixed=T),
                                grep(paste(".", tn[k], "_pub.", sep=""), dt, fixed=T))]
    tt.all <- lapply(names.k.all, function(xn) {
      if(length(which(xn==""))>0) {
        xn <- xn[-which(xn=="")]
      }
      if(length(xn)==5) {
        return("lab")
      } else {
        if(length(grep("[0-9]{4}-[0-9]{2}", xn))>0) {
          return("site-date")
        }
        else {
          return("site-all")
        }
      }
    })
    tt.temp <- unique(unlist(tt.all))
    if(length(tt.temp)>1) {
      stop(paste("In files to be stacked, table ", tn[k], 
                 " has been published under conflicting schedules. To avoid this problem, either work only with released data, or stack released and provisional data separately.", 
                 sep=""))
    }
    tt[k] <- tt.temp
  }
  
  tf <- cbind(tn,tt)
  tf <- data.frame(tf)
  names(tf) <- c("tableName","tableType")
  
  return(tf)
}
