##############################################################################################
#' @title Convert a number of bytes into megabytes or gigabytes

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' For any number of bytes, convert to a number of MB or GB
#'
#'
#' @param objSize The size in bytes
#' @return The size of the file in megabytes or gigabytes
#'
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2020-09-24)
##############################################################################################

convByteSize <- function(objSize){
  mbs <- objSize/1e6
  if(mbs < 1000) {
    return(paste(mbs, " MB", sep=""))
  } else {
    gbs <- objSize/1e9
    return(paste(gbs, " GB", sep=""))
  }
}
