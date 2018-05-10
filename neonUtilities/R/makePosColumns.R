##############################################################################################
#' @title Create position (horizontal and vertical) columns

#' @author
#' Christine Laney \email{claney@battelleecology.org}

#' @description
#' For instrumented meteorological data products, create position (horizontal and vertical) columns based on values
#' embedded in the file names.
#'
#' @keywords internal
#' @param d A data table
#' @param datafl A data file name
#' @return A data table with new columns

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2017-09-28 (Christine Laney): Created original function
#   2018-04-13 (Christine Laney):
#     * Continuous stream discharge (DP4.00130.001) is an OS product in IS format, and the HOR.VER
#       values given (100.100) are always the same. New HOR and VER columns are not needed in the
#       stacked product.
##############################################################################################

makePosColumns <- function(d, datafl){
  datafl.splitFile <- strsplit(x = datafl, split = "\\/")
  datafl.splitName <- strsplit(x = datafl.splitFile[[1]][length(datafl.splitFile[[1]])], split = "\\.")
  if((datafl.splitName[[1]][4]=="DP4") && (datafl.splitName[[1]][5]=="00130")){return(d)}

  nc <- ncol(d)
  if(length(datafl.splitName[[1]]) %in% c(12,14)){
    if(length(datafl.splitName[[1]]) == 12){
      horPos <- 8
      verPos <- 9
    }
    if(length(datafl.splitName[[1]]) == 14){
      horPos <- 7
      verPos <- 8
    }
    if(!("siteID" %in% names(d))){
      d$domainID <- rep(as.character(datafl.splitName[[1]][2]), nrow(d))
      d$siteID <- rep(as.character(datafl.splitName[[1]][3]), nrow(d))
    }
    d$horizontalPosition <- rep(as.character(datafl.splitName[[1]][horPos]), nrow(d))
    d$verticalPosition <- rep(as.character(datafl.splitName[[1]][verPos]), nrow(d))
    d$horizontalPosition <- as.character(d$horizontalPosition)
    d$verticalPosition <- as.character(d$verticalPosition)
    d <- data.table::setcolorder(d, c((nc+1):(nc+4),1:nc))
  }

  return(d)
}

