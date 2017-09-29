##############################################################################################
#' @title Create position (horizontal and vertical) columns

#' @author
#' Christine Laney \email{claney@battelleecology.org}

#' @description
#' For instrumented meteorological data products, create position (horizontal and vertical) columns based on values
#' embedded in the file names.
#'
#' @keywords internal
#' @param d A data frame
#' @param datafl A data file name
#' @return A data frame with new columns

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Christine Laney (2017-09-28)
##############################################################################################

makePosColumns <- function(d, datafl){
  d.splitFile <- strsplit(x = datafl, split = "\\/")
  d.splitName <- strsplit(x = d.splitFile[[1]][length(d.splitFile[[1]])], split = "\\.")
  nc <- ncol(d)
  if(length(d.splitName[[1]]) %in% c(12,14)){
    if(length(d.splitName[[1]]) == 12){
      horPos <- 8
      verPos <- 9
    }
    if(length(d.splitName[[1]]) == 14){
      horPos <- 7
      verPos <- 8
    }
    if(!("siteID" %in% names(d))){
      d$domainID <- rep(as.character(d.splitName[[1]][2]), nrow(d))
      d$siteID <- rep(as.character(d.splitName[[1]][3]), nrow(d))
    }
    d$horizontalPosition <- rep(as.character(d.splitName[[1]][horPos]), nrow(d))
    d$verticalPosition <- rep(as.character(d.splitName[[1]][verPos]), nrow(d))
    d$horizontalPosition <- as.character(d$horizontalPosition)
    d$verticalPosition <- as.character(d$verticalPosition)
    d <- d[ , c((nc+1):(nc+4), 1:nc)]
    return(d)
  }
  return(d)
}
