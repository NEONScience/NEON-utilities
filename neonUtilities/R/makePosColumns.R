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
#   2019-10-17 (Nathan Mietkiewicz): Add domainID, siteID, and collection YYYY-MM columns for sensor position files
##############################################################################################

makePosColumns <- function(d, datafl, site){
  
  datafl.splitFile <- strsplit(x = datafl, split = "\\/")
  datafl.splitName <- strsplit(x = datafl.splitFile[[1]][length(datafl.splitFile[[1]])], split = "\\.")

  sensor_positions <- grepl('sensor_positions', datafl.splitName)
  
  if((datafl.splitName[[1]][4]=="DP4") && (datafl.splitName[[1]][5]=="00130")){return(d)}
  
  nc <- ncol(d)
  if(length(datafl.splitName[[1]]) %in% c(12,14) || (TRUE %in% sensor_positions)){
    if(length(datafl.splitName[[1]]) == 12){
      horPos <- 8
      verPos <- 9
    }
    if(length(datafl.splitName[[1]]) == 14){
      horPos <- 7
      verPos <- 8
    }
    if(TRUE %in% sensor_positions) {
      # Make sure there is a start/end column, if not create one with NAs
      if("start" %in% names(d) & "end" %in% names(d)) {
        d <- d 
      } else { 
        d$start <- rep(NA, nrow(d))
        d$end <- rep(NA, nrow(d))
      }
      
      # Make sure there is a referenceStart/end column, if not create one with NAs
      if("referenceStart" %in% names(d) & "referenceEnd" %in% names(d)) {
        d <- d 
      } else { 
        d$referenceStart <- rep(NA, nrow(d))
        d$referenceEnd <- rep(NA, nrow(d))
      }
      d$siteID <- rep(site, nrow(d))
      d <- data.table::setcolorder(d, c(ncol(d), 1:I(ncol(d)-1)))
    } else {
      if(!("siteID" %in% names(d))){
        d$domainID <- as.character(unlist(datafl.splitName)[2])
        d$siteID <- as.character(unlist(datafl.splitName)[3])
      }
      d$horizontalPosition <- as.character(rep(as.character(datafl.splitName[[1]][horPos]), nrow(d)))
      d$verticalPosition <- as.character(rep(as.character(datafl.splitName[[1]][verPos]), nrow(d)))
      d <- data.table::setcolorder(d, c((nc+1):(nc+4),1:nc))
    }
  }
  d$publicationDate <- as.character(rep(datafl.splitName[[1]][length(datafl.splitName[[1]])-1]), nrow(d))
  return(d)
}