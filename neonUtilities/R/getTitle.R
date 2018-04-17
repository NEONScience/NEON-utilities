##############################################################################################
#' @title Get NEON data product title
#'
#' @author
#' Christine Laney \email{claney@battelleecology.org}

#' @description
#' Create a title for a NEON data CSV file
#'
#' @keywords internal

#' @param filename A NEON file name
#' @return A title for the respective GeoCSV file

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2018-01-09 (Christine Laney): created function
##############################################################################################

getTitle <- function(filename) {
  splitName <- strsplit(x = filename, split = "\\.")
  if(length(splitName[[1]])==2){
    tableName <- splitName[[1]][1]
    title <- paste0("NEON ", tableName, " data from multiple sites and/or months")
  }
  if(length(splitName[[1]]) > 2){
    site <- splitName[[1]][3]
    dpID <- substr(filename, 15, 27)
    hor <- splitName[[1]][7]
    ver <- splitName[[1]][8]
    time <- paste(splitName[[1]][9], " minute resolution") #this will need updating for non met data products
    ym <- splitName[[1]][11]
    ym <- as.Date(x = paste0(ym, "-01"), format = "%Y-%m-%d")
    y <- format(ym, "%Y")
    m <- format(ym, "%B")
    uri <- paste0("http://data.neonscience.org/api/v0/products/", dpID)
    data_info <- jsonlite::fromJSON(txt = uri)
    dpName <- data_info$data$productName
    title <- paste("NEON", dpName, "data from", site, m, y, "at horizontal level", hor, "and vertical position", ver, sep = " " )
  }
 return(title)
}
