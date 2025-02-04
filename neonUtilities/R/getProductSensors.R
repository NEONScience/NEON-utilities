##############################################################################################
#' @title Get data product-sensor relationships

#' @author
#' Christine Laney \email{claney@battelleecology.org}

#' @description
#' Pull all data from the NEON API /products endpoint, create a data frame with data product ID, data product name, and sensor type.

#' @return A data frame

#' @examples
#' \dontrun{
#' sensors <- getProductSensors()
#' }

#' @export


#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2018-04-24 Christine Laney: Created function
##############################################################################################

getProductSensors <- function(){
  products <- jsonlite::fromJSON("https://data.neonscience.org/api/v0/products", flatten=T)
  products <- products[["data"]]
  df <- data.frame(dpID = NA, dpName = NA, sensor = NA)
  for(i in 1:nrow(products)){
    dpID <- products$productCode[i]
    dpName <- products$productName[i]
    sensor <- products$productSensor[i]
    df <- rbind(df, c(dpID, dpName, sensor))
  }
  df <- df[-1, ]
  return(df)
}

