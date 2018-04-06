##############################################################################################
#' @title Get NEON data product information

#' @author
#' Christine Laney \email{claney@battelleecology.org}

#' @description
#' Use the NEON API to get data product information such as availability, science team, etc.

#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @param dpID The data product id (optional), formated as DP0.00000.000
#' @return A data table with product information

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Christine Laney (2017-10-01)
##############################################################################################
getProductInfo <- function(dpID){
  productUrl <- paste0("http://data.neonscience.org/api/v0/products/", dpID)
  req <- httr::GET(productUrl)
  avail <- jsonlite::fromJSON(httr::content(req, as="text"), simplifyDataFrame=TRUE, flatten=TRUE)
  avail <- avail[["data"]]
  return(avail)
}

# query the products endpoint for the product requested
