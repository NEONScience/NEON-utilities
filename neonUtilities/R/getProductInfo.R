##############################################################################################
#' @title Get NEON data product information

#' @author
#' Christine Laney \email{claney@battelleecology.org}

#' @description
#' Use the NEON API to get data product information such as availability, science team, etc.

#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @param dpID The data product id (optional), formated as DP#.#####.###
#' @param token User specific API token (generated within neon.datascience user accounts)

#' @return A named list of metadata and availability information for a single data product. If the dpID argument is omitted, a table of information for all data products in the NEON catalog. 

#' @examples
#' # Get documentation and availability of plant foliar properties data product
#' cfcInfo <- getProductInfo("DP1.10026.001")

#' @export

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Christine Laney (2017-10-01)
##############################################################################################
getProductInfo <- function(dpID="", token = NA){

  avail <- getAPI(apiURL = "http://data.neonscience.org/api/v0/products/", dpID = dpID, token = token)
  
  avail <- avail[["data"]]
  return(avail)
}

# query the products endpoint for the product requested
