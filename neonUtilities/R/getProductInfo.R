##############################################################################################
#' @title Get NEON data product information

#' @author
#' Christine Laney \email{claney@battelleecology.org}

#' @description
#' Use the NEON API to get data product information such as availability, science team, etc.

#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @param dpID The data product id (optional), formated as DP#.#####.###
#' @param token User specific API token (generated within data.neonscience.org user accounts)

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

  req <- getAPI(apiURL = paste0("https://data.neonscience.org/api/v0/products/", dpID), 
                  token = token)
  
  if(is.null(req)) {
    return(invisible())
  }
  
  avail <- jsonlite::fromJSON(httr::content(req, as="text", encoding="UTF-8"), 
                              simplifyDataFrame=TRUE, flatten=TRUE)
  
  avail <- avail[["data"]]
  return(avail)
}

# query the products endpoint for the product requested
