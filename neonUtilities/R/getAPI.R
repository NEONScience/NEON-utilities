##############################################################################################
#' @title Get the data from API

#' @author
#' Nate Mietkiewicz \email{mietkiewicz@battelleecology.org}

#' @description
#' 
#'
#' @param dpID The identifier of the NEON data product to pull, in the form DPL.PRNUM.REV, e.g. DP1.10023.001
#' @param apiURL The four-letter code of a single NEON site, e.g. 'CLBJ'.
#' @param token The four-digit year to search for data. Defaults to 2017.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2020-03-21 (Nate Mietkiewicz): Created original function
##############################################################################################

getAPI <- function(apiURL, dpID, token=NA){
  
  # query the products endpoint for the product requested
  productUrl <- paste0(apiURL, dpID)
  
  if(is.na(token)) {
    
    req <- httr::GET(productUrl)
    avail <- jsonlite::fromJSON(httr::content(req, as="text"), simplifyDataFrame=TRUE, flatten=TRUE)
    
  } else {
    req <- httr::GET(productUrl, 
                     add_headers(.headers = c('X-API-Token'= token,
                                              'accept' = 'application/json')))
    avail <- jsonlite::fromJSON(httr::content(req, as="text"), simplifyDataFrame=TRUE, flatten=TRUE)
  }
  
  return(avail)
  
}