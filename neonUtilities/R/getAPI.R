##############################################################################################
#' @title Get the data from API

#' @author
#' Nate Mietkiewicz \email{mietkiewicz@battelleecology.org}

#' @description Accesses the API with options to use the user-specific API token generated within neon.datascience user accounts.
#'
#'
#' @param dpID The identifier of the NEON data product to pull, in the form DPL.PRNUM.REV, e.g. DP1.10023.001
#' @param apiURL The API endpoint URL
#' @param token User specific API token (generated within neon.datascience user accounts)

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2020-03-21 (Nate Mietkiewicz): Created original function
##############################################################################################

getAPI <- function(apiURL, dpID = NA, token=NA){

  # query the products endpoint for the product requested
  if(is.na(dpID)) {

    productUrl <- apiURL

  } else {
    productUrl <- paste0(apiURL, dpID)

  }

  if(is.na(token)) {

    req <- httr::GET(productUrl)

  } else {
    req <- httr::GET(productUrl,
                     add_headers(.headers = c('X-API-Token'= token,
                                              'accept' = 'application/json')))
  }

  avail <- jsonlite::fromJSON(httr::content(req, as='text', encoding='UTF-8'), 
                              simplifyDataFrame=TRUE, flatten=TRUE)

  return(avail)

}
