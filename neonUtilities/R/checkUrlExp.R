##############################################################################################
#' @title Check for expiration of a signed url

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' For a signed url, check whether the expiration date-time has been reached
#'
#' @keywords internal
#' @param urlIn A signed url to be checked

#' @return Expiration date-time for the url

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2026-08-26 (Claire Lunch): Created original function
##############################################################################################

checkUrlExp <- function(urlIn) {
  
  urlps <- curl::curl_parse_url(urlIn)
  urlcreatedate <- urlps$params[grep('Date', names(urlps$params))]
  urlexpire <- urlps$params[grep('xpire', names(urlps$params))]
  
  if(length(urlcreatedate)==0 | length(urlexpire)==0) {
    return(invisible())
  } else {
    urlcreatedate <- as.POSIXct(urlcreatedate, format="%Y%m%dT%H%M%SZ", tz="GMT")
    urlexpdate <- urlcreatedate + as.numeric(urlexpire)
    return(urlexpdate)
  }
  
}
