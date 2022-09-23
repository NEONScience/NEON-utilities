##############################################################################################
#' @title Get only headers from API

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description Accesses the API with options to use the user-specific API token generated within neon.datascience user accounts.
#'
#'
#' @param apiURL The API endpoint URL
#' @param token User specific API token (generated within neon.datascience user accounts). Optional.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2021-01-24 (Claire Lunch): Adapted from getAPI()
##############################################################################################

getAPIHeaders <- function(apiURL, token=NA_character_){

  if(!curl::has_internet()) {
    message("No internet connection detected. Cannot access NEON API.")
    return(invisible())
  }
  
  if(identical(token, "")) {
    token <- NA_character_
  }
  
  if(is.na(token)) {
    
    # make 5 attempts to access - if rate limit is reached every time, give up
    j <- 1
    while(j < 6) {

      req <- try(httr::HEAD(apiURL), silent=T)
      
      # check for no response
      if(!inherits(req, "response")) {
        message("No response. NEON API may be unavailable, check NEON data portal for outage alerts.")
        return(invisible())
      }
      
      # if rate limit is reached, pause
      if(!is.null(req$headers$`x-ratelimit-limit`)) {
        
        if(req$headers$`x-ratelimit-remaining`<=1) {
          cat(paste("\nRate limit reached. Pausing for ", 
                    req$headers$`x-ratelimit-reset`,
                    " seconds to reset.\n", sep=""))
          Sys.sleep(req$headers$`x-ratelimit-reset`)
          j <- j+1
        } else {
          j <- j+5
        }
      } else {
        j <- j+5
      }
    }

  } else {
    
    # same process as in non-token case: make 5 attempts
    
    j <- 1
    while(j < 6) {

      req <- try(httr::HEAD(apiURL,
                       httr::add_headers(.headers = c('X-API-Token'= token,
                                                      'accept' = 'application/json'))),
                 silent=T)
      
      # check for no response
      if(!inherits(req, "response")) {
        message("No response. NEON API may be unavailable, check NEON data portal for outage alerts.")
        return(invisible())
      }

      # first check for null, since unlimited tokens don't have these headers
      if(!is.null(req$headers$`x-ratelimit-limit`)) {

        # if rate limit is reached, pause
        if(req$headers$`x-ratelimit-remaining`<=1) {
          cat(paste("\nRate limit reached. Pausing for ", 
                    req$headers$`x-ratelimit-reset`,
                    " seconds to reset.\n", sep=""))
          Sys.sleep(req$headers$`x-ratelimit-reset`)
          j <- j+1
        } else {
          j <- j+5
        }
      } else {
        j <- j+5
      }
    }
  }

  return(req)

}
