##############################################################################################
#' @title Check for expired API token

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description Extracts the expiration date from API token and check whether it has expired.
#'
#' @keywords internal
#' @param token User specific API token (generated within data.neonscience.org user accounts)
#' 
#' @return Returns a token value: either the original token, if unexpired, or NA, if the token has expired

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2025-07-11 (Claire Lunch): Original creation
##############################################################################################

tokenCheck <- function(token){

  if(!is.na(token)) {
    # get token expiration date
    expdate <- tokenDate(token)
    
    # check against current date
    if(expdate < Sys.time()) {
      message("API token has expired. Function will proceed using public access rate. Go to your NEON user account to generate a new token.")
      token <- NA_character_
    }
  }
  
  return(token)

}
