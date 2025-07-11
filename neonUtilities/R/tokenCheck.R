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
    # get token components
    splittoken <- strsplit(token, ".", fixed = TRUE)[[1]]
    dubsplit <- unlist(strsplit(rawToChar(jose::base64url_decode(splittoken[2])), 
                         split=","))
    # get expiration date component
    expsplit <- dubsplit[grep("exp", dubsplit)]
    expval <- regmatches(expsplit, regexpr("[0-9]+", expsplit))
    expdate <- as.POSIXct(as.numeric(expval), origin="1970-01-01")
    
    # check against current date
    if(expdate < Sys.time()) {
      message("API token has expired. Function will proceed using public access rate. Go to your NEON user account to generate a new token.")
      token <- NA_character_
    }
  }
  
  return(token)

}
