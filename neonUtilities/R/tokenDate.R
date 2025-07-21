##############################################################################################
#' @title Get expiration date for a NEON API token

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description Extracts the expiration date from a NEON API token.
#'
#' @param token User specific API token (generated within data.neonscience.org user accounts)
#' 
#' @return Returns the date when the token will expire (or has expired).
#' 
#' @export

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2025-07-17 (Claire Lunch): Original creation
##############################################################################################

tokenDate <- function(token){

  # get token components
  splittoken <- strsplit(token, ".", fixed = TRUE)[[1]]
  dubsplit <- unlist(strsplit(rawToChar(jose::base64url_decode(splittoken[2])), 
                              split=","))
  # get expiration date component
  expsplit <- dubsplit[grep("exp", dubsplit)]
  expval <- regmatches(expsplit, regexpr("[0-9]+", expsplit))
  expdate <- as.POSIXct(as.numeric(expval), origin="1970-01-01")
  
  return(expdate)

}
