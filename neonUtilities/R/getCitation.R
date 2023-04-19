##############################################################################################
#' @title Get a Bibtex citation for NEON data with a DOI

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Use the DOI Foundation API to get Bibtex-formatted citations for NEON data. Technically this function will work for any DOI, but is not exported since there are much more thorough wrappers for DOI APIs.

#' @param doi The DOI of the data to be cited [character]

#' @return A character string containing the Bibtex citation

#' @examples
#' \dontrun{
#' # Get the citation for Breeding landbird point counts (DP1.10003.001), RELEASE-2023
#' cit <- getCitation(doi="10.48443/S730-DY13")
#' }

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2023-04-18)
##############################################################################################

getCitation <- function(doi) {
    
  # query for DOI
  req <- httr::GET(paste("https://doi.org/", doi, sep=""),
                   httr::accept("application/x-bibtex"))
  
  # check for no or empty response
  if(is.null(req)) {
    message("No response. DOI Foundation API may be unavailable, check for outage alerts.")
    return(invisible())
  }
  
  if(!inherits(req, "response")) {
    message("No response. DOI Foundation API may be unavailable, check for outage alerts.")
    return(invisible())
  }
  
  # read response
  cit <- httr::content(req, as="text", encoding="UTF-8")
  
  return(cit)

}
