##############################################################################################
#' @title Get a Bibtex citation for NEON data with a DOI, or generate a provisional Bibtex citation

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Use the DOI Foundation API to get Bibtex-formatted citations for NEON data, or use a template to generate a Bibtex citation for provisional data. Helper function to download and stacking functions.

#' @param doi The DOI of the data to be cited [character]
#' @param dpID The data product ID of the data to be cited [character]

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

getCitation <- function(doi=NA_character_, dpID=NA_character_) {
    
  # query for DOI
  if(!is.na(doi)) {
    
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
    
  } else {
    
    # if dpID is provided instead of DOI, construct provisional citation from template
    if(!is.na(dpID)) {
      
      cit <- cit_prov_template
      cit <- base::gsub(pattern="DPID", replacement=dpID, x=cit)
      cit <- base::gsub(pattern="YEAR", replacement=format(Sys.Date(), "%Y"), x=cit)
      
      # get product name from NEON API
      req <- httr::GET(paste("https://data.neonscience.org/api/v0/products/", 
                             dpID, sep=""))
      if(is.null(req)) {return(invisible())}
      if(!inherits(req, "response")) {return(invisible())}
      
      df <- jsonlite::fromJSON(httr::content(req, as="text", encoding="UTF-8"), 
                               simplifyDataFrame=TRUE, flatten=TRUE)
      nm <- df$data$productName
      cit <- base::gsub(pattern="NAME", replacement=nm, x=cit)
      
    } else {
      return(invisible())
    }

  }

  return(cit)

}
