##############################################################################################
#' @title Get either a list of NEON DOIs, or the DOI for a specific data product and release

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Use the DataCite API to get NEON data DOIs in a user-friendly format

#' @param dpID The data product identifier, formatted as DP#.#####.### [character]
#' @param release Name of a specific release, e.g. RELEASE-2022 [character]

#' @return A table of data product IDs and DOIs.

#' @examples
#' \dontrun{
#' # Get all NEON data DOIs
#' allDOIs <- getNeonDOI()
#' }

#' @export

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Christine Laney, Claire Lunch (2022-02-01)
#   Updated to use DataCite API only when searching across all data products; otherwise use NEON API. Claire Lunch (2023-04-18)
##############################################################################################

getNeonDOI <- function(dpID=NA_character_, release=NA_character_) {
  
  usera <- paste("neonUtilities/", utils::packageVersion("neonUtilities"), " R/", 
                 R.Version()$major, ".", R.Version()$minor, " ", commandArgs()[1], 
                 " ", R.Version()$platform, sep="")
  
  if(is.na(dpID)) {
    
    # query DataCite API for NEON prefix
    req <- httr::GET("https://api.datacite.org/dois?query=prefix:10.48443&page[size]=1000",
                     httr::user_agent(usera))
    
    # check for no or empty response
    if(is.null(req)) {
      message("No response. DataCite API may be unavailable, check for outage alerts.")
      return(invisible())
    }
    
    if(!inherits(req, "response")) {
      message("No response. DataCite API may be unavailable, check for outage alerts.")
      return(invisible())
    }
    
    # read and flatten response
    df <- jsonlite::fromJSON(httr::content(req, as="text", encoding="UTF-8"), 
                             simplifyDataFrame=TRUE, flatten=TRUE)
    
    # pull out the essential data from the response
    df.all <- df$data
    
    # extract release and DPID from url
    urls <- df.all$attributes.url
    rel <- rep(NA, length(urls))
    rel[base::grep("RELEASE[-][0-9]{4}", urls)] <- 
      base::regmatches(urls, base::regexpr("RELEASE[-][0-9]{4}", urls))
    dp <- rep(NA, length(urls))
    dp[base::grep("DP[0-9]{1}[.][0-9]{5}[.]00[0-9]{1}", urls)] <- 
      base::regmatches(urls, base::regexpr("DP[0-9]{1}[.][0-9]{5}[.]00[0-9]{1}", urls))
    
    # extract product name from titles (this is the only identifier for prototype data)
    titles <- df.all$attributes.titles
    nm <- base::lapply(titles, "[[", "title")
    nm <- base::lapply(nm, "[[", 1)
    nm <- base::unlist(nm)
    
    df.dois <- base::cbind(dp, nm, rel, df.all$attributes.doi)
    df.dois <- data.frame(df.dois)
    names(df.dois) <- c("DPID", "name", "release", "DOI")
    
    if(!is.na(release)) {
      df.dois <- df.dois[which(df.dois$release==release),]
    }
    
  } else {
    
    # query NEON API for product
    req <- httr::GET(paste("https://data.neonscience.org/api/v0/products/", 
                           dpID, sep=""), httr::user_agent(usera))
    
    # check for no or empty response
    if(is.null(req)) {
      message("No response. NEON API may be unavailable, check for outage alerts.")
      return(invisible())
    }
    
    if(!inherits(req, "response")) {
      message("No response. NEON API may be unavailable, check for outage alerts.")
      return(invisible())
    }
    
    # read and flatten response
    df <- jsonlite::fromJSON(httr::content(req, as="text", encoding="UTF-8"), 
                             simplifyDataFrame=TRUE, flatten=TRUE)
    
    # pull out the releases from the response
    df.all <- df$data
    rel <- df.all$releases
    
    # add context columns
    dp <- rep(dpID, nrow(rel))
    nm <- rep(df.all$productName, nrow(rel))
    doi.only <- base::gsub(pattern="https://doi.org/", replacement="", x=rel$productDoi.url)
    
    df.dois <- base::cbind(dp, nm, doi.only, rel)
    names(df.dois)[1:3] <- c("DPID", "name", "DOI")
    
    if(!is.na(release)) {
      df.dois <- df.dois[which(df.dois$release==release),]
    }
    
  }
  
  return(df.dois)

}
