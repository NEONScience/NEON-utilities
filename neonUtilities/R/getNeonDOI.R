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
##############################################################################################

getNeonDOI <- function(dpID=NA_character_, release=NA_character_) {
  
  # query DataCite API for NEON prefix
  req <- httr::GET("https://api.datacite.org/dois?query=prefix:10.48443&page[size]=1000")
  
  # check for no or empty response
  if(is.null(req)) {
    message("No response. DataCite API may be unavailable, check for outage alerts.")
    return(invisible())
  }
  
  if(!identical(class(req), "response")) {
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
  
  if(!is.na(dpID)) {
    df.dois <- df.dois[which(df.dois$DPID==dpID),]
  }
  
  if(!is.na(release)) {
    df.dois <- df.dois[which(df.dois$release==release),]
  }
  
  if(nrow(df.dois)==0) {
    message("No DOIs match input criteria. Check data product ID and release name.")
    return(invisible())
  }
  
  return(df.dois)

}
