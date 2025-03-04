##############################################################################################
#' @title Get the issue log for a specific data product

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Use the NEON API to get the issue log in a user-friendly format

#' @param dpID The data product identifier, formatted as DP#.#####.###
#' @param token User specific API token (generated within data.neonscience.org user accounts)

#' @return A table of issues reported for the data product.

#' @examples
#' # Get documentation and availability of plant foliar properties data product
#' cfcIssues <- getIssueLog("DP1.10026.001")

#' @export

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2021-10-26)
##############################################################################################
getIssueLog <- function(dpID=NA, token=NA_character_) {

  # error message if dpID isn't formatted as expected
  if(regexpr("DP[1-4]{1}.[0-9]{5}.00[0-9]{1}",dpID)[1]!=1) {
    stop(paste(dpID, "is not a properly formatted data product ID. The correct format is DP#.#####.00#", sep=" "))
  }
  
  if(dpID=="DP4.00200.001") {
    issuelog <- getEddyLog(token=token)
  } else {
    req <- getAPI(apiURL = paste0("https://data.neonscience.org/api/v0/products/", dpID), 
                  token = token)
    
    if(is.null(req)) {
      return(invisible())
    }
    
    allproductinfo <- jsonlite::fromJSON(httr::content(req, as="text", encoding="UTF-8"), 
                                         simplifyDataFrame=TRUE, flatten=TRUE)
    
    issuelog <- allproductinfo[["data"]]$changeLogs
  }
  
  return(issuelog)
}
