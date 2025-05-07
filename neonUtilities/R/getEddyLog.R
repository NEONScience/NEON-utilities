##############################################################################################
#' @title Get the full issue log set for the SAE bundle

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Use the NEON API to get the issue log from all products in the bundle in a user-friendly format

#' @param token User specific API token (generated within data.neonscience.org user accounts)
#' @keywords internal
#' 
#' @return A table of issues reported for the data product.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2021-10-27)
##############################################################################################
getEddyLog <- function(token=NA_character_) {

  bundleDPs <- c("DP1.00007.001", "DP1.00010.001", "DP1.00034.001", "DP1.00035.001", 
                 "DP1.00036.001", "DP1.00037.001", "DP1.00099.001", "DP1.00100.001", 
                 "DP2.00008.001", "DP2.00009.001", "DP2.00024.001", "DP3.00008.001", 
                 "DP3.00009.001", "DP3.00010.001", "DP4.00002.001", "DP4.00007.001", 
                 "DP4.00067.001", "DP4.00137.001", "DP4.00201.001", "DP4.00200.001")
  
  eddyLog <- character()
  for(i in bundleDPs) {
    
    req <- getAPI(apiURL = paste0("https://data.neonscience.org/api/v0/products/", i), 
                  token = token)
    
    if(is.null(req)) {
      return(invisible())
    }
    
    allproductinfo <- jsonlite::fromJSON(httr::content(req, as="text", encoding="UTF-8"), 
                                         simplifyDataFrame=TRUE, flatten=TRUE)
    
    productlog <- allproductinfo[["data"]]$changeLogs
    
    if(!is.null(productlog)) {
      if(nrow(productlog)>0) {
        dataSubproduct <- rep(i, nrow(productlog))
        productlog <- cbind(dataSubproduct, productlog)
        eddyLog <- rbind(eddyLog, productlog)
      }
    }
    
  }
  return(eddyLog)
}
