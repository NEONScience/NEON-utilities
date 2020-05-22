##############################################################################################
#' @title Get a list of the available averaging intervals for a data product

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Most IS products are available at multiple averaging intervals; get a list of what's available for a given data product
#'
#' @param dpID The identifier of the NEON data product, in the form DPL.PRNUM.REV, e.g. DP1.00006.001
#' @param token User specific API token (generated within neon.datascience user accounts)

#' @return A vector of the available averaging intervals, typically in minutes.

#' @examples 
#' # Get available averaging intervals for PAR data product
#' getAvg("DP1.00024.001")

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2018-05-11)
#     original creation
##############################################################################################

getAvg <- function(dpID, token = NA_character_) {
  
  # error message if dpID isn't formatted as expected
  if(regexpr("DP[1-4]{1}.[0-9]{5}.001",dpID)!=1) {
    stop(paste(dpID, "is not a properly formatted data product ID. The correct format is DP#.#####.001", sep=" "))
  }
  
  # error message if dpID is the EC product
  if(dpID=="DP4.00200.001") {
    stop(paste(dpID, "is the bundled eddy covariance data product; download by averaging interval is not available.", sep=" "))
  }
  
  # error message if dpID isn't in table_types
  if(!dpID %in% table_types$productID) {
    stop(paste(dpID, "is not a supported data product ID. Check for typos.", sep=" "))
  }
  
  # error message if dpID isn't IS
  req <- getAPI(paste(apiURL = "http://data.neonscience.org/api/v0/products/", dpID, sep=""), 
                  token = token)
  
  avail <- jsonlite::fromJSON(httr::content(req, as='text', encoding='UTF-8'), 
                              simplifyDataFrame=TRUE, flatten=TRUE)
  
  if(avail$data$productScienceTeamAbbr %in% c("TOS","AOS","AOP") | 
     dpID %in% c("DP1.20267.001","DP1.00101.001","DP1.00013.001","DP1.00038.001",
                 "DP1.00096.001","DP1.00097.001","DP4.00133.001")) {
    stop(paste(dpID, "is not a streaming sensor (IS) data product; averaging interval is not relevant.", sep=" "))
  }
  
  # look up TMI in table_types
  tmi <- table_types$tableTMI[which(table_types$productID==dpID)]
  return(tmi)
  
}
