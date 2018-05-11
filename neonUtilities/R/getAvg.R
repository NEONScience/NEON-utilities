##############################################################################################
#' @title Get a list of the available averaging intervals for a data product

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Most IS products are available at multiple averaging intervals; get a list of what's available for a given data product
#'
#' @param dpID The identifier of the NEON data product, in the form DPL.PRNUM.REV, e.g. DP1.00006.001

#' @return A list of the available averaging intervals, typically in minutes.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2018-05-11)
#     original creation
##############################################################################################

getAvg <- function(dpID) {
  
  # error message if dpID isn't formatted as expected
  if(regexpr("DP[1-4]{1}.[0-9]{5}.001",dpID)!=1) {
    stop(paste(dpID, "is not a properly formatted data product ID. The correct format is DP#.#####.001", sep=" "))
  }
  
  # error message if dpID isn't IS
  
  
  
}
