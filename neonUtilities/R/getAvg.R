##############################################################################################
#' @title Get a list of the available averaging intervals for a data product

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Most IS products are available at multiple averaging intervals; get a list of what's available for a given data product
#'
#' @param dpID The identifier of the NEON data product, in the form DPL.PRNUM.REV, e.g. DP1.00006.001
#' @param token User specific API token (generated within data.neonscience.org user accounts)

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
  
  message("getAvg() is deprecated; use getTimeIndex() in future", call.=FALSE)
  
}
