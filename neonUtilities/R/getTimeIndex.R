##############################################################################################
#' @title Get a list of the available time intervals for a data product

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Most IS products are available at multiple time intervals; get a list of what's available for a given data product
#'
#' @param dpID The identifier of the NEON data product, in the form DPL.PRNUM.REV, e.g. DP1.00006.001
#' @param token User specific API token (generated within neon.datascience user accounts)

#' @return A vector of the available time intervals, typically in minutes.

#' @examples 
#' # Get available time intervals for PAR data product
#' getTimeIndex("DP1.00024.001")

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2020-09-02)
#     duplication of getAvg() to match new nomenclature
##############################################################################################

getTimeIndex <- function(dpID, token = NA_character_) {

  getAvg(dpID=dpID, token=token)

}
