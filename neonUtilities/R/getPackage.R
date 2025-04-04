##############################################################################################
#' @title Get NEON data package
#'
#' @author
#' Christine Laney \email{claney@battelleecology.org}

#' @description
#' Get a zipped file for a single data product, site, and year-month combination. Use the NEON data portal or API to determine data
#' availability by data product, site, and year-month combinations.
#'
#' @param dpID The identifier of the NEON data product to pull, in the form DPL.PRNUM.REV, e.g. DP1.10023.001
#' @param site_code A four-letter NEON research site code, such as HEAL for Healy.
#' @param year_month The year and month of interest, in format YYYY-MM.
#' @param package Either 'basic' or 'expanded', indicating which data package to download. Defaults to basic.
#' @param savepath The location to save the output files to
#' @return A zipped monthly file

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# Changelog and author contributions / copyrights
#   2018-01-09 (Christine Laney): Created function
#   2018-04-10 (Christine Laney): Exported function and added parameter for where to save the downloaded file.
#   2022-02-07 (Claire Lunch): Original function relied on zip files, which are no longer stored. Redirected to zipsByProduct(), as first step to deprecating.
##############################################################################################

getPackage <- function(dpID, site_code, year_month, package="basic", savepath = getwd()){
  
  warning("getPackage() will be deprecated soon, use zipsByProduct(). For back-compatibility, your request has been sent to zipsByProduct().",
          call. = FALSE)
  zipsByProduct(dpID=dpID, site=site_code, startdate=year_month, 
                enddate=year_month, package=package, 
                savepath=savepath, check.size=FALSE)
  
}
