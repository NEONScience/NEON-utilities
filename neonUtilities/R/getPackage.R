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
##############################################################################################

getPackage <- function(dpID, site_code, year_month, package="basic", savepath = getwd()){
  uri <- paste0("http://data.neonscience.org/api/v0/data/", dpID, "/", site_code, "/",
                year_month, "?package=", package)
  data_info <- jsonlite::fromJSON(txt = uri)
  data_info <- data_info[["data"]]
  data_url <- data_info$files$url[grep(pattern = ".zip", data_info$files$name)]
  data_name <- data_info$files$name[grep(pattern = ".zip", data_info$files$name)]
  downloader::download(data_url, paste0(savepath, "/", data_name), mode="wb")
}
