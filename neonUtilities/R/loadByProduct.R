##############################################################################################
#' @title Get files from NEON API, stack tables, and load into the current environment

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Pull files from the NEON API, by data product, merge data for each table, and read into the current R environment
#'
#' @param dpID The identifier of the NEON data product to pull, in the form DPL.PRNUM.REV, e.g. DP1.10023.001
#' @param site Either the string 'all', meaning all available sites, or a character vector of 4-letter NEON site codes, e.g. c('ONAQ','RMNP'). Defaults to all.
#' @param startdate Either NA, meaning all available dates, or a character vector in the form YYYY-MM, e.g. 2017-01. Defaults to NA.
#' @param enddate Either NA, meaning all available dates, or a character vector in the form YYYY-MM, e.g. 2017-01. Defaults to NA.
#' @param package Either 'basic' or 'expanded', indicating which data package to download. Defaults to basic.
#' @param avg Deprecated; use timeIndex
#' @param timeIndex Either the string 'all', or the time index of data to download, in minutes. Only applicable to sensor (IS) data. Defaults to 'all'.
#' @param tabl Either the string 'all', or the name of a single data table to download. Defaults to 'all'.
#' @param check.size T or F, should the user approve the total file size before downloading? Defaults to T. When working in batch mode, or other non-interactive workflow, use check.size=F.
#' @param nCores The number of cores to parallelize the stacking procedure. By default it is set to a single core.
#' @param forceParallel If the data volume to be processed does not meet minimum requirements to run in parallel, this overrides. Set to FALSE as default.
#' @param token User specific API token (generated within neon.datascience user accounts)
#'
#' @details All available data meeting the query criteria will be downloaded. Most data products are collected at only a subset of sites, and dates of collection vary. Consult the NEON data portal for sampling details.
#' Dates are specified only to the month because NEON data are provided in monthly packages. Any month included in the search criteria will be included in the download. Start and end date are inclusive.

#' @return A named list of all the data tables in the data product downloaded, plus a validation file and a variables file, as available.

#' @examples
#' \dontrun{
#' # To download plant foliar properties data from all sites, expanded data package:
#' cfc <- loadByProduct(dpID="DP1.10026.001", site="all", package="expanded")
#' }

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2019-01-10)
#     original creation
##############################################################################################

loadByProduct <- function(dpID, site="all", startdate=NA, enddate=NA, package="basic",
                          timeIndex="all", tabl="all", check.size=TRUE, nCores=1, 
                          forceParallel=FALSE, token=NA_character_, avg=NA) {

  # error message if package is not basic or expanded
  if(!package %in% c("basic", "expanded")) {
    stop(paste(package, "is not a valid package name. Package must be basic or expanded", sep=" "))
  }

  # error message if dpID isn't formatted as expected
  if(regexpr("DP[1-4]{1}.[0-9]{5}.001", dpID)!=1) {
    stop(paste(dpID, "is not a properly formatted data product ID. The correct format is DP#.#####.001", sep=" "))
  }

  # error message if for AOP data
  if(substring(dpID, 5, 5)==3 & dpID!="DP1.30012.001") {
    stop(paste(dpID, "is a remote sensing data product and cannot be loaded directly to R with this function. Use the byFileAOP() function to download locally.", sep=" "))
  }

  # error message for phenocam data
  if(dpID %in% c("DP1.00033.001", "DP1.00042.001")) {
    stop(paste(dpID, "is a phenological image product, data are hosted by Phenocam.", sep=" "))
  }

  # error message for SAE data
  if(dpID == "DP4.00200.001"){
    stop("The bundled eddy covariance data product cannot be stacked and loaded directly from download.\nTo use these data, download with zipsByProduct() and then stack with stackEddy().")
  }

  # create a temporary directory to save to
  temppath <- file.path(tempdir(), paste("zips", format(Sys.time(), "%Y%m%d%H%M%S"), sep=""))
  dir.create(temppath)

  # pass the request to zipsByProduct() to download
  zipsByProduct(dpID=dpID, site=site, startdate=startdate, enddate=enddate, package=package,
                avg=avg, timeIndex=timeIndex, tabl=tabl, check.size=check.size, savepath=temppath, 
                load=TRUE, token=token)

  # stack and load the downloaded files using stackByTable
  out <- stackByTable(filepath=paste(temppath, "/filesToStack", substr(dpID, 5, 9), sep=""),
                      savepath="envt", folder=TRUE, nCores=nCores, 
                      saveUnzippedFiles=FALSE)
  # Remove temppath directory
  unlink(temppath, recursive=T)
  return(out)
  }
