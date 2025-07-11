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
#' @param release The data release to be downloaded; either 'current' or the name of a release, e.g. 'RELEASE-2021'. 'current' returns the most recent release, as well as provisional data if include.provisional is set to TRUE. To download only provisional data, use release='PROVISIONAL'. Defaults to 'current'.
#' @param avg Deprecated; use timeIndex
#' @param timeIndex Either the string 'all', or the time index of data to download, in minutes. Only applicable to sensor (IS) data. Defaults to 'all'.
#' @param tabl Either the string 'all', or the name of a single data table to download. Defaults to 'all'.
#' @param cloud.mode T or F, are files transferred cloud-to-cloud? Defaults to F; set to true only if the destination location (where you are downloading the files to) is in the cloud.
#' @param check.size T or F, should the user approve the total file size before downloading? Defaults to T. When working in batch mode, or other non-interactive workflow, use check.size=F.
#' @param include.provisional T or F, should provisional data be included in downloaded files? Defaults to F. See https://www.neonscience.org/data-samples/data-management/data-revisions-releases for details on the difference between provisional and released data.
#' @param nCores The number of cores to parallelize the stacking procedure. By default it is set to a single core.
#' @param forceParallel If the data volume to be processed does not meet minimum requirements to run in parallel, this overrides. Set to FALSE as default.
#' @param token User specific API token (generated within data.neonscience.org user accounts)
#' @param useFasttime Should the fasttime package be used to read date-time fields? Defaults to false.
#' @param progress T or F, should progress bars be printed? Defaults to TRUE.
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
                          release="current", timeIndex="all", tabl="all", cloud.mode=FALSE,
                          check.size=TRUE, include.provisional=FALSE,
                          nCores=1, forceParallel=FALSE, token=NA_character_, 
                          useFasttime=FALSE, avg=NA, progress=TRUE) {

  # error message if package is not basic or expanded
  if(!package %in% c("basic", "expanded")) {
    stop(paste(package, "is not a valid package name. Package must be basic or expanded", sep=" "))
  }

  # error message if dpID isn't formatted as expected
  if(regexpr("DP[1-4]{1}[.][0-9]{5}[.]00[1-2]{1}", dpID)[1]!=1) {
    stop(paste(dpID, "is not a properly formatted data product ID. The correct format is DP#.#####.00#", sep=" "))
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
  
  # check for fasttime package, if used
  if(useFasttime & !requireNamespace("fasttime", quietly=T)) {
    stop("Parameter useFasttime is TRUE but fasttime package is not installed. Install and re-try.")
  }
  
  # if token is an empty string, set to NA
  if(identical(token, "")) {
    token <- NA_character_
  }
  
  # check for token expiration
  token <- tokenCheck(token)

  # cloud mode option: pass list of files from queryFiles() to stackByTable(); don't download anything
  if(isTRUE(cloud.mode)) {
    fls <- queryFiles(dpID=dpID, site=site, 
                      startdate=startdate, enddate=enddate,
                      package=package, release=release,
                      timeIndex=timeIndex, tabl=tabl, metadata=TRUE,
                      include.provisional=include.provisional, 
                      token=token)
    out <- stackByTable(filepath=fls, savepath="envt", 
                        cloud.mode=TRUE, folder=TRUE, 
                        nCores=nCores, saveUnzippedFiles=FALSE, 
                        useFasttime=useFasttime, progress=progress)
  } else {
    
    # create a temporary directory to save to
    temppath <- file.path(tempdir(), paste("zips", format(Sys.time(), "%Y%m%d%H%M%S"), sep=""))
    dir.create(temppath)
    
    # pass the request to zipsByProduct() to download
    zipsByProduct(dpID=dpID, site=site, startdate=startdate, enddate=enddate, package=package,
                  release=release, avg=avg, timeIndex=timeIndex, tabl=tabl, check.size=check.size, 
                  savepath=temppath, include.provisional=include.provisional, load=TRUE, 
                  token=token, progress=progress)
    
    # if zipsByProduct() can't download anything, don't pass to stackByTable()
    if(length(list.files(temppath))==0) {
      return(invisible())
    }
    
    # stack and load the downloaded files using stackByTable
    out <- stackByTable(filepath=paste(temppath, "/filesToStack", substr(dpID, 5, 9), sep=""),
                        savepath="envt", folder=TRUE, nCores=nCores, 
                        saveUnzippedFiles=FALSE, useFasttime=useFasttime,
                        progress=progress)
    # Remove temppath directory
    unlink(temppath, recursive=T)
    
  }

  return(out)
  }
