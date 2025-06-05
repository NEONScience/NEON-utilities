##############################################################################################
#' @title Query the query endpoint of the NEON API and create an arrow dataset from the results

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description Uses the query endpoint of the NEON API to find the full list of files for a given data product, release, site(s), and date range, then turns them into an arrow dataset.
#'
#' @param dpID The identifier of the NEON data product to pull, in the form DPL.PRNUM.REV, e.g. DP1.10023.001
#' @param site Either the string 'all', meaning all available sites, or a character vector of 4-letter NEON site codes, e.g. c('ONAQ','RMNP'). Defaults to all.
#' @param startdate Either NA, meaning all available dates, or a character vector in the form YYYY-MM, e.g. 2017-01. Defaults to NA.
#' @param enddate Either NA, meaning all available dates, or a character vector in the form YYYY-MM, e.g. 2017-01. Defaults to NA.
#' @param tabl The name of a single data table to download.
#' @param hor The horizontal index of data to download. Only applicable to sensor (IS) data.
#' @param ver The vertical index of data to download. Only applicable to sensor (IS) data.
#' @param package Either 'basic' or 'expanded', indicating which data package to download. Defaults to basic.
#' @param release The data release to be downloaded; either 'current' or the name of a release, e.g. 'RELEASE-2021'. 'current' returns the most recent release, as well as provisional data if include.provisional is set to TRUE. To download only provisional data, use release='PROVISIONAL'. Defaults to 'current'.
#' @param include.provisional T or F, should provisional data be included in downloaded files? Defaults to F. See https://www.neonscience.org/data-samples/data-management/data-revisions-releases for details on the difference between provisional and released data.
#' @param token User specific API token (generated within data.neonscience.org user accounts). Optional.
#' 
#' @return An arrow dataset for the data requested.

#' @export

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2025-04-22 Claire Lunch: Original creation
#   
##############################################################################################

datasetQuery <- function(dpID, site="all", 
                         startdate=NA, enddate=NA, 
                         tabl=NA, hor=NA, ver=NA,
                         package="basic", release="current", 
                         include.provisional=FALSE, token=NA_character_) {
  
  # check inputs: this only works for OS and tabular IS data
  # and for IS products, HOR and VER are required, and only one site can be queried
  prod.req <- getAPI(apiURL = paste("https://data.neonscience.org/api/v0/products/", 
                                    dpID, sep=""), token = token)
  avail <- jsonlite::fromJSON(httr::content(prod.req, as='text', encoding='UTF-8'), 
                              simplifyDataFrame=TRUE, flatten=TRUE)
  pubType <- avail$data$productPublicationFormatType
  
  if(pubType=="AOP Data Product Type") {
    stop(paste(dpID, " is a remote sensing data product. Remote sensing data can't be queried using this function.", sep=""))
  }

  if(pubType %in% c("TIS Data Product Type","AIS Data Product Type")) {
    if(dpID %in% c("DP4.00200.001",
                   "DP1.00007.001","DP1.00010.001","DP1.00034.001","DP1.00035.001",
                   "DP1.00036.001","DP1.00037.001","DP1.00099.001","DP1.00100.001",
                   "DP2.00008.001","DP2.00009.001","DP2.00024.001","DP3.00008.001",
                   "DP3.00009.001","DP3.00010.001","DP4.00002.001","DP4.00007.001",
                   "DP4.00067.001","DP4.00137.001","DP4.00201.001","DP1.00030.001")) {
      stop(paste(dpID, " is an eddy covariance data product and can't be queried using this function.", sep=""))
    }
    if(grepl(pattern="^ais[.]", x=tabl)) {
      tabl <- tabl
    } else {
      if(site=="all" | length(site)>1) {
        stop(paste(dpID, " is a sensor data product and can only be queried at a single site using this function. If you need data at multiple sites, run multiple queries or use loadByProduct().", sep=""))
      }
      if(any(is.na(c(hor, ver)))) {
        stop(paste(dpID, " is a sensor data product. hor and ver indices must be provided to use this query function.", sep=""))
      }
      if(any(nchar(c(hor,ver))!=3)) {
        stop("hor and ver must be 3-digit codes in character format.")
      }
    }
  }
  
  # get files
  urlset <- queryFiles(dpID=dpID, site=site, 
                       startdate=startdate, enddate=enddate,
                       package=package, release=release,
                       timeIndex="all", tabl=tabl, metadata=FALSE,
                       include.provisional=include.provisional, 
                       token=token)
  
  # exit if no files
  if(is.null(urlset)) {
    return(invisible())
  }
  
  # subset by hor and ver
  if(!is.na(ver)) {
    urlset[[1]] <- base::grep(pattern=paste("[.]", hor, "[.]", ver, "[.]", 
                                      sep=""), x=urlset[[1]], value=TRUE)
  }
  
  if(isTRUE(urlset[[3]])) {
    # steps: check for column name differences, if different, can I find the new ones and make them strings?
    # will it allow missing columns in some files?
    message("Differing variables files detected. Schema will be inferred; performance may be reduced. This can usually be avoided by excluding provisional data.")
    ds <- arrow::open_csv_dataset(sources=urlset[[1]], 
                                  unify_schemas=TRUE,
                                  col_names=TRUE,
                                  skip=0)
  } else {
    ds <- arrow::open_csv_dataset(sources=urlset[[1]], 
                                  schema=schemaFromVar(urlset[[2]],
                                                       tab=tabl,
                                                       package=package), 
                                  skip=1)
  }
  
  return(ds)
  
  # need to figure out how to handle site-all and lab tables
  # and how to detect site-all and lab tables in this context
  
}
