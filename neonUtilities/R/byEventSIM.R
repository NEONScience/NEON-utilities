##############################################################################################
#' @title Get site management data by event type.

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Query site management data to return records matching a specific eventType.
#'
#' @param eventType The value of eventType to search for. Can be multiple values. See categoricalCodes file for DP1.10111.001 for possible values.
#' @param site Either the string 'all', meaning all available sites, or a character vector of 4-letter NEON site codes, e.g. c('ONAQ','RMNP'). Defaults to all.
#' @param startdate Either NA, meaning all available dates, or a character vector in the form YYYY-MM, e.g. 2017-01. Defaults to NA.
#' @param enddate Either NA, meaning all available dates, or a character vector in the form YYYY-MM, e.g. 2017-01. Defaults to NA.
#' @param release The data release to be downloaded; either 'current' or the name of a release, e.g. 'RELEASE-2021'. 'current' returns the most recent release, as well as provisional data if include.provisional is set to TRUE. To download only provisional data, use release='PROVISIONAL'. Defaults to 'current'.
#' @param include.provisional T or F, should provisional data be included in downloaded files? Defaults to F. See https://www.neonscience.org/data-samples/data-management/data-revisions-releases for details on the difference between provisional and released data.
#' @param token User specific API token (generated within data.neonscience.org user accounts)

#' @return A data frame of sim_eventData data, matching the query criteria. Note that metadata are not included in the data returned via this function.

#' @examples 
#' \dontrun{
#' # Search for fires across all NEON event data
#' sim.fires <- byEventSIM(eventType="fire")
#' 
#' # Search for grazing events at several sites
#' sim.graz <- byEventSIM(eventType="grazing", site=c("CPER","KONA","MOAB","STER","LAJA"))
#' }

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2025-03-11)
#     original creation
##############################################################################################

byEventSIM <- function(eventType, 
                       site="all",
                       startdate=NA,
                       enddate=NA,
                       release="current",
                       include.provisional=FALSE,
                       token=NA_character_) {
  
  # check for dplyr
  if(!requireNamespace("dplyr", quietly=T)) {
    stop("The dplyr package is required to use this function. Install and re-try.")
  }
  
  # check for GCS and S3 enabled
  if(!arrow::arrow_with_gcs()) {
    if(!arrow::arrow_with_s3()) {
      stop("Package arrow is installed with S3 and GCS disabled. Consult documentation at https://arrow.apache.org/docs/r/articles/fs.html , update installation and re-try.")
    } else {
      message("Package arrow is installed with GCS disabled. S3 will be used to access data; performance may be reduced.")
    }
  }
  
  urlset <- queryFiles(dpID="DP1.10111.001", site=site,
                       package='basic', startdate=startdate,
                       enddate=enddate, release=release, 
                       tabl='sim_eventData',
                       include.provisional=include.provisional,
                       token=Sys.getenv('NEON_TOKEN'))
  
  # subset to only the table to query
  edlst <- base::grep("sim_eventData", urlset, value=TRUE)
  
  # get most recent variables file
  varfls <- base::grep("variables", urlset, value=TRUE)
  varfl <- getRecentPublication(varfls)[[1]]
  
  # add GCS prefix, if GCS is enabled
  if(arrow::arrow_with_gcs()) {
    edlst <- paste("gs://anonymous@", edlst, sep="")
    varfl <- paste("gs://anonymous@", varfl, sep="")
  } else {
    # S3 prefix and suffix if GCS is not enabled
    edlst <- paste("s3://", edlst, 
                   "/?endpoint_override=https%3A%2F%2Fstorage.googleapis.com", sep="")
    varfl <- paste("s3://", varfl, 
                   "/?endpoint_override=https%3A%2F%2Fstorage.googleapis.com", sep="")
  }
  
  # read in variables file
  vartab <- try(data.frame(arrow::read_csv_arrow(varfl)), silent=TRUE)
  vartab <- try(vartab[which(vartab$table=="sim_eventData"),], silent=TRUE)
  if(inherits(vartab, "try-error")) {
    message("There was a problem reading the variables file. All data are loaded as character strings.")
    # use string schema - need to write function
  } else {
    if(nrow(vartab)==0) {
      message("There was a problem reading the variables file. All data are loaded as character strings.")
      # use string schema - need to write function
    }
  }
  
  vschema <- schemaFromVar(vartab)
  
  
}
