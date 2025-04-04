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
#' 
#' @importFrom rlang .data

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
  
  urlset <- queryFiles(dpID="DP1.10111.001", site=site,
                       package='basic', startdate=startdate,
                       enddate=enddate, release=release, 
                       tabl='sim_eventData', metadata=TRUE,
                       include.provisional=include.provisional,
                       token=token)
  
  # subset to only the table to query
  edlst <- base::grep("sim_eventData", urlset[[1]], value=TRUE)
  
  # get variables file and translate to schema
  varfl <- urlset[[2]]
  vschema <- schemaFromVar(varfl, tab="sim_eventData", package="basic")
  
  # open dataset and query for event type
  ds <- arrow::open_csv_dataset(sources=edlst, schema=vschema, skip=1)
  
  evFilter <- eventType
  evds <- dplyr::filter(.data=ds, .data$eventType==evFilter)
  events <- data.frame(dplyr::collect(evds))
  
  return(events)
  
}
