#' Get NEON taxon table
#'
#'
#' @author  Eric R. Sokol \email{esokol@battelleecology.org}
#'
#'
#' @description This is a function to retrieve a taxon table
#' from the NEON data portal for the taxon type by the
#' enduser.
#'
#' @import httr jsonlite
#'
#' @param taxonType Character string for the taxonTypeCode. Must be one of ALGAE, BEETLE, BIRD,
#' FISH, HERPETOLOGY, MACROINVERTEBRATE, MOSQUITO, MOSQUITO_PATHOGENS, SMALL_MAMMAL, PLANT, TICK
#' @param recordReturnLimit Integer. The number of items to limit the result set to. If NA, will return all records in table.
#' @param stream Character string, true or false. Option to obtain the result as a stream. Utilize for large requests.
#' @param token User specific API token (generated within neon.datascience user accounts)
#'
#' @return data frame with selected NEON data
#'
#'
#' @references License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#'
#'
#' @export

getTaxonTable <- function(
  taxonType = NA, #string, taxonTypeCode, one of ALGAE, BEETLE, BIRD, FISH, HERPETOLOY, MACROINVERTEBRATE, MOSQUITO, MOSQUITO_PATHOGENS, SMALL_MAMMAL, PLANT, TICK
  recordReturnLimit = NA, #integer, The number of items to limit the result set to. If NA, will return all records in table.
  stream = 'true', #string, Option to obtain the result as a stream. Utilize for large requests.
  token = NA
  ){

  # set options
  options(stringsAsFactors = FALSE)

  # required packages
  requireNamespace('httr')
  requireNamespace('jsonlite')

  url_prefix = 'http://data.neonscience.org/api/v0/taxonomy?taxonTypeCode=' #hard code endpoint into function
  url_to_get <- as.character(paste0(url_prefix, taxonType))

  if(!is.na(recordReturnLimit)) url_to_get <- paste0(url_to_get,'&limit=',recordReturnLimit)
  if(!is.na(stream)) url_to_get <- paste0(url_to_get,'&stream=',stream)


  req.df <- data.frame()
  req <- NULL

  try({req <- getAPI(apiURL = url_prefix, dpID = NA, token = token)}, silent = TRUE)

  # request code error handling
  if (req$status_code == 204) {
    stop(paste("No data are available"))
  }else if (req$status_code == 413) {
    stop(paste("Data GET failed with status code ", req$status_code,
               ": Payload Too Large. Query a smaller dataset.",
               sep = ""))
  }else {
    if (req$status_code != 200) {
      stop(paste("Data GET failed with status code ", req$status_code,
                 ". Check the formatting of your inputs.", sep = ""))
    }
  }

  if(is.null(req)) stop(paste("No data were returned"))

  if(!is.null(req)){
    taxa_list <- jsonlite::fromJSON(httr::content(req, as='text'))
    taxa_table <- taxa_list$data

    # get rid of prefixes in column names on left side of ":"
    names(taxa_table) <- gsub('.+?\\:','',names(taxa_table))
  }

  ###########
  return(taxa_table)
}
