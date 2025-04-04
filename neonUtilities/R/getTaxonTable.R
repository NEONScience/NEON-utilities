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
#' @param token User specific API token (generated within data.neonscience.org user accounts)
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

  message("getTaxonTable() is deprecated in neonUtilities. Use the updated version of this function, renamed to getTaxonList(), in the neonOS package.")
  
}
