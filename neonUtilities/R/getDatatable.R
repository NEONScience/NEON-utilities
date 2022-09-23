#' Get NEON data table
#'
#'
#' @author  Eric R. Sokol \email{esokol@battelleecology.org}
#'
#'
#' @description This is a function to retrieve a data table
#' from the NEON data portal for sites and dates provided by the
#' enduser. NOTE that this function only works for NEON
#' Observation System (OS) data products, and only for select tables
#'
#' @param dpid character sting for NEON data product ID
#' @param data_table_name character sting for name of the data table to download, e.g., 'sls_soilCoreCollection'
#' @param sample_location_list list of sites, domains, etc. If NA, retrieve all data for the given data table / dpid combination.
#' @param sample_location_type character sting for location type, such as 'siteID'. Must be one of the NEON controlled terms. If you're unsure, use 'siteID'
#' @param sample_date_min start date for query. Default is 1-Jan-2012, and this should capture the earliest NEON data record.
#' @param sample_date_max end date for query. Default is current date.
#' @param sample_date_format date format. Default/expected format is yyyy-mm-dd
#' @param data_package_type package type, either 'basic' or 'expanded'. If unsure, use 'expanded'
#' @param url_prefix_data data endpoint for NEON API.
#' @param url_prefix_products products endpoint for NEON API.
#' @param token User specific API token (generated within neon.datascience user accounts)
#'
#' @return data frame with selected NEON data
#'
#'
#' @references License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#'
#'
#' @export
getDatatable <- function(
  dpid = NA, #data product ID
  data_table_name = NA, #data table name
  sample_location_list = NA, # list of sites, domains, etc.
  sample_location_type = 'siteID', #location type
  sample_date_min = '2012-01-01', #start date -- default is 1-Jan-2012
  sample_date_max = Sys.Date(), #end date -- default is current date
  sample_date_format = '%Y-%m-%d', #date format
  ### more defaults
  data_package_type = 'basic',
  url_prefix_data = 'https://data.neonscience.org/api/v0/data/',
  url_prefix_products = 'https://data.neonscience.org/api/v0/products/',
  token = NA_character_){

  message("getDatatable() is deprecated. Use loadByProduct() with input parameter tabl=")
  return(invisible())
  
}

