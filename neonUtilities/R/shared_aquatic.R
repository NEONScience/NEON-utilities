#' Terrestrial-aquatic shared data information
#'
#' A dataset containing NEON site codes and data product IDs for places where meteorological data from terrestrial sites are used as the data of record for nearby aquatic sites as well.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{site}{site code of a NEON aquatic site}
#'   \item{towerSite}{site code of the NEON terrestrial site used as the data source for the corresponding aquatic site}
#'   \item{product}{Data product ID of the data products to which the corresponding terrestrial-aquatic site relationship relates}
#' }
#' @source NEON site layouts and spatial design
"shared_aquatic"
