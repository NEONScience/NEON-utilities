#' Flight coverage information
#'
#' A dataset containing NEON site codes for places where a single AOP flight may cover multiple sites
#'
#' @format A data frame with 2 variables:
#' \describe{
#'   \item{site}{site code of a NEON site}
#'   \item{flightSite}{site code that matches the file naming for flights that may include "site"}
#' }
#' @source NEON flight plans
"shared_flights"
