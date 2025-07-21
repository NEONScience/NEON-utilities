##############################################################################################
#' @title Get the horizontal and vertical location indices for a given data product and site

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Get the available horizontal and vertical location indices for a given data product and site. Only relevant to sensor (IS) data products.

#' @param dpID The data product ID to get HOR and VER codes for [character]
#' @param site The site to get HOR and VER codes for [character]
#' @param token User token for the NEON API [character]

#' @return A data frame of HOR and VER indices
#' 
#' @export

#' @examples
#' \dontrun{
#' # Get the HOR and VER codes for PAR (DP1.00024.001) at Wind River
#' ind <- getHorVer(dpID="DP1.00024.001", site="WREF")
#' }

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2025-07-15)
##############################################################################################

getHorVer <- function(dpID=NA_character_, 
                      site=NA_character_,
                      token=NA_character_) {
    
  if(is.na(dpID) | is.na(site)) {
    stop("dpID and site are required inputs.")
  }
  
  # if token is an empty string, set to NA
  if(identical(token, "")) {token <- NA_character_}
  
  # check for token expiration
  token <- tokenCheck(token)
  
  # look up product availability
  req <- getAPI(paste("https://data.neonscience.org/api/v0/products/", 
                         dpID, sep=""), token=token)
  if(is.null(req)) {
    message(paste("Product lookup failed. Check that", dpID, "is a valid NEON data product."))
    return(invisible())
    }

  df <- jsonlite::fromJSON(httr::content(req, as="text", encoding="UTF-8"), 
                           simplifyDataFrame=TRUE, flatten=TRUE)
  month.urls <- unlist(df$data$siteCodes$availableDataUrls)
  
  # get queried site
  site.urls <- grep(pattern=paste("/", site, "/", sep=""), 
                    x=month.urls, fixed=TRUE, value=TRUE)
  
  if(length(site.urls)==0) {
    message(paste("No data found for data product", dpID, 
                  "at site", site))
    return(invisible())
  }
  
  # get files from most recent site-month
  rec.url <- max(site.urls)
  rec.dat <- getAPI(rec.url, token=token)
  dat <- jsonlite::fromJSON(httr::content(rec.dat, as="text", encoding="UTF-8"), 
                           simplifyDataFrame=TRUE, flatten=TRUE)

  fls <- dat$data$files
  
  # get sensor positions file
  sens.ind <- grep(pattern="sensor_positions", x=fls$name)
  
  if(length(sens.ind)==0) {
    message(paste("No sensor positions file found. Check that", dpID,
                  "is a sensor data product; HOR and VER codes do not exist for observational and remote sensing data products."))
    return(invisible())
  }
  
  # get indices from file
  sens.tab <- utils::read.csv(fls$url[sens.ind[1]], colClasses=c(HOR.VER="character"))
  HOR <- substring(sens.tab$HOR.VER, 1, 3)
  VER <- substring(sens.tab$HOR.VER, 5, 7)
  
  return(unique(data.frame(HOR, VER)))

}
