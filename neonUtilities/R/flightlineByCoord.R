##############################################################################################
#' @title Find the flightline number for flightlines matching a set of coordinates

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}
#' Kelly Hondula

#' @description
#' Use KML files to determine the flightlines corresponding to a set of coordinates
#'
#' @param dpID The identifier of the NEON data product to pull, in the form DPL.PRNUM.REV, e.g. DP1.10023.001
#' @param site The four-letter code of a single NEON site, e.g. 'CLBJ'.
#' @param year The four-digit year to search for data. Defaults to 2017.
#' @param easting A vector containing the easting UTM coordinates of the locations to download.
#' @param northing A vector containing the northing UTM coordinates of the locations to download.
#' @param token User specific API token (generated within neon.datascience user accounts)

#' @return A vector of flightline numbers.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# Changelog and author contributions / copyrights
#   Kelly Hondula and Claire Lunch (2022-02-25): original creation

##############################################################################################

byTileAOP <- function(dpID, site, year, easting, northing,
                      token=NA_character_) {

  # error message if dpID isn't formatted as expected
  if(regexpr("DP[1-4]{1}.[0-9]{5}.00[1-2]{1}",dpID)!=1) {
    stop(paste(dpID, "is not a properly formatted data product ID. The correct format is DP#.#####.00#", sep=" "))
  }

  # error message if site is left blank
  if(regexpr('[[:alpha:]]{4}', site)!=1) {
    stop("A four-letter site code is required. NEON sites codes can be found here: https://www.neonscience.org/field-sites/field-sites-map/list")
  }

  # error message if year is left blank
  if(regexpr('[[:digit:]]{4}', year)!=1) {
    stop("Year is required (e.g. '2017').")
  }

  # check for sp and sf packages
  if(!requireNamespace("sf", quietly=T)) {
    stop("Package sf is required for this function to work. Install and re-try.")
  }
  if(!requireNamespace("sp", quietly=T)) {
    stop("Package sp is required for this function to work. Install and re-try.")
  }
  
  # error message if easting and northing vector lengths don't match
  easting <- as.numeric(easting)
  northing <- as.numeric(northing)
  easting <- easting[which(!is.na(easting) & !is.null(easting) & easting!="")]
  northing <- northing[which(!is.na(northing) & !is.null(northing) & northing!="")]
  if(length(easting)!=length(northing)) {
    stop("Easting and northing vector lengths do not match, and/or contain null values. Cannot identify paired coordinates.")
  }

  # if token is an empty string, set to NA
  if(identical(token, "")) {
    token <- NA_character_
  }

  # query the products endpoint for the product requested
  prod.req <- getAPI(apiURL = paste("http://data.neonscience.org/api/v0/products/", dpID, sep=""), 
                  token = token)
  
  avail <- jsonlite::fromJSON(httr::content(prod.req, as='text', encoding='UTF-8'), 
                              simplifyDataFrame=TRUE, flatten=TRUE)

  # error message if product not found
  if(!is.null(avail$error$status)) {
    stop(paste("No data found for product", dpID, sep=" "))
  }
  
  # check that token was used
  if(!is.na(token) & !is.null(prod.req$headers$`x-ratelimit-limit`)) {
    if(prod.req$headers$`x-ratelimit-limit`==200) {
      cat('API token was not recognized. Public rate limit applied.\n')
    }
  }

  # check for sites that are flown under the flight box of a different site
  if(site %in% shared_flights$site) {
    flightSite <- shared_flights$flightSite[which(shared_flights$site==site)]
    if(site %in% c('TREE','CHEQ','KONA')) {
      cat(paste(site, ' is part of the flight box for ', flightSite,
                '. Downloading data from ', flightSite, '.\n', sep=''))
    } else {
      cat(paste(site, ' is an aquatic site and is sometimes included in the flight box for ', flightSite,
                '. Aquatic sites are not always included in flight coverage every year.\nDownloading data from ',
                flightSite, '. Check data to confirm coverage of ', site, '.\n', sep=''))
    }
    site <- flightSite
  }

  # get the urls for months with data available, and subset to site
  month.urls <- unlist(avail$data$siteCodes$availableDataUrls)
  month.urls <- month.urls[grep(paste(site, year, sep="/"), month.urls)]

  # error message if nothing is available
  if(length(month.urls)==0) {
    stop("There are no data at the selected site and year.")
  }

  # convert easting & northing coordinates for Blandy (BLAN)
  # Blandy contains plots in 18N and plots in 17N; flight data are all in 17N
  if(site=='BLAN' & length(which(easting<=250000))>0) {
    easting17 <- easting[which(easting>250000)]
    northing17 <- northing[which(easting>250000)]

    easting18 <- easting[which(easting<=250000)]
    northing18 <- northing[which(easting<=250000)]

    df18 <- cbind(easting18, northing18)
    df18 <- data.frame(df18)
    names(df18) <- c('easting','northing')

    sp::coordinates(df18) <- c('easting', 'northing')
    
    epsg.z <- relevant_EPSG$code[grep("+proj=utm +zone=17", 
                                      relevant_EPSG$prj4, fixed=T)]
    if(utils::packageVersion("sp")<"1.4.2") {
      sp::proj4string(df18) <- sp::CRS('+proj=utm +zone=18N ellps=WGS84')
      df18conv <- sp::spTransform(df18, sp::CRS('+proj=utm +zone=17N ellps=WGS84'))
    } else {
      raster::crs(df18) <- sp::CRS("+proj=utm +zone=18")
      df18conv <- sp::spTransform(df18, sp::CRS(paste("+init=epsg:", epsg.z, sep='')))
    }

    easting <- c(easting17, df18conv$easting)
    northing <- c(northing17, df18conv$northing)

    cat('Blandy (BLAN) plots include two UTM zones, flight data are all in 17N.
        Coordinates in UTM zone 18N have been converted to 17N to download the correct tiles.
        You will need to make the same conversion to connect airborne to ground data.')
  }

  # go through available data and get KMLs, check against coordinates
  for(i in 1:length(month.urls)) {
    tmp <- getAPI(month.urls[i], token)
    tmp.files <- jsonlite::fromJSON(httr::content(tmp, as="text", encoding="UTF-8"),
                                    simplifyDataFrame=T, flatten=T)
    
    # check for no files
    if(length(tmp.files$data$files)==0) {
      url.messages <- c(url.messages, paste("No files found for site", tmp.files$data$siteCode,
                                            "and year", tmp.files$data$month, sep=" "))
      next
    }
    
    # subset to only KML files
    kml.urls <- tmp.files$data$files$url[grep("[.]kml$", tmp.files$data$files$name)]
    
    flightlines <- numeric()
    # iterate over kml files and check coordinates
    for(j in 1:length(kml.urls)) {
      kmli <- sf::st_read(j)
      
      # next steps:
      # if kmli contains any of the points, add its flightline number to list
      # return list of flightline numbers

    }
    
  }
  
}
