##############################################################################################
#' @title Download a file from the NEON API

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Download a file via the NEON API, using a token as necessary.
#'
#' @keywords internal
#' 
#' @param url The url pointing to the file to be downloaded
#' @param outpath The file path to save the file to
#' @param useragent The user agent to send to the API
#' @param token The user's API token
#' 
#' @return A status message indicating whether the download was successful or not
#'
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# changelog and author contributions / copyrights
#   Claire Lunch (2026-08-12)
##############################################################################################

downloadNEONFile <- function(url,
                             outpath,
                             useragent,
                             token) {
  
  if(is.na(token)) {
    t <- tryCatch(
      {
        suppressWarnings(downloader::download(url, destfile=outpath,
                                              mode="wb", quiet=T,
                                              headers=c("User-Agent"=useragent)))
      }, error = function(e) { e } )
  } else {
    t <- tryCatch(
      {
        suppressWarnings(downloader::download(url, destfile=outpath,
                                              mode="wb", quiet=T,
                                              headers=c("User-Agent"=useragent,
                                                        "X-API-Token"=token)))
      }, error = function(e) { e } )
  }
  
  return(t)
  
}

