##############################################################################################
#' @title Get and store the file names, S3 URLs, file size, and download status (default = 0) in a data frame

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}
#' Christine Laney \email{claney@battelleecology.org}

#' @description Used to generate a data frame of available AOP files.
#'
#' @param m.urls The monthly API URL for the AOP files
#' @param include.provisional T or F, should provisional data be included in downloaded files?
#' @param token User specific API token (generated within data.neonscience.org user accounts)
#' 
#' @keywords internal

#' @return A dataframe comprised of file names, S3 URLs, file size, and download status (default = 0)

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2018-02-19): original creation
#   Christine Laney (2018-03-05): Added functionality to get new list of URLs if the old ones expire, during the download stream.

##############################################################################################

# get and stash the file names, S3 URLs, file size, and download status (default = 0) in a data frame
getFileUrls <- function(m.urls, include.provisional, token=NA){
  url.messages <- character()
  file.urls <- c(NA, NA, NA)
  releases <- character()
  prov.message <- "no"
  for(i in 1:length(m.urls)) {

    tmp <- getAPI(apiURL = m.urls[i], token = token)
    
    if(tmp$status_code!=200) {
      message(paste("Data file retrieval failed with code ", tmp$status_code, 
                    ". Check NEON data portal for outage alerts.", sep=""))
      return(invisible())
    }
    
    tmp.files <- jsonlite::fromJSON(httr::content(tmp, as='text', encoding='UTF-8'), 
                                simplifyDataFrame=TRUE, flatten=TRUE)

    # check for no files
    if(length(tmp.files$data$files)==0) {
      message(paste("No files found for site", tmp.files$data$siteCode,
                    "and year", tmp.files$data$month, sep=" "))
      next
    }
    
    # get release info
    if(isFALSE(include.provisional) & tmp.files$data$release=="PROVISIONAL") {
      file.urls <- file.urls
      releases <- releases
      prov.message <- "yes"
    } else {
      releases <- c(releases, tmp.files$data$release)
      
      file.urls <- rbind(file.urls, cbind(tmp.files$data$files$name,
                                          tmp.files$data$files$url,
                                          tmp.files$data$files$size))
    }
  }
  
  if(all(is.na(file.urls)) & prov.message=="yes") {
    stop("All files found were provisional. Modify download query (dates and/or sites) or, if you want to use provisional data, use input parameter include.provisional=TRUE.")
  }
  
  if(prov.message=="yes") {
    message("Provisional data were excluded from available files list. To download provisional data, use input parameter include.provisional=TRUE.")
  }
  
  # get size info
  file.urls <- data.frame(file.urls, row.names=NULL)
  colnames(file.urls) <- c("name", "URL", "size")
  file.urls$URL <- as.character(file.urls$URL)
  file.urls$name <- as.character(file.urls$name)
  
  if(length(url.messages) > 0){writeLines(url.messages)}
  file.urls <- file.urls[-1,]
  release <- unique(releases)
  return(list(file.urls, release))
  
}
