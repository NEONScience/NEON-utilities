##############################################################################################
#' @title Extract attributes from eddy covariance H5 files

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Extract attribute metadata from H5 files
#'
#' @param fil File path to the H5 file to extract attributes from [character]
#' @param sit The site, for site attributes. Must match site of file path. [character]

#' @keywords internal
#' @return A data frame with one row containing the extracted attributes

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# changelog and author contributions / copyrights
#   Claire Lunch (2023-03-21)
##############################################################################################

getAttributes <- function(fil, sit) {
  
  siteAttr <- base::try(rhdf5::h5readAttributes(fil, name=sit), silent=T)
  mnth <- regmatches(fil, regexpr("20[0-9]{2}-[0-9]{2}", fil))
  if(inherits(siteAttr, "try-error")) {
    siteAttr <- data.frame(matrix(data=c(sit, mnth), ncol=2, nrow=1))
    names(siteAttr) <- c("site","date")
  } else {
    if(any(lapply(siteAttr, length)>1)) {
      for(i in which(lapply(siteAttr, length)>1)) {
        siteAttr[i] <- paste0(siteAttr[i], collapse=",")
      }
    }
    siteAttr <- c(sit, mnth, siteAttr)
    names(siteAttr)[1:2] <- c("site", "date")
  }
  
  return(siteAttr)
  
}

