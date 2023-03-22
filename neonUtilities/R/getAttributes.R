##############################################################################################
#' @title Extract attributes from eddy covariance H5 files

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Extract attribute metadata from H5 files
#'
#' @param fil File path to the H5 file to extract attributes from [character]
#' @param sit The site, for site attributes. Must match site of file path. [character]
#' @param type The type of attributes to retrieve. [character]
#' @param valName If CO2 validation metadata are requested, the H5 name of the level where they can be found. [character]

#' @keywords internal
#' @return A data frame with one row containing the extracted attributes

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# changelog and author contributions / copyrights
#   Claire Lunch (2023-03-21)
##############################################################################################

getAttributes <- function(fil, sit, type, valName) {
  
  if(grepl("basic", fil)) {
    mnth <- regmatches(fil, regexpr("20[0-9]{2}-[0-9]{2}", fil))
  } else {
    mnth <- regmatches(fil, regexpr("20[0-9]{2}-[0-9]{2}-[0-9]{2}", fil))
  }
  
  if(type=="site") {
    lev <- sit
  }
  if(type=="root") {
    lev <- "/"
  }
  if(type=="val") {
    lev <- valName
  }
  
  gAttr <- base::try(rhdf5::h5readAttributes(fil, name=lev), silent=T)
  
  if(inherits(gAttr, "try-error")) {
    gAttr <- data.frame(matrix(data=c(sit, mnth), ncol=2, nrow=1))
    names(gAttr) <- c("site","date")
  } else {
    if(any(lapply(gAttr, length)>1)) {
      for(i in which(lapply(gAttr, length)>1)) {
        gAttr[i] <- paste0(gAttr[i], collapse=",")
      }
    }
    gAttr <- c(sit, mnth, gAttr)
    names(gAttr)[1:2] <- c("site", "date")
  }
  return(gAttr)
}

