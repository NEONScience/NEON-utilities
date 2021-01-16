##############################################################################################
#' @title Get all zip file names within a zipped NEON data package

#' @author
#' Christine Laney \email{claney@battelleecology.org}

#' @description
#' Given the data frame of all the files within the top level zip file,
#' return an array of just the zip file names (no pdf, xml, or other files).
#'
#' @param zippath The path to a zip file
#' @return An array of all zip files contained within the focal zip file
#'
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# changelog and author contributions / copyrights
#   Christine Laney (2017-07-02)
##############################################################################################

listZipfiles <- function(zippath){
  df <- utils::unzip(zipfile = zippath, list = T)
  ns <- df[,1]
  fn <- ns[which(substr(ns, nchar(ns)-3, nchar(ns)) == ".zip")]
  return(fn)
}
