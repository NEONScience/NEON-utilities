##############################################################################################
#' @title Get a data frame with the names of all files within a zipped NEON data package

#' @author
#' Christine Laney \email{claney@battelleecology.org}

#' @description
#' Given the top level zip file, return dataframe of all of the files within it without
#' unzipping the file

#' @param zippath The path to a zip file
#' @return A list of filenames within the given zip file
#'
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# changelog and author contributions / copyrights
#   Christine Laney (2017-07-02)
##############################################################################################

listFilesInZip <- function(zippath){
  utils::unzip(zipfile = zippath, list = T)
}
