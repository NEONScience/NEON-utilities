##############################################################################################
#' @title Get a file's size in megabytes

#' @author
#' Christine Laney \email{claney@battelleecology.org}

#' @description
#' For any file, read the size and translate into a human readable string
#'
#' @importFrom gdata humanReadable
#'
#' @param filepath The path to the file
#' @return The size of the file in megabytes
#'
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Christine Laney (2017-07-02)
##############################################################################################

getFilesize <- function(filepath){
  fs <- humanReadable(file.size(filepath), units = "auto", standard = "SI")
  return(fs)
}
