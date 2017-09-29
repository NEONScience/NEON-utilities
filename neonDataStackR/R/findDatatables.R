##############################################################################################
#' @title Find data tables

#' @author
#' Christine Laney \email{claney@battelleecology.org}

#' @description
#' List the names of the data tables within each folder
#'
#' @keywords internal
#' @param folder The folder of the outputs
#' @param fnames Full names - if true, then return the full file names including enclosing folders, if false, return only the file names
#' @return a data frame of file names

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Christine Laney (2017-09-28)
##############################################################################################

findDatatables <- function(folder, fnames = T){
  fs <- list.dirs(folder)
  g <- grep(pattern = "stackedFiles", x = fs)
  if(length(g) > 0){fs <- fs[-g]}
  fls <- character()
  for(i in 1:length(fs)){
    fls <- c(fls, list.files(fs[i], full.names = fnames))
  }
  return(fls[which(substr(fls, nchar(fls)-3, nchar(fls)) == ".csv")])
}
