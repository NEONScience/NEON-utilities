##############################################################################################
#' @title Clean up folder after stacking

#' @author
#' Christine Laney \email{claney@battelleecology.org}

#' @description
#' Remove unzipped monthly data folders
#'
#' @keywords internal
#' @param folder The file path to the folder that needs to be cleaned up (the root directory of the data package)
#' @param orig The list of files that were present in the folder before unzipping and stacking
#' @return Only the folders created during unzip will be deleted. All custom folders/files and the stackedFiles output folder will be retained.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Christine Laney (2018-01-10)
#   Nathan Mietkiewicz (2020-02-12)
##############################################################################################
cleanUp <- function(folder, orig) {
  
  currentFileList <- orig
  
  if(length(currentFileList) > 0) {
    unlink(currentFileList, recursive = TRUE)
  }
  
  dirlist <- list.dirs(folder)
  for(i in 1:length(dirlist)) {
    if(length(list.files(dirlist[i]))==0) {
      unlink(dirlist[i], recursive=TRUE)
    }
  }
  
  message("All unzipped monthly data folders have been removed.")
}
