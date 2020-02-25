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
  dirs <- list.dirs(folder, recursive = FALSE)
  dirsNotStacked <- dirs[-grep(pattern = "stackedFiles", x = dirs)]
  dirsNotStacked <- dirsNotStacked[!dirsNotStacked %in% orig]
  if(length(dirsNotStacked) > 0) {unlink(dirsNotStacked, recursive = TRUE)}
  fil <- list.files(folder, recursive = FALSE)
  csvfil <- fil[grep("csv", fil)]
  csvfil <- csvfil[!csvfil %in% orig]
  if(length(csvfil) > 0) {unlink(paste(folder, csvfil, sep="/"), recursive = FALSE)}
  
  writeLines("All unzipped monthly data folders have been removed.")
}
