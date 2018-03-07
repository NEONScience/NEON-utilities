##############################################################################################
#' @title Join data files in a zipped NEON data package by table type

#' @author
#' Christine Laney \email{claney@battelleecology.org}
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Given a zipped data file, do a full join of all data files, grouped by table type.
#' This should result in a small number of large files.

#' @param filepath The location of the zip file
#' @param folder T or F: does the filepath point to a parent, unzipped folder, or a zip file? If F, assumes the filepath points to a zip file. Defaults to F.
#' @param saveUnzippedFiles T or F: should the unzipped monthly data folders be retained?
#' @return All files are unzipped and one file for each table type is created and written.

#' @export

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Christine Laney (2017-07-02)
#   Claire Lunch (2017-09-28)
##############################################################################################

stackByTable <- function(dpID, filepath, package = 'basic', folder=FALSE, saveUnzippedFiles = TRUE){
  if(missing(dpID)){
    stop("Missing a value for dpID")
  }

  # error message if package is not basic or expanded
  if(!package %in% c("basic", "expanded")) {
    stop(paste(package, "is not a valid package name. Package must be basic or expanded", sep=" "))
  }

  # error message if dpID isn't formatted as expected
  if(regexpr("DP[1-4]{1}.[0-9]{5}.001",dpID)!=1) {
    stop(paste(dpID, "is not a properly formatted data product ID. The correct format is DP#.#####.001, where the first placeholder must be between 1 and 4.", sep=" "))
  }

  checkNonstackables(dpID, package)

  if(folder==FALSE) {
    location.data <- substr(filepath, 1, nchar(filepath)-4)
    unzipZipfile(zippath = filepath, outpath = location.data, level = "all")
    stackDataFiles(location.data)
    if(saveUnzippedFiles == FALSE){cleanUp(location.data)}
  } else {
    unzipZipfile(zippath = filepath, outpath = filepath, level = "in")
    stackDataFiles(filepath)
    if(saveUnzippedFiles == FALSE){cleanUp(filepath)}
  }
}


