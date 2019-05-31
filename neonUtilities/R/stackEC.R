##############################################################################################
#' @title Extract eddy covariance variables from HDF5 format

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Convert variables of choice from HDF5 to tabular format. Specific to eddy covariance data product: DP4.00200.001
#'
#' @param filepath The folder containing the H5 files [character]
#' @param level The level of data to extract; one of dp01, dp02, dp03, dp04, dp0p [character]
#' @param var The variable set to extract, e.g. co2Turb [character]
#' @param avg The averaging interval to extract, in minutes [numeric]

#' @return A data frame of the specified variables

#' @examples

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2019-05-29)
#     partially adapted from eddy4R.base::def.hdf5.extr() authored by David Durden
##############################################################################################

stackEC <- function(filepath, level, var, avg) {
  
  # get list of files
  files <- list.files(filepath, recursive=T)
  
  # unzip if necessary
  if(length(grep(".zip", files))==length(files)) {
    for(i in 1:length(files)) {
      utils::unzip(paste(filepath, files[i], sep="/"), exdir=filepath)
    }
    files <- list.files(filepath, recursive=T)
  }
  
  files <- files[grep(".h5", files)]
  
  # set up progress bar
  writeLines(paste0("Extracting data"))
  pb <- utils::txtProgressBar(style=3)
  utils::setTxtProgressBar(pb, 0)

  # extract data from each file
  gp <- vector("list", length(files))
  names(gp) <- substring(files, 1, nchar(files)-3)
  for(i in 1:length(files)) {
    
    listObj <- base::try(rhdf5::h5ls(paste(filepath, files[i], sep="/")), silent=T)
    
    if(class(listObj)=="try-error") {
      stop(paste("\n", paste(filepath, files[i], sep="/"), " could not be read.", sep=""))
    }
    
    listDataObj <- listObj[listObj$otype == "H5I_DATASET",]
    listDataName <- base::paste(listDataObj$group, listDataObj$name, sep = "/")
    
    # filter by variable/level selections
    if(!is.na(level)) {
      listDataName <- listDataName[grep(level, listDataName)]
    }
    if(!is.na(var)) {
      listDataName <- listDataName[listDataObj$name==var]
    }
    if(!is.na(avg)) {
      listDataName <- listDataName[grep(paste(avg, "m", sep=""), listDataName)]
    }
    
    gp[[i]] <- base::lapply(listDataName, rhdf5::h5read, 
                                 file=paste(filepath, files[1], sep="/"))
    base::names(gp[[i]]) <- listDataName
    
    utils::setTxtProgressBar(pb, i/length(files))
    
  }
  
}
