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

stackEC <- function(filepath, level="dp04", var=c("nsae","stor","turb"), avg=NA) {
  
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
  
  # make empty, named list for the data tables
  gp <- vector("list", length(files))
  names(gp) <- substring(files, 1, nchar(files)-3)
  
  # set up progress bar
  writeLines(paste0("Extracting data"))
  pb <- utils::txtProgressBar(style=3)
  utils::setTxtProgressBar(pb, 0)

  # extract data from each file
  for(i in 1:length(files)) {
    
    listObj <- base::try(rhdf5::h5ls(paste(filepath, files[i], sep="/")), silent=T)
    
    if(class(listObj)=="try-error") {
      stop(paste("\n", paste(filepath, files[i], sep="/"), " could not be read.", sep=""))
    }
    
    listDataObj <- listObj[listObj$otype == "H5I_DATASET",]
    listDataName <- base::paste(listDataObj$group, listDataObj$name, sep = "/")
    
    # filter by variable/level selections
    if(!is.na(level)) {
      levelInd <- grep(level, listDataName)
    } else {
      levelInd <- 1:length(listDataName)
    }
    if(!all(is.na(var))) {
      varInd <- which(listDataObj$name %in% var)
    } else {
      varInd <- 1:length(listDataName)
    }
    if(!is.na(avg)) {
      avgInd <- grep(paste(avg, "m", sep=""), listDataName)
    } else {
      avgInd <- 1:length(listDataName)
    }
    
    ind <- intersect(levelInd, intersect(varInd, avgInd))
    
    # check that you haven't filtered to nothing
    if(length(ind)==0) {
      stop(paste("There are no data meeting the criteria level ", level, 
                 ", averaging interval ", avg, ", and variables ", var, sep=""))
    }
    
    listDataName <- listDataName[ind]
    
    gp[[i]] <- base::lapply(listDataName, rhdf5::h5read, 
                                 file=paste(filepath, files[i], sep="/"))
    base::names(gp[[i]]) <- substring(listDataName, 2, nchar(listDataName))
    
    utils::setTxtProgressBar(pb, i/length(files))
    
  }
  
  close(pb)
  
  # make empty, named list for the merged data tables
  tabs <- character()
  for(k in 1:length(gp)) {
    tabs <- c(tabs, names(gp[[k]]))
  }
  tabs <- unique(tabs)
  mg <- vector("list", length(tabs))
  names(mg) <- tabs
  
  # set up progress bar
  writeLines(paste0("Stacking data tables by month"))
  pb2 <- utils::txtProgressBar(style=3)
  utils::setTxtProgressBar(pb2, 0)
  
  # concatenate tables
  for(j in 1:length(mg)) {
    nm <- names(mg)[j]
    mg[[j]] <- gp[[1]][[nm]]
    for(k in 2:length(gp)) {
      mg[[j]] <- rbind(mg[[j]], gp[[k]][[nm]])
    }
    utils::setTxtProgressBar(pb2, j/length(mg))
  }
  close(pb2)
  
  return(mg)
}
