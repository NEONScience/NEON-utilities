##############################################################################################
#' @title Extract eddy covariance data from HDF5 format

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Convert data of choice from HDF5 to tabular format. Specific to eddy covariance data product: DP4.00200.001
#'
#' @param filepath The folder containing the H5 files [character]
#' @param level The level of data to extract; one of dp01, dp02, dp03, dp04, dp0p [character]
#' @param var The variable set to extract, e.g. co2Turb [character]
#' @param avg The averaging interval to extract, in minutes [numeric]

#' @return A data frame of the specified variables

#' @examples
#' \dontrun{
#' # To extract and merge Level 4 data tables, where data files are in the working directory
#' flux <- stackEC(filepath=getwd(), level='dp04', var=NA, avg=NA)
#' }

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2019-05-29)
#     partially adapted from eddy4R.base::def.hdf5.extr() authored by David Durden
##############################################################################################

stackEC <- function(filepath, level="dp04", var=NA, avg=NA) {
  
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
  tableList <- vector("list", length(files))
  names(tableList) <- substring(files, 1, nchar(files)-3)
  
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
    if(level!="dp04" & !all(is.na(var))) {
      varInd <- which(listDataObj$name %in% var)
    } else {
      varInd <- 1:length(listDataName)
    }
    if(level!="dp04" & !is.na(avg)) {
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
    
    tableList[[i]] <- base::lapply(listDataName, rhdf5::h5read, 
                                 file=paste(filepath, files[i], sep="/"))
    base::names(tableList[[i]]) <- substring(listDataName, 2, nchar(listDataName))
    
    utils::setTxtProgressBar(pb, i/length(files))
    
  }
  
  close(pb)
  
  # make empty, named list for the merged data tables
  tabs <- character()
  for(k in 1:length(tableList)) {
    tabs <- c(tabs, names(tableList[[k]]))
  }
  tabs <- unique(tabs)
  timeMergList <- vector("list", length(tabs))
  names(timeMergList) <- tabs

  # set up progress bar
  writeLines(paste0("Stacking data tables by month"))
  pb2 <- utils::txtProgressBar(style=3)
  utils::setTxtProgressBar(pb2, 0)
  
  # concatenate tables
  for(j in 1:length(timeMergList)) {
    
    # table to concatenate
    nm <- names(timeMergList)[j]
    
    # subset to one site at a time
    tableListSub <- tableList[grep(substring(nm, 1, 4), names(tableList))]
    
    # get full set of variable names for the table to concatenate
    colN <- character()
    for(k in 1:length(tableListSub)) {
      colN <- c(colN, names(tableListSub[[k]][[nm]]))
    }
    colN <- unique(colN)
    
    # stack the tables
    tempDF <- data.frame(rep(NA, length(colN)))
    tempDF <- t(tempDF)
    colnames(tempDF) <- colN
    timeMergList[[j]] <- tempDF
    for(k in 1:length(tableListSub)) {
      timeMergList[[j]] <- rbind(timeMergList[[j]], tableListSub[[k]][[nm]])
    }
    timeMergList[[j]] <- timeMergList[[j]][-1,]
    utils::setTxtProgressBar(pb2, j/length(timeMergList))
  }
  close(pb2)
  
  # for level=dp04, join the concatenated tables
  # can the same function work for different levels?

  sites <- unique(substring(names(timeMergList), 1, 4))
  varMergList <- vector("list", length(sites))
  names(varMergList) <- sites
  
  # set up progress bar
  writeLines(paste0("Joining data variables"))
  pb3 <- utils::txtProgressBar(style=3)
  utils::setTxtProgressBar(pb3, 0)
  
  # make one merged table per site
  for(m in 1:length(varMergList)) {
    
    timeMergPerSite <- timeMergList[grep(sites[m], names(timeMergList))]
    
    # initiate merged table with just the time stamps
    # this only works for dp04
    varMergTabl <- timeMergPerSite[[grep("nsae", names(timeMergList))[1]]][,c("timeBgn","timeEnd")]
    
    # merge individual variable tables into one
    for(l in 1:length(timeMergPerSite)) {
      names(timeMergPerSite[[l]])[which(!names(timeMergPerSite[[l]]) %in% c("timeBgn","timeEnd"))] <- 
        paste(names(timeMergPerSite)[l], names(timeMergPerSite[[l]])[which(!names(timeMergPerSite[[l]]) %in% c("timeBgn","timeEnd"))],
              sep=".")
      names(timeMergPerSite[[l]]) <- gsub("/", ".", names(timeMergPerSite[[l]]), fixed=T)
      names(timeMergPerSite[[l]])[which(!names(timeMergPerSite[[l]]) %in% c("timeBgn","timeEnd"))] <- 
        substring(names(timeMergPerSite[[l]])[which(!names(timeMergPerSite[[l]]) %in% c("timeBgn","timeEnd"))], 
                  11, nchar(names(timeMergPerSite[[l]])[which(!names(timeMergPerSite[[l]]) %in% c("timeBgn","timeEnd"))]))
      varMergTabl <- merge(varMergTabl, 
                           timeMergPerSite[[l]][,-which(names(timeMergPerSite[[l]])=="timeEnd")],
                           by="timeBgn")
      utils::setTxtProgressBar(pb3, (l*m)/(length(timeMergPerSite)*length(sites)))
    }
    
    varMergList[[m]] <- varMergTabl
  }
  close(pb3)

  return(varMergList)
}
