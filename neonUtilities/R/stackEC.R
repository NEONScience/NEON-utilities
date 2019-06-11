##############################################################################################
#' @title Extract eddy covariance data from HDF5 format

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Convert data of choice from HDF5 to tabular format. Specific to eddy covariance data product: DP4.00200.001
#'
#' @param filepath The folder containing the H5 files [character]
#' @param level The level of data to extract; one of dp01, dp02, dp03, dp04 [character]
#' @param var The variable set to extract, e.g. co2Turb [character]
#' @param avg The averaging interval to extract, in minutes [numeric]

#' @details Given a filepath containing H5 files of DP4.00200.001 data, extracts variables, stacks data tables over time, and joins variables into a single table.
#' For data product levels 2-4 (dp02, dp03, dp04), joins all available data, except for the flux footprint data in the expanded package.
#' For dp01, an averaging interval and a set of variable names must be provided as inputs.

#' @return A named list of data frames. One data frame per site, plus one data frame containing the variable metadata (objDesc) table.

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
  
  # get list of files, unzipping if necessary
  if(substring(filepath, nchar(filepath)-3, nchar(filepath))==".zip") {
    outpath <- gsub(".zip", "", filepath)
    if(!dir.exists(outpath)) {
      dir.create(outpath)
    }
    utils::unzip(filepath, exdir=outpath)
    filepath <- outpath
  }
  files <- list.files(filepath, recursive=T)
  
  # unzip files if necessary
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
    levelInd <- grep(level, listDataName)
    
    if(level!="dp04" & level!="dp03" & level!="dp02" & !all(is.na(var))) {
      varInd <- which(listDataObj$name %in% var)
    } else {
      varInd <- 1:length(listDataName)
    }
    if(level!="dp04" & level!="dp03" & level!="dp02" & !is.na(avg)) {
      avgInd <- grep(paste(avg, "m", sep=""), listDataName)
    } else {
      if(level=="dp01") {
        stop("If level=='dp01', avg is a required input.")
      } else {
        avgInd <- 1:length(listDataName)
      }
    }
    
    # exclude footprint grid data
    if(length(grep("foot/grid", listDataName))>0) {
      gridInd <- grep("foot/grid", listDataName, invert=T)
    } else {
      gridInd <- 1:length(listDataName)
    }
    
    ind <- intersect(intersect(levelInd, intersect(varInd, avgInd)), gridInd)
    
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
  
  # join the concatenated tables

  sites <- unique(substring(names(timeMergList), 1, 4))
  varMergList <- vector("list", length(sites)+1)
  names(varMergList) <- c(sites, "variables")
  
  # set up progress bar
  writeLines(paste0("Joining data variables"))
  pb3 <- utils::txtProgressBar(style=3)
  utils::setTxtProgressBar(pb3, 0)
  idx <- 0
  
  # make one merged table per site
  for(m in 1:I(length(varMergList)-1)) {
    
    timeMergPerSite <- timeMergList[grep(sites[m], names(timeMergList))]
    
    # initiate merged table with just the time stamps
    # except turbulent flux, bc end times don't match other variables
    varMergTabl <- timeMergPerSite[[grep("turb", names(timeMergList), invert=T)[1]]][,c("timeBgn","timeEnd")]
    
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
      idx <- idx + 1
      utils::setTxtProgressBar(pb3, idx/(length(timeMergPerSite)*length(sites)))
    }
    
    varMergList[[m]] <- varMergTabl
  }
  utils::setTxtProgressBar(pb3, 1)
  close(pb3)

  # get one objDesc table and add to list
  variables <- base::try(rhdf5::h5read(paste(filepath, files[1], sep="/"), name="//objDesc"), silent=T)
  # if processing gets this far without failing, don't fail here, just return data without objDesc table
  if(class(variables)=="try-error") {
    variables <- NA
  }
  varMergList[["variables"]] <- variables
  
  return(varMergList)
}
