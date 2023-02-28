##############################################################################################
#' @title Extract eddy covariance data from HDF5 format

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Convert data of choice from HDF5 to tabular format. Specific to eddy covariance data product: DP4.00200.001
#'
#' @param filepath One of: a folder containing NEON EC H5 files, a zip file of DP4.00200.001 data downloaded from the NEON data portal, a folder of DP4.00200.001 data downloaded by the neonUtilities::zipsByProduct() function, or a single NEON EC H5 file [character]
#' @param level The level of data to extract; one of dp01, dp02, dp03, dp04 [character]
#' @param var The variable set to extract. Can be any of the variables in the "name" level or the "system" level of the H5 file; use the getVarsEddy() function to see the available variables. From the inputs, all variables from "name" and all variables from "system" will be returned, but if variables from both "name" and "system" are specified, the function will return only the intersecting set. This allows the user to, e.g., return only the pressure data ("pres") from the CO2 storage system ("co2Stor"), instead of all the pressure data from all instruments.  [character]
#' @param avg The averaging interval to extract, in minutes [numeric]

#' @details Given a filepath containing H5 files of DP4.00200.001 data, extracts variables, stacks data tables over time, and joins variables into a single table.
#' For data product levels 2-4 (dp02, dp03, dp04), joins all available data, except for the flux footprint data in the expanded package.
#' For dp01, an averaging interval and a set of variable names must be provided as inputs.

#' @return A named list of data frames. One data frame per site, plus one data frame containing the metadata (objDesc) table and one data frame containing units for each variable (variables).

#' @examples
#' \dontrun{
#' # To extract and merge Level 4 data tables, where data files are in the working directory
#' flux <- stackEddy(filepath=getwd(), level='dp04', var=NA, avg=NA)
#' }

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2019-05-29)
#     partially adapted from eddy4R.base::def.hdf5.extr() authored by David Durden
##############################################################################################

stackEddy <- function(filepath, level="dp04", var=NA, avg=NA) {
  
  # first check for rhdf5 package
  if(!requireNamespace("rhdf5", quietly=T)) {
    stop("Package rhdf5 is required for this function to work.
         \nrhdf5 is a Bioconductor package. To install, use:\ninstall.packages('BiocManager')\nBiocManager::install('rhdf5')\n")
  }
  
  files <- NA
  # check for vector of files as input
  if(length(filepath)>1) {
    if(length(grep(".h5$", filepath))==length(filepath)) {
      files <- filepath
    } else {
      stop("Input list of files must be .h5 files.")
    }
    if(any(!file.exists(files))) {
      stop("Files not found in specified filepaths. Check that the input list contains the correct filepaths.")
    }
  }
  
  # get list of files, unzipping if necessary
  if(any(is.na(files)) & identical(substring(filepath, nchar(filepath)-3, nchar(filepath)), ".zip")) {
    outpath <- gsub(".zip", "", filepath)
    if(!dir.exists(outpath)) {
      dir.create(outpath)
    }
    if(length(grep(".zip", utils::unzip(filepath, list=T)$Name, fixed=T))>0) {
      utils::unzip(filepath, exdir=outpath)
    } else {
      utils::unzip(filepath, exdir=outpath, junkpaths=T)
    }
    filepath <- outpath
  }
  
  # allow for a single H5 file
  if(any(is.na(files)) & identical(substring(filepath, nchar(filepath)-2, nchar(filepath)), ".h5")) {
    files <- filepath
  } else {
    if(any(is.na(files))) {
      files <- list.files(filepath, recursive=F, full.names=T)
    }
  }
  
  # unzip files if necessary
  if(length(grep(".zip", files))==length(files)) {
    lapply(files, function(x) {
      utils::unzip(x, exdir=filepath)
    })
    files <- list.files(filepath, recursive=F, full.names=T)
  }
  
  # after unzipping, check for .gz
  if(length(grep(".h5.gz", files))>0) {
    lapply(files[grep(".h5.gz", files)], function(x) {
      R.utils::gunzip(x)
    })
    files <- list.files(filepath, recursive=F, full.names=T)
  }
  
  # need the H5 files for data extraction and the SRF tables
  scienceReviewList <- unique(files[grep("science_review_flags", files)])
  files <- files[grep(".h5$", files)]
  
  # check for duplicate files and use the most recent
  fileDups <- gsub("[0-9]{8}T[0-9]{6}Z.h5", "", files)
  if(any(base::duplicated(fileDups))) {
    maxFiles <- character()
    for(i in unique(fileDups)) {
      maxFiles <- c(maxFiles, 
                    max(files[grep(i, files)]))
    }
    files <- maxFiles
  }
  
  # check for no files
  if(identical(length(files), as.integer(0))) {
    stop("No .h5 files found in specified file path. Check the inputs and file contents.")
  }
  
  # make empty, named list for the data tables
  tableList <- vector("list", length(files))
  names(tableList) <- substring(basename(files), 1, nchar(basename(files))-3)
  
  # set up progress bar
  writeLines(paste0("Extracting data"))
  pb <- utils::txtProgressBar(style=3)
  utils::setTxtProgressBar(pb, 0)

  # extract data from each file
  for(i in 1:length(files)) {
    
    listObj <- base::try(rhdf5::h5ls(files[i]), silent=T)
    
    if(inherits(listObj, "try-error")) {
      cat(paste("\n", paste(files[i], " could not be read.", sep="")))
      next
    }
    
    listDataObj <- listObj[listObj$otype == "H5I_DATASET",]
    listDataName <- base::paste(listDataObj$group, listDataObj$name, sep = "/")
    listObjSpl <- tidyr::separate(listDataObj, col="group", 
                                  into=c(NA, "site", "level", "category", "system", 
                                         "horvertmi", "subsys"), 
                                  sep="/", fill="right")
    
    # filter by variable/level selections
    levelInd <- grep(level, listDataName)
    
    if(level!="dp04" & level!="dp03" & level!="dp02" & !all(is.na(var))) {
      if(length(which(listObjSpl$system %in% var))>0) {
        if(length(which(listDataObj$name %in% var))>0) {
          varInd <- base::intersect(which(listDataObj$name %in% var), 
                                    which(listObjSpl$system %in% var))
        } else {
          varInd <- which(listObjSpl$system %in% var)
        }
      } else {
        if(length(which(listDataObj$name %in% var))>0) {
          varInd <- which(listObjSpl$name %in% var)
        } else {
          stop(paste("No data found for variables ", paste(var, collapse=" "), sep=""))
        }
      }
      
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
    
    # exclude footprint grid data from expanded packages
    if(length(grep("foot/grid", listDataName))>0) {
      gridInd <- grep("foot/grid", listDataName, invert=T)
    } else {
      gridInd <- 1:length(listDataName)
    }
    
    # index that includes all filtering criteria
    ind <- intersect(intersect(levelInd, intersect(varInd, avgInd)), gridInd)
    
    # check that you haven't filtered to nothing
    if(length(ind)==0) {
      stop(paste("There are no data meeting the criteria level ", level, 
                 ", averaging interval ", avg, ", and variables ", 
                 paste(var, collapse=" "), sep=""))
    }
    
    listDataName <- listDataName[ind]
    
    # add extracted data to the list
    tableList[[i]] <- base::lapply(listDataName, rhdf5::h5read, 
                                 file=files[i], read.attributes=T)
    base::names(tableList[[i]]) <- substring(listDataName, 2, nchar(listDataName))
    
    utils::setTxtProgressBar(pb, i/length(files))
    
  }
  close(pb)
  
  # get variable units
  variables <- character(5)
  for(p in 1:length(tableList[[1]])) {
    if(!is.null(attributes(tableList[[1]][[p]])$unit)) {
      var.nm <- strsplit(names(tableList[[1]])[p], 
                         split="/", fixed=T)[[1]][c(3,4,length(strsplit(names(tableList[[1]])[p], 
                                                             split="/", fixed=T)[[1]]))]
      if(length(attributes(tableList[[1]][[p]])$unit)>1) {
        var.nm <- matrix(var.nm, ncol=3, nrow=length(attributes(tableList[[1]][[p]])$unit), byrow=T)
        if(length(attributes(tableList[[1]][[p]])$unit)==length(attributes(tableList[[1]][[p]])$names)) {
          var.nm <- cbind(var.nm, attributes(tableList[[1]][[p]])$names)
        } else {
          if("index" %in% attributes(tableList[[1]][[p]])$names) {
            var.nm <- cbind(var.nm, 
                            attributes(tableList[[1]][[p]])$names[-which(attributes(tableList[[1]][[p]])$names=="index")])
          } else {
            var.nm <- cbind(var.nm, 
                            attributes(tableList[[1]][[p]])$names[-which(attributes(tableList[[1]][[p]])$names 
                                                                         %in% c("timeBgn","timeEnd"))])
          }
        }
        var.nm <- cbind(var.nm, attributes(tableList[[1]][[p]])$unit)
        variables <- rbind(variables, var.nm)
      } else {
        variables <- rbind(variables, c(var.nm, "", attributes(tableList[[1]][[p]])$unit))
      }
    }
  }
  variables <- data.frame(unique(variables))
  if(nrow(variables)==1) {
    variables <- NA
  } else {
    variables <- variables[-1,]
    colnames(variables) <- c("category","system","variable","stat","units")
    rownames(variables) <- 1:nrow(variables)
  }
  
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
    nm <- base::names(timeMergList)[j]
    
    # subset to one site at a time
    tableListSub <- tableList[base::grep(substring(nm, 1, 4), base::names(tableList))]

    # go through list of tables for the site, extract the table that matches current table name
    tableListToMerge <- vector('list', length(tableListSub))
    for(k in 1:length(tableListSub)) {
      tableListToMerge[[k]] <- tableListSub[[k]][[nm]]
    }
    
    # stack list of tables with matching table name
    timeMergList[[j]] <- data.frame(data.table::rbindlist(tableListToMerge, fill=T))
    
    utils::setTxtProgressBar(pb2, j/length(timeMergList))
  }
  close(pb2)
  
  # convert all time stamps to time format, then filter out instances with:
  # 1) only one record for a day
  # 2) all values = NaN
  # these are instances when a sensor was offline, and they don't join correctly
  err <- FALSE
  timeMergList <- lapply(timeMergList, function(x) {
    tabtemp <- eddyStampCheck(x)
    if(tabtemp[[2]]) {
      err <- TRUE
    }
    return(tabtemp[[1]])
  })
  if(err) {
    message("Some time stamps could not be converted. Variable join may be affected; check data carefully for disjointed time stamps.")
  }

  # for dp01 and dp02, stack tower levels and calibration gases
  if(level=="dp01" | level=="dp02") {
    namesSpl <- data.frame(matrix(unlist(strsplit(names(timeMergList), split="/", fixed=T)), 
                       nrow=length(names(timeMergList)), byrow=T))
    verSpl <- tidyr::separate(namesSpl, col="X5", 
                              into=c("hor", "ver", "tmi"), 
                              sep="_", fill="left")
    
    # add hor and ver index to tables
    for(n in 1:length(timeMergList)) {
      verticalPosition <- rep(verSpl$ver[n], nrow(timeMergList[[n]]))
      horizontalPosition <- rep(verSpl$hor[n], nrow(timeMergList[[n]]))
      timeMergList[[n]] <- cbind(horizontalPosition, verticalPosition, timeMergList[[n]])
    }
    
    # next: stack everything with names that match when index is excluded
    # timeMergList becomes list of the stacked, renamed objects
    profNames <- apply(verSpl[,grep("X", names(verSpl))], 1, paste0, collapse="/")
    profTabs <- unique(profNames)
    
    # make empty, named list for the merged data tables
    verMergList <- vector("list", length(profTabs))
    names(verMergList) <- profTabs
    
    for(o in 1:length(verMergList)) {

      # table to concatenate
      nm <- names(verMergList)[o]
      
      # get matching tables
      verListSub <- timeMergList[which(profNames==nm)]
      
      # stack contents
      verMergList[[o]] <- data.frame(data.table::rbindlist(verListSub, fill=T))
    }
    
    timeMergList <- verMergList

  }
  
  # join the concatenated tables

  sites <- unique(substring(names(timeMergList), 1, 4))
  varMergList <- vector("list", length(sites)+4)
  names(varMergList) <- c(sites, "variables", "objDesc", 
                          "issueLog", "scienceReviewFlags")
  
  # set up progress bar
  writeLines(paste0("Joining data variables"))
  pb3 <- utils::txtProgressBar(style=3)
  utils::setTxtProgressBar(pb3, 0)
  idx <- 0
  
  # make one merged table per site
  for(m in 1:I(length(varMergList)-4)) {
    
    timeMergPerSite <- timeMergList[grep(sites[m], names(timeMergList))]

    if(level=="dp01" | level=="dp02") {
      nameSet <- c("timeBgn","timeEnd","horizontalPosition","verticalPosition")
      mergSet <- c("horizontalPosition","verticalPosition","timeBgn")
    } else {
      nameSet <- c("timeBgn","timeEnd")
      mergSet <- "timeBgn"
    }

    # get a set of time stamps to initiate the table. leave out qfqm to exclude 
    # filler records created as placeholders for days with no data
    timeSet <- timeMergPerSite[grep("qfqm", names(timeMergPerSite), invert=T)]
    # turbulent flux and footprint end time stamps don't quite match the others
    timeSet <- timeSet[grep("turb", names(timeSet), invert=T)]
    timeSet <- timeSet[grep("foot", names(timeSet), invert=T)]
    
    # initiate the table with consensus set of time stamps
    timeSetInit <- timeSet[[1]][,nameSet]
    if(length(timeSet)==1) {
      timeSetInit <- timeSetInit
    } else {
      for(q in 2:length(timeSet)) {
        # check for additional start time stamps
        timeSetTemp <- timeSet[[q]][,nameSet]
        timeSetTempMerg <- data.table::as.data.table(timeSetTemp[,mergSet])
        timeSetInitMerg <- data.table::as.data.table(timeSetInit[,mergSet])
        misTime <- data.table::fsetdiff(timeSetTempMerg, timeSetInitMerg)
        if(nrow(misTime)==0) {
          timeSetInit <- timeSetInit
        } else {
          # combine all, then de-dup
          allTime <- data.table::rbindlist(list(timeSetInit, timeSetTemp), fill=TRUE)
          timeSetInit <- unique(allTime, by=mergSet)
        }
      }
    }
    
    varMergTabl <- as.data.frame(timeSetInit)
    
    # merge individual variable tables into one
    for(l in 1:length(timeMergPerSite)) {
      names(timeMergPerSite[[l]])[which(!names(timeMergPerSite[[l]]) %in% nameSet)] <- 
        paste(names(timeMergPerSite)[l], names(timeMergPerSite[[l]])[which(!names(timeMergPerSite[[l]]) %in% nameSet)],
              sep=".")
      names(timeMergPerSite[[l]]) <- gsub("/", ".", names(timeMergPerSite[[l]]), fixed=T)
      names(timeMergPerSite[[l]])[which(!names(timeMergPerSite[[l]]) %in% nameSet)] <- 
        substring(names(timeMergPerSite[[l]])[which(!names(timeMergPerSite[[l]]) %in% nameSet)], 
                  11, nchar(names(timeMergPerSite[[l]])[which(!names(timeMergPerSite[[l]]) %in% nameSet)]))

      varMergTabl <- base::merge(varMergTabl, 
                           timeMergPerSite[[l]][,-which(names(timeMergPerSite[[l]])=="timeEnd")],
                           by=mergSet, all.x=T, all.y=F)
      idx <- idx + 1
      utils::setTxtProgressBar(pb3, idx/(length(timeMergPerSite)*length(sites)))
    }
    if(level=="dp01" | level=="dp02") {
      varMergTabl <- varMergTabl[order(varMergTabl$horizontalPosition, 
                                       varMergTabl$verticalPosition, 
                                       varMergTabl$timeBgn),]
    } else {
      varMergTabl <- varMergTabl[order(varMergTabl$timeBgn),]
    }
    varMergList[[m]] <- varMergTabl
  }
  utils::setTxtProgressBar(pb3, 1)
  close(pb3)

  
  # site attributes, objDesc, SRF table, and issue log
  writeLines(paste0("Getting metadata tables"))
  pb4 <- utils::txtProgressBar(style=3)
  utils::setTxtProgressBar(pb4, 0)
  
  # get one objDesc table and add it and variables table to list
  objDesc <- base::try(rhdf5::h5read(files[1], name="//objDesc"), silent=T)
  # if processing gets this far without failing, don't fail here, just return data without objDesc table
  if(inherits(objDesc, "try-error")) {
    objDesc <- NA
  }
  varMergList[["variables"]] <- variables
  varMergList[["objDesc"]] <- objDesc
  
  utils::setTxtProgressBar(pb4, 0.5)
  
  # get issue log
  if(!curl::has_internet()) {
    message("No internet connection, issue log file not accessed. Issue log can be found on the data product details pages.")
  } else {
    # token not used here, since token is not otherwise used/accessible in this function
    varMergList[["issueLog"]] <- getIssueLog(dpID="DP4.00200.001")
  }
  
  utils::setTxtProgressBar(pb4, 0.75)
  
  # aggregate the science_review_flags files
  if(length(scienceReviewList)>0) {
    outputScienceReview <- data.table::rbindlist(lapply(scienceReviewList, 
                                                                   function(x) {
                                                                     
              outTbl <- data.table::fread(x, header=TRUE, encoding="UTF-8", keepLeadingZeros = TRUE,
                                      colClasses = list(character = c('startDateTime','endDateTime',
                                                                      'createDateTime',
                                                                      'lastUpdateDateTime')))
                            if(identical(nrow(outTbl), as.integer(0))) {
                                return()
                            }
                            return(outTbl)
                  }), fill=TRUE)
    
    # remove duplicates
    outputScienceReview <- unique(outputScienceReview)
    
    # check for non-identical duplicates with the same ID and keep the most recent one
    if(length(unique(outputScienceReview$srfID))!=nrow(outputScienceReview)) {
      dupRm <- numeric()
      rowids <- 1:nrow(outputScienceReview)
      origNames <- colnames(outputScienceReview)
      outputScienceReview <- cbind(rowids, outputScienceReview)
      for(k in unique(outputScienceReview$srfID)) {
        scirvwDup <- outputScienceReview[which(outputScienceReview$srfID==k),]
        if(nrow(scirvwDup)>1) {
          dupRm <- c(dupRm, 
                     scirvwDup$rowids[which(scirvwDup$lastUpdateDateTime!=max(scirvwDup$lastUpdateDateTime))])
        }
      }
      if(length(dupRm)>0) {
        outputScienceReview <- outputScienceReview[-dupRm,origNames]
      } else {
        outputScienceReview <- outputScienceReview[,origNames]
      }
    }
    varMergList[["scienceReviewFlags"]] <- outputScienceReview
  } else {
    varMergList <- varMergList[-grep("scienceReviewFlags", names(varMergList))]
  }
  
  utils::setTxtProgressBar(pb4, 1)
  close(pb4)
  
  return(varMergList)
}
