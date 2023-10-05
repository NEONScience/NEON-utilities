##############################################################################################
#' @title Join data files in a unzipped NEON data package by table type

#' @author
#' Christine Laney \email{claney@battelleecology.org}

#' @description
#' Given a folder of unzipped files (unzipped NEON data file), do a full join of all data files, grouped by table type.
#' This should result in a small number of large files.

#' @param folder The location of the data
#' @param nCores The number of cores to parallelize the stacking procedure. To automatically use the maximum number of cores on your machine we suggest setting 'nCores=parallel::detectCores()'. By default it is set to a single core. If the files are less than 25000 bytes the userdefined nCores will be overridden to a single core.
#' @param dpID The data product identifier
#' @return One file for each table type is created and written.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2017-07-02 (Christine Laney): Original creation
#   2018-04-03 (Christine Laney):
#     * Swap read.csv() for data.table::fread() for faster data table loading
#     * Swap data.table::rbind() for dplyr::join() for faster table joins
#     * Remove join messages, replace with progress bars
#     * Provide comparison of number of rows expected per stacked table vs number of row in final table
#   2018-04-13 (Christine Laney):
#     * Continuous stream discharge (DP4.00130.001) is an OS product in IS format. Adjusted script to stack properly.
#   2019-11-14 (Nathan Mietkiewicz)
#     * Parallelized the function
##############################################################################################

stackDataFilesParallel <- function(folder, nCores=1, dpID){
  
  starttime <- Sys.time()
  messages <- character()
  releases <- character()
  
  # get the in-memory list of table types (site-date, site-all, etc.). This list must be updated often.
  #data("table_types")
  ttypes <- table_types[which(table_types$productID==dpID),]
  dpnum <- substring(dpID, 5, 9)
  
  # filenames without full path
  filenames <- findDatatables(folder = folder, fnames = F)
  
  # filenames with full path
  filepaths <- findDatatables(folder = folder, fnames = T)
  
  # get release file, if it exists
  relfl <- grep("release_status", filepaths)
  if(length(relfl)==1) {
    reltab <- data.table::fread(filepaths[relfl],
                                header=TRUE, encoding="UTF-8")
  } else {
    reltab <- NA
  }
  
  # handle per-sample tables separately
  if(dpID %in% c("DP1.30012.001", "DP1.10081.001", "DP1.20086.001", 
                 "DP1.20141.001", "DP1.20190.001", "DP1.20193.001") & 
     length(grep("^NEON.", basename(filenames), invert=TRUE))>0) {
    framefiles <- filepaths[grep("^NEON.", basename(filenames), invert=TRUE)]
    filepaths <- filepaths[grep("^NEON.", basename(filenames))]
    filenames <- filenames[grep("^NEON.", basename(filenames))]
    
    # stack frame files
    writeLines("Stacking per-sample files. These files may be very large; download data in smaller subsets if performance problems are encountered.")
    if(dir.exists(paste0(folder, "/stackedFiles")) == F) {dir.create(paste0(folder, "/stackedFiles"))}
    
    frm <- data.table::rbindlist(pbapply::pblapply(as.list(framefiles), function(x) {
      tempf <- data.table::fread(x)
      tempf$fileName <- rep(basename(x), nrow(tempf))
      return(tempf)
      }), fill=TRUE)
    
    if(dpID=="DP1.20190.001") {
      data.table::fwrite(frm, paste0(folder, "/stackedFiles/", "rea_conductivityRawData", ".csv"))
    } else {
      if(dpID=="DP1.20193.001") {
        data.table::fwrite(frm, paste0(folder, "/stackedFiles/", "sbd_conductivityRawData", ".csv"))
      } else {
      data.table::fwrite(frm, paste0(folder, "/stackedFiles/", "per_sample", ".csv"))
      }
    }
    
  }
  
  # make a list, where filenames are the keys to the filepath values
  filelist <- stats::setNames(as.list(filepaths), filenames)
  
  datafls <- filelist
  
  # if there are no datafiles, exit
  if(length(datafls) == 0){
    stop("No data files are present in specified file path.")
  }
  
  # if there is just one data file (and thus one table name), copy file into stackedFiles folder
  if(length(datafls) == 1){
    if(dir.exists(paste0(folder, "/stackedFiles")) == F) {dir.create(paste0(folder, "/stackedFiles"))}
    file.copy(from = datafls[1][[1]], to = "/stackedFiles")
    m <- 0
    n <- 1
  }
  
  # if there is more than one data file, stack files
  if(length(datafls) > 1){
    
    if(dir.exists(paste0(folder, "/stackedFiles")) == F) {dir.create(paste0(folder, "/stackedFiles"))}
    
    # detecting table types by file format, then checking against table_types
    # reducing dependency on table_types updating
    tableForm <- findTablesByFormat(names(datafls))
    tables <- tableForm$tableName
    
    # check against table_types. use grep() since format only identifies to 'lab'
    mis <- 0
    newtables <- character()
    for(j in 1:nrow(tableForm)) {
      ind <- which(ttypes$tableName==tableForm$tableName[j])
      if(length(ind)==0) {
        newtables <- rbind(newtables, tableForm[j,])
        next
        }
      indt <- grep(tableForm$tableType[j], ttypes$tableType[ind])
      if(length(indt)==0) {
        mis <- mis+1
      }
    }
    
    # for new tables, always use inferred types. for mismatches, decide based on publication dates
    if(!is.null(nrow(newtables))) {
      cat("Table(s)", newtables[,1], "are unexpected. Stacking will proceed based on inferred table format; check for updates to neonUtilities.\n")
      ttypes <- data.table::rbindlist(list(ttypes, newtables), fill=T)
    }
    
    if(mis>0) {
      dats <- character()
      filespl <- strsplit(filenames, "\\.")
      for(k in 1:length(filespl)) {
        dats <- c(dats, filespl[[k]][length(filespl[[k]])-1])
      }
      dats <- as.Date(dats, format="%Y%m%dT%H%M%SZ")
      if(min(dats, na.rm=T) > utils::packageDate("neonUtilities")) {
        cat("Downloaded data formats do not match expected formats. Data publication dates are more recent than neonUtilities version. Stacking will proceed using inference from downloaded data formats. Check results carefully, and check for updates to neonUtilities.\n")
        ttypes <- tableForm
      } else {
        if(max(dats, na.rm=T) < utils::packageDate("neonUtilities")) {
          cat("Downloaded data formats do not match expected formats. neonUtilities version is more recent than data publication dates. Stacking will proceed based on neonUtilities expectations. Check results carefully, and consider re-downloading data.\n")
        } else {
        cat("Downloaded data formats do not match expected formats. Data publication dates include both older and more recent data than neonUtilities package version. Stacking will proceed based on neonUtilities expectations. Check results carefully, and check for updates to neonUtilities.\n")
      }
      }
    }
    
    n <- 0
    m <- 0
    
    
    # metadata files
    # copy variables and validation files to /stackedFiles using the most recent publication date
    if(TRUE %in% stringr::str_detect(filepaths,'variables.20')) {
      varpath <- getRecentPublication(filepaths[grep("variables.20", filepaths)])[[1]]
      variables <- getVariables(varpath)   # get the variables from the chosen variables file
      v <- suppressWarnings(data.table::fread(varpath, sep=','))
      
      # if science review flags are present but missing from variables file, add variables
      if(!"science_review_flags" %in% v$table) {
        if(length(grep("science_review_flags", filepaths))>0) {
          v <- rbind(v, science_review_variables)
        }
      }
      
      vlist <- base::split(v, v$table)
    }
    
    if(TRUE %in% stringr::str_detect(filepaths,'validation')) {
      valpath <- getRecentPublication(filepaths[grep("validation", filepaths)])[[1]]
      file.copy(from = valpath, to = paste0(folder, "/stackedFiles/validation_", dpnum, ".csv"))
      messages <- c(messages, "Copied the most recent publication of validation file to /stackedFiles")
      m <- m + 1
    }
    
    # copy categoricalCodes file to /stackedFiles using the most recent publication date
    if(TRUE %in% stringr::str_detect(filepaths,'categoricalCodes')) {
      lovpath <- getRecentPublication(filepaths[grep("categoricalCodes", filepaths)])[[1]]
      file.copy(from = lovpath, to = paste0(folder, "/stackedFiles/categoricalCodes_", dpnum, ".csv"))
      messages <- c(messages, "Copied the most recent publication of categoricalCodes file to /stackedFiles")
      m <- m + 1
    }
    
    # find external lab tables (lab-current, lab-all) and stack the most recently published file from each lab
    labTables <- tables[which(tables %in% ttypes$tableName[grep("lab", ttypes$tableType)])]
    if(length(labTables)>0){
      externalLabs <- names(datafls)[grep(paste(paste('[.]', labTables, '[.]', sep=''), 
                                                       collapse='|'), names(datafls))]
      externalLabs <- unique(gsub("[0-9]{8}T[0-9]{6}Z.csv", "", externalLabs))
      
      for(j in 1:length(labTables)) {
        
        tablesj <- externalLabs[grep(paste("[.]", labTables[j], "[.]", sep=""), externalLabs)]
        if(length(tablesj)>0) {

          writeLines(paste0("Stacking table ", labTables[j]))
          
          outputLab <- data.table::rbindlist(pbapply::pblapply(as.list(tablesj), function(x, filepaths) {
            
            labpath <- getRecentPublication(filepaths[grep(x, filepaths)])
            
            if(nchar(labpath[[1]]) > 260 & Sys.info()[["sysname"]]=="Windows") {
              warning(paste("Filepath", labpath[[1]], "is", nchar(labpath[[1]]), "characters long. Filepaths longer than 260 characters can cause problems in Windows operating systems. Updating to R version 4.3.0 and higher resolves this issue. If updating R is not an option, move files closer to the root directory, or, if you are using loadByProduct(), switch to using zipsByProduct() followed by stackByTable()."))
            }
            
            outputj <- data.table::fread(labpath[[1]], header=TRUE, encoding="UTF-8")
            outputj <- assignClasses(outputj, variables)
            outputj$publicationDate <- rep(labpath[[2]], nrow(outputj))
            
            # add column for release tag, if available
            outputj$release <- rep(NA, nrow(outputj))
            dir.splitName <- strsplit(dirname(filepaths[grep(x, filepaths)]), split = "\\.")
            relind <- grep("RELEASE|PROVISIONAL|LATEST", dir.splitName[[1]])
            if(length(relind)>0) {
              outputj$release <- rep(dir.splitName[[1]][relind],
                                     nrow(outputj))
            } else {
              if(all(!is.na(reltab))) {
                if(basename(filepaths[grep(x, filepaths)]) %in% reltab$name) {
                  outputj$release <- rep(reltab$release[which(reltab$name==
                                                                basename(filepaths[grep(x, filepaths)]))],
                                         nrow(outputj))
                } else {
                  outputj$release <- rep("undetermined", nrow(outputj))
                }
              } else {
                outputj$release <- rep("undetermined", nrow(outputj))
              }
            }
            
            return(outputj)
            }, filepaths=filepaths), fill=TRUE)
          
        data.table::fwrite(outputLab, paste0(folder, "/stackedFiles/", labTables[j], ".csv"))
        
        # add publication and release field names to variables file
        if(!is.null(vlist)) {
          vtable <- which(names(vlist)==labTables[j])
          if(length(vtable==1)) {
            if("publicationDate" %in% names(outputLab)) {
              vlist[[vtable]] <- data.table::rbindlist(list(vlist[[vtable]], 
                                                            c(table=labTables[j], added_fields[5,])), fill=TRUE)
            }
            if("release" %in% names(outputLab)) {
              vlist[[vtable]] <- data.table::rbindlist(list(vlist[[vtable]], 
                                                            c(table=labTables[j], added_fields[6,])), fill=TRUE)
            }
          }
        }
        n <- n + 1
        }
      }
      tables <- setdiff(tables, labTables)
    }

    # get most recent sensor_positions file for each site and stack
    if(TRUE %in% stringr::str_detect(filepaths,'sensor_position')) {
      sensorPositionList <- unique(filepaths[grep("sensor_position", filepaths)])
      uniqueSites <- stringr::str_split(unique(basename(sensorPositionList)), "\\.")
      uniqueSites <- unique(unlist(lapply(uniqueSites, "[", 3)))
      sensorPosNames <- c("siteID","HOR.VER","sensorLocationID","sensorLocationDescription",
                          "positionStartDateTime","positionEndDateTime","referenceLocationID",
                          "referenceLocationIDDescription","referenceLocationIDStartDateTime",
                          "referenceLocationIDEndDateTime","xOffset","yOffset","zOffset","pitch",
                          "roll","azimuth","locationReferenceLatitude","locationReferenceLongitude",
                          "locationReferenceElevation","eastOffset","northOffset",
                          "xAzimuth","yAzimuth","publicationDate")
      oldSensorPosNames <- c("siteID","HOR.VER","name","description","start","end","referenceName",
                             "referenceDescription","referenceStart","referenceEnd",
                             "xOffset","yOffset","zOffset","pitch","roll","azimuth",
                             "referenceLatitude","referenceLongitude","referenceElevation",
                             "eastOffset","northOffset","xAzimuth","yAzimuth","publicationDate")

      outputSensorPositions <- data.table::rbindlist(pbapply::pblapply(as.list(uniqueSites), 
                                                                       function(x, sensorPositionList) {
        
        sppath <- getRecentPublication(sensorPositionList[grep(x, sensorPositionList)])[[1]]
        outTbl <- data.table::fread(sppath, header=TRUE, encoding="UTF-8", keepLeadingZeros = TRUE,
                                    colClasses = list(character = c('HOR.VER')))
        if(identical(nrow(outTbl), as.integer(0))) {
          return()
        }
        if('start' %in% names(outTbl)) {outTbl$start <- as.character(outTbl$start)}
        if('end' %in% names(outTbl)) {outTbl$end <- as.character(outTbl$end)}
        if('referenceStart' %in% names(outTbl)) {outTbl$referenceStart <- as.character(outTbl$referenceStart)}
        if('referenceEnd' %in% names(outTbl)) {outTbl$referenceEnd <- as.character(outTbl$referenceEnd)}
        if('positionStartDateTime' %in% names(outTbl)) {outTbl$positionStartDateTime <- as.character(outTbl$positionStartDateTime)}
        if('positionEndDateTime' %in% names(outTbl)) {outTbl$positionEndDateTime <- as.character(outTbl$positionEndDateTime)}
        if('referenceLocationIDStartDateTime' %in% names(outTbl)) {outTbl$referenceLocationIDStartDateTime <- as.character(outTbl$referenceLocationIDStartDateTime)}
        if('referenceLocationIDEndDateTime' %in% names(outTbl)) {outTbl$referenceLocationIDEndDateTime <- as.character(outTbl$referenceLocationIDEndDateTime)}
        outTbl <- makePosColumns(outTbl, sppath, x)
        # check column names, names updated in March 2023
        if(any(!names(outTbl) %in% sensorPosNames)) {
          if(all(names(outTbl) %in% oldSensorPosNames)) {
            names(outTbl)[which(names(outTbl)=="name")] <- "sensorLocationID"
            names(outTbl)[which(names(outTbl)=="description")] <- "sensorLocationDescription"
            names(outTbl)[which(names(outTbl)=="start")] <- "positionStartDateTime"
            names(outTbl)[which(names(outTbl)=="end")] <- "positionEndDateTime"
            names(outTbl)[which(names(outTbl)=="referenceName")] <- "referenceLocationID"
            names(outTbl)[which(names(outTbl)=="referenceDescription")] <- "referenceLocationIDDescription"
            names(outTbl)[which(names(outTbl)=="referenceStart")] <- "referenceLocationIDStartDateTime"
            names(outTbl)[which(names(outTbl)=="referenceEnd")] <- "referenceLocationIDEndDateTime"
            names(outTbl)[which(names(outTbl)=="referenceLatitude")] <- "locationReferenceLatitude"
            names(outTbl)[which(names(outTbl)=="referenceLongitude")] <- "locationReferenceLongitude"
            names(outTbl)[which(names(outTbl)=="referenceElevation")] <- "locationReferenceElevation"
          } else {
            outTbl <- invisible()
          }
        }
        return(outTbl)
      }, sensorPositionList=sensorPositionList), fill=TRUE)

      if(!identical(nrow(outputSensorPositions), as.integer(0))) {
        data.table::fwrite(outputSensorPositions, paste0(folder, "/stackedFiles/sensor_positions_", dpnum, ".csv"))
        messages <- c(messages, "Merged the most recent publication of sensor position files for each site and saved to /stackedFiles")
        m <- m + 1
        if(length(unique(outputSensorPositions$siteID))!=length(uniqueSites)) {
          messages <- c(messages, "There was an error in stacking one or more sensor positions files. Sensor positions table may be missing metadata from one or more sites.")
        }
        
      }
      
    }
    
    # aggregate the science_review_flags files
    if(TRUE %in% stringr::str_detect(filepaths,'science_review_flags')) {
      scienceReviewList <- unique(filepaths[grep("science_review_flags", filepaths)])

      # stack all files
      outputScienceReview <- data.table::rbindlist(pbapply::pblapply(scienceReviewList, 
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
      
      if(!identical(nrow(outputScienceReview), as.integer(0))) {
        data.table::fwrite(outputScienceReview, paste0(folder, "/stackedFiles/science_review_flags_", dpnum, ".csv"))
        messages <- c(messages, "Aggregated the science review flag files for each site and saved to /stackedFiles")
        m <- m + 1
      }
    }
    
    if(nCores > parallel::detectCores()) {
      stop(paste("The number of cores selected exceeds the available cores on your machine.  The maximum number of cores allowed is", parallel::detectCores(), "not", nCores))
    }
    
    # Make a decision on parallel processing based on the total size of directories or whether there are lots of 1_minute files
    directories <- sum(file.info(grep(list.files(folder, full.names=TRUE, pattern = 'NEON'), pattern = "stacked|*.zip", invert=TRUE, value=TRUE))$size)
    if(directories <= 25000 | nCores == 1) {
      cl <- 1
      if(nCores == 1) {
        writeLines(paste0("Stacking operation across a single core."))
      } else {
        writeLines(paste0("File requirements do not meet the threshold for automatic parallelization. Running on single core."))
      }
    } else {
      cl <- parallel::makeCluster(getOption("cl.cores", nCores),
                                  setup_strategy='sequential')
      parallel::clusterEvalQ(cl, library(data.table)) 
      writeLines(paste0("Parallelizing stacking operation across ", nCores, " cores."))
      # If error, crash, or completion , closes all clusters
      suppressWarnings(on.exit(parallel::stopCluster(cl)))
      }
    
    for(i in 1:length(tables)){
      tbltype <- unique(ttypes$tableType[which(ttypes$tableName == gsub(tables[i], pattern = "_pub", replacement = ""))])
      variables <- getVariables(varpath)  # get the variables from the chosen variables file

      writeLines(paste0("Stacking table ", tables[i]))
      file_list <- sort(union(filepaths[grep(paste(".", tables[i], "_pub.", sep=""), filepaths, fixed=T)],
                         filepaths[grep(paste(".", tables[i], ".", sep=""), filepaths, fixed=T)]))

      if(tbltype == "site-all") {
        sites <- as.list(unique(substr(basename(file_list), 10, 13)))

        tblfls <- lapply(sites, function(j, file_list) {
          tbl_list <- getRecentPublication(file_list[grep(j, file_list)])[[1]]
        }, file_list=file_list)
        
      }
      if(tbltype == "site-date") {
        tblfls <- file_list
      }
      
      stackingList <- pbapply::pblapply(tblfls, function(x, variables) {
        
        if(nchar(x) > 260 & Sys.info()[["sysname"]]=="Windows") {
          warning(paste("Filepath", x, "is", nchar(x), "characters long. Filepaths on Windows are limited to 260 characters. Move files closer to the root directory, or, if you are using loadByProduct(), switch to using zipsByProduct() followed by stackByTable()."))
        }
        tabtemp <- suppressWarnings(data.table::fread(x, header=T, 
                                                      encoding="UTF-8", keepLeadingZeros=T))
        # skip if file is empty - rare publication error
        if(identical(nrow(tabtemp), as.integer(0))) {
          return()
        }
        
        tabtemp <- assignClasses(tabtemp, variables)
        tabtemp <- makePosColumns(tabtemp, basename(x))
        
        # add column for release tag, if available
        tabtemp$release <- rep(NA, nrow(tabtemp))
        dir.splitName <- strsplit(dirname(x), split = "\\.")
        relind <- grep("RELEASE|PROVISIONAL|LATEST", dir.splitName[[1]])
        if(length(relind)==1) {
          tabtemp$release <- rep(dir.splitName[[1]][relind],
                                 nrow(tabtemp))
        } else {
          if(all(!is.na(reltab))) {
            if(basename(x) %in% reltab$name) {
              tabtemp$release <- rep(reltab$release[which(reltab$name==basename(x))],
                                     nrow(tabtemp))
            } else {
              tabtemp$release <- rep("undetermined", nrow(tabtemp))
            }
          } else {
            tabtemp$release <- rep("undetermined", nrow(tabtemp))
          }
        }
        
        return(tabtemp)
      }, variables=variables)
      
      stackedDf <- data.table::rbindlist(stackingList, fill=T)

      if(!identical(nrow(stackedDf), as.integer(0))) {
        data.table::fwrite(stackedDf, paste0(folder, "/stackedFiles/", tables[i], ".csv"),
                           nThread = nCores)
        
        # add location and publication field names to variables file
        if(!is.null(vlist)) {
          vtable <- which(names(vlist)==tables[i])
          if(length(vtable==1)) {
            if("horizontalPosition" %in% names(stackedDf)) {
              vlist[[vtable]] <- data.table::rbindlist(list(data.frame(base::cbind(table=rep(tables[i],4), 
                                                                                   added_fields[1:4,])), 
                                                            vlist[[vtable]]), fill=TRUE)
            }
            if("publicationDate" %in% names(stackedDf)) {
              vlist[[vtable]] <- data.table::rbindlist(list(vlist[[vtable]], 
                                                            c(table=tables[i], added_fields[5,])), fill=TRUE)
            }
            if("release" %in% names(stackedDf)) {
              vlist[[vtable]] <- data.table::rbindlist(list(vlist[[vtable]], 
                                                            c(table=tables[i], added_fields[6,])), fill=TRUE)
              releases <- c(releases, unique(stackedDf$release))
            }
          }
        }
        invisible(rm(stackedDf))
        n <- n + 1
      }
    }
    
    # write out complete variables file
    vfull <- data.table::rbindlist(vlist, fill=TRUE)
    utils::write.csv(vfull, paste0(folder, "/stackedFiles/variables_", dpnum, ".csv"), row.names=F)
    messages <- c(messages, "Copied the most recent publication of variable definition file to /stackedFiles")
    m <- m + 1
    
  }
  
  # get issue log
  if(!curl::has_internet()) {
    messages <- c(messages, "No internet connection, issue log file not accessed. Issue log can be found in the readme file.")
  } else {
    # token not used here, since token is not otherwise used/accessible in this function
    issues <- getIssueLog(dpID=dpID)
    if(!is.null(issues)) {
      utils::write.csv(issues, paste0(folder, "/stackedFiles/issueLog_", dpnum, ".csv"),
                       row.names=FALSE)
      m <- m + 1
    }
  }
  
  # get DOIs and generate citation(s)
  releases <- unique(releases)
  if("PROVISIONAL" %in% releases) {
    cit <- try(getCitation(dpID=dpID, release="PROVISIONAL"), silent=TRUE)
    if(!inherits(cit, "try-error")) {
      base::write(cit, paste0(folder, "/stackedFiles/citation_", dpnum, "_PROVISIONAL", ".txt"))
    }
  }
  if(length(grep("RELEASE", releases))==0) {
    releases <- releases
  } else {
    if(length(grep("RELEASE", releases))>1) {
      unlink(paste0(folder, "/stackedFiles/"), recursive=TRUE)
      stop("Multiple data releases were stacked together. This is not appropriate, check your input data.")
    } else {
      rel <- releases[grep("RELEASE", releases)]
      cit <- try(getCitation(dpID=dpID, release=rel), silent=TRUE)
      if(!inherits(cit, "try-error")) {
        base::write(cit, paste0(folder, "/stackedFiles/citation_", dpnum, "_", rel, ".txt"))
      }
    }
  }
  
  writeLines(paste0(messages, collapse = "\n"))
  writeLines(paste("Finished: Stacked", n, "data tables and", m, "metadata tables!"))
  endtime <- Sys.time()
  writeLines(paste0("Stacking took ", format((endtime-starttime), units = "auto")))
  
}
