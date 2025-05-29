##############################################################################################
#' @title Join data files in a unzipped NEON data package by table type

#' @author
#' Christine Laney \email{claney@battelleecology.org}
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Given a folder of unzipped files (unzipped NEON data file), do a full join of all data files, grouped by table type.
#' This should result in a small number of large files.

#' @param folder The location of the data
#' @param cloud.mode T or F, are data transferred from one cloud environment to another? If T, this function returns a list of url paths to data files.
#' @param nCores The number of cores to parallelize the stacking procedure. To automatically use the maximum number of cores on your machine we suggest setting 'nCores=parallel::detectCores()'. By default it is set to a single core. If the files are less than 25000 bytes the userdefined nCores will be overridden to a single core.
#' @param dpID The data product identifier
#' @return One file for each table type is created and written.
#' @keywords internal

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
#   2025-05-27 (Claire Lunch)
#     * Rewrote from stackDataFilesParallel() to use arrow package for stacking
##############################################################################################

stackDataFilesArrow <- function(folder, cloud.mode=FALSE, nCores=1, dpID){
  
  starttime <- Sys.time()
  releases <- character()
  
  # get the in-memory list of table types (site-date, site-all, etc.). This list must be updated often.
  # note this is no longer used - table type is inferred below. we're keeping the table in the package 
  # for reference
  ttypes <- table_types[which(table_types$productID==dpID),]
  dpnum <- substring(dpID, 5, 9)
  
  if(isTRUE(cloud.mode)) {
    filenames <- basename(folder[[1]]) # this doesn't work for S3 override
    filpaths <- folder[[1]]
  } else {
    # filenames without full path
    filenames <- findDatatables(folder = folder, fnames = F)
    
    # filenames with full path
    filepaths <- findDatatables(folder = folder, fnames = T)
  }
  
  # get release file, if it exists
  relfl <- grep("release_status", filepaths)
  if(length(relfl)==1) {
    reltab <- data.table::fread(filepaths[relfl],
                                header=TRUE, encoding="UTF-8")
  } else {
    reltab <- NA
  }
  
  # handle per-sample (data frame) tables separately
  if(dpID %in% c("DP1.30012.001", "DP1.10081.001", "DP1.20086.001", 
                 "DP1.20141.001", "DP1.20190.001", "DP1.20193.001",
                 "DP1.10081.002", "DP1.20086.002", "DP1.20141.002",
                 "DP4.00132.001") & 
     length(grep("^NEON.", basename(filenames), invert=TRUE))>0) {
    framefiles <- filepaths[grep("^NEON.", basename(filenames), invert=TRUE)]
    filepaths <- filepaths[grep("^NEON.", basename(filenames))]
    filenames <- filenames[grep("^NEON.", basename(filenames))]
    
    # stack frame files
    message("Stacking per-sample files. These files may be very large; download data in smaller subsets if performance problems are encountered.")
    if(dir.exists(paste0(folder, "/stackedFiles")) == F) {dir.create(paste0(folder, "/stackedFiles"))}
    
    # this is clunky - streamline in v3.0
    # stacking for everything except community taxonomy
    if(!dpID %in% c("DP1.10081.002", "DP1.20086.002", "DP1.20141.002")) {
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
          if(dpID=="DP4.00132.001") {
            data.table::fwrite(frm, paste0(folder, "/stackedFiles/", "bat_processedSonarFile", ".csv"))
          } else {
            data.table::fwrite(frm, paste0(folder, "/stackedFiles/", "per_sample", ".csv"))
          }
        }
      }
    } else {
      fungifiles <- grep("[_]ITS[_]", framefiles, value=TRUE)
      bacteriafiles <- grep("[_]16S[_]", framefiles, value=TRUE)
      
      # stack ITS data
      if(length(fungifiles)>0) {
        fungifrm <- data.table::rbindlist(pbapply::pblapply(as.list(fungifiles), function(x) {
          tempf <- data.table::fread(x)
          tempf$fileName <- rep(basename(x), nrow(tempf))
          return(tempf)
        }), fill=TRUE)
        
        if(nrow(fungifrm)>0) {
          if(dpID=="DP1.10081.002") {
            data.table::fwrite(fungifrm, paste0(folder, "/stackedFiles/", "mct_soilPerSampleTaxonomy_ITS", ".csv"))
          }
          if(dpID=="DP1.20086.002") {
            data.table::fwrite(fungifrm, paste0(folder, "/stackedFiles/", "mct_benthicPerSampleTaxonomy_ITS", ".csv"))
          }
          if(dpID=="DP1.20141.002") {
            data.table::fwrite(fungifrm, paste0(folder, "/stackedFiles/", "mct_surfaceWaterPerSampleTaxonomy_ITS", ".csv"))
          }
        }
      }
      
      # stack 16S data
      if(length(bacteriafiles)>0) {
        bactfrm <- data.table::rbindlist(pbapply::pblapply(as.list(bacteriafiles), function(x) {
          tempf <- data.table::fread(x)
          tempf$fileName <- rep(basename(x), nrow(tempf))
          return(tempf)
        }), fill=TRUE)
        
        if(dpID=="DP1.10081.002") {
          data.table::fwrite(bactfrm, paste0(folder, "/stackedFiles/", "mct_soilPerSampleTaxonomy_16S", ".csv"))
        }
        if(dpID=="DP1.20086.002") {
          data.table::fwrite(bactfrm, paste0(folder, "/stackedFiles/", "mct_benthicPerSampleTaxonomy_16S", ".csv"))
        }
        if(dpID=="DP1.20141.002") {
          data.table::fwrite(bactfrm, paste0(folder, "/stackedFiles/", "mct_surfaceWaterPerSampleTaxonomy_16S", ".csv"))
        }
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
    
    # need to change this - folder can be a list of filepaths (cloud.mode)
    if(dir.exists(paste0(folder, "/stackedFiles")) == F) {dir.create(paste0(folder, "/stackedFiles"))}
    
    # detecting table types by file format, then checking against table_types
    # reducing dependency on table_types updating
    tableForm <- findTablesByFormat(names(datafls))
    
    # as of v2.4.1, removing table type check - follow the type inferred by publication format
    # keeping table_types updated in the package for reference
    ttypes <- tableForm
    if(length(grep(pattern="sensor_positions", x=filepaths))>0) {
      ttypes <- rbind(ttypes, c("sensor_positions", "site-all"))
    }
    if(length(grep(pattern="science_review_flags", x=filepaths))>0) {
      ttypes <- rbind(ttypes, c("science_review_flags", "site-date"))
    }
    tables <- ttypes$tableName
    
    package <- ifelse(any(length(grep(pattern="expanded", x=filepaths))>0),
                      "expanded", "basic")
    
    n <- 0
    m <- 0
    stacklist <- list()
    
    # METADATA FILES
    # get variables and validation files from the most recent publication date
    if(length(grep(pattern="variables.20", x=filepaths))>0) {
      varpath <- getRecentPublication(filepaths[grep("variables.20", filepaths)])[[1]]
      # get the variables from the chosen variables file
      variables <- getVariables(varpath)
      v <- suppressWarnings(data.table::fread(varpath, sep=','))
      
      # if science review flags are present but missing from variables file, add variables
      if(!"science_review_flags" %in% v$table) {
        if(length(grep(pattern="science_review_flags", x=filepaths))>0) {
          v <- rbind(v, science_review_variables)
        }
      }
      
      vlist <- base::split(v, v$table)
    }
    
    if(length(grep(pattern="validation", x=filepaths))>0) {
      valpath <- getRecentPublication(filepaths[grep("validation", filepaths)])[[1]]
      vals <- suppressWarnings(data.table::fread(valpath, sep=','))
      stacklist[[paste("validation", dpnum, sep="_")]] <- vals
      m <- m + 1
    }
    
    # get categoricalCodes file from most recent publication date
    if(length(grep(pattern="categoricalCodes", x=filepaths))>0) {
      lovpath <- getRecentPublication(filepaths[grep("categoricalCodes", filepaths)])[[1]]
      lovs <- suppressWarnings(data.table::fread(lovpath, sep=','))
      stacklist[[paste("categoricalCodes", dpnum, sep="_")]] <- lovs
      m <- m + 1
    }
    
    # aggregate the science_review_flags files
    # need to move this post-stacking
    if(length(grep(pattern="science_review_flags", x=filepaths))>0) {
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
        message("Aggregated the science review flag files for each site and saved to /stackedFiles")
        m <- m + 1
      }
    }
    
    # DATA STACKING
    # stack each table and add to list
    for(i in 1:length(tables)) {
      tbltype <- unique(ttypes$tableType[which(ttypes$tableName == gsub(tables[i], pattern = "_pub", replacement = ""))])
      
      # create arrow schema from variables file
      tableschema <- schemaFromVar(variables=v, tab=tables[i], package=package)

      message(paste0("Stacking table ", tables[i]))
      file_list <- sort(union(filepaths[grep(paste(".", tables[i], "_pub.", sep=""), filepaths, fixed=T)],
                         filepaths[grep(paste(".", tables[i], ".", sep=""), filepaths, fixed=T)]))

      # all files for site-date
      if(tbltype == "site-date") {
        tblfls <- file_list
      }
      # most recent for each site for site-all
      if(tbltype == "site-all") {
        sites <- as.list(unique(substr(basename(file_list), 10, 13)))

        tblfls <- lapply(sites, function(j, file_list) {
          tbl_list <- getRecentPublication(file_list[grep(j, file_list)])[[1]]
        }, file_list=file_list)
        
      }
      # most recent for each lab for lab-all
      if(tbltype == "lab") {
        labs <- unique(unlist(lapply(strsplit(file_list, split="[.]"), 
                                     FUN="[[", 2)))
        
        tblfls <- lapply(labs, function(j, file_list) {
          tbl_list <- getRecentPublication(file_list[grep(j, file_list)])[[1]]
        }, file_list=file_list)
      }
      
      # point to files as dataset
      # need to set up options for missing or inconsistent schema
      dat <- arrow::open_csv_dataset(sources=tblfls, schema=tableschema, skip=1)
      
      # add file name column and stream to table
      datf <- dplyr::mutate(.data=dat, file=arrow::add_filename())
      dattab <- data.frame(dplyr::collect(datf))
      

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
    message("Copied the most recent publication of variable definition file to /stackedFiles")
    m <- m + 1
    
  }
  
  # get issue log
  if(!curl::has_internet()) {
    message("No internet connection, issue log file not accessed. Issue log can be found in the readme file.")
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
  
  message(paste("Finished: Stacked", n, "data tables and", m, "metadata tables!"))
  endtime <- Sys.time()
  message(paste0("Stacking took ", format((endtime-starttime), units = "auto")))
  
}
