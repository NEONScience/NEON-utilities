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
#' @param progress T or F, should progress bars and messages be printed?
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

stackDataFilesArrow <- function(folder, cloud.mode=FALSE, progress=TRUE, dpID){
  
  starttime <- Sys.time()
  releases <- character()
  
  # get the in-memory list of table types (site-date, site-all, etc.). This list must be updated often.
  # note this is no longer used - table type is inferred below. we're keeping the table in the package 
  # for reference
  ttypes <- table_types[which(table_types$productID==dpID),]
  dpnum <- substring(dpID, 5, 9)
  
  # get file names and file paths
  if(isTRUE(cloud.mode)) {
    filepaths <- folder[["files"]]
    basepaths <- folder[["filesall"]]$urlbase
    if(length(grep(pattern="endpoint_override", x=filepaths))>0) {
      filenames <- gsub(pattern="/?endpoint_override=https%3A%2F%2Fstorage.googleapis.com",
                        replacement="", x=filepaths, fixed=TRUE)
      filenames <- basename(filenames)
    } else {
      filenames <- basename(filepaths)
    }
  } else {
    # filenames without full path
    filenames <- findDatatables(folder = folder, fnames = F)
    
    # filenames with full path
    filepaths <- findDatatables(folder = folder, fnames = T)
    basepaths <- filepaths
  }
  
  # start list of tables
  stacklist <- list()
  
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

    # stacking for everything except community taxonomy
    if(!dpID %in% c("DP1.10081.001", "DP1.20086.001", "DP1.20141.001", 
                    "DP1.10081.002", "DP1.20086.002", "DP1.20141.002")) {
      
      # pass to custom stacking function
      frmlst <- stackFrameFiles(framefiles, dpID=dpID, 
                                seqType=NA_character_, 
                                cloud.mode=cloud.mode)
      
      stacklist[[frmlst[[2]]]] <- frmlst[[1]]
      
    } else {
      
      # stacking for community composition/taxonomy - split 16S and ITS
      fungifiles <- grep("[_]ITS[_]", framefiles, value=TRUE)
      bacteriafiles <- grep("[_]16S[_]", framefiles, value=TRUE)
      
      # pass to custom stacking function
      if(length(fungifiles)>0) {
        fungilst <- stackFrameFiles(fungifiles, dpID=dpID, 
                                    seqType=NA_character_, 
                                    cloud.mode=cloud.mode)
        
        stacklist[[fungilst[[2]]]] <- fungilst[[1]]
      }
      
      if(length(bacteriafiles)>0) {
        bacterialst <- stackFrameFiles(bacteriafiles, dpID=dpID, 
                                       seqType=NA_character_, 
                                       cloud.mode=cloud.mode)
        
        stacklist[[bacterialst[[2]]]] <- bacterialst[[1]]
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
  
  # if there is more than one data file, stack files
  if(length(datafls) > 1){
    
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
    
    # METADATA FILES
    
    # get readme
    if(length(grep(pattern="[.]readme[.]", x=filepaths))>0) {
      readmepath <- getRecentPublication(grep(pattern="[.]readme[.]",
                                              x=filepaths, value=TRUE))[[1]]
      readmetab <- try(arrow::read_delim_arrow(readmepath, delim="\t"), silent=TRUE)
      if(inherits(readmetab, "try-error")) {
        message("Readme file could not be read.")
      } else {
        readmetab <- data.frame(readmetab)
        names(readmetab) <- "V1"
        stacklist[[paste("readme", dpnum, sep="_")]] <- formatReadme(readmetab, dpID)
        m <- m + 1
      }
    }
    
    # get variables and validation files from the most recent publication date
    if(length(grep(pattern="variables.20", x=filepaths))>0) {
      varpath <- getRecentPublication(filepaths[grep("variables.20", filepaths)])[[1]]
      v <- try(arrow::read_csv_arrow(varpath, as_data_frame=TRUE), silent=TRUE)
      if(inherits(v, "try-error")) {
        varpath <- getRecentPublication(basepaths[grep("variables.20", basepaths)])[[1]]
        v <- try(arrow::read_csv_arrow(varpath, as_data_frame=TRUE), silent=TRUE)
      }
      
      # if science review flags are present but missing from variables file, add variables
      if(!"science_review_flags" %in% v$table) {
        if(length(grep(pattern="science_review_flags", x=filepaths))>0) {
          v <- rbind(v, science_review_variables)
        }
      }

      # if sensor positions are present but missing from variables file, add variables
      if(!"sensor_positions" %in% v$table) {
        if(length(grep(pattern="sensor_positions", x=filepaths))>0) {
          v <- rbind(v, sensor_position_variables)
        }
      }
      
      vlist <- base::split(v, v$table)
    }
    
    if(length(grep(pattern="validation", x=filepaths))>0) {
      valpath <- getRecentPublication(filepaths[grep("validation", filepaths)])[[1]]
      vals <- try(arrow::read_csv_arrow(valpath, as_data_frame=TRUE), silent=TRUE)
      if(inherits(vals, "try-error")) {
        valpath <- getRecentPublication(basepaths[grep("validation", basepaths)])[[1]]
        vals <- try(arrow::read_csv_arrow(valpath, as_data_frame=TRUE), silent=TRUE)
      }
      stacklist[[paste("validation", dpnum, sep="_")]] <- vals
      m <- m + 1
    }
    
    # get categoricalCodes file from most recent publication date
    if(length(grep(pattern="categoricalCodes", x=filepaths))>0) {
      lovpath <- getRecentPublication(filepaths[grep("categoricalCodes", filepaths)])[[1]]
      lovs <- try(arrow::read_csv_arrow(lovpath, as_data_frame=TRUE), silent=TRUE)
      if(inherits(lovs, "try-error")) {
        lovpath <- getRecentPublication(basepaths[grep("categoricalCodes", basepaths)])[[1]]
        lovs <- try(arrow::read_csv_arrow(lovpath, as_data_frame=TRUE), silent=TRUE)
      }
      stacklist[[paste("categoricalCodes", dpnum, sep="_")]] <- lovs
      m <- m + 1
    }
    
    # DATA STACKING
    # stack each table and add to list
    if(isTRUE(progress)) {
      message("Stacking data files")
      pb <- utils::txtProgressBar(style=3)
      utils::setTxtProgressBar(pb, 0)
    }
    
    for(i in 1:length(tables)) {
      tbltype <- unique(ttypes$tableType[which(ttypes$tableName == gsub(tables[i], pattern = "_pub", replacement = ""))])
      
      # create arrow schema from variables file
      tableschema <- schemaFromVar(variables=v, tab=tables[i], package=package)

      file_list <- sort(union(filepaths[grep(paste(".", tables[i], "_pub.", sep=""), filepaths, fixed=T)],
                              filepaths[grep(paste(".", tables[i], ".", sep=""), filepaths, fixed=T)]))

      # all files for site-date
      if(tbltype == "site-date") {
        tblfls <- file_list
      }
      # most recent for each site for site-all
      if(tbltype == "site-all") {
        if(length(grep(pattern="endpoint_override", x=file_list))>0) {
          filetemp <- gsub(pattern="/?endpoint_override=https%3A%2F%2Fstorage.googleapis.com",
                            replacement="", x=file_list, fixed=TRUE)
        } else {
          filetemp <- file_list
        }
        sites <- as.list(unique(substring(basename(filetemp), 10, 13)))

        tblfls <- unique(unlist(lapply(sites, function(j, file_list) {
          tbl_list <- getRecentPublication(file_list[grep(j, file_list)])[[1]]
        }, file_list=file_list)))
        
      }
      # most recent for each lab for lab-all and lab-current
      if(tbltype == "lab") {
        if(length(grep(pattern="endpoint_override", x=file_list))>0) {
          filetemp <- gsub(pattern="/?endpoint_override=https%3A%2F%2Fstorage.googleapis.com",
                           replacement="", x=file_list, fixed=TRUE)
        } else {
          filetemp <- file_list
        }
        labs <- unique(unlist(lapply(strsplit(basename(filetemp), split="[.]"), 
                                     FUN="[[", 2)))
        
        tblfls <- unique(unlist(lapply(labs, function(j, file_list) {
          tbl_list <- getRecentPublication(file_list[grep(j, file_list)])[[1]]
        }, file_list=file_list)))
      }
      
      # point to files as dataset
      dat <- arrow::open_csv_dataset(sources=tblfls, schema=tableschema, skip=1)
      
      # add file name column and stream to table
      datf <- dplyr::mutate(.data=dat, file=addFilename())
      dattab <- try(data.frame(dplyr::collect(datf)), silent=TRUE)
      
      # if stacking fails, look for multiple variables files
      if(inherits(dattab, "try-error")) {
        
        if(tables[i]=="sensor_positions") {
          # try arrow schema from old version of column names
          tableschemalegacy <- schemaFromVar(variables=sensor_position_legacy, 
                                             tab=tables[i], package=package)
          dat <- arrow::open_csv_dataset(sources=tblfls, 
                                         schema=tableschemalegacy, skip=1)
          
          # add file name column and stream to table
          datf <- dplyr::mutate(.data=dat, file=addFilename())
          dattab <- try(data.frame(dplyr::collect(datf)), silent=TRUE)
          
          if(inherits(dattab, "try-error")) {
            # try string schema
            stringschema <- schemaAllStringsFromSet(tblfls)
            dat <- arrow::open_csv_dataset(sources=tblfls, 
                                           schema=stringschema, skip=1)
            
            # add file name column and stream to table
            datf <- dplyr::mutate(.data=dat, file=addFilename())
            dattab <- try(data.frame(dplyr::collect(datf)), silent=TRUE)
            
            if(inherits(dattab, "try-error")) {
              message("Stacking sensor positions files failed. Try excluding provisional data, and contact NEON if unable to resolve.")
            }
          }
        } else {
          
          varpaths <- filepaths[grep("variables.20", filepaths)]
          # get or calculate checksums and separate unique variables files
          varset <- list()
          if(isTRUE(cloud.mode)) {
            # go back to the original list of files to get the metadata; have to re-subset to table
            flset <- folder[["filesall"]]
            flset <- flset[sort(union(grep(paste(".", tables[i], "_pub.", sep=""), flset$url, fixed=T),
                                      grep(paste(".", tables[i], ".", sep=""), flset$url, fixed=T))),]
            varu <- unique(flset$md5var)
            for(k in varu) {
              varset[[k]] <- flset$url[which(flset$md5var==k)][1]
            }
          } else {
            md5var <- tools::md5sum(varpaths)
            varu <- unique(md5var)
            for(k in varu) {
              varset[[k]] <- getRecentPublication(varpaths[which(md5var==k)])[[1]]
            }
            # for local files, match variables file to specific data files by publication date
            varPubDate <- regmatches(basename(varpaths), 
                                     regexpr("[0-9]{8}T[0-9]{6}Z", 
                                             basename(varpaths)))
            tblPubDate <- regmatches(basename(tblfls), 
                                     regexpr("[0-9]{8}T[0-9]{6}Z", 
                                             basename(tblfls)))
            flset <- cbind(tblfls, tblPubDate)
            flset <- data.frame(flset)
            names(flset) <- c("url", "urlPubDate")
            flset$urlvar <- NA
            flset$md5var <- NA
            for(b in 1:nrow(flset)) {
              mpath <- varpaths[which(varPubDate==flset$urlPubDate[b])]
              m5path <- md5var[which(varPubDate==flset$urlPubDate[b])]
              if(length(mpath)==0) {
                dist <- as.numeric(substring(flset$urlPubDate[b], 10, 15)) - 
                  as.numeric(substring(flset$varPubDate[b], 10, 15))
                mpath <- varpaths[which(dist==min(dist))]
                m5path <- md5var[which(dist==min(dist))]
              }
              flset$urlvar[b] <- mpath[1]
              flset$md5var[b] <- m5path[1]
            }
          }
          
          # if variables files match, go straight to string schema
          trystring <- FALSE
          if(length(varset)==1) {
            trystring <- TRUE
          } else {
            # check for field name differences among files
            varFieldDiff <- checkVarFields(variableSet=varset, tableName=tables[i])
            if(isFALSE(varFieldDiff)) {
              # if field names match, go to string schema
              trystring <- TRUE
            } else {
              # if there are inconsistencies, read each separately, then unify
              mdlist <- flset$md5var
              tablist <- list()
              piecewise <- TRUE
              trystring <- FALSE
              for(p in unique(mdlist)) {
                
                varp <- getRecentPublication(flset$urlvar[which(mdlist==p)])[[1]]
                flsp <- flset$url[which(mdlist==p)]
                
                ds <- try(arrow::open_csv_dataset(sources=flsp, 
                                                  schema=schemaFromVar(varp,
                                                                       tab=tables[i],
                                                                       package=package),
                                                  skip=1), silent=TRUE)
                
                if(inherits(ds, "try-error")) {
                  piecewise <- FALSE
                  next
                } else {
                  tablist[[p]] <- ds
                }
                
              }
              
              # if any chunks failed, try for a string schema
              if(isFALSE(piecewise)) {
                trystring <- TRUE
              } else {
                # if all chunks succeeded, merge them
                ds <- try(arrow::open_csv_dataset(sources=tablist, 
                                                  unify_schemas=TRUE,
                                                  skip=0), silent=TRUE)
                
                # add file name column and stream to table
                datf <- try(dplyr::mutate(.data=ds, file=addFilename()), silent=TRUE)
                dattab <- try(data.frame(dplyr::collect(datf)), silent=TRUE)
                
                # if merge fails, try for a string schema
                if(inherits(dattab, "try-error")) {
                  trystring <- TRUE
                }
              }
            }
          }
          
          # if making dataset via any path above failed, try a string schema
          if(isTRUE(trystring)) {
            message(paste("Data retrieval using variables file to generate schema failed for table ", tables[i], ". All fields will be read as strings. This can be slow, and can usually be avoided by excluding provisional data.", sep=""))
            stringtablist <- list()
            stringpiecewise <- TRUE
            for(p in unique(flset$md5var)) {
              
              flsetp <- flset[which(flset$md5var==p),]
              flsp <- flsetp$url
              stringschema <- schemaAllStringsFromSet(flsp)
              
              ds <- try(arrow::open_csv_dataset(sources=flsp, 
                                                schema=stringschema,
                                                skip=1), silent=TRUE)
              if(inherits(ds, "try-error")) {
                stringpiecewise <- FALSE
                next
              } else {
                stringtablist[[p]] <- ds
              }
              
            }
            
            if(isFALSE(stringpiecewise)) {
              message(paste("Reading data as strings failed for table ", tables[i], ". Try excluding provisional data, and contact NEON if unable to resolve.", sep=""))
              next
            } else {
              # if all chunks succeeded, merge them
              ds <- try(arrow::open_csv_dataset(sources=stringtablist, 
                                                unify_schemas=TRUE,
                                                skip=0), silent=TRUE)
              
              # add file name column and stream to table
              datf <- try(dplyr::mutate(.data=ds, file=addFilename()), silent=TRUE)
              dattab <- try(data.frame(dplyr::collect(datf)), silent=TRUE)
              
              if(inherits(dattab, "try-error")) {
                message(paste("Reading data as strings failed for table ", tables[i], ". Try excluding provisional data, and contact NEON if unable to resolve.", sep=""))
                next
              }
            }
          }
        }
      }
      
      # for SRF tables, remove duplicates and updated records
      if(tables[i]=="science_review_flags") {
        dattab <- removeSrfDups(dattab)
      }
      
      # append publication date
      dattab$publicationDate <- regmatches(basename(dattab$file), 
                                           regexpr("[0-9]{8}T[0-9]{6}Z", 
                                                   basename(dattab$file)))
      # append release tag
      if(isFALSE(cloud.mode)) {
        reldat <- regmatches(dattab$file, regexpr("20[0-9]{6}T[0-9]{6}Z\\..*\\/", 
                                                  dattab$file))
        rtry <- try(dattab$release <- gsub(pattern=".*\\.|\\/", 
                                           replacement="", x=reldat), 
                    silent=TRUE)
        if(inherits(rtry, "try-error")) {
          message(paste("\nRelease tag could not be determined for table ", 
                        tables[i], sep=""))
        }
      } else {
        relmap <- folder[["filesall"]][,c("urlbase","release")]
        relmap$urlbase <- gsub(pattern="https://storage.googleapis.com/",
                               replacement="", x=relmap$urlbase)
        dattab <- base::merge(dattab, relmap, by.x="file", by.y="urlbase", 
                              all.x=TRUE)
      }
      releases <- c(releases, unique(dattab$release))
      
      # for IS products, append domainID, siteID, HOR, VER
      if(tbltype!="lab" & !"siteID" %in% names(dattab)) {
        domainID <- regmatches(dattab$file, regexpr("D[0-2]{1}[0-9]{1}", 
                                                    dattab$file))
        siteID <- regmatches(dattab$file, regexpr("[.]D[0-9]{2}[.][A-Z]{4}[.]|[.]D[0-9]{2}[.][A-Z]{2}[0-9]{2}[.]", 
                                                  dattab$file))
        siteID <- gsub(pattern="[.]D[0-9]{2}[.]|[.]", replacement="", x=siteID)
        locinds <- regmatches(dattab$file, regexpr("[.][0-9]{3}[.][0-9]{3}[.][0-9]{3}[.][0-9]{3}[.]|[.][0-9]{3}[.][0-9]{3}[.][0-9]{3}[.][0-9]{2}[A-Z]{1}[.]", 
                                                   dattab$file))
        # a few tables are missing domainID and siteID but don't have hor/ver
        if(identical(length(locinds), as.integer(0))) {
          dattab <- cbind(domainID, siteID, dattab)
          dattab <- dattab[order(dattab$domainID, dattab$siteID),]
        } else {
          horizontalPosition <- substring(locinds, 6, 8)
          verticalPosition <- substring(locinds, 10, 12)
          dattab <- cbind(domainID, siteID, horizontalPosition,
                          verticalPosition, dattab)
          if("endDateTime" %in% names(dattab)) {
            dattab <- dattab[order(dattab$domainID, dattab$siteID,
                                   dattab$horizontalPosition,
                                   dattab$verticalPosition, 
                                   dattab$endDateTime),]
          } else {
            dattab <- dattab[order(dattab$domainID, dattab$siteID,
                                   dattab$horizontalPosition,
                                   dattab$verticalPosition),]
          }
        }
      }
      
      # get rid of filename column
      dattab <- dattab[,which(colnames(dattab)!="file")]

      # add location and publication field names to variables file
      if(!is.null(vlist)) {
        vtable <- which(names(vlist)==tables[i])
        if(length(vtable==1)) {
          if("horizontalPosition" %in% names(dattab)) {
            vlist[[vtable]] <- data.table::rbindlist(list(data.frame(base::cbind(table=rep(tables[i],4), 
                                                                                 added_fields[1:4,])), 
                                                          vlist[[vtable]]), 
                                                     fill=TRUE, ignore.attr=TRUE)
          }
          if("publicationDate" %in% names(dattab)) {
            vlist[[vtable]] <- data.table::rbindlist(list(vlist[[vtable]], 
                                                          c(table=tables[i], 
                                                            added_fields[5,])), 
                                                     fill=TRUE, ignore.attr=TRUE)
          }
          if("release" %in% names(dattab)) {
            vlist[[vtable]] <- data.table::rbindlist(list(vlist[[vtable]], 
                                                          c(table=tables[i], 
                                                            added_fields[6,])), 
                                                     fill=TRUE, ignore.attr=TRUE)
            releases <- c(releases, unique(dattab$release))
          }
        }
      }
      
      # add stacked table to list
      if(tables[i] %in% c("science_review_flags", "sensor_positions")) {
        stacklist[[paste(tables[i], dpnum, sep="_")]] <- dattab
      } else {
        stacklist[[tables[i]]] <- dattab
      }
      n <- n + 1
      if(isTRUE(progress)) {
        utils::setTxtProgressBar(pb, i/length(tables))
      }
    }
  
    # write out complete variables file after all tables are done
    vfull <- data.table::rbindlist(vlist, fill=TRUE, ignore.attr=TRUE)
    stacklist[[paste("variables", dpnum, sep="_")]] <- vfull
    m <- m + 1
    
  }
  
  # get issue log
  if(!curl::has_internet()) {
    message("No internet connection, issue log file not accessed. Issue log can be found in the readme file.")
  } else {
    # token not used here, since token is not otherwise used/accessible in this function
    issues <- getIssueLog(dpID=dpID)
    if(!is.null(issues)) {
      stacklist[[paste("issueLog", dpnum, sep="_")]] <- issues
      m <- m + 1
    }
  }
  
  if(isTRUE(progress)) {
    utils::setTxtProgressBar(pb, 1)
    close(pb)
  }
  
  # get DOIs and generate citation(s)
  releases <- unique(releases)
  if("PROVISIONAL" %in% releases) {
    cit <- try(getCitation(dpID=dpID, release="PROVISIONAL"), silent=TRUE)
    if(!inherits(cit, "try-error")) {
      stacklist[[paste("citation", dpnum, "PROVISIONAL", sep="_")]] <- cit
    }
  }
  if(length(grep("RELEASE", releases))==0) {
    releases <- releases
  } else {
    if(length(grep("RELEASE", releases))>1) {
      stop("Multiple data releases were stacked together. This is not appropriate, check your input data.")
    } else {
      rel <- releases[grep("RELEASE", releases)]
      cit <- try(getCitation(dpID=dpID, release=rel), silent=TRUE)
      if(!inherits(cit, "try-error")) {
        stacklist[[paste("citation", dpnum, rel, sep="_")]] <- cit
      }
    }
  }
  
  # order tables in stacklist
  stacklist <- stacklist[order(names(stacklist))]
  
  if(isTRUE(progress)) {
    message(paste("Finished: Stacked", n, "data tables and", m, "metadata tables!"))
    endtime <- Sys.time()
    message(paste0("Stacking took ", format((endtime-starttime), units = "auto")))
  }
  
  return(stacklist)
  
}
